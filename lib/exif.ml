(* zikh — a stateless, non-destructive photo organizer.
 * SPDX-License-Identifier: ISC
 * lib/exif.ml — native EXIF extraction for JPEG, TIFF, PNG, and HEIC. *)


let tag_exif_ifd          = 0x8769
let tag_datetime_original = 0x9003
let tag_create_date       = 0x9004


type bo = LE | BE

let get8 buf off = Char.code (Bytes.get buf off)

let get16 bo buf off = match bo with
  | LE ->  get8 buf off        lor (get8 buf (off + 1) lsl 8)
  | BE -> (get8 buf off lsl 8) lor  get8 buf (off + 1)

let get32 bo buf off = match bo with
  | LE ->
    get8 buf  off
    lor (get8 buf (off + 1) lsl  8)
    lor (get8 buf (off + 2) lsl 16)
    lor (get8 buf (off + 3) lsl 24)
  | BE ->
    (get8 buf  off        lsl 24)
    lor (get8 buf (off + 1) lsl 16)
    lor (get8 buf (off + 2) lsl  8)
    lor  get8 buf (off + 3)


let trim_null s =
  match String.index_opt s '\x00' with
  | Some i -> String.sub s 0 i
  | None   -> s

let find_tag bo buf tiff_base ifd_off target =
  let n = get16 bo buf (tiff_base + ifd_off) in
  let rec loop i =
    if i >= n then None
    else
      let e = tiff_base + ifd_off + 2 + i * 12 in
      if get16 bo buf e = target
      then Some (get32 bo buf (e + 8))
      else loop (i + 1)
  in
  loop 0

let read_ascii_tag bo buf tiff_base ifd_off target =
  let n = get16 bo buf (tiff_base + ifd_off) in
  let rec loop i =
    if i >= n then None
    else
      let e    = tiff_base + ifd_off + 2 + i * 12 in
      let tag  = get16 bo buf e in
      let typ  = get16 bo buf (e + 2) in
      let cnt  = get32 bo buf (e + 4) in
      let voff = get32 bo buf (e + 8) in
      if tag = target && typ = 2 && cnt > 1 then
        let off = tiff_base + voff in
        let len = min (cnt - 1) (Bytes.length buf - off) in
        if len < 1 then None
        else Some (trim_null (Bytes.sub_string buf off len))
      else loop (i + 1)
  in
  loop 0

let parse_tiff buf tiff_base =
  let b0 = get8 buf tiff_base and b1 = get8 buf (tiff_base + 1) in
  let bo = match b0, b1 with
    | 0x49, 0x49 -> LE
    | 0x4D, 0x4D -> BE
    | _          -> raise Exit
  in
  if get16 bo buf (tiff_base + 2) <> 42 then raise Exit;
  let ifd0_off = get32 bo buf (tiff_base + 4) in
  let dto, cd =
    match find_tag bo buf tiff_base ifd0_off tag_exif_ifd with
    | Some exif_off ->
      let dto = read_ascii_tag bo buf tiff_base exif_off tag_datetime_original in
      let cd  = read_ascii_tag bo buf tiff_base exif_off tag_create_date in
      dto, cd
    | None -> None, None
  in
  let dto = match dto with
    | Some _ -> dto
    | None   -> read_ascii_tag bo buf tiff_base ifd0_off tag_datetime_original
  in
  let cd = match cd with
    | Some _ -> cd
    | None   -> read_ascii_tag bo buf tiff_base ifd0_off tag_create_date
  in
  dto, cd


let read_n ic n =
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  buf

let read_be16 ic =
  let b = read_n ic 2 in
  (get8 b 0 lsl 8) lor get8 b 1

let read_be32 ic =
  let b = read_n ic 4 in
  (get8 b 0 lsl 24) lor (get8 b 1 lsl 16) lor (get8 b 2 lsl 8) lor get8 b 3

let parse_jpeg ic =
  let soi = read_n ic 2 in
  if get8 soi 0 <> 0xFF || get8 soi 1 <> 0xD8 then raise Exit;
  let rec scan () =
    if input_byte ic <> 0xFF then raise Exit;
    let marker = input_byte ic in
    match marker with
    | 0xD9 ->
      None, None
    | 0xE1 ->
      let len     = read_be16 ic in
      let payload = read_n ic (len - 2) in
      if Bytes.length payload >= 8
      && Bytes.sub_string payload 0 6 = "Exif\x00\x00"
      then (try parse_tiff payload 6 with _ -> None, None)
      else scan ()
    | m when (m >= 0xE0 && m <= 0xEF) || m = 0xFE ->
      let len = read_be16 ic in
      seek_in ic (pos_in ic + len - 2);
      scan ()
    | _ ->
      None, None
  in
  scan ()

let parse_tiff_file ic =
  let size = in_channel_length ic in
  let cap  = min size 65536 in
  let buf  = Bytes.create cap in
  really_input ic buf 0 cap;
  parse_tiff buf 0

let parse_png ic =
  let hdr = read_n ic 8 in
  if Bytes.sub_string hdr 0 8 <> "\x89PNG\r\n\x1a\n" then raise Exit;
  let rec scan () =
    let len = read_be32 ic in
    let typ = Bytes.sub_string (read_n ic 4) 0 4 in
    match typ with
    | "IEND" ->
      None, None
    | "eXIf" ->
      let data = read_n ic len in
      ignore (read_n ic 4);
      (try parse_tiff data 0 with _ -> None, None)
    | _ ->
      seek_in ic (pos_in ic + len + 4);
      scan ()
  in
  scan ()

let parse_heic ic =
  let size = in_channel_length ic in
  let cap  = min size (1024 * 1024) in
  let buf  = Bytes.create cap in
  really_input ic buf 0 cap;
  let n      = Bytes.length buf in
  let result = ref (None, None) in
  (try
    for i = 0 to n - 8 do
      let b0 = get8 buf i and b1 = get8 buf (i + 1) in
      let b2 = get8 buf (i + 2) and b3 = get8 buf (i + 3) in
      let is_tiff =
        (b0 = 0x49 && b1 = 0x49 && b2 = 0x2A && b3 = 0x00)
        || (b0 = 0x4D && b1 = 0x4D && b2 = 0x00 && b3 = 0x2A)
      in
      if is_tiff then
        (try
          let r = parse_tiff buf i in
          (match r with
           | (Some _, _) | (_, Some _) ->
             result := r;
             raise Exit
           | _ -> ())
        with Exit -> raise Exit | _ -> ())
    done
  with Exit -> ());
  !result

let read path =
  try
    let ic = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
      let magic = read_n ic 12 in
      seek_in ic 0;
      try
        if get8 magic 0 = 0xFF && get8 magic 1 = 0xD8 then
          parse_jpeg ic
        else if Bytes.sub_string magic 0 8 = "\x89PNG\r\n\x1a\n" then
          parse_png ic
        else if (get8 magic 0 = 0x49 && get8 magic 1 = 0x49
                 && get8 magic 2 = 0x2A && get8 magic 3 = 0x00)
             || (get8 magic 0 = 0x4D && get8 magic 1 = 0x4D
                 && get8 magic 2 = 0x00 && get8 magic 3 = 0x2A) then
          parse_tiff_file ic
        else if Bytes.length magic >= 8
             && Bytes.sub_string magic 4 4 = "ftyp" then
          parse_heic ic
        else
          None, None
      with _ -> None, None)
  with _ -> None, None
