(* zikh — a stateless, non-destructive photo organizer.
 * SPDX-License-Identifier: ISC
 * lib/plan.ml — planning phase. *)

open Types

let supported_extensions = [".jpg"; ".jpeg"; ".heic"; ".png"; ".tiff"; ".tif"]

let is_supported ext =
  List.mem (String.lowercase_ascii ext) supported_extensions


let parse_timestamp s =
  if String.length s <> 19 then None
  else if s.[4]  <> ':' || s.[7]  <> ':' || s.[10] <> ' '
       || s.[13] <> ':' || s.[16] <> ':' then None
  else
    match
      int_of_string_opt (String.sub s  0 4),
      int_of_string_opt (String.sub s  5 2),
      int_of_string_opt (String.sub s  8 2),
      int_of_string_opt (String.sub s 11 2),
      int_of_string_opt (String.sub s 14 2),
      int_of_string_opt (String.sub s 17 2)
    with
    | Some year, Some month, Some day, Some hour, Some min, Some sec
      when year  >= 1970
        && month >= 1  && month <= 12
        && day   >= 1  && day   <= 31
        && hour  <= 23
        && min   <= 59
        && sec   <= 59 ->
      Some { year; month; day; hour; min; sec }
    | _ -> None

let select_timestamp dto cd =
  let parse_opt = function Some s -> parse_timestamp s | None -> None in
  match parse_opt dto with
  | Some _ as ts -> ts
  | None         -> parse_opt cd

let path_exists path =
  match Unix.lstat path with
  | _                                              -> true
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false

let resolve ~claimed ~existing dir base ext =
  let candidate n =
    let fname =
      if n = 0 then base ^ ext
      else Printf.sprintf "%s-%d%s" base n ext
    in
    dir ^ "/" ^ fname
  in
  let rec loop n =
    let c = candidate n in
    if Hashtbl.mem claimed c || Hashtbl.mem existing c || path_exists c
    then loop (n + 1)
    else c
  in
  let path = loop 0 in
  Hashtbl.replace claimed path ();
  path


let make_plan ~sources ~metadata ~dest_root ~existing =
  let claimed      = Hashtbl.create 64 in
  let existing_tbl = Hashtbl.create (List.length existing) in
  List.iter (fun p -> Hashtbl.replace existing_tbl p ()) existing;

  let plan_one sf =
    let { abs_path; ext } = sf in
    if String.contains abs_path '\n' then
      Err (sf, Exiftool_failure
             "path contains newline; cannot transmit to daemon")
    else if not (is_supported ext) then
      Skip sf
    else
      let dto, cd = match List.assoc_opt abs_path metadata with
        | Some pair -> pair
        | None      -> (None, None)
      in
      let name = Filename.basename abs_path in
      let stem = String.sub name 0 (String.length name - String.length ext) in
      match select_timestamp dto cd with
      | None ->
        let dest_path =
          resolve ~claimed ~existing:existing_tbl
            (dest_root ^ "/unparsed") stem ext
        in
        Unparsed (sf, { abs_path = dest_path })
      | Some ts ->
        let dir  = Printf.sprintf "%s/%04d/%02d"
                     dest_root ts.year ts.month in
        let base = Printf.sprintf "%04d-%02d-%02d_%02d-%02d-%02d_%s"
                     ts.year ts.month ts.day ts.hour ts.min ts.sec stem in
        let dest_path =
          resolve ~claimed ~existing:existing_tbl dir base ext
        in
        if dest_path = abs_path then Skip sf
        else Move (sf, { abs_path = dest_path })
  in

  sources
  |> List.sort (fun (a : source_file) b -> String.compare a.abs_path b.abs_path)
  |> List.map plan_one
