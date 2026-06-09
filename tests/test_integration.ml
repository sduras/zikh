(* zikh — a stateless, non-destructive photo organizer.
 * SPDX-License-Identifier: ISC
 * tests/test_integration.ml — integration tests against a real filesystem. *)

open Zikh.Types


let rec rm_rf path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
      let dh = Unix.opendir path in
      let entries = ref [] in
      (try while true do
        let e = Unix.readdir dh in
        if e <> "." && e <> ".." then
          entries := Filename.concat path e :: !entries
      done with End_of_file -> ());
      Unix.closedir dh;
      List.iter rm_rf !entries;
      Unix.rmdir path
    | _ -> Unix.unlink path
  with Unix.Unix_error _ -> ()

let with_temp_dir f =
  let dir = Filename.temp_file "zikh_itest_" "" in
  Unix.unlink dir;
  Unix.mkdir dir 0o755;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

let write_file path content =
  let oc = open_out_bin path in
  output_string oc content;
  close_out oc

let touch path = write_file path ""

let jpeg_with_dto =
  "\xFF\xD8\xFF\xE1\x00\x48\x45\x78\x69\x66\x00\x00"
  ^ "\x49\x49\x2A\x00\x08\x00\x00\x00"
  ^ "\x01\x00\x69\x87\x04\x00\x01\x00\x00\x00\x1A\x00\x00\x00\x00\x00\x00\x00"
  ^ "\x01\x00\x03\x90\x02\x00\x14\x00\x00\x00\x2C\x00\x00\x00\x00\x00\x00\x00"
  ^ "\x32\x30\x32\x34\x3A\x30\x36\x3A\x31\x35\x20\x31\x34\x3A\x33\x32\x3A\x30\x38\x00"
  ^ "\xFF\xD9"

let jpeg_no_exif =
  "\xFF\xD8\xFF\xD9"

let test_discover_collects_regular_files () =
  with_temp_dir (fun dir ->
    touch (Filename.concat dir "a.jpg");
    touch (Filename.concat dir "b.png");
    let files = Zikh.Discover.discover dir in
    Alcotest.(check int) "two files" 2 (List.length files);
    let paths = List.sort String.compare
      (List.map (fun (sf : source_file) -> sf.abs_path) files) in
    Alcotest.(check string) "first"
      (Filename.concat dir "a.jpg") (List.nth paths 0);
    Alcotest.(check string) "second"
      (Filename.concat dir "b.png") (List.nth paths 1))

let test_discover_recurses_into_subdirs () =
  with_temp_dir (fun dir ->
    Unix.mkdir (Filename.concat dir "sub") 0o755;
    touch (Filename.concat dir "top.jpg");
    touch (Filename.concat dir "sub/nested.jpg");
    let files = Zikh.Discover.discover dir in
    Alcotest.(check int) "two files" 2 (List.length files))

let test_discover_skips_symlinks () =
  with_temp_dir (fun dir ->
    let real = Filename.concat dir "real.jpg" in
    let link = Filename.concat dir "link.jpg" in
    touch real;
    Unix.symlink real link;
    let files = Zikh.Discover.discover dir in
    Alcotest.(check int) "one file (symlink excluded)" 1 (List.length files);
    let path = (List.hd files).abs_path in
    Alcotest.(check string) "real file" real path)

let test_discover_skips_fifos () =
  with_temp_dir (fun dir ->
    touch (Filename.concat dir "photo.jpg");
    Unix.mkfifo (Filename.concat dir "pipe") 0o600;
    let files = Zikh.Discover.discover dir in
    Alcotest.(check int) "one file (FIFO excluded)" 1 (List.length files))

let test_discover_collects_newline_path () =
  with_temp_dir (fun dir ->
    let name = Filename.concat dir "bad\nfile.jpg" in
    touch name;
    touch (Filename.concat dir "good.jpg");
    let files = Zikh.Discover.discover dir in
    Alcotest.(check int) "two files including newline" 2 (List.length files))

let test_discover_preserves_extension_case () =
  with_temp_dir (fun dir ->
    touch (Filename.concat dir "IMG.JPG");
    let files = Zikh.Discover.discover dir in
    match files with
    | [sf] -> Alcotest.(check string) "ext case" ".JPG" sf.ext
    | _ -> Alcotest.fail "expected one file")


let test_exif_nonexistent_file () =
  let result = Zikh.Exif.read "/nonexistent/path/photo.jpg" in
  Alcotest.(check (pair (option string) (option string)))
    "nonexistent → (None, None)" (None, None) result

let test_exif_empty_file () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "empty.jpg" in
    touch path;
    let result = Zikh.Exif.read path in
    Alcotest.(check (pair (option string) (option string)))
      "empty → (None, None)" (None, None) result)

let test_exif_not_a_jpeg () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "fake.jpg" in
    write_file path "not a jpeg at all, just text";
    let result = Zikh.Exif.read path in
    Alcotest.(check (pair (option string) (option string)))
      "wrong magic → (None, None)" (None, None) result)

let test_exif_jpeg_without_exif () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "no_exif.jpg" in
    write_file path jpeg_no_exif;
    let result = Zikh.Exif.read path in
    Alcotest.(check (pair (option string) (option string)))
      "no APP1 → (None, None)" (None, None) result)

let test_exif_jpeg_with_datetime_original () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "photo.jpg" in
    write_file path jpeg_with_dto;
    let dto, cd = Zikh.Exif.read path in
    Alcotest.(check (option string))
      "DateTimeOriginal" (Some "2024:06:15 14:32:08") dto;
    Alcotest.(check (option string))
      "CreateDate absent" None cd)

let test_metadata_query_all_filters_unsupported () =
  with_temp_dir (fun dir ->
    let jpg  = Filename.concat dir "photo.jpg" in
    let mp4  = Filename.concat dir "video.mp4" in
    write_file jpg jpeg_with_dto;
    touch mp4;
    let sources = Zikh.Discover.discover dir in
    let meta = Zikh.Metadata.query_all sources in
    Alcotest.(check int) "one metadata entry" 1 (List.length meta);
    let (path, (dto, _)) = List.hd meta in
    Alcotest.(check string) "correct path" jpg path;
    Alcotest.(check (option string))
      "timestamp extracted" (Some "2024:06:15 14:32:08") dto)

let pp_summary ppf s =
  Format.fprintf ppf
    "{processed=%d moved=%d unparsed=%d skipped=%d errors=%d}"
    s.processed s.moved s.unparsed s.skipped s.errors

let summary_t = Alcotest.testable pp_summary ( = )

let make_move src_path dst_path =
  Move ({ abs_path = src_path; ext = Filename.extension src_path },
        { abs_path = dst_path })

let test_execute_same_device_move () =
  with_temp_dir (fun src_dir ->
    with_temp_dir (fun dst_dir ->
      let src = Filename.concat src_dir "photo.jpg" in
      let dst = Filename.concat dst_dir "photo.jpg" in
      write_file src "photo data";
      let ops = [make_move src dst] in
      let _, summary = Zikh.Execute.execute ops in
      Alcotest.(check bool) "source gone"    false (Sys.file_exists src);
      Alcotest.(check bool) "dest exists"    true  (Sys.file_exists dst);
      Alcotest.(check summary_t) "summary"
        { processed = 1; moved = 1; unparsed = 0; skipped = 0; errors = 0 }
        summary))

let test_execute_creates_parent_directories () =
  with_temp_dir (fun src_dir ->
    with_temp_dir (fun dst_dir ->
      let src = Filename.concat src_dir "photo.jpg" in
      let dst = Filename.concat dst_dir "2024/06/photo.jpg" in
      write_file src "photo data";
      let ops = [make_move src dst] in
      let _, summary = Zikh.Execute.execute ops in
      Alcotest.(check bool) "dest exists" true (Sys.file_exists dst);
      Alcotest.(check int) "moved" 1 summary.moved))

let test_execute_dest_exists_records_error () =
  with_temp_dir (fun src_dir ->
    with_temp_dir (fun dst_dir ->
      let src      = Filename.concat src_dir "photo.jpg" in
      let dst      = Filename.concat dst_dir "photo.jpg" in
      let original = "original content" in
      write_file src "new content";
      write_file dst original;
      let ops = [make_move src dst] in
      let results, summary = Zikh.Execute.execute ops in
      Alcotest.(check bool) "source still exists" true (Sys.file_exists src);
      let ic = open_in dst in
      let content = input_line ic in
      close_in ic;
      Alcotest.(check string) "dest unchanged" original content;
      Alcotest.(check int) "errors" 1 summary.errors;
      Alcotest.(check int) "moved"  0 summary.moved;
      match results with
      | [(_, Failed _)] -> ()
      | _ -> Alcotest.fail "expected Failed result"))

let test_execute_empty_plan () =
  let _, summary = Zikh.Execute.execute [] in
  Alcotest.(check summary_t) "all zeros"
    { processed = 0; moved = 0; unparsed = 0; skipped = 0; errors = 0 }
    summary

let test_execute_skip_not_touched () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "video.mp4" in
    touch path;
    let sf = { abs_path = path; ext = ".mp4" } in
    let ops = [Skip sf] in
    let _, summary = Zikh.Execute.execute ops in
    Alcotest.(check bool) "file still exists" true (Sys.file_exists path);
    Alcotest.(check int) "skipped" 1 summary.skipped;
    Alcotest.(check int) "moved"   0 summary.moved)

let test_execute_source_disappeared () =
  with_temp_dir (fun src_dir ->
    with_temp_dir (fun dst_dir ->
      let src = Filename.concat src_dir "ghost.jpg" in
      let dst = Filename.concat dst_dir "ghost.jpg" in
      let ops = [make_move src dst] in
      let _, summary = Zikh.Execute.execute ops in
      Alcotest.(check int) "errors" 1 summary.errors;
      Alcotest.(check int) "moved"  0 summary.moved))

let test_execute_preserves_file_content () =
  with_temp_dir (fun src_dir ->
    with_temp_dir (fun dst_dir ->
      let src     = Filename.concat src_dir "data.jpg" in
      let dst     = Filename.concat dst_dir "data.jpg" in
      let content = "binary\x00data\xFF\xFE" in
      write_file src content;
      ignore (Zikh.Execute.execute [make_move src dst]);
      let ic  = open_in_bin dst in
      let len = in_channel_length ic in
      let buf = Bytes.create len in
      really_input ic buf 0 len;
      close_in ic;
      Alcotest.(check string) "content preserved"
        content (Bytes.to_string buf)))

let () =
  Alcotest.run "integration" [
    "discover", [
      Alcotest.test_case "collects regular files"       `Quick test_discover_collects_regular_files;
      Alcotest.test_case "recurses into subdirectories" `Quick test_discover_recurses_into_subdirs;
      Alcotest.test_case "skips symlinks"               `Quick test_discover_skips_symlinks;
      Alcotest.test_case "skips FIFOs"                  `Quick test_discover_skips_fifos;
      Alcotest.test_case "collects newline paths"       `Quick test_discover_collects_newline_path;
      Alcotest.test_case "preserves extension case"     `Quick test_discover_preserves_extension_case;
    ];
    "exif", [
      Alcotest.test_case "nonexistent file"             `Quick test_exif_nonexistent_file;
      Alcotest.test_case "empty file"                   `Quick test_exif_empty_file;
      Alcotest.test_case "wrong magic bytes"            `Quick test_exif_not_a_jpeg;
      Alcotest.test_case "JPEG without EXIF"            `Quick test_exif_jpeg_without_exif;
      Alcotest.test_case "JPEG with DateTimeOriginal"   `Quick test_exif_jpeg_with_datetime_original;
      Alcotest.test_case "metadata filters unsupported" `Quick test_metadata_query_all_filters_unsupported;
    ];
    "execute", [
      Alcotest.test_case "same-device move"             `Quick test_execute_same_device_move;
      Alcotest.test_case "creates parent directories"   `Quick test_execute_creates_parent_directories;
      Alcotest.test_case "dest exists → error"          `Quick test_execute_dest_exists_records_error;
      Alcotest.test_case "empty plan"                   `Quick test_execute_empty_plan;
      Alcotest.test_case "skip not touched"             `Quick test_execute_skip_not_touched;
      Alcotest.test_case "source disappeared → error"   `Quick test_execute_source_disappeared;
      Alcotest.test_case "preserves file content"       `Quick test_execute_preserves_file_content;
    ];
  ]
