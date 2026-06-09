(* zikh — a stateless, non-destructive photo organizer.
 * SPDX-License-Identifier: ISC
 * lib/metadata.ml — metadata extraction via native EXIF parser. *)

let supported = [".jpg"; ".jpeg"; ".heic"; ".png"; ".tiff"; ".tif"]

let is_supported ext = List.mem (String.lowercase_ascii ext) supported

let query_all (source_files : Types.source_file list) =
  List.filter_map (fun (sf : Types.source_file) ->
    if String.contains sf.abs_path '\n' then None
    else if not (is_supported sf.ext)   then None
    else Some (sf.abs_path, Exif.read sf.abs_path)
  ) source_files
