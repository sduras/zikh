(* zikh — a stateless, non-destructive photo organizer.
 * SPDX-License-Identifier: ISC
 * lib/discover.ml — recursive directory traversal. *)

open Types

let extract_ext name =
  match String.rindex_opt name '.' with
  | Some i -> String.sub name i (String.length name - i)
  | None   -> ""

let rec traverse acc dir =
  let dh =
    try Unix.opendir dir
    with Unix.Unix_error (e, _, _) ->
      Printf.eprintf "zikh: cannot open directory %s: %s\n"
        dir (Unix.error_message e);
      exit 71
  in
  let acc = ref acc in
  (try
    while true do
      let name = Unix.readdir dh in
      if name = "." || name = ".." then ()
      else
        let abs_path = Filename.concat dir name in
        let kind =
          try Some (Unix.lstat abs_path).Unix.st_kind
          with Unix.Unix_error _ -> None
        in
        match kind with
        | None             -> ()
        | Some Unix.S_REG  ->
          let ext = extract_ext name in
          acc := { abs_path; ext } :: !acc
        | Some Unix.S_DIR  -> acc := traverse !acc abs_path
        | Some _           -> ()
    done
  with End_of_file -> ());
  Unix.closedir dh;
  !acc

let discover source_root =
  traverse [] source_root
