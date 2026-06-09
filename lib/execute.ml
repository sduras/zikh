(* zikh — a stateless, non-destructive photo organizer.
 * SPDX-License-Identifier: ISC
 * lib/execute.ml — execution phase: filesystem operations. *)

open Types

let active_temp : string option ref = ref None

exception Op_error of error

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let copy_fd src dst =
  let buf = Bytes.create 65536 in
  let rec loop () =
    let n = Unix.read src buf 0 (Bytes.length buf) in
    if n > 0 then begin
      let rec write off =
        if off < n then
          write (off + Unix.write dst buf off (n - off))
      in
      write 0;
      loop ()
    end
  in
  loop ()

let move_cross_device (sf : source_file) (df : dest_file) =
  let dest_dir  = Filename.dirname df.abs_path in
  let temp_path =
    try Filename.temp_file ~temp_dir:dest_dir ".zikh_tmp_" ""
    with Sys_error msg ->
      raise (Op_error (Filesystem_error
        (Printf.sprintf "cannot create temp file in %s: %s" dest_dir msg)))
  in
  active_temp := Some temp_path;
  let src_fd =
    try Unix.openfile sf.abs_path [Unix.O_RDONLY] 0
    with Unix.Unix_error (e, _, _) ->
      (try Unix.unlink temp_path with _ -> ());
      active_temp := None;
      raise (Op_error (Filesystem_error
        ("cannot open source: " ^ Unix.error_message e)))
  in
  let tmp_fd =
    try Unix.openfile temp_path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
    with Unix.Unix_error (e, _, _) ->
      Unix.close src_fd;
      (try Unix.unlink temp_path with _ -> ());
      active_temp := None;
      raise (Op_error (Filesystem_error
        ("cannot open temp file: " ^ Unix.error_message e)))
  in
  (try copy_fd src_fd tmp_fd
   with Unix.Unix_error (e, _, _) ->
     Unix.close src_fd; Unix.close tmp_fd;
     (try Unix.unlink temp_path with _ -> ());
     active_temp := None;
     raise (Op_error (Filesystem_error
       ("copy failed: " ^ Unix.error_message e))));
  Unix.close src_fd;
  Unix.close tmp_fd;
  let src_size  = (Unix.stat sf.abs_path).Unix.st_size in
  let temp_size = (Unix.stat temp_path).Unix.st_size in
  if src_size <> temp_size then begin
    (try Unix.unlink temp_path with _ -> ());
    active_temp := None;
    raise (Op_error (Filesystem_error
      "copy verification failed: byte count mismatch"))
  end;
  (try Unix.link temp_path df.abs_path
   with Unix.Unix_error (e, _, _) ->
     (try Unix.unlink temp_path with _ -> ());
     active_temp := None;
     let msg = match e with
       | Unix.EEXIST -> "destination exists"
       | Unix.EPERM | Unix.EOPNOTSUPP | Unix.EMLINK ->
         "destination filesystem does not support hardlinks; cannot publish safely"
       | _ -> Unix.error_message e
     in
     raise (Op_error (Filesystem_error msg)));
  (try Unix.unlink temp_path with _ -> ());
  active_temp := None;
  (try Unix.unlink sf.abs_path
   with Unix.Unix_error (e, _, _) ->
     raise (Op_error (Filesystem_error
       (Printf.sprintf
          "copied to %s but failed to remove source: %s; both copies exist"
          df.abs_path (Unix.error_message e)))))

let exec_file_op (sf : source_file) (df : dest_file) =
  let dest_dir = Filename.dirname df.abs_path in
  (try mkdir_p dest_dir
   with Unix.Unix_error (e, _, _) ->
     raise (Op_error (Filesystem_error
       (Printf.sprintf "cannot create %s: %s"
          dest_dir (Unix.error_message e)))));
  match
    (try Unix.link sf.abs_path df.abs_path; `Linked
     with Unix.Unix_error (e, _, _) -> `Link_error e)
  with
  | `Linked ->
    (try Unix.unlink sf.abs_path
     with Unix.Unix_error (e, _, _) ->
       raise (Op_error (Filesystem_error
         (Printf.sprintf "linked to %s but failed to remove source: %s"
            df.abs_path (Unix.error_message e)))))
  | `Link_error Unix.EEXIST ->
    raise (Op_error (Filesystem_error "destination exists"))
  | `Link_error Unix.ENOENT ->
    raise (Op_error (Filesystem_error "source disappeared before execution"))
  | `Link_error Unix.EXDEV ->
    move_cross_device sf df
  | `Link_error Unix.EPERM
  | `Link_error Unix.EOPNOTSUPP
  | `Link_error Unix.EMLINK ->
    raise (Op_error (Filesystem_error
      "destination filesystem does not support hardlinks; cannot move safely"))
  | `Link_error e ->
    raise (Op_error (Filesystem_error (Unix.error_message e)))

let execute_one op =
  match op with
  | Skip _  -> Skipped
  | Err _   -> Failed (Filesystem_error "not executed: planning error")
  | Move ((sf : source_file), df) | Unparsed (sf, df) ->
    (try exec_file_op sf df; Succeeded
     with Op_error e -> Failed e)


let execute operations =
  let moved    = ref 0
  and unparsed = ref 0
  and skipped  = ref 0
  and errors   = ref 0 in
  let results = List.map (fun op ->
    let result = execute_one op in
    (match op, result with
     | Move _,     Succeeded -> incr moved
     | Unparsed _, Succeeded -> incr unparsed
     | Skip _,     Skipped   -> incr skipped
     | _,          Failed _  -> incr errors
     | _,          _         -> ());
    (op, result)
  ) operations in
  let summary = {
    processed = !moved + !unparsed + !skipped + !errors;
    moved     = !moved;
    unparsed  = !unparsed;
    skipped   = !skipped;
    errors    = !errors;
  } in
  (results, summary)
