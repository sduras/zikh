(* zikh — a stateless, non-destructive photo organizer.
 * SPDX-License-Identifier: ISC
 * bin/main.ml — entry point: argument parsing, pipeline, output. *)

open Zikh
open Types


let cyan_code   = "\x1b[36m"
let orange_code = "\x1b[38;5;166m"
let red_code    = "\x1b[31m"
let reset_code  = "\x1b[0m"
let full_block  = "\xe2\x96\x88"

let use_colour oc ~json =
  not json
  && (match Sys.getenv_opt "NO_COLOR" with Some s -> s = "" | None -> true)
  && (match Sys.getenv_opt "TERM" with
      | None | Some "" | Some "dumb" -> false | Some _ -> true)
  && (try Unix.isatty (Unix.descr_of_out_channel oc) with _ -> false)

let paint enabled code s =
  if enabled then code ^ s ^ reset_code else s

let _ = orange_code

let json_str s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter (function
    | '"'  -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | '\n' -> Buffer.add_string b "\\n"
    | '\r' -> Buffer.add_string b "\\r"
    | '\t' -> Buffer.add_string b "\\t"
    | c    -> Buffer.add_char b c) s;
  Buffer.add_char b '"';
  Buffer.contents b

let error_kind = function
  | Missing_timestamp    -> "missing_timestamp"
  | Invalid_timestamp _  -> "invalid_timestamp"
  | Exiftool_failure _   -> "exiftool_failure"
  | Filesystem_error _   -> "filesystem_error"
  | Destination_exists _ -> "destination_exists"

let error_detail = function
  | Missing_timestamp    -> ""
  | Invalid_timestamp s | Exiftool_failure s
  | Filesystem_error s  | Destination_exists s -> s

let source_of = function
  | Move ((s : source_file), _) | Unparsed (s, _) | Skip s | Err (s, _) -> s

let string_of_violation = function
  | Validate.Dest_inside_source      -> "DEST is inside SOURCE"
  | Validate.Source_inside_dest      -> "SOURCE is inside DEST"
  | Validate.Duplicate_destination p -> "duplicate destination: " ^ p
  | Validate.Source_equals_dest p    -> "source path equals destination: " ^ p
  | Validate.Non_absolute_source p   -> "non-absolute source path: " ^ p

let zero_summary =
  { processed = 0; moved = 0; unparsed = 0; skipped = 0; errors = 0 }

let count_ops ops =
  List.fold_left (fun acc op -> match op with
    | Move _     -> { acc with moved    = acc.moved    + 1 }
    | Unparsed _ -> { acc with unparsed = acc.unparsed + 1 }
    | Skip _     -> { acc with skipped  = acc.skipped  + 1 }
    | Err _      -> { acc with errors   = acc.errors   + 1 }
  ) zero_summary ops

let print_plan_op_human ~colour oc op =
  match op with
  | Move (sf, df) ->
    Printf.fprintf oc "MOVE     %s \xe2\x86\x92 %s\n"
      sf.abs_path (paint colour cyan_code df.abs_path)
  | Unparsed (sf, df) ->
    Printf.fprintf oc "UNPARSED %s \xe2\x86\x92 %s\n"
      sf.abs_path (paint colour cyan_code df.abs_path)
  | Skip sf ->
    Printf.fprintf oc "SKIP     %s\n" sf.abs_path
  | Err (sf, e) ->
    Printf.fprintf oc "%s %s  %s: %s\n"
      (paint colour red_code "ERROR   ")
      sf.abs_path (error_kind e) (error_detail e)

let print_plan_op_json oc op =
  (match op with
   | Move (sf, df) ->
     Printf.fprintf oc "{\"op\":\"move\",\"source\":%s,\"dest\":%s}\n"
       (json_str sf.abs_path) (json_str df.abs_path)
   | Unparsed (sf, df) ->
     Printf.fprintf oc "{\"op\":\"unparsed\",\"source\":%s,\"dest\":%s}\n"
       (json_str sf.abs_path) (json_str df.abs_path)
   | Skip sf ->
     Printf.fprintf oc
       "{\"op\":\"skip\",\"source\":%s,\"reason\":\"unsupported_extension\"}\n"
       (json_str sf.abs_path)
   | Err (sf, e) ->
     Printf.fprintf oc
       "{\"op\":\"error\",\"source\":%s,\"error\":%s,\"detail\":%s}\n"
       (json_str sf.abs_path) (json_str (error_kind e)) (json_str (error_detail e)));
  flush oc

let print_plan_summary_human oc s =
  Printf.fprintf oc
    "plan: %d move%s, %d unparsed, %d skip%s, %d error%s\n"
    s.moved    (if s.moved   = 1 then "" else "s")
    s.unparsed
    s.skipped  (if s.skipped = 1 then "" else "s")
    s.errors   (if s.errors  = 1 then "" else "s")

let print_plan_summary_json oc s =
  Printf.fprintf oc
    "{\"summary\":true,\"moves\":%d,\"unparsed\":%d,\"skipped\":%d,\"errors\":%d}\n"
    s.moved s.unparsed s.skipped s.errors;
  flush oc

let print_exec_op_verbose ~colour oc (op, result) =
  match op, result with
  | Move (sf, df), Succeeded ->
    Printf.fprintf oc "MOVE     %s \xe2\x86\x92 %s\n"
      sf.abs_path (paint colour cyan_code df.abs_path)
  | Unparsed (sf, df), Succeeded ->
    Printf.fprintf oc "UNPARSED %s \xe2\x86\x92 %s\n"
      sf.abs_path (paint colour cyan_code df.abs_path)
  | Skip sf, Skipped ->
    Printf.fprintf oc "SKIP     %s\n" sf.abs_path
  | _, Failed e ->
    let sf = source_of op in
    Printf.fprintf oc "%s %s  %s: %s\n"
      (paint colour red_code "ERROR   ")
      sf.abs_path (error_kind e) (error_detail e)
  | _ -> ()

let print_exec_op_json oc (op, result) =
  (match op, result with
   | Move (sf, df), Succeeded ->
     Printf.fprintf oc
       "{\"op\":\"move\",\"source\":%s,\"dest\":%s,\"result\":\"ok\"}\n"
       (json_str sf.abs_path) (json_str df.abs_path)
   | Unparsed (sf, df), Succeeded ->
     Printf.fprintf oc
       "{\"op\":\"unparsed\",\"source\":%s,\"dest\":%s,\"result\":\"ok\"}\n"
       (json_str sf.abs_path) (json_str df.abs_path)
   | _, Failed e ->
     let sf = source_of op in
     Printf.fprintf oc
       "{\"op\":\"error\",\"source\":%s,\"error\":%s,\"detail\":%s}\n"
       (json_str sf.abs_path) (json_str (error_kind e)) (json_str (error_detail e))
   | _ -> ());
  flush oc

let print_exec_summary_human oc s =
  Printf.fprintf oc "zikh: processed %d file%s\n"
    s.processed (if s.processed = 1 then "" else "s");
  Printf.fprintf oc "  moved:     %d\n" s.moved;
  Printf.fprintf oc "  unparsed:  %d\n" s.unparsed;
  Printf.fprintf oc "  skipped:   %d\n" s.skipped;
  Printf.fprintf oc "  errors:    %d\n" s.errors

let print_exec_summary_json oc s =
  Printf.fprintf oc
    "{\"summary\":true,\"processed\":%d,\"moved\":%d,\
     \"unparsed\":%d,\"skipped\":%d,\"errors\":%d}\n"
    s.processed s.moved s.unparsed s.skipped s.errors;
  flush oc

let setup_signals () =
  let handler _ =
    (match !Execute.active_temp with
     | Some p -> (try Unix.unlink p with _ -> ())
     | None   -> ());
    exit 1
  in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle handler);
  Sys.set_signal Sys.sigint  (Sys.Signal_handle handler)

let build_plan source dest =
  let source_files = Discover.discover source in
  let metadata     = Metadata.query_all source_files in
  let operations   = Plan.make_plan
    ~sources:source_files ~metadata ~dest_root:dest ~existing:[] in
  let violations   = Validate.validate
    ~source_root:source ~dest_root:dest ~operations in
  (operations, violations)

let compute_year_counts dest_root ops =
  let tbl = Hashtbl.create 16 in
  let prefix = dest_root ^ "/" in
  let plen   = String.length prefix in
  List.iter (function
    | Move (_, (df : dest_file)) when String.length df.abs_path >= plen + 4 ->
      let year = String.sub df.abs_path plen 4 in
      Hashtbl.replace tbl year
        (1 + (try Hashtbl.find tbl year with Not_found -> 0))
    | _ -> ()
  ) ops;
  List.sort (fun (a, _) (b, _) -> String.compare a b)
    (Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [])

let print_year_histogram ~colour oc dest unparsed ops =
  let by_year = compute_year_counts dest ops in
  if by_year = [] && unparsed = 0 then ()
  else begin
    let max_move    = List.fold_left (fun m (_, n) -> max m n) 0 by_year in
    let max_count   = max max_move unparsed in
    let bar_width   = 20 in
    let count_width = String.length (string_of_int max_count) in
    Printf.fprintf oc "\n";
    List.iter (fun (year, count) ->
      let n   = max 1 (count * bar_width / max_count) in
      let bar = String.concat "" (List.init n (fun _ -> full_block)) in
      Printf.fprintf oc "  %s  %s%s  %*d\n"
        year
        (paint colour orange_code bar)
        (String.make (bar_width - n) ' ')
        count_width count
    ) by_year;
    if unparsed > 0 then
      Printf.fprintf oc "  unparsed  %d\n" unparsed;
    Printf.fprintf oc "\n"
  end

let run_plan ~verbose ~json source dest =
  let operations, violations = build_plan source dest in
  if violations <> [] then begin
    List.iter (fun v -> Printf.eprintf "zikh: %s\n" (string_of_violation v))
      violations;
    exit 2
  end;
  let s      = count_ops operations in
  let colour = use_colour stdout ~json in
  if json then begin
    List.iter (print_plan_op_json stdout) operations;
    print_plan_summary_json stdout s
  end else begin
    print_plan_summary_human stdout s;
    print_year_histogram ~colour stdout dest s.unparsed operations;
    if verbose then
      List.iter (print_plan_op_human ~colour stdout) operations
  end;
  exit 0

let run_execute ~verbose ~json source dest =
  setup_signals ();
  let operations, violations = build_plan source dest in
  if violations <> [] then begin
    List.iter (fun v -> Printf.eprintf "zikh: %s\n" (string_of_violation v))
      violations;
    exit 2
  end;
  let results, summary = Execute.execute operations in
  let colour = use_colour stderr ~json in
  if json then
    List.iter (print_exec_op_json stdout) results
  else if verbose then
    List.iter (print_exec_op_verbose ~colour stderr) results;
  if json then
    print_exec_summary_json stdout summary
  else
    print_exec_summary_human stderr summary;
  let exit_code =
    if summary.errors = 0 then 0
    else if summary.moved + summary.unparsed = 0 then 3
    else 1
  in
  exit exit_code


let usage () =
  print_string
    "Usage: zikh <subcommand> [OPTIONS] SOURCE DEST\n\
     \n\
     Subcommands:\n\
     \  plan      Discover and plan operations; no filesystem changes\n\
     \  execute   Discover, plan, and apply; moves files into DEST\n\
     \n\
     Options:\n\
     \  -v        Verbose: print each operation as it executes\n\
     \  --json    Machine-readable NDJSON output\n\
     \  --help    Show this message\n"

let parse_subcommand_args () =
  let verbose    = ref false in
  let json       = ref false in
  let positional = ref [] in
  let spec = [
    "-v",     Arg.Set verbose, " Verbose output";
    "--json", Arg.Set json,    " NDJSON output";
    "--help", Arg.Unit (fun () -> usage (); exit 0), " Show help";
  ] in
  let anon s = positional := s :: !positional in
  let current = ref 1 in
  (try Arg.parse_argv ~current Sys.argv spec anon ""
   with
   | Arg.Bad msg ->
     Printf.eprintf "zikh: %s\nTry 'zikh --help'.\n" msg; exit 64
   | Arg.Help msg ->
     print_string msg; exit 0);
  if !verbose && !json then begin
    Printf.eprintf "zikh: -v and --json are mutually exclusive\n"; exit 64
  end;
  (!verbose, !json, List.rev !positional)

let () =
  if Array.length Sys.argv < 2 then (usage (); exit 64);
  match Sys.argv.(1) with
  | "--help" | "-h" -> usage (); exit 0
  | "plan" | "execute" as sub ->
    let verbose, json, args = parse_subcommand_args () in
    (match args with
     | [source; dest] ->
       if sub = "plan" then run_plan    ~verbose ~json source dest
       else                 run_execute ~verbose ~json source dest
     | _ ->
       Printf.eprintf "zikh: expected SOURCE and DEST, got %d argument%s\n"
         (List.length args) (if List.length args = 1 then "" else "s");
       exit 64)
  | unknown ->
    Printf.eprintf "zikh: unknown subcommand '%s'\nTry 'zikh --help'.\n" unknown;
    exit 64
