(* zikh — a stateless, non-destructive photo organizer.
 * SPDX-License-Identifier: ISC
 * lib/validate.ml — validation phase. *)

open Types

type violation =
  | Dest_inside_source
  | Source_inside_dest
  | Duplicate_destination of string
  | Source_equals_dest    of string
  | Non_absolute_source   of string

let is_prefix a b =
  let alen = String.length a in
  String.length b >= alen && String.sub b 0 alen = a

let validate ~source_root ~dest_root ~operations =
  let src_slash = source_root ^ "/" in
  let dst_slash = dest_root   ^ "/" in

  let v = [] in
  let v = if is_prefix src_slash dst_slash then Dest_inside_source :: v else v in
  let v = if is_prefix dst_slash src_slash then Source_inside_dest :: v else v in

  let seen = Hashtbl.create 64 in
  let v = List.fold_left (fun acc op ->
    match op with
    | Move (_, d) | Unparsed (_, d) ->
      if Hashtbl.mem seen d.abs_path then
        Duplicate_destination d.abs_path :: acc
      else
        (Hashtbl.replace seen d.abs_path (); acc)
    | Skip _ | Err _ -> acc
  ) v operations in

  let v = List.fold_left (fun acc op ->
    match op with
    | Move ((s : source_file), d) when s.abs_path = d.abs_path ->
      Source_equals_dest s.abs_path :: acc
    | _ -> acc
  ) v operations in

  let v = List.fold_left (fun acc op ->
    let s = match op with
      | Move ((s : source_file), _) | Unparsed (s, _) | Skip s | Err (s, _) -> s
    in
    if String.length s.abs_path = 0 || s.abs_path.[0] <> '/' then
      Non_absolute_source s.abs_path :: acc
    else
      acc
  ) v operations in

  List.rev v
