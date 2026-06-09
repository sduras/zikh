(* zikh — a stateless, non-destructive photo organizer.
 * SPDX-License-Identifier: ISC
 * lib/types.ml — core type definitions. *)

type timestamp = {
  year  : int;
  month : int;
  day   : int;
  hour  : int;
  min   : int;
  sec   : int;
}

type source_file = {
  abs_path : string;
  ext      : string;
}

type dest_file = {
  abs_path : string;
}

type error =
  | Missing_timestamp
  | Invalid_timestamp  of string
  | Exiftool_failure   of string
  | Filesystem_error   of string
  | Destination_exists of string

type operation =
  | Move     of source_file * dest_file
  | Unparsed of source_file * dest_file
  | Skip     of source_file
  | Err      of source_file * error

type plan = {
  source_root : string;
  dest_root   : string;
  operations  : operation list;
}

type file_result =
  | Succeeded
  | Skipped
  | Failed of error

type run_summary = {
  processed : int;
  moved     : int;
  unparsed  : int;
  skipped   : int;
  errors    : int;
}
