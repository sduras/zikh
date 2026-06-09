(* zikh — a stateless, non-destructive photo organizer.
 * SPDX-License-Identifier: ISC
 * tests/test_validation.ml — validation phase tests. *)

open Zikh.Types
open Zikh.Validate

let src_root  = "/src"
let dest_root = "/dst"

let sf rel      = { abs_path = src_root  ^ "/" ^ rel; ext = ".jpg" }
let dst_path rel = dest_root ^ "/" ^ rel

let run ?(src = src_root) ?(dst = dest_root) ops =
  validate ~source_root:src ~dest_root:dst ~operations:ops

let pp_violation ppf = function
  | Dest_inside_source      -> Format.pp_print_string ppf "Dest_inside_source"
  | Source_inside_dest      -> Format.pp_print_string ppf "Source_inside_dest"
  | Duplicate_destination s -> Format.fprintf ppf "Duplicate_destination %S" s
  | Source_equals_dest s    -> Format.fprintf ppf "Source_equals_dest %S" s
  | Non_absolute_source s   -> Format.fprintf ppf "Non_absolute_source %S" s

let viol_t  = Alcotest.testable pp_violation ( = )
let viols_t = Alcotest.(list viol_t)

let check msg expected actual =
  Alcotest.(check viols_t) msg expected actual

let check_empty msg actual =
  Alcotest.(check viols_t) msg [] actual

let check_contains msg expected actual =
  List.iter (fun v ->
    Alcotest.(check bool) (msg ^ ": contains " ^ Format.asprintf "%a" pp_violation v)
      true (List.mem v actual)
  ) expected


let test_7_1_dest_inside_source () =
  check "DEST inside SOURCE"
    [Dest_inside_source]
    (run ~src:"/photos" ~dst:"/photos/archive" [])

let test_7_2_source_inside_dest () =
  check "SOURCE inside DEST"
    [Source_inside_dest]
    (run ~src:"/photos/inbox" ~dst:"/photos" [])

let test_7_3_similar_prefix_not_inside () =
  check_empty "similar prefix, distinct dirs"
    (run ~src:"/data/photos" ~dst:"/data/photos-archive" [])

let test_containment_equal_roots () =
  let v = run ~src:"/photos" ~dst:"/photos" [] in
  check_contains "equal roots"
    [Dest_inside_source; Source_inside_dest] v

let test_containment_adjacent_siblings () =
  check_empty "adjacent siblings"
    (run ~src:"/archive/inbox" ~dst:"/archive/organized" [])

let test_7_4_duplicate_dest_in_moves () =
  let dest = dst_path "2024/06/photo.jpg" in
  let ops  = [
    Move (sf "a.jpg", { abs_path = dest });
    Move (sf "b.jpg", { abs_path = dest });
  ] in
  check "duplicate dest in Move"
    [Duplicate_destination dest]
    (run ops)

let test_7_4_duplicate_dest_in_unparsed () =
  let dest = dst_path "unparsed/scan.png" in
  let ops  = [
    Unparsed ({ abs_path = src_root ^ "/a/scan.png"; ext = ".png" }, { abs_path = dest });
    Unparsed ({ abs_path = src_root ^ "/b/scan.png"; ext = ".png" }, { abs_path = dest });
  ] in
  check "duplicate dest in Unparsed"
    [Duplicate_destination dest]
    (run ops)

let test_7_4_duplicate_across_move_and_unparsed () =
  let dest = dst_path "unparsed/photo.jpg" in
  let ops  = [
    Move    (sf "photo.jpg", { abs_path = dest });
    Unparsed (sf "other.jpg", { abs_path = dest });
  ] in
  check "duplicate across Move and Unparsed"
    [Duplicate_destination dest]
    (run ops)

let test_7_4_skip_and_err_ignored_in_duplicate_check () =
  let ops = [
    Skip (sf "video.mp4");
    Err  (sf "bad.jpg", Exiftool_failure "reason");
  ] in
  check_empty "Skip and Err ignored"
    (run ops)

let test_7_4_multiple_duplicate_pairs () =
  let d1 = dst_path "2024/01/a.jpg" in
  let d2 = dst_path "2024/01/b.jpg" in
  let ops = [
    Move (sf "x1.jpg", { abs_path = d1 });
    Move (sf "x2.jpg", { abs_path = d1 });
    Move (sf "y1.jpg", { abs_path = d2 });
    Move (sf "y2.jpg", { abs_path = d2 });
  ] in
  let v = run ops in
  check_contains "both duplicates reported"
    [Duplicate_destination d1; Duplicate_destination d2] v

let test_7_5_source_equals_dest () =
  let path = src_root ^ "/photo.jpg" in
  let ops  = [Move ({ abs_path = path; ext = ".jpg" }, { abs_path = path })] in
  check "source = dest"
    [Source_equals_dest path]
    (run ops)

let test_7_5_unparsed_not_checked_for_src_eq_dest () =
  let path = src_root ^ "/scan.png" in
  let ops  = [Unparsed ({ abs_path = path; ext = ".png" }, { abs_path = path })] in
  check_empty "Unparsed src=dst not a violation"
    (run ops)

let test_non_absolute_source_path () =
  let ops = [Move ({ abs_path = "relative/photo.jpg"; ext = ".jpg" },
                   { abs_path = dst_path "2024/01/photo.jpg" })] in
  check "non-absolute source"
    [Non_absolute_source "relative/photo.jpg"]
    (run ops)

let test_valid_empty_plan () =
  check_empty "empty plan"
    (run [])

let test_valid_plan_all_operation_types () =
  let ops = [
    Move     (sf "photo.jpg",  { abs_path = dst_path "2024/06/photo.jpg" });
    Unparsed (sf "scan.png",   { abs_path = dst_path "unparsed/scan.png" });
    Skip     (sf "video.mp4");
    Err      (sf "bad\njpg",   Exiftool_failure "path contains newline");
  ] in
  check_empty "valid plan, all types"
    (run ops)

let test_valid_plan_unique_dests () =
  let ops = [
    Move (sf "a.jpg", { abs_path = dst_path "2024/06/a.jpg" });
    Move (sf "b.jpg", { abs_path = dst_path "2024/06/b.jpg" });
    Move (sf "c.jpg", { abs_path = dst_path "2024/06/c.jpg" });
  ] in
  check_empty "unique dests"
    (run ops)

let () =
  Alcotest.run "validation" [
    "§7 containment", [
      Alcotest.test_case "7.1 DEST inside SOURCE"     `Quick test_7_1_dest_inside_source;
      Alcotest.test_case "7.2 SOURCE inside DEST"     `Quick test_7_2_source_inside_dest;
      Alcotest.test_case "7.3 similar prefix"         `Quick test_7_3_similar_prefix_not_inside;
      Alcotest.test_case "equal roots"                `Quick test_containment_equal_roots;
      Alcotest.test_case "adjacent siblings"          `Quick test_containment_adjacent_siblings;
    ];
    "§7.4 duplicate destinations", [
      Alcotest.test_case "duplicate in Move"          `Quick test_7_4_duplicate_dest_in_moves;
      Alcotest.test_case "duplicate in Unparsed"      `Quick test_7_4_duplicate_dest_in_unparsed;
      Alcotest.test_case "duplicate Move+Unparsed"    `Quick test_7_4_duplicate_across_move_and_unparsed;
      Alcotest.test_case "Skip and Err ignored"       `Quick test_7_4_skip_and_err_ignored_in_duplicate_check;
      Alcotest.test_case "multiple pairs"             `Quick test_7_4_multiple_duplicate_pairs;
    ];
    "§7.5 source = dest", [
      Alcotest.test_case "Move src = dst"             `Quick test_7_5_source_equals_dest;
      Alcotest.test_case "Unparsed src = dst ignored" `Quick test_7_5_unparsed_not_checked_for_src_eq_dest;
    ];
    "absolute paths", [
      Alcotest.test_case "non-absolute source"        `Quick test_non_absolute_source_path;
    ];
    "valid plans", [
      Alcotest.test_case "empty plan"                 `Quick test_valid_empty_plan;
      Alcotest.test_case "all operation types"        `Quick test_valid_plan_all_operation_types;
      Alcotest.test_case "unique dests"               `Quick test_valid_plan_unique_dests;
    ];
  ]
