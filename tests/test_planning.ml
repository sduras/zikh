(* zikh — a stateless, non-destructive photo organizer.
 * SPDX-License-Identifier: ISC
 * tests/test_planning.ml — planning phase tests. *)

open Zikh.Types

let src_root  = "/src"
let dest_root = "/dst"

let sf rel =
  let abs_path = src_root ^ "/" ^ rel in
  let name = Filename.basename rel in
  let ext = match String.rindex_opt name '.' with
    | Some i -> String.sub name i (String.length name - i)
    | None   -> ""
  in
  { abs_path; ext }

let sf_noext rel =
  { abs_path = src_root ^ "/" ^ rel; ext = "" }

let dp path = { abs_path = dest_root ^ "/" ^ path }

let dto ts  = (Some ts, None)
let cd  ts  = (None,    Some ts)
let both d c = (Some d, Some c)
let no_tags  = (None,   None)


let run ?(meta = []) ?(existing = []) sources =
  Zikh.Plan.make_plan
    ~sources
    ~metadata:meta
    ~dest_root
    ~existing

let contains_sub s sub =
  let sn = String.length s and subn = String.length sub in
  let rec loop i =
    if i > sn - subn then false
    else if String.sub s i subn = sub then true
    else loop (i + 1)
  in
  sn >= subn && loop 0


let pp_sf ppf (f : source_file) =
  Format.fprintf ppf "{abs_path=%S; ext=%S}" f.abs_path f.ext

let pp_df ppf d =
  Format.fprintf ppf "{abs_path=%S}" d.abs_path

let pp_err ppf = function
  | Missing_timestamp    -> Format.pp_print_string ppf "Missing_timestamp"
  | Invalid_timestamp s  -> Format.fprintf ppf "Invalid_timestamp(%S)" s
  | Exiftool_failure s   -> Format.fprintf ppf "Exiftool_failure(%S)" s
  | Filesystem_error s   -> Format.fprintf ppf "Filesystem_error(%S)" s
  | Destination_exists s -> Format.fprintf ppf "Destination_exists(%S)" s

let pp_op ppf = function
  | Move (s, d)     -> Format.fprintf ppf "Move(%a, %a)"     pp_sf s pp_df d
  | Unparsed (s, d) -> Format.fprintf ppf "Unparsed(%a, %a)" pp_sf s pp_df d
  | Skip s          -> Format.fprintf ppf "Skip(%a)"         pp_sf s
  | Err (s, e)      -> Format.fprintf ppf "Err(%a, %a)"      pp_sf s pp_err e

let op_t  = Alcotest.testable pp_op ( = )
let ops_t = Alcotest.(list op_t)
let check msg expected actual = Alcotest.(check ops_t) msg expected actual

let test_1_1_single_file () =
  let meta = [src_root ^ "/IMG_1234.JPG", dto "2024:06:15 14:32:08"] in
  check "basic move"
    [Move (sf "IMG_1234.JPG", dp "2024/06/2024-06-15_14-32-08_IMG_1234.JPG")]
    (run ~meta [sf "IMG_1234.JPG"])

let test_1_2_epoch_boundary () =
  let meta = [src_root ^ "/A.jpg", dto "1970:01:01 00:00:00"] in
  check "epoch boundary"
    [Move (sf "A.jpg", dp "1970/01/1970-01-01_00-00-00_A.jpg")]
    (run ~meta [sf "A.jpg"])

let test_1_3_month_zero_padded () =
  let meta = [src_root ^ "/A.jpg", dto "2024:03:07 09:05:02"] in
  check "month zero-padded"
    [Move (sf "A.jpg", dp "2024/03/2024-03-07_09-05-02_A.jpg")]
    (run ~meta [sf "A.jpg"])

let test_1_4_uppercase_extension_preserved () =
  let meta = [src_root ^ "/IMG_1234.JPG", dto "2024:06:15 14:32:08"] in
  match run ~meta [sf "IMG_1234.JPG"] with
  | [Move (_, { abs_path })] ->
    let len = String.length abs_path in
    Alcotest.(check string) "ext" ".JPG" (String.sub abs_path (len - 4) 4)
  | _ -> Alcotest.fail "expected one Move"

let test_1_5_mixed_case_extension_preserved () =
  let meta = [src_root ^ "/photo.Jpeg", dto "2024:06:15 14:32:08"] in
  match run ~meta [sf "photo.Jpeg"] with
  | [Move (_, { abs_path })] ->
    let len = String.length abs_path in
    Alcotest.(check string) "ext" ".Jpeg" (String.sub abs_path (len - 5) 5)
  | _ -> Alcotest.fail "expected one Move"

let test_1_6_stem_with_internal_dots () =
  let meta = [src_root ^ "/my.photo.2024.jpg", dto "2024:06:15 14:32:08"] in
  check "internal dots in stem"
    [Move (sf "my.photo.2024.jpg",
           dp "2024/06/2024-06-15_14-32-08_my.photo.2024.jpg")]
    (run ~meta [sf "my.photo.2024.jpg"])

let test_1_7_stem_with_spaces () =
  let name = "IMG 1234_burst.jpg" in
  let meta = [src_root ^ "/" ^ name, dto "2024:06:15 14:32:08"] in
  check "spaces in stem"
    [Move (sf name, dp ("2024/06/2024-06-15_14-32-08_" ^ name))]
    (run ~meta [sf name])

let test_1_8_subdir_not_reproduced_in_dest () =
  let rel  = "vacation/beach/IMG_0001.jpg" in
  let meta = [src_root ^ "/" ^ rel, dto "2024:08:01 10:00:00"] in
  check "subdir not reproduced"
    [Move (sf rel, dp "2024/08/2024-08-01_10-00-00_IMG_0001.jpg")]
    (run ~meta [sf rel])

let test_2_1_unsupported_extension () =
  check "unsupported ext"
    [Skip (sf "video.mp4")]
    (run [sf "video.mp4"])

let test_2_2_no_extension () =
  check "no extension"
    [Skip (sf_noext "README")]
    (run [sf_noext "README"])

let test_2_3_mixed_supported_and_unsupported () =
  let meta = [
    src_root ^ "/IMG_0001.jpg", dto "2024:01:01 12:00:00";
    src_root ^ "/IMG_0002.png", dto "2024:01:01 12:01:00";
  ] in
  check "mixed types"
    [ Move (sf "IMG_0001.jpg", dp "2024/01/2024-01-01_12-00-00_IMG_0001.jpg");
      Move (sf "IMG_0002.png", dp "2024/01/2024-01-01_12-01-00_IMG_0002.png");
      Skip (sf "document.pdf") ]
    (run ~meta [sf "IMG_0001.jpg"; sf "document.pdf"; sf "IMG_0002.png"])

let test_2_4_extension_match_case_insensitive_dest_case_preserved () =
  let meta = [
    src_root ^ "/IMG_0001.JPG", dto "2024:01:01 10:00:00";
    src_root ^ "/IMG_0002.Jpg", dto "2024:01:01 10:01:00";
    src_root ^ "/IMG_0003.jpg", dto "2024:01:01 10:02:00";
  ] in
  let ops = run ~meta [sf "IMG_0001.JPG"; sf "IMG_0002.Jpg"; sf "IMG_0003.jpg"] in
  let ends s sfx =
    let sl = String.length s and pl = String.length sfx in
    sl >= pl && String.sub s (sl - pl) pl = sfx
  in
  let dest_of i = match List.nth ops i with
    | Move (_, { abs_path }) -> abs_path
    | _ -> Alcotest.fail "expected Move"
  in
  Alcotest.(check bool) "dest 0 ends .JPG" true (ends (dest_of 0) ".JPG");
  Alcotest.(check bool) "dest 1 ends .Jpg" true (ends (dest_of 1) ".Jpg");
  Alcotest.(check bool) "dest 2 ends .jpg" true (ends (dest_of 2) ".jpg")

let test_3_1_no_tags () =
  let meta = [src_root ^ "/scan.png", no_tags] in
  check "no tags"
    [Unparsed (sf "scan.png", dp "unparsed/scan.png")]
    (run ~meta [sf "scan.png"])

let test_3_1b_absent_metadata_entry () =
  check "absent metadata entry"
    [Unparsed (sf "scan.png", dp "unparsed/scan.png")]
    (run [sf "scan.png"])

let test_3_2_create_date_fallback () =
  let meta = [src_root ^ "/import.jpg", cd "2024:06:15 14:32:08"] in
  check "CreateDate fallback"
    [Move (sf "import.jpg", dp "2024/06/2024-06-15_14-32-08_import.jpg")]
    (run ~meta [sf "import.jpg"])

let test_3_3_dto_wins_over_create_date () =
  let meta = [
    src_root ^ "/photo.jpg",
    both "2024:06:15 14:32:08" "2024:12:31 23:59:59";
  ] in
  check "DTO wins over CreateDate"
    [Move (sf "photo.jpg", dp "2024/06/2024-06-15_14-32-08_photo.jpg")]
    (run ~meta [sf "photo.jpg"])

let invalid_timestamps = [
  "0000:00:00 00:00:00",        "all zeros";
  "2024-06-15 14:32:08",        "wrong date separator";
  "2024:13:01 12:00:00",        "month > 12";
  "2024:00:15 12:00:00",        "month = 0";
  "2024:06:00 12:00:00",        "day = 0";
  "1969:12:31 23:59:59",        "year < 1970";
  "2024:06:15 14:32:08+03:00",  "timezone suffix";
  "2024:06:15T14:32:08",        "ISO 8601 format";
]

let test_invalid_timestamp (ts, _label) () =
  let meta = [src_root ^ "/photo.jpg", dto ts] in
  check ("invalid: " ^ ts)
    [Unparsed (sf "photo.jpg", dp "unparsed/photo.jpg")]
    (run ~meta [sf "photo.jpg"])

let test_3_11_dto_invalid_falls_through_to_create_date () =
  let meta = [
    src_root ^ "/photo.jpg",
    both "0000:00:00 00:00:00" "2024:06:15 14:32:08";
  ] in
  check "DTO invalid, CreateDate valid"
    [Move (sf "photo.jpg", dp "2024/06/2024-06-15_14-32-08_photo.jpg")]
    (run ~meta [sf "photo.jpg"])

let test_3_12_both_invalid_becomes_unparsed () =
  let meta = [
    src_root ^ "/photo.jpg",
    both "0000:00:00 00:00:00" "0000:00:00 00:00:00";
  ] in
  check "both tags invalid"
    [Unparsed (sf "photo.jpg", dp "unparsed/photo.jpg")]
    (run ~meta [sf "photo.jpg"])

let test_3_unparsed_preserves_original_filename () =
  let name = "my scan (2).png" in
  let meta = [src_root ^ "/" ^ name, no_tags] in
  check "original filename preserved"
    [Unparsed (sf name, dp ("unparsed/" ^ name))]
    (run ~meta [sf name])

let test_4_1_newline_in_path () =
  let bad = src_root ^ "/IMG_\n1234.jpg" in
  let ops = run [{ abs_path = bad; ext = ".jpg" }] in
  match ops with
  | [Err ({ abs_path; _ }, Exiftool_failure reason)] ->
    Alcotest.(check string) "source path" bad abs_path;
    Alcotest.(check bool) "reason mentions newline"
      true (contains_sub reason "newline")
  | _ -> Alcotest.fail "expected [Err (_, Exiftool_failure _)]"

let test_4_1_newline_does_not_contaminate_other_files () =
  let bad  = src_root ^ "/bad\nfile.jpg" in
  let good = src_root ^ "/good.jpg" in
  let meta = [good, dto "2024:06:15 14:32:08"] in
  let ops  = run ~meta [
    { abs_path = bad;  ext = ".jpg" };
    { abs_path = good; ext = ".jpg" };
  ] in
  let count p = List.length (List.filter p ops) in
  Alcotest.(check int) "one Err"  1 (count (function Err _  -> true | _ -> false));
  Alcotest.(check int) "one Move" 1 (count (function Move _ -> true | _ -> false))

let test_5_1_two_files_same_timestamp_same_stem () =
  let meta = [
    src_root ^ "/card1/IMG_0001.jpg", dto "2024:06:15 14:32:08";
    src_root ^ "/card2/IMG_0001.jpg", dto "2024:06:15 14:32:08";
  ] in
  check "two files, same stem"
    [ Move (sf "card1/IMG_0001.jpg",
            dp "2024/06/2024-06-15_14-32-08_IMG_0001.jpg");
      Move (sf "card2/IMG_0001.jpg",
            dp "2024/06/2024-06-15_14-32-08_IMG_0001-1.jpg") ]
    (run ~meta [sf "card1/IMG_0001.jpg"; sf "card2/IMG_0001.jpg"])

let test_5_2_three_files_same_timestamp_same_stem () =
  let meta = List.map (fun d ->
    src_root ^ "/" ^ d ^ "/IMG_0001.jpg", dto "2024:06:15 14:32:08"
  ) ["a"; "b"; "c"] in
  check "three files, same stem"
    [ Move (sf "a/IMG_0001.jpg", dp "2024/06/2024-06-15_14-32-08_IMG_0001.jpg");
      Move (sf "b/IMG_0001.jpg", dp "2024/06/2024-06-15_14-32-08_IMG_0001-1.jpg");
      Move (sf "c/IMG_0001.jpg", dp "2024/06/2024-06-15_14-32-08_IMG_0001-2.jpg") ]
    (run ~meta [sf "a/IMG_0001.jpg"; sf "b/IMG_0001.jpg"; sf "c/IMG_0001.jpg"])

let test_5_3_different_stems_no_collision () =
  let meta = [
    src_root ^ "/IMG_0001.jpg", dto "2024:06:15 14:32:08";
    src_root ^ "/IMG_0002.jpg", dto "2024:06:15 14:32:08";
  ] in
  check "different stems, no collision"
    [ Move (sf "IMG_0001.jpg", dp "2024/06/2024-06-15_14-32-08_IMG_0001.jpg");
      Move (sf "IMG_0002.jpg", dp "2024/06/2024-06-15_14-32-08_IMG_0002.jpg") ]
    (run ~meta [sf "IMG_0001.jpg"; sf "IMG_0002.jpg"])

let test_5_4_existing_file_occupies_unsuffixed_slot () =
  let meta     = [src_root ^ "/IMG_0001.jpg", dto "2024:06:15 14:32:08"] in
  let existing = [dest_root ^ "/2024/06/2024-06-15_14-32-08_IMG_0001.jpg"] in
  check "existing occupies slot"
    [Move (sf "IMG_0001.jpg", dp "2024/06/2024-06-15_14-32-08_IMG_0001-1.jpg")]
    (run ~meta ~existing [sf "IMG_0001.jpg"])

let test_5_5_existing_and_dash1_forces_dash2 () =
  let meta     = [src_root ^ "/IMG_0001.jpg", dto "2024:06:15 14:32:08"] in
  let existing = [
    dest_root ^ "/2024/06/2024-06-15_14-32-08_IMG_0001.jpg";
    dest_root ^ "/2024/06/2024-06-15_14-32-08_IMG_0001-1.jpg";
  ] in
  check "existing -1 forces -2"
    [Move (sf "IMG_0001.jpg", dp "2024/06/2024-06-15_14-32-08_IMG_0001-2.jpg")]
    (run ~meta ~existing [sf "IMG_0001.jpg"])

let test_5_6_collision_in_unparsed_dir () =
  let meta = [
    src_root ^ "/dir1/scan.png", no_tags;
    src_root ^ "/dir2/scan.png", no_tags;
  ] in
  check "collision in unparsed/"
    [ Unparsed (sf "dir1/scan.png", dp "unparsed/scan.png");
      Unparsed (sf "dir2/scan.png", dp "unparsed/scan-1.png") ]
    (run ~meta [sf "dir1/scan.png"; sf "dir2/scan.png"])

let test_5_7_sort_order_determines_suffix_assignment () =
  let meta = [
    src_root ^ "/z/IMG_0001.jpg", dto "2024:06:15 14:32:08";
    src_root ^ "/a/IMG_0001.jpg", dto "2024:06:15 14:32:08";
  ] in
  check "lexicographic sort determines suffix"
    [ Move (sf "a/IMG_0001.jpg", dp "2024/06/2024-06-15_14-32-08_IMG_0001.jpg");
      Move (sf "z/IMG_0001.jpg", dp "2024/06/2024-06-15_14-32-08_IMG_0001-1.jpg") ]
    (run ~meta [sf "z/IMG_0001.jpg"; sf "a/IMG_0001.jpg"])

let source_of = function
  | Move (s, _) | Unparsed (s, _) | Skip s | Err (s, _) -> s.abs_path

let test_8_1_output_sorted_by_source_path () =
  let meta = [
    src_root ^ "/c.jpg", dto "2024:01:01 10:02:00";
    src_root ^ "/a.jpg", dto "2024:01:01 10:00:00";
    src_root ^ "/b.jpg", dto "2024:01:01 10:01:00";
  ] in
  let ops   = run ~meta [sf "c.jpg"; sf "a.jpg"; sf "b.jpg"] in
  let paths = List.map source_of ops in
  Alcotest.(check (list string)) "sorted"
    (List.sort String.compare paths) paths

let test_8_1_stable_regardless_of_input_order () =
  let meta = [
    src_root ^ "/a.jpg", dto "2024:01:01 10:00:00";
    src_root ^ "/b.jpg", dto "2024:01:01 10:01:00";
  ] in
  check "stable output"
    (run ~meta [sf "a.jpg"; sf "b.jpg"])
    (run ~meta [sf "b.jpg"; sf "a.jpg"])

let test_11_1_empty_source () =
  check "empty source" [] (run [])

let test_11_2_all_unsupported () =
  let ops = run [sf "video.mp4"; sf "readme.txt"; sf "data.csv"] in
  Alcotest.(check int) "count" 3 (List.length ops);
  List.iter (function
    | Skip _ -> ()
    | op -> Alcotest.failf "expected Skip, got %a" pp_op op
  ) ops

let test_11_3_all_unparsed () =
  let meta = [src_root ^ "/a.jpg", no_tags; src_root ^ "/b.jpg", no_tags] in
  let ops  = run ~meta [sf "a.jpg"; sf "b.jpg"] in
  Alcotest.(check int) "count" 2 (List.length ops);
  List.iter (function
    | Unparsed _ -> ()
    | op -> Alcotest.failf "expected Unparsed, got %a" pp_op op
  ) ops

let test_11_4_all_four_operation_types () =
  let bad  = src_root ^ "/bad\nfile.jpg" in
  let meta = [
    src_root ^ "/IMG_0001.jpg", dto "2024:06:15 14:32:08";
    src_root ^ "/scan.png",     no_tags;
  ] in
  let ops = run ~meta [
    sf "IMG_0001.jpg";
    sf "scan.png";
    sf "video.mp4";
    { abs_path = bad; ext = ".jpg" };
  ] in
  Alcotest.(check int) "total" 4 (List.length ops);
  let has f = List.exists f ops in
  Alcotest.(check bool) "Move"     true (has (function Move _     -> true | _ -> false));
  Alcotest.(check bool) "Unparsed" true (has (function Unparsed _ -> true | _ -> false));
  Alcotest.(check bool) "Skip"     true (has (function Skip _     -> true | _ -> false));
  Alcotest.(check bool) "Err"      true (has (function Err _      -> true | _ -> false))

let () =
  Alcotest.run "planning" [
    "§1 basic move", [
      Alcotest.test_case "1.1 single file"             `Quick test_1_1_single_file;
      Alcotest.test_case "1.2 epoch boundary"          `Quick test_1_2_epoch_boundary;
      Alcotest.test_case "1.3 month zero-padded"       `Quick test_1_3_month_zero_padded;
      Alcotest.test_case "1.4 uppercase ext preserved" `Quick test_1_4_uppercase_extension_preserved;
      Alcotest.test_case "1.5 mixed case ext"          `Quick test_1_5_mixed_case_extension_preserved;
      Alcotest.test_case "1.6 internal dots in stem"   `Quick test_1_6_stem_with_internal_dots;
      Alcotest.test_case "1.7 spaces in stem"          `Quick test_1_7_stem_with_spaces;
      Alcotest.test_case "1.8 subdir not reproduced"   `Quick test_1_8_subdir_not_reproduced_in_dest;
    ];
    "§2 skip", [
      Alcotest.test_case "2.1 unsupported ext"         `Quick test_2_1_unsupported_extension;
      Alcotest.test_case "2.2 no extension"            `Quick test_2_2_no_extension;
      Alcotest.test_case "2.3 mixed types"             `Quick test_2_3_mixed_supported_and_unsupported;
      Alcotest.test_case "2.4 case-insensitive match"  `Quick test_2_4_extension_match_case_insensitive_dest_case_preserved;
    ];
    "§3 unparsed", [
      Alcotest.test_case "3.1 no tags"                 `Quick test_3_1_no_tags;
      Alcotest.test_case "3.1b absent entry"           `Quick test_3_1b_absent_metadata_entry;
      Alcotest.test_case "3.2 CreateDate fallback"     `Quick test_3_2_create_date_fallback;
      Alcotest.test_case "3.3 DTO wins"                `Quick test_3_3_dto_wins_over_create_date;
    ] @ List.map (fun (ts, label) ->
      Alcotest.test_case ("invalid: " ^ label) `Quick (test_invalid_timestamp (ts, label))
    ) invalid_timestamps @ [
      Alcotest.test_case "3.11 DTO invalid, CD valid"  `Quick test_3_11_dto_invalid_falls_through_to_create_date;
      Alcotest.test_case "3.12 both invalid"           `Quick test_3_12_both_invalid_becomes_unparsed;
      Alcotest.test_case "3 filename preserved"        `Quick test_3_unparsed_preserves_original_filename;
    ];
    "§4 err", [
      Alcotest.test_case "4.1 newline in path"         `Quick test_4_1_newline_in_path;
      Alcotest.test_case "4.1 no contamination"        `Quick test_4_1_newline_does_not_contaminate_other_files;
    ];
    "§5 collision", [
      Alcotest.test_case "5.1 two same stem"           `Quick test_5_1_two_files_same_timestamp_same_stem;
      Alcotest.test_case "5.2 three same stem"         `Quick test_5_2_three_files_same_timestamp_same_stem;
      Alcotest.test_case "5.3 different stems"         `Quick test_5_3_different_stems_no_collision;
      Alcotest.test_case "5.4 existing occupies slot"  `Quick test_5_4_existing_file_occupies_unsuffixed_slot;
      Alcotest.test_case "5.5 existing -1 forces -2"   `Quick test_5_5_existing_and_dash1_forces_dash2;
      Alcotest.test_case "5.6 collision in unparsed/"  `Quick test_5_6_collision_in_unparsed_dir;
      Alcotest.test_case "5.7 sort determines suffix"  `Quick test_5_7_sort_order_determines_suffix_assignment;
    ];
    "§8 ordering", [
      Alcotest.test_case "8.1 sorted by source path"   `Quick test_8_1_output_sorted_by_source_path;
      Alcotest.test_case "8.1 stable output"           `Quick test_8_1_stable_regardless_of_input_order;
    ];
    "§11 edge cases", [
      Alcotest.test_case "11.1 empty source"           `Quick test_11_1_empty_source;
      Alcotest.test_case "11.2 all unsupported"        `Quick test_11_2_all_unsupported;
      Alcotest.test_case "11.3 all unparsed"           `Quick test_11_3_all_unparsed;
      Alcotest.test_case "11.4 all four types"         `Quick test_11_4_all_four_operation_types;
    ];
  ]
