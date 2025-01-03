(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open More

let spair = Test.(pair T.string T.string)
let spair_opt = Test.(option (T.pair T.string T.string))
let slist = Test.(list T.string)
let int_opt = Test.(option T.int)

let test_find_first =
  Test.test "String.find_first" @@ fun () ->
  Test.invalid_arg ~__POS__ (fun () ->
      String.find_first ~start:(-1) ~sub:"" "");
  int_opt (String.find_first ~start:0 ~sub:"" "") (Some 0);
  Test.invalid_arg ~__POS__ (fun () -> String.find_first ~start:1 ~sub:"" "");
  int_opt (String.find_first ~start:0 ~sub:"" "ab") (Some 0);
  int_opt (String.find_first ~start:1 ~sub:"" "ab") (Some 1);
  int_opt (String.find_first ~start:2 ~sub:"" "ab") (Some 2);
  Test.invalid_arg ~__POS__ (fun () -> String.find_first ~start:3 ~sub:"" "ab");
  int_opt (String.find_first ~start:0 ~sub:"a" "") (None);
  int_opt (String.find_first ~start:0 ~sub:"a" "a") (Some 0);
  int_opt (String.find_first ~start:1 ~sub:"a" "a") (None);
  int_opt (String.find_first ~start:0 ~sub:"a" "ba") (Some 1);
  int_opt (String.find_first ~start:1 ~sub:"a" "ba") (Some 1);
  int_opt (String.find_first ~start:2 ~sub:"a" "ba") (None);
  int_opt (String.find_first ~start:0 ~sub:"ab" "") (None);
  int_opt (String.find_first ~start:0 ~sub:"ab" "ab") (Some 0);
  int_opt (String.find_first ~start:0 ~sub:"ab" "aab") (Some 1);
  int_opt (String.find_first ~start:1 ~sub:"ab" "aab") (Some 1);
  int_opt (String.find_first ~start:2 ~sub:"ab" "aab") (None);
  int_opt (String.find_first ~start:3 ~sub:"ab" "aab") (None);
  Test.invalid_arg ~__POS__
    (fun () -> String.find_first ~start:(-1) ~sub:"abaa" "aba");
  int_opt (String.find_first ~start:0 ~sub:"abaa" "aba") (None);
  int_opt (String.find_first ~start:2 ~sub:"abaa" "aba") (None);
  int_opt (String.find_first ~start:3 ~sub:"abaa" "aba") (None);
  Test.invalid_arg ~__POS__
    (fun () -> String.find_first ~start:4 ~sub:"abaa" "aba");
  int_opt (String.find_first ~start:0 ~sub:"aba" "ababa") (Some 0);
  int_opt (String.find_first ~start:1 ~sub:"aba" "ababa") (Some 2);
  int_opt (String.find_first ~start:2 ~sub:"aba" "ababa") (Some 2);
  int_opt (String.find_first ~start:3 ~sub:"aba" "ababa") (None);
  int_opt (String.find_first ~start:4 ~sub:"aba" "ababa") (None);
  int_opt (String.find_first ~start:5 ~sub:"aba" "ababa") (None);
  ()

let test_find_last =
  Test.test "String.find_last" @@ fun () ->
  Test.invalid_arg ~__POS__ (fun () -> String.find_last ~start:(-1) ~sub:"" "");
  int_opt (String.find_last ~start:0 ~sub:"" "") (Some 0) ~__POS__;
  Test.invalid_arg ~__POS__ (fun () -> String.find_last ~start:1 ~sub:"" "");
  int_opt (String.find_last ~start:0 ~sub:"" "ab") (Some 0) ~__POS__;
  int_opt (String.find_last ~start:1 ~sub:"" "ab") (Some 1) ~__POS__;
  int_opt (String.find_last ~start:2 ~sub:"" "ab") (Some 2) ~__POS__;
  Test.invalid_arg ~__POS__ (fun () -> String.find_last ~start:3 ~sub:"" "ab");
  int_opt (String.find_last ~start:0 ~sub:"a" "") (None) ~__POS__;
  int_opt (String.find_last ~start:0 ~sub:"a" "a") (Some 0) ~__POS__;
  int_opt (String.find_last ~start:1 ~sub:"a" "a") (Some 0) ~__POS__;
  int_opt (String.find_last ~start:0 ~sub:"a" "ba") (None) ~__POS__;
  int_opt (String.find_last ~start:1 ~sub:"a" "ba") (Some 1) ~__POS__;
  int_opt (String.find_last ~start:2 ~sub:"a" "ba") (Some 1) ~__POS__;
  int_opt (String.find_last ~start:0 ~sub:"ab" "") (None) ~__POS__;
  int_opt (String.find_last ~start:0 ~sub:"ab" "ab") (Some 0) ~__POS__;
  int_opt (String.find_last ~start:0 ~sub:"ab" "aab") (None) ~__POS__;
  int_opt (String.find_last ~start:1 ~sub:"ab" "aab") (Some 1) ~__POS__;
  int_opt (String.find_last ~start:2 ~sub:"ab" "aab") (Some 1) ~__POS__;
  int_opt (String.find_last ~start:3 ~sub:"ab" "aab") (Some 1) ~__POS__;
  Test.invalid_arg ~__POS__
    (fun () -> String.find_last ~start:(-1) ~sub:"abaa" "aba");
  int_opt (String.find_last ~start:0 ~sub:"abaa" "aba") (None) ~__POS__;
  int_opt (String.find_last ~start:2 ~sub:"abaa" "aba") (None) ~__POS__;
  int_opt (String.find_last ~start:3 ~sub:"abaa" "aba") (None) ~__POS__;
  Test.invalid_arg ~__POS__
    (fun () -> String.find_last ~start:4 ~sub:"abaa" "aba");
  int_opt (String.find_last ~start:0 ~sub:"aba" "ababa") (Some 0) ~__POS__;
  int_opt (String.find_last ~start:1 ~sub:"aba" "ababa") (Some 0) ~__POS__;
  int_opt (String.find_last ~start:2 ~sub:"aba" "ababa") (Some 2) ~__POS__;
  int_opt (String.find_last ~start:3 ~sub:"aba" "ababa") (Some 2) ~__POS__;
  int_opt (String.find_last ~start:4 ~sub:"aba" "ababa") (Some 2) ~__POS__;
  int_opt (String.find_last ~start:5 ~sub:"aba" "ababa") (Some 2) ~__POS__;
  int_opt (String.find_last ~sub:"ab" "aabb") (Some 1) ~__POS__;
  ()

let test_includes =
  Test.test "String.includes" @@ fun () ->
  Test.bool (String.includes ~affix:"" "") true;
  Test.bool (String.includes ~affix:"" "a") true;
  Test.bool (String.includes ~affix:"" "ab") true;
  Test.bool (String.includes ~affix:"a" "") false;
  Test.bool (String.includes ~affix:"a" "a") true;
  Test.bool (String.includes ~affix:"a" "ab") true;
  Test.bool (String.includes ~affix:"a" "ba") true;
  Test.bool (String.includes ~affix:"a" "bab") true;
  Test.bool (String.includes ~affix:"ab" "") false;
  Test.bool (String.includes ~affix:"ab" "a") false;
  Test.bool (String.includes ~affix:"ab" "ab") true;
  Test.bool (String.includes ~affix:"ab" "aab") true;
  Test.bool (String.includes ~affix:"aab" "ab") false;
  Test.bool (String.includes ~affix:"ab" "aba") true;
  Test.bool (String.includes ~affix:"ab" "aaba") true;
  Test.bool (String.includes ~affix:"abab" "aababa") true;
  ()

let test_replace_first =
  Test.test "String.replace_first" @@ fun () ->
  Test.string (String.replace_first ~sub:"" ~by:"" "") "";
  Test.string (String.replace_first ~sub:"" ~by:"a" "") "a";
  Test.string (String.replace_first ~sub:"" ~by:"a" "123") "a123";
  Test.string (String.replace_first ~start:1 ~sub:"" ~by:"a" "123") "1a23";
  Test.string (String.replace_first ~start:2 ~sub:"" ~by:"a" "123") "12a3";
  Test.string (String.replace_first ~start:3 ~sub:"" ~by:"a" "123") "123a";
  Test.string (String.replace_first ~sub:"1" ~by:"" "123") "23";
  Test.string (String.replace_first ~sub:"3" ~by:"" "123") "12";
  Test.string (String.replace_first ~sub:"1" ~by:"" "1") "";
  Test.string (String.replace_first ~sub:"12" ~by:"z" "123") "z3";
  Test.string (String.replace_first ~start:2 ~sub:"" ~by:"z" "123") "12z3";
  Test.string (String.replace_first ~start:3 ~sub:"" ~by:"z" "123") "123z";
  Test.string (String.replace_first ~start:3 ~sub:"a" ~by:"z" "123") "123";
  Test.string (String.replace_first ~sub:"aba" ~by:"c" "ababa") "cba";
  ()

let test_replace_last =
  Test.test "String.replace_last" @@ fun () ->
  Test.string (String.replace_last ~sub:"" ~by:"" "") "";
  Test.string (String.replace_last ~sub:"" ~by:"a" "") "a";
  Test.string (String.replace_last ~sub:"" ~by:"a" "123") "123a";
  Test.string (String.replace_last ~start:1 ~sub:"" ~by:"a" "123") "1a23";
  Test.string (String.replace_last ~start:2 ~sub:"" ~by:"a" "123") "12a3";
  Test.string (String.replace_last ~start:3 ~sub:"" ~by:"a" "123") "123a";
  Test.string (String.replace_last ~sub:"1" ~by:"" "123") "23";
  Test.string (String.replace_last ~sub:"3" ~by:"" "123") "12";
  Test.string (String.replace_last ~sub:"1" ~by:"" "1") "";
  Test.string (String.replace_last ~sub:"12" ~by:"z" "123") "z3";
  Test.string (String.replace_last ~start:2 ~sub:"" ~by:"z" "123") "12z3";
  Test.string (String.replace_last ~start:3 ~sub:"" ~by:"z" "123") "123z";
  Test.string (String.replace_last ~start:3 ~sub:"a" ~by:"z" "123") "123";
  Test.string (String.replace_last ~sub:"aba" ~by:"c" "ababa") "abc";
  ()

let test_replace_all =
  Test.test "String.replace_all" @@ fun () ->
  Test.string (String.replace_all ~sub:"" ~by:"" "") "" ~__POS__;
  Test.string (String.replace_all ~sub:"" ~by:"" "1") "1" ~__POS__;
  Test.string (String.replace_all ~sub:"" ~by:"" "12") "12" ~__POS__;
  Test.string (String.replace_all ~sub:"" ~by:"a" "") "a" ~__POS__;
  Test.string (String.replace_all ~sub:"" ~by:"a" "1") "a1a" ~__POS__;
  Test.string (String.replace_all ~sub:"" ~by:"a" "12") "a1a2a" ~__POS__;
  Test.string (String.replace_all ~sub:"" ~by:"a" "123") "a1a2a3a" ~__POS__;
  Test.string (String.replace_all ~start:0 ~sub:"" ~by:"a" "123")
    "a1a2a3a" ~__POS__;
  Test.string (String.replace_all ~start:1 ~sub:"" ~by:"a" "123")
    "1a2a3a" ~__POS__;
  Test.string (String.replace_all ~start:2 ~sub:"" ~by:"a" "123")
    "12a3a" ~__POS__;
  Test.string (String.replace_all ~start:3 ~sub:"" ~by:"a" "123")
    "123a" ~__POS__;
  Test.string (String.replace_all ~sub:"1" ~by:"" "121") "2" ~__POS__;
  Test.string (String.replace_all ~sub:"1" ~by:"3" "121") "323" ~__POS__;
  Test.string (String.replace_all ~sub:"1" ~by:"" "1") "" ~__POS__;
  Test.string (String.replace_all ~sub:"12" ~by:"a" "123") "a3" ~__POS__;
  Test.string (String.replace_all ~sub:"12" ~by:"a" "123112") "a31a" ~__POS__;
  Test.string (String.replace_all ~start:1 ~sub:"12" ~by:"a" "123112")
    "1231a" ~__POS__;
  ()

let test_subrange =
  Test.test "String.subrange" @@ fun () ->
  Test.string (String.subrange "") "" ~__POS__;
  Test.string (String.subrange ~first:(-1) "") "" ~__POS__;
  Test.string (String.subrange ~last:(-1) "") "" ~__POS__;
  Test.string (String.subrange ~first:1 "") "" ~__POS__;
  Test.string (String.subrange ~last:1 "") "" ~__POS__;
  Test.string (String.subrange "ab") "ab" ~__POS__;
  Test.string (String.subrange ~first:(-1) "ab") "ab" ~__POS__;
  Test.string (String.subrange ~last:(-1) "ab") "" ~__POS__;
  Test.string (String.subrange ~first:1 "ab") "b" ~__POS__;
  Test.string (String.subrange ~last:1 "ab") "ab" ~__POS__;
  Test.string (String.subrange ~first:1 ~last:1 "ab") "b" ~__POS__;
  Test.string (String.subrange ~first:1 ~last:0 "ab") "" ~__POS__;
  ()

let test_breaking_magnitude =
  Test.test "String.{take,drop,cut}_{first,last}" @@ fun () ->
  Test.string (String.take_first (-1) "") "" ~__POS__;
  Test.string (String.take_first (-1) "a") "" ~__POS__;
  Test.string (String.take_first (-1) "ab") "" ~__POS__;
  Test.string (String.take_first 0 "") "" ~__POS__;
  Test.string (String.take_first 0 "a") "" ~__POS__;
  Test.string (String.take_first 0 "ab") "" ~__POS__;
  Test.string (String.take_first 1 "") "" ~__POS__;
  Test.string (String.take_first 1 "a") "a" ~__POS__;
  Test.string (String.take_first 1 "ab") "a" ~__POS__;
  Test.string (String.take_first 2 "") "" ~__POS__;
  Test.string (String.take_first 2 "a") "a" ~__POS__;
  Test.string (String.take_first 2 "ab") "ab" ~__POS__;
  Test.string (String.take_last (-1) "") "" ~__POS__;
  Test.string (String.take_last (-1) "a") "" ~__POS__;
  Test.string (String.take_last (-1) "ab") "" ~__POS__;
  Test.string (String.take_last 0 "") "" ~__POS__;
  Test.string (String.take_last 0 "a") "" ~__POS__;
  Test.string (String.take_last 0 "ab") "" ~__POS__;
  Test.string (String.take_last 1 "") "" ~__POS__;
  Test.string (String.take_last 1 "a") "a" ~__POS__;
  Test.string (String.take_last 1 "ab") "b" ~__POS__;
  Test.string (String.take_last 2 "") "" ~__POS__;
  Test.string (String.take_last 2 "a") "a" ~__POS__;
  Test.string (String.take_last 2 "ab") "ab" ~__POS__;
  Test.string (String.drop_first (-1) "") "" ~__POS__;
  Test.string (String.drop_first (-1) "a") "a" ~__POS__;
  Test.string (String.drop_first (-1) "ab") "ab" ~__POS__;
  Test.string (String.drop_first 0 "") "" ~__POS__;
  Test.string (String.drop_first 0 "a") "a" ~__POS__;
  Test.string (String.drop_first 0 "ab") "ab" ~__POS__;
  Test.string (String.drop_first 1 "") "" ~__POS__;
  Test.string (String.drop_first 1 "a") "" ~__POS__;
  Test.string (String.drop_first 1 "ab") "b" ~__POS__;
  Test.string (String.drop_first 2 "") "" ~__POS__;
  Test.string (String.drop_first 2 "a") "" ~__POS__;
  Test.string (String.drop_first 2 "ab") "" ~__POS__;
  Test.string (String.drop_last (-1) "") "" ~__POS__;
  Test.string (String.drop_last (-1) "a") "a" ~__POS__;
  Test.string (String.drop_last (-1) "ab") "ab" ~__POS__;
  Test.string (String.drop_last 0 "") "" ~__POS__;
  Test.string (String.drop_last 0 "a") "a" ~__POS__;
  Test.string (String.drop_last 0 "ab") "ab" ~__POS__;
  Test.string (String.drop_last 1 "") "" ~__POS__;
  Test.string (String.drop_last 1 "a") "" ~__POS__;;
  Test.string (String.drop_last 1 "ab") "a" ~__POS__;
  Test.string (String.drop_last 2 "") "" ~__POS__;
  Test.string (String.drop_last 2 "a") "" ~__POS__;
  Test.string (String.drop_last 2 "ab") "" ~__POS__;
  spair (String.cut_first (-1) "") ("", "") ~__POS__;
  spair (String.cut_first (-1) "a") ("", "a") ~__POS__;
  spair (String.cut_first (-1) "ab") ("", "ab") ~__POS__;
  spair (String.cut_first 0 "") ("", "") ~__POS__;
  spair (String.cut_first 0 "a") ("", "a") ~__POS__;
  spair (String.cut_first 0 "ab") ("", "ab") ~__POS__;
  spair (String.cut_first 1 "") ("", "") ~__POS__;
  spair (String.cut_first 1 "a") ("a", "") ~__POS__;
  spair (String.cut_first 1 "ab") ("a", "b") ~__POS__;
  spair (String.cut_first 2 "") ("", "") ~__POS__;
  spair (String.cut_first 2 "a") ("a", "") ~__POS__;
  spair (String.cut_first 2 "ab") ("ab", "") ~__POS__;
  spair (String.cut_last (-1) "") ("", "") ~__POS__;
  spair (String.cut_last (-1) "a") ("a", "") ~__POS__;
  spair (String.cut_last (-1) "ab") ("ab", "") ~__POS__;
  spair (String.cut_last 0 "") ("", "") ~__POS__;
  spair (String.cut_last 0 "a") ("a", "") ~__POS__;
  spair (String.cut_last 0 "ab") ("ab", "") ~__POS__;
  spair (String.cut_last 1 "") ("", "") ~__POS__;
  spair (String.cut_last 1 "a") ("", "a") ~__POS__;
  spair (String.cut_last 1 "ab") ("a", "b") ~__POS__;
  spair (String.cut_last 2 "") ("", "") ~__POS__;
  spair (String.cut_last 2 "a") ("", "a") ~__POS__;
  spair (String.cut_last 2 "ab") ("", "ab") ~__POS__;
  ()

let test_breaking_predicates =
  Test.test "String.{take,drop,cut}_{first,last}_while" @@ fun () ->
  Test.string (String.take_first_while Char.Ascii.is_white "") "" ~__POS__;
  Test.string (String.take_first_while Char.Ascii.is_white "abc") "" ~__POS__;
  Test.string (String.take_first_while Char.Ascii.is_white "  abc") "  "
    ~__POS__;
  Test.string (String.drop_first_while Char.Ascii.is_white "") "" ~__POS__;
  Test.string (String.drop_first_while Char.Ascii.is_white "abc") "abc"
    ~__POS__;
  Test.string (String.drop_first_while Char.Ascii.is_white "  abc") "abc"
    ~__POS__;
  spair (String.cut_first_while Char.Ascii.is_white "") ("", "") ~__POS__;
  spair (String.cut_first_while Char.Ascii.is_white "abc") ("", "abc") ~__POS__;
  spair (String.cut_first_while Char.Ascii.is_white "  abc") ("  ", "abc")
    ~__POS__;
  Test.string (String.take_last_while Char.Ascii.is_white "") "" ~__POS__;
  Test.string (String.take_last_while Char.Ascii.is_white "abc") "" ~__POS__;
  Test.string (String.take_last_while Char.Ascii.is_white "abc  ") "  "
    ~__POS__;
  Test.string (String.drop_last_while Char.Ascii.is_white "") "" ~__POS__;
  Test.string (String.drop_last_while Char.Ascii.is_white "abc") "abc" ~__POS__;
  Test.string (String.drop_last_while Char.Ascii.is_white "abc  ") "abc"
    ~__POS__;
  spair (String.cut_last_while Char.Ascii.is_white "") ("", "") ~__POS__;
  spair (String.cut_last_while Char.Ascii.is_white "abc") ("abc", "") ~__POS__;
  spair (String.cut_last_while Char.Ascii.is_white "abc  ") ("abc", "  ")
    ~__POS__;
  ()

let test_next_token =
  Test.test "String.{take,drop,cut}_token" @@ fun () ->
  let snap = Snap.string in
  snap (String.take_token "bla") @> __POS_OF__ "bla";
  snap (String.take_token " bla ha") @> __POS_OF__ "bla";
  snap (String.take_token "\xFF") @> __POS_OF__ "";
  snap (String.take_token " \xFF") @> __POS_OF__ "";
  snap (String.take_token "") @> __POS_OF__ "";
  snap (String.drop_token "bla") @> __POS_OF__ "";
  snap (String.drop_token " bla ha") @> __POS_OF__ " ha";
  snap (String.drop_token "\xFF") @> __POS_OF__ "\xFF";
  snap (String.drop_token " \xFF") @> __POS_OF__ "\xFF";
  snap (String.drop_token "") @> __POS_OF__ "";
  let snap = Snap.(t2 T.string T.string) in
  snap (String.cut_token "bla") @> __POS_OF__ ("bla", "");
  snap (String.cut_token " bla ha") @> __POS_OF__ ("bla", " ha");
  snap (String.cut_token "\xFF") @> __POS_OF__ ("", "\xFF");
  snap (String.cut_token " \xFF") @> __POS_OF__ ("", "\xFF");
  snap (String.cut_token "") @> __POS_OF__ ("", "");
  ()

let test_split_first =
  Test.test "String.split_first" @@ fun () ->
  spair_opt (String.split_first ~sep:"" "") (Some ("", "")) ~__POS__;
  spair_opt (String.split_first ~sep:"" "a") (Some ("", "a")) ~__POS__;
  spair_opt (String.split_first ~sep:"" "ab") (Some ("", "ab")) ~__POS__;
  spair_opt (String.split_first ~sep:"a" "") None ~__POS__;
  spair_opt (String.split_first ~sep:"a" "b") None ~__POS__;
  spair_opt (String.split_first ~sep:"a" "ab") (Some ("", "b")) ~__POS__;
  spair_opt (String.split_first ~sep:"a" "ba") (Some ("b", "")) ~__POS__;
  spair_opt (String.split_first ~sep:"ab" "") None ~__POS__;
  spair_opt (String.split_first ~sep:"ab" "a") None ~__POS__;
  spair_opt (String.split_first ~sep:"ab" "b") None ~__POS__;
  spair_opt (String.split_first ~sep:"ab" "ab") (Some ("", "")) ~__POS__;
  spair_opt (String.split_first ~sep:"ab" "aab") (Some ("a", "")) ~__POS__;
  spair_opt (String.split_first ~sep:"ab" "aba") (Some ("", "a")) ~__POS__;
  spair_opt (String.split_first ~sep:"ab" "abab") (Some ("", "ab")) ~__POS__;
  spair_opt (String.split_first ~sep:"ab" "aabb") (Some ("a", "b")) ~__POS__;
  ()

let test_split_last =
  Test.test "String.split_last" @@ fun () ->
  spair_opt (String.split_last ~sep:"" "") (Some ("", "")) ~__POS__;
  spair_opt (String.split_last ~sep:"" "a") (Some ("a", "")) ~__POS__;
  spair_opt (String.split_last ~sep:"" "ab") (Some ("ab", "")) ~__POS__;
  spair_opt (String.split_last ~sep:"a" "") None ~__POS__;
  spair_opt (String.split_last ~sep:"a" "b") None ~__POS__;
  spair_opt (String.split_last ~sep:"a" "ab") (Some ("", "b")) ~__POS__;
  spair_opt (String.split_last ~sep:"a" "ba") (Some ("b", "")) ~__POS__;
  spair_opt (String.split_last ~sep:"ab" "") None ~__POS__;
  spair_opt (String.split_last ~sep:"ab" "a") None ~__POS__;
  spair_opt (String.split_last ~sep:"ab" "b") None ~__POS__;
  spair_opt (String.split_last ~sep:"ab" "ab") (Some ("", "")) ~__POS__;
  spair_opt (String.split_last ~sep:"ab" "aab") (Some ("a", "")) ~__POS__;
  spair_opt (String.split_last ~sep:"ab" "aba") (Some ("", "a")) ~__POS__;
  spair_opt (String.split_last ~sep:"ab" "abab") (Some ("ab", "")) ~__POS__;
  spair_opt (String.split_last ~sep:"ab" "aabb") (Some ("a", "b")) ~__POS__;
  ()

let test_split_all =
  Test.test "String.split_all" @@ fun () ->
  slist (String.split_all ~sep:"" "") [""; ""] ~__POS__;
  slist (String.split_all ~sep:"" "a") [""; "a"; ""] ~__POS__;
  slist (String.split_all ~sep:"" "ab") [""; "a"; "b"; ""] ~__POS__;
  slist (String.split_all ~sep:"" "abc") [""; "a"; "b"; "c"; ""] ~__POS__;
  slist (String.split_all ~sep:"a" "") [""] ~__POS__;
  slist (String.split_all ~sep:"a" "a") ["";""] ~__POS__;
  slist (String.split_all ~sep:"a" "ab") [""; "b"] ~__POS__;
  slist (String.split_all ~sep:"a" "ba") ["b"; ""] ~__POS__;
  slist (String.split_all ~sep:"a" "abc") [""; "bc"] ~__POS__;
  slist (String.split_all ~sep:"a" "aba") [""; "b"; ""] ~__POS__;
  slist (String.split_all ~sep:"a" "bab") ["b"; "b"] ~__POS__;
  slist (String.split_all ~sep:"a" "babbab") ["b"; "bb"; "b"] ~__POS__;
  slist (String.split_all ~sep:"ab" "") [""] ~__POS__;
  slist (String.split_all ~sep:"ab" "a") ["a"] ~__POS__;
  slist (String.split_all ~sep:"ab" "b") ["b"] ~__POS__;
  slist (String.split_all ~sep:"ab" "ab") [""; ""] ~__POS__;
  slist (String.split_all ~sep:"ab" "aab") ["a"; ""] ~__POS__;
  slist (String.split_all ~sep:"ab" "aba") [""; "a"] ~__POS__;
  slist (String.split_all ~sep:"ab" "abab") [""; ""; ""] ~__POS__;
  slist (String.split_all ~sep:"ab" "aabb") ["a"; "b"] ~__POS__;
  slist (String.split_all ~sep:"ab" "aaabbb") ["aa"; "bb"] ~__POS__;
  slist (String.split_all ~sep:"aba" "ababa") [""; "ba"] ~__POS__;
  slist (String.split_all ~sep:"a" "abaa") [""; "b"; ""; ""] ~__POS__;
  slist (String.split_all ~drop:String.is_empty ~sep:"a" "abaa") ["b"] ~__POS__;
  ()

let test_rsplit_all =
  Test.test "String.rsplit_all" @@ fun () ->
  slist (String.rsplit_all ~sep:"" "") [""; ""] ~__POS__;
  slist (String.rsplit_all ~sep:"" "a") [""; "a"; ""] ~__POS__;
  slist (String.rsplit_all ~sep:"" "ab") [""; "a"; "b"; ""] ~__POS__;
  slist (String.rsplit_all ~sep:"" "abc") [""; "a"; "b"; "c"; ""] ~__POS__;
  slist (String.rsplit_all ~sep:"a" "") [""] ~__POS__;
  slist (String.rsplit_all ~sep:"a" "a") ["";""] ~__POS__;
  slist (String.rsplit_all ~sep:"a" "ab") [""; "b"] ~__POS__;
  slist (String.rsplit_all ~sep:"a" "ba") ["b"; ""] ~__POS__;
  slist (String.rsplit_all ~sep:"a" "abc") [""; "bc"] ~__POS__;
  slist (String.rsplit_all ~sep:"a" "aba") [""; "b"; ""] ~__POS__;
  slist (String.rsplit_all ~sep:"a" "bab") ["b"; "b"] ~__POS__;
  slist (String.rsplit_all ~sep:"a" "babbab") ["b"; "bb"; "b"] ~__POS__;
  slist (String.rsplit_all ~sep:"ab" "") [""] ~__POS__;
  slist (String.rsplit_all ~sep:"ab" "a") ["a"] ~__POS__;
  slist (String.rsplit_all ~sep:"ab" "b") ["b"] ~__POS__;
  slist (String.rsplit_all ~sep:"ab" "ab") [""; ""] ~__POS__;
  slist (String.rsplit_all ~sep:"ab" "aab") ["a"; ""] ~__POS__;
  slist (String.rsplit_all ~sep:"ab" "aba") [""; "a"] ~__POS__;
  slist (String.rsplit_all ~sep:"ab" "abab") [""; ""; ""] ~__POS__;
  slist (String.rsplit_all ~sep:"ab" "aabb") ["a"; "b"] ~__POS__;
  slist (String.rsplit_all ~sep:"ab" "aaabbb") ["aa"; "bb"] ~__POS__;
  slist (String.rsplit_all ~sep:"aba" "ababa") ["ab"; ""] ~__POS__;
  slist (String.rsplit_all ~sep:"a" "abaa") [""; "b"; ""; ""] ~__POS__;
  slist (String.rsplit_all
           ~drop:String.is_empty ~sep:"a" "abaa") ["b"] ~__POS__;
  ()

let test_find_first_index =
  Test.test "String.find_{first,last}_index" @@ fun () ->
  let is_letter = Char.Ascii.is_letter in
  Test.invalid_arg ~__POS__ (fun () ->
      String.find_first_index ~start:(-1) is_letter "");
  Test.invalid_arg ~__POS__ (fun () ->
      String.find_last_index ~start:(-1) is_letter "");
  int_opt (String.find_first_index ~start:0 is_letter "") None;
  int_opt (String.find_last_index ~start:0 is_letter "") None;
  Test.invalid_arg ~__POS__ (fun () ->
      String.find_first_index ~start:1 is_letter "");
  Test.invalid_arg ~__POS__ (fun () ->
      String.find_last_index ~start:1 is_letter "");
  int_opt (String.find_first_index ~start:0 is_letter "-") None;
  int_opt (String.find_first_index ~start:1 is_letter "-") None;
  int_opt (String.find_last_index ~start:0 is_letter "-") None;
  int_opt (String.find_last_index ~start:1 is_letter "-") None;
  int_opt (String.find_first_index ~start:0 is_letter "a-") (Some 0);
  int_opt (String.find_first_index ~start:1 is_letter "a-") None;
  int_opt (String.find_first_index ~start:2 is_letter "a-") None;
  int_opt (String.find_last_index ~start:0 is_letter "a-") (Some 0);
  int_opt (String.find_last_index ~start:1 is_letter "a-") (Some 0);
  int_opt (String.find_last_index ~start:2 is_letter "a-") (Some 0);
  int_opt (String.find_first_index ~start:0 is_letter "a-a") (Some 0);
  int_opt (String.find_first_index ~start:1 is_letter "a-a") (Some 2);
  int_opt (String.find_first_index ~start:2 is_letter "a-a") (Some 2);
  int_opt (String.find_first_index ~start:3 is_letter "a-a") None;
  int_opt (String.find_last_index ~start:0 is_letter "a-a") (Some 0);
  int_opt (String.find_last_index ~start:1 is_letter "a-a") (Some 0);
  int_opt (String.find_last_index ~start:2 is_letter "a-a") (Some 2);
  int_opt (String.find_last_index ~start:3 is_letter "a-a") (Some 2);
  int_opt (String.find_first_index ~start:0 is_letter "-a-") (Some 1);
  int_opt (String.find_first_index ~start:1 is_letter "-a-") (Some 1);
  int_opt (String.find_first_index ~start:2 is_letter "-a-") None;
  int_opt (String.find_first_index ~start:3 is_letter "-a-") None;
  int_opt (String.find_last_index ~start:0 is_letter "-a-") None;
  int_opt (String.find_last_index ~start:1 is_letter "-a-") (Some 1);
  int_opt (String.find_last_index ~start:2 is_letter "-a-") (Some 1);
  int_opt (String.find_last_index ~start:3 is_letter "-a-") (Some 1);
  ()

let test_detach_endline =
  Test.test "String.detach_endline" @@ fun () ->
  let eq = Test.eq Test.T.(pair string string) in
  let lf = String.endlines Lf and crlf = String.endlines Crlf
  and rl = String.endlines Readline in
  eq (String.detach_endline lf "") ("", "");
  eq (String.detach_endline lf "\n") ("", "\n");
  eq (String.detach_endline lf "hello") ("hello", "");
  eq (String.detach_endline lf "hello\n") ("hello", "\n");
  eq (String.detach_endline lf "hel\nlo") ("hel\nlo", "");
  eq (String.detach_endline lf "hel\nlo\n") ("hel\nlo", "\n");
  eq (String.detach_endline crlf "") ("", "");
  eq (String.detach_endline crlf "\r\n") ("", "\r\n");
  eq (String.detach_endline crlf "hello\r") ("hello\r", "");
  eq (String.detach_endline crlf "hello\r\n") ("hello", "\r\n");
  eq (String.detach_endline rl "hello\n") ("hello", "\n");
  eq (String.detach_endline rl "hello\u{2028}") ("hello", "\u{2028}");
  eq (String.detach_endline rl "hello\r") ("hello", "\r");
  eq (String.detach_endline rl "hello\r\n") ("hello", "\r\n");
  ()

let test_split_first_line =
  Test.test "String.split_first_line" @@ fun () ->
  let lf = String.endlines Lf in
  let eq = Test.eq Test.T.(pair string string) in
  eq (String.split_first_line lf ~drop_endline:true "") ("", "") ~__POS__;
  eq (String.split_first_line lf ~drop_endline:true "\n") ("", "") ~__POS__;
  eq (String.split_first_line lf ~drop_endline:false "\n") ("\n", "")
    ~__POS__;
  eq (String.split_first_line lf ~drop_endline:true "\r") ("\r", "") ~__POS__;
  eq (String.split_first_line lf ~drop_endline:true "\r") ("\r", "") ~__POS__;
  eq (String.split_first_line lf ~drop_endline:true "hello\n")
    ("hello", "") ~__POS__;
  eq (String.split_first_line lf ~drop_endline:true "hello\n\n")
    ("hello", "\n") ~__POS__;
  eq (String.split_first_line lf ~drop_endline:true "hello\nworld\n")
    ("hello", "world\n") ~__POS__;
  ()

let test_lines =
  Test.test "String.split_lines" @@ fun () ->
  let t ?__POS__ (es : String.endline_convention) ~drop input split =
    let es = String.endlines es in
    Test.(list T.string) ?__POS__
      (String.split_lines es ~drop_endlines:drop input) split
  in
  t Lf ~drop:true "" [] ~__POS__;
  t Lf ~drop:true "\n" [""] ~__POS__;
  t Lf ~drop:true "\n\n" [""; ""] ~__POS__;
  t Lf ~drop:true "hello" ["hello"] ~__POS__;
  t Lf ~drop:true "hello\n" ["hello"] ~__POS__;
  t Lf ~drop:true "\nhello" [""; "hello"] ~__POS__;
  t Lf ~drop:true "hello\nworld" ["hello"; "world"] ~__POS__;
  t Lf ~drop:true "hello\n\nworld" ["hello"; ""; "world"] ~__POS__;
  t Crlf ~drop:true "hello\r\nworld" ["hello"; "world"] ~__POS__;
  t Crlf ~drop:true "hello\r\n\r\nworld" ["hello"; ""; "world"] ~__POS__;
  t Crlf ~drop:true "hello\r\n\r\nworld" ["hello"; ""; "world"] ~__POS__;
  t Crlf ~drop:true "hello\r\nworld\r\n" ["hello"; "world"] ~__POS__;
  t Crlf ~drop:true "hello\r\nworld\r" ["hello"; "world\r"] ~__POS__;
  t Cr_lf_crlf ~drop:true "hello\nworld\rthere\r\n!\r\n"
    ["hello"; "world"; "there"; "!"] ~__POS__;
  t Nlf ~drop:true "hello\u{0085}world\nthere\r"
    ["hello"; "world"; "there"] ~__POS__;
  t Readline ~drop:true "hello\u{2028}world\u{2029}there"
      ["hello"; "world"; "there"];
  t Lf ~drop:false "hello\nworld" ["hello\n"; "world"] ~__POS__;
  t Lf ~drop:false "hello\n" ["hello\n"] ~__POS__;
  t Lf ~drop:false "hello\n\nworld" ["hello\n"; "\n"; "world"] ~__POS__;
  t Cr_lf_crlf ~drop:false "hello\nworld\rthere\r\n!\r\n"
    ["hello\n"; "world\r"; "there\r\n"; "!\r\n"] ~__POS__;
  ()

let test_fold_lines =
  Test.test "String.fold_lines" @@ fun () ->
  let t ?__POS__ (es : String.endline_convention) ~drop input split =
    let es = String.endlines es in
    let f i acc l = (i, l) :: acc in
    Test.(list T.(pair int string)) ?__POS__
      (List.rev (String.fold_lines es ~drop_endlines:drop f [] input)) split
  in
  t Lf ~drop:true "" [] ~__POS__;
  t Lf ~drop:true "hello" [1, "hello"] ~__POS__;
  t Lf ~drop:true "hello\n" [1, "hello"] ~__POS__;
  t Lf ~drop:true "\nhello" [1, ""; 2, "hello"] ~__POS__;
  t Lf ~drop:true "hello\nworld" [1, "hello"; 2, "world"] ~__POS__;
  t Lf ~drop:true "hello\n\nworld" [1, "hello"; 2, ""; 3, "world"] ~__POS__;
  t Crlf ~drop:true "hello\r\nworld" [1, "hello"; 2, "world"] ~__POS__;
  t Crlf ~drop:true "hello\r\n\r\nworld" [1, "hello"; 2, ""; 3, "world"]
    ~__POS__;
  t Crlf ~drop:true "hello\r\nworld\r\n" [1, "hello"; 2, "world"] ~__POS__;
  t Crlf ~drop:true "hello\r\nworld\r" [1, "hello"; 2, "world\r"] ~__POS__;
  t Cr_lf_crlf ~drop:true "hello\nworld\rthere\r\n!\r\n"
    [1, "hello"; 2, "world"; 3, "there"; 4, "!"] ~__POS__;
  t Nlf ~drop:true "hello\u{0085}world\nthere\r"
    [1, "hello"; 2, "world"; 3, "there"] ~__POS__;
  t Readline ~drop:true "hello\u{2028}world\u{2029}there"
      [1, "hello"; 2, "world"; 3, "there"];
  t Lf ~drop:true "" [] ~__POS__;
  t Lf ~drop:true "\n" [1, ""] ~__POS__;
  t Lf ~drop:true "\n\n" [1, ""; 2, ""] ~__POS__;
  t Lf ~drop:false "hello\nworld" [1, "hello\n"; 2, "world"] ~__POS__;
  t Lf ~drop:false "hello\n" [1, "hello\n"] ~__POS__;
  t Lf ~drop:false "hello\n\nworld" [1, "hello\n"; 2, "\n"; 3, "world"]
    ~__POS__;
  t Cr_lf_crlf ~drop:false "hello\nworld\rthere\r\n!\r\n"
    [1, "hello\n"; 2, "world\r"; 3, "there\r\n"; 4, "!\r\n"] ~__POS__;
  ()



let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
