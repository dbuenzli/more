(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open More

let ( ~~ ) =
  (* Note if Fpath.{v,to_string} are buggy it can become all confusing since the
     WYSIWYGness of snapshots depends on its correctness. *)
  Fpath.v

module Snap_fpath = struct
  type t = Fpath.t
  let equal = Fpath.equal
  let pp ppf p = Fmt.pf ppf "~~%a" Fmt.OCaml.string (Fpath.to_string p)
end

let snap = Test.snap (module Snap_fpath)
let test ?__POS__  = Test.eq ?__POS__ (module Fpath)

let test_double_sep =
  Test.test "Fpath double sep normalization" @@ fun () ->
  let test ?__POS__ p q =
    Test.string ?__POS__ (Fpath.to_string (Fpath.v p)) q
  in
  (* Note this will fail on Windows but we should take that as an opportunity
     to make good tests on Windows. *)
  test "/" "/" ~__POS__;
  test "//" "/" ~__POS__; (* A volume cannot be the empty segemt,
                             POSIX is unclear on that one. *)
  test "///" "/" ~__POS__; (* A volume cannot be the empty segment.
                              POSIX is clear that more than initial two
                              is root. *)
  test "////" "/" ~__POS__;
  test "/////" "/" ~__POS__;
  test "//v" "//v/" ~__POS__; (* Volume without root, normalize *)
  test "//v/" "//v/" ~__POS__;
  test "//v//" "//v/" ~__POS__;
  test "//v///" "//v/" ~__POS__;
  test "//v/a" "//v/a" ~__POS__;
  test "//v/a/" "//v/a/" ~__POS__;
  test "//v/a//" "//v/a/" ~__POS__;
  test "//v//a//" "//v/a/" ~__POS__;
  test "///a/" "/a/" ~__POS__;
  test "//////a///" "/a/" ~__POS__;
  test "/a///bc" "/a/bc" ~__POS__;
  test "a///bc//" "a/bc/" ~__POS__;
  test "a///bc//c///////////////d" "a/bc/c/d" ~__POS__;
  test "a//u" "a/u" ~__POS__;
  test "aa//u" "aa/u" ~__POS__;
  test "aaa//u" "aaa/u" ~__POS__;
  ()

let test_root =
  Test.test "Fpath.{is_root,root_of,drop_root_sep,ensure_root_sep}" @@ fun () ->
  Test.bool (Fpath.is_root ~~"//bla/") true ~__POS__;
  Test.bool (Fpath.is_root ~~"//bla/a") false ~__POS__;
  Test.bool (Fpath.is_root ~~"/") true ~__POS__;
  snap (Fpath.root_of ~~"//bla/") @> __POS_OF__ ~~"//bla/";
  snap (Fpath.root_of ~~"//bla/a") @> __POS_OF__ ~~"//bla/";
  snap (Fpath.root_of ~~"//bla/a/c/") @> __POS_OF__ ~~"//bla/";
  snap (Fpath.root_of ~~"/") @> __POS_OF__ ~~"/";
  snap (Fpath.root_of ~~".") @> __POS_OF__ ~~"/";
  snap (Fpath.root_of ~~"./") @> __POS_OF__ ~~"/";
  snap (Fpath.root_of ~~"../") @> __POS_OF__ ~~"/";
  snap (Fpath.root_of ~~"..") @> __POS_OF__ ~~"/";
  snap (Fpath.root_of ~~"a/b/c") @> __POS_OF__ ~~"/";
  snap (Fpath.drop_root_sep ~~"bla/") @> __POS_OF__ ~~"bla/";
  snap (Fpath.drop_root_sep ~~"/bla/") @> __POS_OF__ ~~"bla/";
  snap (Fpath.drop_root_sep ~~"/bla") @> __POS_OF__ ~~"bla";
  snap (Fpath.drop_root_sep ~~"/") @> __POS_OF__ ~~".";
  snap (Fpath.drop_root_sep ~~"//bla/") @> __POS_OF__ ~~".";
  snap (Fpath.ensure_root_sep ~~"/") @> __POS_OF__ ~~"/";
  snap (Fpath.ensure_root_sep ~~"/a/b/c") @> __POS_OF__ ~~"/a/b/c";
  snap (Fpath.ensure_root_sep ~~"//aa/a/b/c") @> __POS_OF__ ~~"//aa/a/b/c";
  snap (Fpath.ensure_root_sep ~~"a") @> __POS_OF__ ~~"/a";
  snap (Fpath.ensure_root_sep ~~"a/b/c") @> __POS_OF__ ~~"/a/b/c";
  snap (Fpath.ensure_root_sep ~~".") @> __POS_OF__ ~~"/";
  snap (Fpath.ensure_root_sep ~~"./") @> __POS_OF__ ~~"/";
  snap (Fpath.ensure_root_sep ~~"..") @> __POS_OF__ ~~"/";
  snap (Fpath.ensure_root_sep ~~"../") @> __POS_OF__ ~~"/";
  snap (Fpath.ensure_root_sep ~~"a/") @> __POS_OF__ ~~"/a/";
  snap (Fpath.ensure_root_sep ~~"/") @> __POS_OF__ ~~"/";
  ()

let test_strict_prefixes =
  Test.test "Fpath.{strictly_starts_with,drop_strict_prefix}" @@ fun () ->
  let test ?__POS__:pos p q r =
    Test.block ?__POS__:pos @@ fun () ->
    let p = Fpath.v p and q = Fpath.v q in
    match r with
    | None ->
        assert (not (Fpath.strictly_starts_with ~prefix:p q));
        assert (Fpath.drop_strict_prefix ~prefix:p q = None);
    | Some r ->
        let r = Fpath.v r in
        assert (Fpath.strictly_starts_with ~prefix:p q);
        match Fpath.drop_strict_prefix ~prefix:p q with
        | None -> assert false
        | Some r' ->
            assert (Fpath.equal r r');
            assert (Fpath.equal (Fpath.( p // r')) q);
  in
  test "a/b/" "a/b" None ~__POS__;
  test "a/b/" "a/b/" None ~__POS__;
  test "a/b" "a/b" None ~__POS__;
  test "a/b" "a/b/" None ~__POS__;
  test "a/b" "a/b/c" (Some "c") ~__POS__;
  test "a/b" "a/b/c/" (Some "c/") ~__POS__;
  test "a/b/" "a/b/c" (Some "c") ~__POS__;
  test "a/b/" "a/b/c/" (Some "c/") ~__POS__;
  test "a/b" "a/b" None ~__POS__;
  test "/a/b/" "/a/b" None ~__POS__;
  test "/a/b/" "/a/b/" None ~__POS__;
  test "/a/b" "/a/bc" None ~__POS__;
  test "/a/b" "/a/b" None ~__POS__;
  test "/a/b/" "/a/b" None ~__POS__;
  test "/a/b" "/a/b/" None ~__POS__;
  test "/a/b/" "/a/b/" None ~__POS__;
  test "/a/b" "/a/b/c" (Some "c") ~__POS__;
  test "/a/b/" "/a/b/c" (Some "c") ~__POS__;
  test "a" "a/b/c" (Some "b/c") ~__POS__;
  if Sys.win32 then begin
    test "C:\\a" "C:\\a\\b" (Some "b") ~__POS__;
  end;
  ()

let test_basename =
  Test.test "Fpath.{basename,basepath}" @@ fun () ->
  let test ?__POS__:pos p b ~drop_exts:b' =
    Test.block ?__POS__:pos @@ fun () ->
    let p = Fpath.v p in
    Test.string (Fpath.basename p) b ~__POS__;
    Test.string (Fpath.basename ~drop_exts:true p) b' ~__POS__;
    let bpath = if b = "" then Fpath.v "." else Fpath.v b in
    let bpath' = if b' = "" then Fpath.v "." else Fpath.v b' in
    test (Fpath.basepath p) bpath ~__POS__;
    test (Fpath.basepath ~drop_exts:true p) bpath' ~__POS__;
  in
  test "bla" "bla" ~drop_exts:"bla" ~__POS__;
  test "bla" "bla" ~drop_exts:"bla" ~__POS__;
  test "/" "" ~drop_exts:"" ~__POS__;
  test "/.." "" ~drop_exts:"" ~__POS__;
  test "/." "" ~drop_exts:"" ~__POS__;
  test "bla/.." "" ~drop_exts:"" ~__POS__;
  test "bla/." "" ~drop_exts:"" ~__POS__;
  test ".." "" ~drop_exts:"" ~__POS__;
  test "." "" ~drop_exts:"" ~__POS__;
  test "./a" "a" ~drop_exts:"a" ~__POS__;
  test "./a/" "a" ~drop_exts:"a" ~__POS__;
  test "./abla" "abla" ~drop_exts:"abla" ~__POS__;
  test "./abla/" "abla" ~drop_exts:"abla" ~__POS__;
  test "/abla" "abla" ~drop_exts:"abla" ~__POS__;
  test "/abla/" "abla" ~drop_exts:"abla" ~__POS__;
  test "/.ocamlinit" ".ocamlinit" ~drop_exts:".ocamlinit" ~__POS__;
  test "/.ocamlinit/" ".ocamlinit" ~drop_exts:".ocamlinit" ~__POS__;
  test "/..ocamlinit/" "..ocamlinit" ~drop_exts:"..ocamlinit" ~__POS__;
  test "hop/.emacs.d" ".emacs.d" ~drop_exts:".emacs" ~__POS__;
  test "hap/.emacs.d/" ".emacs.d" ~drop_exts:".emacs" ~__POS__;
  test "hop/.emacs.d" ".emacs.d" ~drop_exts:".emacs" ~__POS__;
  test "hap/.emacs.d/" ".emacs.d" ~drop_exts:".emacs" ~__POS__;
  test "hap/archive.tar.gz/" "archive.tar.gz" ~drop_exts:"archive" ~__POS__;
  test "hap/archive.tar.gz" "archive.tar.gz" ~drop_exts:"archive" ~__POS__;
  test "/archive.tar.gz" "archive.tar.gz" ~drop_exts:"archive" ~__POS__;
  test "archive.tar.gz/" "archive.tar.gz" ~drop_exts:"archive" ~__POS__;
  test "archive.tar.gz" "archive.tar.gz" ~drop_exts:"archive" ~__POS__;
  if Sys.win32 then begin
    test "C:archive.tar.gz" "archive.tar.gz" ~drop_exts:"archive" ~__POS__;
  end;
  ()

let test_split_volume =
  Test.test "Fpath.take_volume" @@ fun () ->
  Snap.string (Fpath.take_volume ~~"//a/b") @> __POS_OF__ "a";
  Snap.string (Fpath.take_volume ~~"//abc/a") @> __POS_OF__ "abc";
  Snap.string (Fpath.take_volume ~~"/a") @> __POS_OF__ "";
  Snap.string (Fpath.take_volume ~~"a") @> __POS_OF__ "";
  Snap.string (Fpath.take_volume ~~"a/") @> __POS_OF__ "";
  ()

let test_drop_volume =
  Test.test "Fpath.drop_volume" @@ fun () ->
  snap (Fpath.drop_volume ~~"//a/b") @> __POS_OF__ ~~"/b";
  snap (Fpath.drop_volume ~~"//abc/a") @> __POS_OF__ ~~"/a";
  snap (Fpath.drop_volume ~~"/a") @> __POS_OF__ ~~"/a";
  snap (Fpath.drop_volume ~~"a") @> __POS_OF__ ~~"a";
  snap (Fpath.drop_volume ~~"a/") @> __POS_OF__ ~~"a/";
  ()

let test_parent =
  Test.test "Fpath.parent" @@ fun () ->
  let test ?__POS__ p pp =
    test (Fpath.parent (Fpath.v p)) (Fpath.v pp);
  in
  Test.Log.fail "%a Some commented out tests need to be fixed" Fmt.putwarn ();
  test "a/b/c" "a/b/" ~__POS__;
  test "a/b" "a/" ~__POS__;
  test "a" "." ~__POS__;
  test "." ".." ~__POS__;
  test "./" ".." ~__POS__;
  test "././" ".." ~__POS__;
(*  test "././a" "." ~__POS__; *)
(*  test "././a/" "." ~__POS__; *)
  test ".." "../.." ~__POS__;
  test "../.." "../../.." ~__POS__;
  test "a/b/." "a/" ~__POS__;
  test "a/b/./" "a/" ~__POS__;
  test "a/b/./a" "a/b/./" ~__POS__;
  test "a/.." "a/../.." ~__POS__;
  test "a/b/.." "a/b/../.." ~__POS__;
(*  test "a/../c" "." ~__POS__; *)
  (* abs of the above *)
  test "/a/b/c" "/a/b/" ~__POS__;
  test "/a/b" "/a/" ~__POS__;
  test "/a" "/" ~__POS__;
  test "/" "/" ~__POS__;
  test "/." "/" ~__POS__;
  test "/./" "/" ~__POS__;
  test "/././" "/" ~__POS__;
(*  test "/././a" "/" ~__POS__; *)
(*  test "/././a/" "/" ~__POS__; *)
  test "/.." "/../.." ~__POS__;
  test "/../.." "/../../.." ~__POS__;
  test "/a/b/." "/a/" ~__POS__;
  test "/a/b/./" "/a/" ~__POS__;
  test "/a/b/./a" "/a/b/./" ~__POS__;
  test "/a/.." "/a/../.." ~__POS__;
  test "/a/b/.." "/a/b/../.." ~__POS__;
  ()

let test_relative =
  Test.test "Fpath.relative" @@ fun () ->
  let test ?__POS__ p ~to_dir q =
    let to_dir = Fpath.v to_dir and p = Fpath.v p and q = Fpath.v q in
    test (Fpath.relative ~to_dir p) q
  in
  test "/a/b" ~to_dir:"/a/b/c" "../../b" ~__POS__;
  test "/a/b" ~to_dir:"a" "/a/b" ~__POS__;
  test "a/b" ~to_dir:"/a/b/c" "a/b" ~__POS__;
  test "a/b" ~to_dir:"a/b" "../b" ~__POS__;
  ()

let test_append_ext =
  Test.test "Fpath.append_ext" @@ fun () ->
  let test ?__POS__ p ~ext q = test (Fpath.append_ext p ext) q in
  test ~~"/file" ~ext:".png" ~~"/file.png";
  test ~~"/dir" ~ext:".bundle" ~~"/dir.bundle";
  test ~~"/dir/" ~ext:".bundle" ~~"/dir.bundle/";
  test ~~"/dir" ~ext:"-srcs" ~~"/dir-srcs";
  test ~~"/dir/" ~ext:"-srcs" ~~"/dir-srcs/";
  Snap.raise (fun () -> Fpath.append_ext ~~"/" ".png") @> __POS_OF__
    (Invalid_argument("Cannot append extension .png to root path /"));
  Snap.raise (fun () -> Fpath.append_ext ~~"//a/" ".png") @> __POS_OF__
    (Invalid_argument("Cannot append extension .png to root path //a/"));
()


let test_segments =
  Test.test "Fpath.{of,to}_segments" @@ fun () ->
  Test.invalid_arg (fun () -> Fpath.of_segments []) ~__POS__;
  Test.invalid_arg (fun () -> Fpath.of_segments ["/"]) ~__POS__;
  Test.invalid_arg (fun () -> Fpath.of_segments [""]) ~__POS__;
  Test.invalid_arg (fun () -> Fpath.of_segments ~volume:"/" ["";""]) ~__POS__;
  test (Fpath.of_segments [""; ""]) ~~"/";
  test (Fpath.of_segments ["a"; ""]) ~~"a/";
  test (Fpath.of_segments ~volume:"v" ["a"; ""]) ~~"//v/a/";
  test (Fpath.of_segments ~volume:"v" [""; "a"; ""]) ~~"//v/a/";
  test (Fpath.of_segments ~volume:"v" ["a"]) ~~"//v/a";
  test (Fpath.of_segments ~volume:"v" ["a"]) ~~"//v/a";
  test (Fpath.of_segments ~volume:"v" ["";""]) ~~"//v/";
  test (Fpath.of_segments ["";"";"bla"; "blu"]) ~~"/bla/blu" ~__POS__;
  test (Fpath.of_segments ["";""; ""; "bla"; ""; ""; "blu"])
    ~~"/bla/blu" ~__POS__;
  test (Fpath.of_segments ["bla"; ""; ""; "blu"])
    ~~"bla/blu" ~__POS__;
  test (Fpath.of_segments ["bla"; ""; ""; "blu"; ""])
    ~~"bla/blu/" ~__POS__;
  test (Fpath.of_segments ["bla"; ""; ""; "blu"; ""; ""])
    ~~"bla/blu/" ~__POS__;
  test (Fpath.of_segments ["";"";"";"bla"; ""; ""; "blu"; ""; ""])
    ~~"/bla/blu/" ~__POS__;
  Test.(list T.string) (Fpath.to_segments ~~"/") [""; ""];
  Test.(list T.string) (Fpath.to_segments ~~"/a") [""; "a"];
  Test.(list T.string) (Fpath.to_segments ~~"a") ["a"];
  Test.(list T.string) (Fpath.to_segments ~~"//v/a") [""; "a"];
  Test.(list T.string) (Fpath.to_segments ~~"//v/") [""; ""];
  ()

let test_last_segment =
  Test.test "Fpath.{take,drop}_last_segment" @@ fun () ->
  Snap.string (Fpath.take_last_segment ~~"a") @> __POS_OF__ "a";
  Snap.string (Fpath.take_last_segment ~~"ab") @> __POS_OF__ "ab";
  Snap.string (Fpath.take_last_segment ~~"ab/") @> __POS_OF__ "";
  Snap.string (Fpath.take_last_segment ~~"/") @> __POS_OF__ "";
  Snap.string (Fpath.take_last_segment ~~"/..") @> __POS_OF__ "..";
  Snap.string (Fpath.take_last_segment ~~"/../a") @> __POS_OF__ "a";
  Snap.string (Fpath.take_last_segment ~~"//bla/") @> __POS_OF__ "";
  Snap.string (Fpath.take_last_segment ~~"//bla/a") @> __POS_OF__ "a";
  Snap.string (Fpath.take_last_segment ~~"//bla/ab") @> __POS_OF__ "ab";
  Snap.string (Fpath.take_last_segment ~~"//bla/ab/") @> __POS_OF__ "";
  snap (Fpath.drop_last_segment ~~"a") @> __POS_OF__ ~~".";
  snap (Fpath.drop_last_segment ~~"ab") @> __POS_OF__ ~~".";
  snap (Fpath.drop_last_segment ~~"ab/") @> __POS_OF__ ~~"ab";
  snap (Fpath.drop_last_segment ~~"/ab/c/d") @> __POS_OF__ ~~"/ab/c";
  snap (Fpath.drop_last_segment ~~"/") @> __POS_OF__ ~~"/";
  snap (Fpath.drop_last_segment ~~"/..") @> __POS_OF__ ~~"/";
  snap (Fpath.drop_last_segment ~~"/../a") @> __POS_OF__ ~~"/..";
  snap (Fpath.drop_last_segment ~~"//bla/") @> __POS_OF__ ~~"//bla/";
  snap (Fpath.drop_last_segment ~~"//bla/a") @> __POS_OF__ ~~"//bla/";
  snap (Fpath.drop_last_segment ~~"//bla/ab") @> __POS_OF__ ~~"//bla/";
  snap (Fpath.drop_last_segment ~~"//bla/ab/") @> __POS_OF__ ~~"//bla/ab";;
  ()

let test_try_drop_relative_dirs =
  Test.test "Fpath.try_drop_relative_dirs" @@ fun () ->
  snap (Fpath.try_drop_relative_dirs ~~"//v/") @> __POS_OF__ ~~"//v/";
  snap (Fpath.try_drop_relative_dirs ~~"//v/..") @> __POS_OF__ ~~"//v/";
  snap (Fpath.try_drop_relative_dirs ~~"//v/../..") @> __POS_OF__ ~~"//v/";
  snap (Fpath.try_drop_relative_dirs ~~"/../..") @> __POS_OF__ ~~"/";
  snap (Fpath.try_drop_relative_dirs ~~"//v/..") @> __POS_OF__ ~~"//v/";
  snap (Fpath.try_drop_relative_dirs ~~"//v/bla/..") @> __POS_OF__ ~~"//v/";
  snap (Fpath.try_drop_relative_dirs ~~"//v/bla/.././bla/../") @> __POS_OF__
    ~~"//v/";
  snap (Fpath.try_drop_relative_dirs ~~"//v/hey/.././bla/") @> __POS_OF__
    ~~"//v/bla/";
  snap (Fpath.try_drop_relative_dirs ~~"..") @> __POS_OF__ ~~"..";
  snap (Fpath.try_drop_relative_dirs ~~"./..") @> __POS_OF__ ~~"..";
  snap (Fpath.try_drop_relative_dirs ~~"./../..") @> __POS_OF__ ~~"../..";
  snap (Fpath.try_drop_relative_dirs ~~"./.././..") @> __POS_OF__ ~~"../..";
  snap (Fpath.try_drop_relative_dirs ~~"./.././b/..") @> __POS_OF__ ~~"..";
  snap (Fpath.try_drop_relative_dirs ~~"./b/./b/..") @> __POS_OF__ ~~"b";
  snap (Fpath.try_drop_relative_dirs ~~"b/..") @> __POS_OF__ ~~".";
  snap (Fpath.try_drop_relative_dirs ~~"b/../") @> __POS_OF__ ~~".";
  snap (Fpath.try_drop_relative_dirs ~~"b/../.") @> __POS_OF__ ~~".";
  snap (Fpath.try_drop_relative_dirs ~~"b/.././..") @> __POS_OF__ ~~"..";
  snap (Fpath.try_drop_relative_dirs ~~"b/.././../b") @> __POS_OF__ ~~"../b";
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
