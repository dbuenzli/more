(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open More

let ( ~~ ) =
  (* Note if Fpath.{v,to_string} are buggy it can become all confusing since the
     WYSIWYGness of snapshots depends on its correctness. *)
  Filepath.v

module Snap_filepath = struct
  type t = Filepath.t
  let equal = Filepath.equal
  let pp ppf p = Fmt.pf ppf "~~%a" Fmt.OCaml.string (Filepath.to_string p)
end

let snap = Test.snap (module Snap_filepath)
let test ?__POS__  = Test.eq ?__POS__ (module Filepath)

let test_double_sep =
  Test.test "Filepath double sep normalization" @@ fun () ->
  let test ?__POS__ p q =
    Test.string ?__POS__ (Filepath.to_string (Filepath.v p)) q
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
  Test.test "Filepath.{is_root,root_of,drop_root_sep,ensure_root_sep}" @@
  fun () ->
  Test.bool (Filepath.is_root ~~"//bla/") true ~__POS__;
  Test.bool (Filepath.is_root ~~"//bla/a") false ~__POS__;
  Test.bool (Filepath.is_root ~~"/") true ~__POS__;
  snap (Filepath.root_of ~~"//bla/") @> __POS_OF__ ~~"//bla/";
  snap (Filepath.root_of ~~"//bla/a") @> __POS_OF__ ~~"//bla/";
  snap (Filepath.root_of ~~"//bla/a/c/") @> __POS_OF__ ~~"//bla/";
  snap (Filepath.root_of ~~"/") @> __POS_OF__ ~~"/";
  snap (Filepath.root_of ~~".") @> __POS_OF__ ~~"/";
  snap (Filepath.root_of ~~"./") @> __POS_OF__ ~~"/";
  snap (Filepath.root_of ~~"../") @> __POS_OF__ ~~"/";
  snap (Filepath.root_of ~~"..") @> __POS_OF__ ~~"/";
  snap (Filepath.root_of ~~"a/b/c") @> __POS_OF__ ~~"/";
  snap (Filepath.drop_root_sep ~~"bla/") @> __POS_OF__ ~~"bla/";
  snap (Filepath.drop_root_sep ~~"/bla/") @> __POS_OF__ ~~"bla/";
  snap (Filepath.drop_root_sep ~~"/bla") @> __POS_OF__ ~~"bla";
  snap (Filepath.drop_root_sep ~~"/") @> __POS_OF__ ~~".";
  snap (Filepath.drop_root_sep ~~"//bla/") @> __POS_OF__ ~~".";
  snap (Filepath.ensure_root_sep ~~"/") @> __POS_OF__ ~~"/";
  snap (Filepath.ensure_root_sep ~~"/a/b/c") @> __POS_OF__ ~~"/a/b/c";
  snap (Filepath.ensure_root_sep ~~"//aa/a/b/c") @> __POS_OF__ ~~"//aa/a/b/c";
  snap (Filepath.ensure_root_sep ~~"a") @> __POS_OF__ ~~"/a";
  snap (Filepath.ensure_root_sep ~~"a/b/c") @> __POS_OF__ ~~"/a/b/c";
  snap (Filepath.ensure_root_sep ~~".") @> __POS_OF__ ~~"/";
  snap (Filepath.ensure_root_sep ~~"./") @> __POS_OF__ ~~"/";
  snap (Filepath.ensure_root_sep ~~"..") @> __POS_OF__ ~~"/";
  snap (Filepath.ensure_root_sep ~~"../") @> __POS_OF__ ~~"/";
  snap (Filepath.ensure_root_sep ~~"a/") @> __POS_OF__ ~~"/a/";
  snap (Filepath.ensure_root_sep ~~"/") @> __POS_OF__ ~~"/";
  ()

let test_strict_prefixes =
  Test.test "Filepath.{strictly_starts_with,drop_strict_prefix}" @@ fun () ->
  let test ?__POS__:pos p q r =
    Test.block ?__POS__:pos @@ fun () ->
    let p = Filepath.v p and q = Filepath.v q in
    match r with
    | None ->
        assert (not (Filepath.strictly_starts_with ~prefix:p q));
        assert (Filepath.drop_strict_prefix ~prefix:p q = None);
    | Some r ->
        let r = Filepath.v r in
        assert (Filepath.strictly_starts_with ~prefix:p q);
        match Filepath.drop_strict_prefix ~prefix:p q with
        | None -> assert false
        | Some r' ->
            assert (Filepath.equal r r');
            assert (Filepath.equal (Filepath.( p // r')) q);
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
  Test.test "Filepath.{basename,basepath}" @@ fun () ->
  let test ?__POS__:pos p b ~drop_exts:b' =
    Test.block ?__POS__:pos @@ fun () ->
    let p = Filepath.v p in
    Test.string (Filepath.basename p) b ~__POS__;
    Test.string (Filepath.basename ~drop_exts:true p) b' ~__POS__;
    let bpath = if b = "" then Filepath.v "." else Filepath.v b in
    let bpath' = if b' = "" then Filepath.v "." else Filepath.v b' in
    test (Filepath.basepath p) bpath ~__POS__;
    test (Filepath.basepath ~drop_exts:true p) bpath' ~__POS__;
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
  Test.test "Filepath.take_volume" @@ fun () ->
  Snap.string (Filepath.take_volume ~~"//a/b") @> __POS_OF__ "a";
  Snap.string (Filepath.take_volume ~~"//abc/a") @> __POS_OF__ "abc";
  Snap.string (Filepath.take_volume ~~"/a") @> __POS_OF__ "";
  Snap.string (Filepath.take_volume ~~"a") @> __POS_OF__ "";
  Snap.string (Filepath.take_volume ~~"a/") @> __POS_OF__ "";
  ()

let test_drop_volume =
  Test.test "Filepath.drop_volume" @@ fun () ->
  snap (Filepath.drop_volume ~~"//a/b") @> __POS_OF__ ~~"/b";
  snap (Filepath.drop_volume ~~"//abc/a") @> __POS_OF__ ~~"/a";
  snap (Filepath.drop_volume ~~"/a") @> __POS_OF__ ~~"/a";
  snap (Filepath.drop_volume ~~"a") @> __POS_OF__ ~~"a";
  snap (Filepath.drop_volume ~~"a/") @> __POS_OF__ ~~"a/";
  ()

let test_parent =
  Test.test "Filepath.parent" @@ fun () ->
  let test ?__POS__ p pp =
    test (Filepath.parent (Filepath.v p)) (Filepath.v pp);
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
  Test.test "Filepath.relative" @@ fun () ->
  let test ?__POS__ p ~to_dir q =
    let to_dir = Filepath.v to_dir and p = Filepath.v p and q = Filepath.v q in
    test (Filepath.relative ~to_dir p) q
  in
  test "/a/b" ~to_dir:"/a/b/c" "../../b" ~__POS__;
  test "/a/b" ~to_dir:"a" "/a/b" ~__POS__;
  test "a/b" ~to_dir:"/a/b/c" "a/b" ~__POS__;
  test "a/b" ~to_dir:"a/b" "../b" ~__POS__;
  ()

let test_append_ext =
  Test.test "Filepath.append_ext" @@ fun () ->
  let test ?__POS__ p ~ext q = test (Filepath.append_ext p ext) q in
  test ~~"/file" ~ext:".png" ~~"/file.png";
  test ~~"/dir" ~ext:".bundle" ~~"/dir.bundle";
  test ~~"/dir/" ~ext:".bundle" ~~"/dir.bundle/";
  test ~~"/dir" ~ext:"-srcs" ~~"/dir-srcs";
  test ~~"/dir/" ~ext:"-srcs" ~~"/dir-srcs/";
  Snap.raise (fun () -> Filepath.append_ext ~~"/" ".png") @> __POS_OF__
    (Invalid_argument("Cannot append extension .png to root path /"));
  Snap.raise (fun () -> Filepath.append_ext ~~"//a/" ".png") @> __POS_OF__
    (Invalid_argument("Cannot append extension .png to root path //a/"));
()


let test_segments =
  Test.test "Filepath.{of,to}_segments" @@ fun () ->
  Test.invalid_arg (fun () -> Filepath.of_segments []) ~__POS__;
  Test.invalid_arg (fun () -> Filepath.of_segments ["/"]) ~__POS__;
  Test.invalid_arg (fun () -> Filepath.of_segments [""]) ~__POS__;
  Test.invalid_arg (fun () -> Filepath.of_segments ~volume:"/" ["";""]) ~__POS__;
  test (Filepath.of_segments [""; ""]) ~~"/";
  test (Filepath.of_segments ["a"; ""]) ~~"a/";
  test (Filepath.of_segments ~volume:"v" ["a"; ""]) ~~"//v/a/";
  test (Filepath.of_segments ~volume:"v" [""; "a"; ""]) ~~"//v/a/";
  test (Filepath.of_segments ~volume:"v" ["a"]) ~~"//v/a";
  test (Filepath.of_segments ~volume:"v" ["a"]) ~~"//v/a";
  test (Filepath.of_segments ~volume:"v" ["";""]) ~~"//v/";
  test (Filepath.of_segments ["";"";"bla"; "blu"]) ~~"/bla/blu" ~__POS__;
  test (Filepath.of_segments ["";""; ""; "bla"; ""; ""; "blu"])
    ~~"/bla/blu" ~__POS__;
  test (Filepath.of_segments ["bla"; ""; ""; "blu"])
    ~~"bla/blu" ~__POS__;
  test (Filepath.of_segments ["bla"; ""; ""; "blu"; ""])
    ~~"bla/blu/" ~__POS__;
  test (Filepath.of_segments ["bla"; ""; ""; "blu"; ""; ""])
    ~~"bla/blu/" ~__POS__;
  test (Filepath.of_segments ["";"";"";"bla"; ""; ""; "blu"; ""; ""])
    ~~"/bla/blu/" ~__POS__;
  Test.(list T.string) (Filepath.to_segments ~~"/") [""; ""];
  Test.(list T.string) (Filepath.to_segments ~~"/a") [""; "a"];
  Test.(list T.string) (Filepath.to_segments ~~"a") ["a"];
  Test.(list T.string) (Filepath.to_segments ~~"//v/a") [""; "a"];
  Test.(list T.string) (Filepath.to_segments ~~"//v/") [""; ""];
  ()

let test_last_segment =
  Test.test "Filepath.{take,drop}_last_segment" @@ fun () ->
  Snap.string (Filepath.take_last_segment ~~"a") @> __POS_OF__ "a";
  Snap.string (Filepath.take_last_segment ~~"ab") @> __POS_OF__ "ab";
  Snap.string (Filepath.take_last_segment ~~"ab/") @> __POS_OF__ "";
  Snap.string (Filepath.take_last_segment ~~"/") @> __POS_OF__ "";
  Snap.string (Filepath.take_last_segment ~~"/..") @> __POS_OF__ "..";
  Snap.string (Filepath.take_last_segment ~~"/../a") @> __POS_OF__ "a";
  Snap.string (Filepath.take_last_segment ~~"//bla/") @> __POS_OF__ "";
  Snap.string (Filepath.take_last_segment ~~"//bla/a") @> __POS_OF__ "a";
  Snap.string (Filepath.take_last_segment ~~"//bla/ab") @> __POS_OF__ "ab";
  Snap.string (Filepath.take_last_segment ~~"//bla/ab/") @> __POS_OF__ "";
  snap (Filepath.drop_last_segment ~~"a") @> __POS_OF__ ~~".";
  snap (Filepath.drop_last_segment ~~"ab") @> __POS_OF__ ~~".";
  snap (Filepath.drop_last_segment ~~"ab/") @> __POS_OF__ ~~"ab";
  snap (Filepath.drop_last_segment ~~"/ab/c/d") @> __POS_OF__ ~~"/ab/c";
  snap (Filepath.drop_last_segment ~~"/") @> __POS_OF__ ~~"/";
  snap (Filepath.drop_last_segment ~~"/..") @> __POS_OF__ ~~"/";
  snap (Filepath.drop_last_segment ~~"/../a") @> __POS_OF__ ~~"/..";
  snap (Filepath.drop_last_segment ~~"//bla/") @> __POS_OF__ ~~"//bla/";
  snap (Filepath.drop_last_segment ~~"//bla/a") @> __POS_OF__ ~~"//bla/";
  snap (Filepath.drop_last_segment ~~"//bla/ab") @> __POS_OF__ ~~"//bla/";
  snap (Filepath.drop_last_segment ~~"//bla/ab/") @> __POS_OF__ ~~"//bla/ab";;
  ()

let test_try_drop_relative_dirs =
  Test.test "Filepath.try_drop_relative_dirs" @@ fun () ->
  snap (Filepath.try_drop_relative_dirs ~~"//v/") @> __POS_OF__ ~~"//v/";
  snap (Filepath.try_drop_relative_dirs ~~"//v/..") @> __POS_OF__ ~~"//v/";
  snap (Filepath.try_drop_relative_dirs ~~"//v/../..") @> __POS_OF__ ~~"//v/";
  snap (Filepath.try_drop_relative_dirs ~~"/../..") @> __POS_OF__ ~~"/";
  snap (Filepath.try_drop_relative_dirs ~~"//v/..") @> __POS_OF__ ~~"//v/";
  snap (Filepath.try_drop_relative_dirs ~~"//v/bla/..") @> __POS_OF__ ~~"//v/";
  snap (Filepath.try_drop_relative_dirs ~~"//v/bla/.././bla/../") @> __POS_OF__
    ~~"//v/";
  snap (Filepath.try_drop_relative_dirs ~~"//v/hey/.././bla/") @> __POS_OF__
    ~~"//v/bla/";
  snap (Filepath.try_drop_relative_dirs ~~"..") @> __POS_OF__ ~~"..";
  snap (Filepath.try_drop_relative_dirs ~~"./..") @> __POS_OF__ ~~"..";
  snap (Filepath.try_drop_relative_dirs ~~"./../..") @> __POS_OF__ ~~"../..";
  snap (Filepath.try_drop_relative_dirs ~~"./.././..") @> __POS_OF__ ~~"../..";
  snap (Filepath.try_drop_relative_dirs ~~"./.././b/..") @> __POS_OF__ ~~"..";
  snap (Filepath.try_drop_relative_dirs ~~"./b/./b/..") @> __POS_OF__ ~~"b";
  snap (Filepath.try_drop_relative_dirs ~~"b/..") @> __POS_OF__ ~~".";
  snap (Filepath.try_drop_relative_dirs ~~"b/../") @> __POS_OF__ ~~".";
  snap (Filepath.try_drop_relative_dirs ~~"b/../.") @> __POS_OF__ ~~".";
  snap (Filepath.try_drop_relative_dirs ~~"b/.././..") @> __POS_OF__ ~~"..";
  snap (Filepath.try_drop_relative_dirs ~~"b/.././../b") @> __POS_OF__ ~~"../b";
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
