(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open More
open Result.Syntax

let file = Fpath.v "file"
let file_sym = Fpath.v "file_sym"
let dang_file_sym = Fpath.v "dang_file_sym"
let dir = Fpath.v "dir"
let dir_sym = Fpath.v "dir_sym"
let dang_dir_sym = Fpath.v "dang_dir_sym"
let dir_ne = Fpath.v "dir_ne"
let dir_ne_sym = Fpath.v "dir_ne_sym"

let setup_fs tmp =
  let force = false and make_path = false in
  let* () = Os.File.write ~force ~make_path file "Yo!" in
  let* () = Os.Path.symlink ~force ~make_path ~src:file file_sym in
  let* () = Os.Path.symlink ~force ~make_path ~src:file dang_file_sym in
  let* _existed = Os.Dir.create ~make_path:false dir in
  let* () = Os.Path.symlink ~force ~make_path ~src:dir dir_sym in
  let* _existed = Os.Dir.create ~make_path:false dir_ne in
  let* () = Os.File.write ~force ~make_path (Fpath.(dir_ne / "ha")) "ho" in
  let* () = Os.Path.symlink ~force ~make_path ~src:dir_ne dir_ne_sym in
  Ok ()

let test_path_delete =
  Test.test "Os.Path.delete" @@ fun () ->
  Result.error_to_failure @@ Result.join @@
  Os.Dir.with_tmp @@ fun tmp ->
  Os.Dir.with_cwd tmp @@ fun () ->
  let () = setup_fs tmp |> Result.error_to_failure in
  let snap = Snap.(result T.bool) in
  let recurse = false in
  snap (Os.Path.delete ~recurse file_sym) @> __POS_OF__ (Ok true);
  snap (Os.Path.delete ~recurse file_sym) @> __POS_OF__ (Ok false);
  snap (Os.Path.exists file_sym) @> __POS_OF__ (Ok false);
  snap (Os.Path.delete ~recurse file) @> __POS_OF__ (Ok true);
  snap (Os.Path.delete ~recurse file) @> __POS_OF__ (Ok false);
  snap (Os.Path.exists file) @> __POS_OF__ (Ok false);
  snap (Os.Path.delete ~recurse dang_file_sym) @> __POS_OF__ (Ok true);
  snap (Os.Path.delete ~recurse dang_file_sym) @> __POS_OF__ (Ok false);
  snap (Os.Path.exists dang_file_sym) @> __POS_OF__ (Ok false);
  snap (Os.Path.delete ~recurse dir_sym) @> __POS_OF__ (Ok true);
  snap (Os.Path.delete ~recurse dir_sym) @> __POS_OF__ (Ok false);
  snap (Os.Path.exists dir_sym) @> __POS_OF__ (Ok false);
  snap (Os.Path.delete ~recurse dir) @> __POS_OF__ (Ok true);
  snap (Os.Path.delete ~recurse dir) @> __POS_OF__ (Ok false);
  snap (Os.Path.exists dir) @> __POS_OF__ (Ok false);
  snap (Os.Path.delete ~recurse dir_ne_sym) @> __POS_OF__ (Ok true);
  snap (Os.Path.delete ~recurse dir_ne_sym) @> __POS_OF__ (Ok false);
  snap (Os.Path.exists dir_ne_sym) @> __POS_OF__ (Ok false);
  snap (Os.Path.delete ~recurse:false dir_ne) @> __POS_OF__
    (Error
       "dir_ne: Delete directory \u{001B}[01mdir_ne\u{001B}[m: Directory not empty");
  snap (Os.Path.delete ~recurse:true dir_ne) @> __POS_OF__ (Ok true);
  snap (Os.Path.delete ~recurse:true dir_ne) @> __POS_OF__ (Ok false);
  snap (Os.Path.exists dir_ne) @> __POS_OF__ (Ok false);
  ()

let test_file_dir_delete =
  Test.test "Os.{File,Dir}.delete" @@ fun () ->
  Result.error_to_failure @@ Result.join @@
  Os.Dir.with_tmp @@ fun tmp ->
  Os.Dir.with_cwd tmp @@ fun () ->
  let () = setup_fs tmp |> Result.error_to_failure in
  let snap = Snap.(result T.bool) in
  snap (Os.File.delete dir) @> __POS_OF__
    (Error "Delete file \u{001B}[01mdir\u{001B}[m: Not a regular file");
  snap (Os.File.delete dir_sym) @> __POS_OF__
    (Error "Delete file \u{001B}[01mdir_sym\u{001B}[m: Not a regular file");
  snap (Os.Dir.delete ~recurse:false file) @> __POS_OF__
    (Error "Delete directory \u{001B}[01mfile\u{001B}[m: Not a directory");
  snap (Os.Dir.delete ~recurse:false file_sym) @> __POS_OF__
    (Error "Delete directory \u{001B}[01mfile_sym\u{001B}[m: Not a directory");
  snap (Os.Dir.delete ~recurse:false dir_ne) @> __POS_OF__
    (Error
       "dir_ne: Delete directory \u{001B}[01mdir_ne\u{001B}[m: Directory not empty");
  snap (Os.File.delete file_sym) @> __POS_OF__ (Ok true);
  snap (Os.File.delete file_sym) @> __POS_OF__ (Ok false);
  snap (Os.File.exists file_sym) @> __POS_OF__ (Ok false);
  snap (Os.File.delete file) @> __POS_OF__ (Ok true);
  snap (Os.File.delete file) @> __POS_OF__ (Ok false);
  snap (Os.File.exists file) @> __POS_OF__ (Ok false);
  snap (Os.File.delete dang_file_sym) @> __POS_OF__ (Ok true);
  snap (Os.File.delete dang_file_sym) @> __POS_OF__ (Ok false);
  snap (Os.File.exists dang_file_sym) @> __POS_OF__ (Ok false);
  snap (Os.Dir.delete ~recurse:false dir_sym) @> __POS_OF__ (Ok true);
  snap (Os.Dir.delete ~recurse:false dir_sym) @> __POS_OF__ (Ok false);
  snap (Os.Dir.exists dir_sym) @> __POS_OF__ (Ok false);
  snap (Os.Dir.delete ~recurse:false dir) @> __POS_OF__ (Ok true);
  snap (Os.Dir.delete ~recurse:false dir) @> __POS_OF__ (Ok false);
  snap (Os.Dir.exists dir) @> __POS_OF__ (Ok false);
  snap (Os.Dir.delete ~recurse:false dir_ne_sym) @> __POS_OF__ (Ok true);
  snap (Os.Dir.delete ~recurse:false dir_ne_sym) @> __POS_OF__ (Ok false);
  snap (Os.Dir.exists dir_ne_sym) @> __POS_OF__ (Ok false);
  snap (Os.Dir.delete ~recurse:true dir_ne) @> __POS_OF__ (Ok true);
  snap (Os.Dir.delete ~recurse:true dir_ne) @> __POS_OF__ (Ok false);
  snap (Os.Dir.exists dir_ne) @> __POS_OF__ (Ok false);
  ()


let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
