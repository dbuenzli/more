#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let cmdliner = Conf.with_pkg "cmdliner"
let bytesrw = Conf.with_pkg "bytesrw"
let () =
  Pkg.describe "more" @@ fun c ->
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib "src/more.mllib";
       Pkg.clib "src/libmore_stubs.clib";
       Pkg.lib "src/more_top_init.ml";
       Pkg.lib "src/runtime.js";
       Pkg.mllib ~cond:cmdliner ~dst_dir:"cli" "src/cli/more_cli.mllib";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/cookbook.mld" ~dst:"odoc-pages/cookbook.mld"; ]
