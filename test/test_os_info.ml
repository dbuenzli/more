(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open More

(* These are not really tests they just log the info *)

let test_name =
  Test.test "Os.{name,version,arch}" @@ fun () ->
  let name = Os.name () in
  let name_like = Os.name ~id_like:true () in
  let version = Os.version () in
  let arch = Os.arch () in
  let field n = Fmt.field n Fun.id in
  Test.Log.msg "%a" (field "     name" Os.Name.pp) name;
  Test.Log.msg "%a (id_like)" (field "     name" Os.Name.pp) name_like;
  Test.Log.msg "%a" (field "  name-id" Os.Name.pp_id) name;
  Test.Log.msg "%a (id_like)" (field "  name-id" Os.Name.pp_id) name_like;
  Test.Log.msg "%a" (field "  version" Fmt.string) version;
  Test.Log.msg "%a" (field "     arch" Os.Arch.pp) arch;
  Test.Log.msg "%a" (field "  arch-id" Os.Arch.pp_id) arch;
  Test.Log.msg "%a" (field "arch-bits" Os.Arch.pp_bits) arch;
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
