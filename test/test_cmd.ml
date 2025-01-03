(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open More

let test_pp_shell =
  Test.test "Cmd.pp_shell" @@ fun () ->
  let dirs = ["/usr/local/bin"; "/usr/local/includes"; "/usr/local/lib";
              "/usr/local/share"; "/local/is/global/"; "/local/has sp/bla";
              "--prefix"; "";]
  in
  Snap.string (Fmt.str "%a" Cmd.pp_shell Cmd.(tool "ls")) @> __POS_OF__ "ls";
  Snap.string (Fmt.str "%a" Cmd.pp_shell Cmd.(tool "ls" %% list dirs))
  @> __POS_OF__
    "ls /usr/local/bin /usr/local/includes /usr/local/lib /usr/local/share \\\n\
     /local/is/global/ '/local/has sp/bla' --prefix ''";
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
