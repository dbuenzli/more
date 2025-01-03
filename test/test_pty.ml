(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open More
open Result.Syntax

let session ~user ~password = (* sample code *)
  Result.join @@ Os.Pty.with_spawn (Cmd.tool "login") @@ fun pty ->
  Os.Fd.close_noerr (Os.Pty.tty pty);
  let* () = Os.Pty.seek pty "login:" in
  let* () = Os.Pty.write pty (user ^ "\n") in
  let* () = Os.Pty.seek pty "Password:" in
  let* () = Os.Pty.write pty (password ^ "\n") in
  print_endline "Login successful, have fun, use Ctrl-D to exit";
  Os.Pty.interact pty


let main () =
  Log.if_error ~use:1 @@
  let* () = session ~user:Sys.argv.(1) ~password:Sys.argv.(2) in
  Ok 0

let main' () =
  Log.if_error ~use:1 @@
  Result.join @@ Os.Pty.with_spawn Cmd.(tool "ocaml" % "-noinit") @@ fun pty ->
  Os.Fd.close_noerr (Os.Pty.tty pty);
  let* () = Os.Pty.set_echo pty true in
  let* () = Os.Pty.seek ~log:true pty "# " in
  let* () = Os.Pty.write pty "let f x = x;;\n" in
  let* () = Os.Pty.seek ~log:true pty "# " in
  let* () = Os.Pty.write pty "f 1;;\n" in
  let* () = Os.Pty.interact pty in
  Ok 0


let () = if !Sys.interactive then () else exit (main ())
