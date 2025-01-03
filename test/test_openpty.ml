(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open More

let test_openpty =
  Test.test "Os.Fd.openpty" @@ fun () ->
  (* This test may be brittle, it requires `cat` and perhaps
     the fd should be set to raw mode. *)
  let pty, tty = Os.Fd.openpty () in
  let finally () = Os.Fd.close_noerr pty; Os.Fd.close_noerr tty in
  let pid = Fun.protect ~finally @@ fun () ->
    let cat = Cmd.tool "cat" in
    let pid =
      let tty_in = Os.Cmd.in_fd tty ~close:true in
      let tty_out = Os.Cmd.out_fd tty ~close:true in
      Os.Cmd.spawn ~stdin:tty_in ~stdout:tty_out ~stderr:tty_out cat
      |> Result.get_ok'
    in
    let msg = "Hey!" in
    let len = String.length msg in
    Test.int (Unix.write_substring pty msg 0 len) len;
    let buf = Bytes.make len '\x00' in
    Test.int (Unix.read pty buf 0 len) len;
    Test.string (Bytes.to_string buf) msg;
    pid
  in
  Test.result ~ok:Test.T.unit (Os.Cmd.spawn_wait pid) (Ok ());
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
