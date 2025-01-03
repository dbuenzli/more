(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open More

let test ~output_details:_ =
  Log.stdout (fun m -> m "Invoke with %a to silence me" Fmt.code "--quiet");
  Cmdliner.Cmd.Exit.ok

open Cmdliner
open Cmdliner.Term.Syntax

let tool_cmd =
  Cmd.make (Cmd.info "test_cli") @@
  let+ () = More_cli.set_log_level ()
  and+ () = More_cli.set_no_color ()
  and+ output_details = More_cli.output_details () in
  test ~output_details

let main () = Cmd.eval' tool_cmd
let () = if !Sys.interactive then () else exit (main ())
