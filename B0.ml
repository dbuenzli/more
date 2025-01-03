open B0_kit.V000

(* Library names *)

let b0_std = B0_ocaml.libname "b0.std"
let unix = B0_ocaml.libname "unix"
let cmdliner = B0_ocaml.libname "cmdliner"

let more = B0_ocaml.libname "more"
let more_cli = B0_ocaml.libname "more.cli"

(* Libraries *)

let more_lib =
  let srcs = [ `Dir ~/"src"; `X ~/"src/more_top_init.ml" ] in
  let requires = [unix] and exports = [unix] in
  B0_ocaml.lib more ~srcs ~exports ~requires

let more_cli_lib =
  let srcs = [ `Dir ~/"src/cli" ] in
  let requires = [more; cmdliner] and exports = [more; cmdliner] in
  B0_ocaml.lib more_cli ~srcs ~exports ~requires

(* Tests *)

let test ?(requires = []) =
  B0_ocaml.test ~requires:(b0_std :: more :: requires)

let test_fmt = test ~/"test/test_fmt.ml"
let test_cmd = test ~/"test/test_cmd.ml"
let test_string = test ~/"test/test_string.ml"
let test_fpath = test ~/"test/test_fpath.ml"
let test_cli = test ~/"test/test_cli.ml" ~run:false ~requires:[more_cli]
let test_net_url = test ~/"test/test_net_url.ml"
let test_os_fs = test ~/"test/test_os_fs.ml"
let test_os_info = test ~/"test/test_os_info.ml"
let test_openpty = test ~/"test/test_openpty.ml"
let test_pty = test ~run:false ~/"test/test_pty.ml"

(* Packs *)

let default =
 let meta =
   B0_meta.empty
   |> ~~ B0_meta.authors ["The more programmers"]
   |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
   |> ~~ B0_meta.homepage "https://erratique.ch/software/more"
   |> ~~ B0_meta.online_doc "https://erratique.ch/software/more/doc"
   |> ~~ B0_meta.licenses ["ISC"]
   |> ~~ B0_meta.repo "git+https://erratique.ch/repos/more.git"
   |> ~~ B0_meta.issues "https://github.com/dbuenzli/more/issues"
   |> ~~ B0_meta.description_tags
     ["os"; "system"; "path"; "log"; "unix"; "org:erratique"; ]
   |> ~~ B0_opam.build
     {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
                 "--with-cmdliner" "%{cmdliner:installed}%"]]|}
   |> ~~ B0_opam.depopts ["cmdliner", ""]
   |> ~~ B0_opam.conflicts [ "cmdliner", {|< "2.0.0"|}]
   |> ~~ B0_opam.depends
     [ "ocaml", {|>= "4.14.0"|};
       "ocamlfind", {|build|};
       "ocamlbuild", {|build|};
       "topkg", {|build & >= "1.1.0"|};
     ]
   |> B0_meta.tag B0_opam.tag
 in
 B0_pack.make "default" ~doc:"The more package" ~meta ~locked:true @@
 B0_unit.list ()
