open Ocamlbuild_plugin
open Command

let os = Ocamlbuild_pack.My_unix.run_and_read "uname -s"

let lib s = match !Ocamlbuild_plugin.Options.ext_lib with
 | "" -> s ^ ".a"
 | x -> s ^ "." ^ x

let system_support_lib = match os with
| "Linux\n" -> [A "-cclib"; A "-lrt"]
| _ -> []

let () =
  dispatch begin function
  | After_rules ->

      dep ["compile";"c"]
          ["src/more_stubs.h"];

      dep ["record_more_stubs"] [lib "src/libmore_stubs"];

      flag_and_dep
        ["link"; "ocaml"; "link_more_stubs"]
        (P (lib "src/libmore_stubs"));

      flag ["library"; "ocaml"; "byte"; "record_more_stubs"]
        (S ([A "-dllib"; A "-lmore_stubs"] @ system_support_lib));

      flag ["library"; "ocaml"; (* byte and native *)
            "record_more_stubs"]
        (S ([A "-cclib"; A "-lmore_stubs"] @ system_support_lib));

      ocaml_lib ~tag_name:"use_more_stubs"
        ~dir:"src" "src/more";

      flag ["link"; "ocaml"; "use_more_stubs"]
        (S [A "-ccopt"; A "-Lsrc"]);
  | _ -> ()
  end
