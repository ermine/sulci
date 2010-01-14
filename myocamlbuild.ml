open Ocamlbuild_plugin
open Myocamlbuild_config

let install_dir = "/tmp/sulci20"

let _ =
  let f = Unix.open_process_in "git describe --always" in
  let answer = input_line f in
    ignore (Unix.close_process_in f);
    let out = open_out "REVISION" in
      output_string out answer;
      close_out out
  
let sulci_plugins () =
  let plugins ext =
    let res =
      List.fold_left (fun acc line ->
                        if line.[0] <> '#' then
                          line -.- ext :: acc
                        else
                          acc
                     ) [] (string_list_of_file "plugins.list") in
      List.rev res
  in
  let plugins_byte = plugins "cmo"
  and plugins_native = plugins "cmx" in
    flag_and_dep ["ocaml"; "compile"; "native"; "use_plugins"] &
      S(List.map (fun f -> P f) plugins_native);
    dep ["ocaml"; "program"; "native"; "use_plugins"] plugins_native;
        
    flag_and_dep ["ocaml"; "compile"; "byte"; "use_plugins"] &
      S(List.map (fun f -> P f) plugins_byte);
    dep ["ocaml"; "program"; "byte"; "use_plugins"] plugins_byte

let lang_msg_list =
  let srcdir = "lang" in
    List.filter (fun f -> not (Pathname.is_directory (srcdir / f)) &&
                   Pathname.check_extension f "msg")
      (Array.to_list (Pathname.readdir srcdir))

let _ = dispatch begin function
  | After_rules ->

      rule "version.ml"
        ~prod:"version.ml"
        ~deps:["version.ml.src"; "REVISION"]
        (fun _ _ ->
           let revision = with_input_file "REVISION" input_line in           
             Seq [rm_f "version.ml";
                  Cmd (S[A"sed"; A"-e";
                         A(Format.sprintf "s,VERSION,%s," revision);
                         Sh"<"; P"version.ml.src"; Sh">"; Px"version.ml"]);
                  chmod (A"-w") "version.ml"]
        );
      
      flag ["ocaml"; "pp"; "use_ulex.syntax"] &
        S[A"-I"; A (ocamlfind_query "ulex"); A"pa_ulex.cma"];

      extern "ulex";

      extern "netsys";
      extern "pcre";
      extern "netclient";
      extern "equeue";
      extern "netstring";
      extern "sqlite3";
      extern "cryptokit";

      flag ["ocaml"; "pp"; "use_json_static.syntax"] &
        S[A"-I"; A (ocamlfind_query "netsys"); A"netsys.cma";
          A"-I"; A (ocamlfind_query "pcre"); A"pcre.cma";
          A"-I"; A (ocamlfind_query "netstring"); A"netstring.cma";
          A"-I"; A (ocamlfind_query "json-wheel"); A"jsonwheel.cma";
          A"-I"; A (ocamlfind_query "json-static"); A"pa_json_static.cmo"];

      extern "json-wheel";
        
      extern "xml";
      extern "mltls";
      extern "treap";
      extern "base64";
      extern ~cma:"XMPP" "xmpp";
      extern "strftime";
      extern "logger";
      extern "scheduler";
      extern "ini_config";
      extern "conversion";
      extern "brainfuck";
      extern "dehtml";
      
      sulci_plugins ();

      flag_and_dep ["ocaml"; "compile"; "native"; "use_lang"] &
        S(List.map (fun a -> P ("lang" / a -.- "cmx"))
            ["ru_time"; "en_time"; "es_time"]);
        
  flag_and_dep ["ocaml"; "compile"; "byte"; "use_lang"] & 
        S(List.map (fun a -> P ("lang" / a -.- "cmo"))
            ["ru_time"; "en_time"; "es_time"]);
        
      rule "generating lang hashtable for sulci"
        ~prod:"%.htbl"
        ~deps:["lang/langcompile.native"; "%.msg"]
        (fun env _ ->
           let msg = env "%.msg" in
           let htbl = env "%.htbl" in
             Cmd (S[Px"lang/langcompile.native";
                    A"import";
                    A (Pathname.remove_extension (Pathname.basename msg));
                    P msg;
                    P htbl])
        );

      rule "generating lang hashtables for sulci"
        ~prod:"lang_msgs"
        ~deps: (List.map
                  (fun f ->
                     "lang" / (Pathname.remove_extension f) -.- "htbl")
                  lang_msg_list)
        (fun _ _ -> Nop);

      rule "generating tlds for sulci"
        ~prod:"tlds/tlds.db"
        ~deps:["tlds/createtlds.native";
               "tlds/tlds.txt"]
        (fun _ _ ->
           Cmd (S [Px "tlds/createtlds.native";
                   P "tlds/tlds.txt";
                   P "tlds/tlds"])
        );
      
      rule "sqlgg"
        ~prod:"%_sql.ml"
        ~dep:"%.sql"
        (fun env _ ->
           let src = env "%.sql" in
           let dst = env "%_sql.ml" in
             Cmd (S [Px "sqlgg.byte"; A"-gen"; A"ocaml"; A"-name"; A"Make";
                     A src; Sh">"; A dst])
        );

      rule "install sulci"
        ~prod:"install"
        ~deps:["sulci.native";
               "sulci.conf.example";
               "decoders";
               "conversion/data/aliases.ini";
               "lang_msgs";
               "tlds/tlds.db"]
        (fun _ _ ->
           Seq [Cmd (S[A"cp"; Px "sulci.native";
                       P(install_dir / "sulci")]);
                Cmd (S[A"cp"; P"sulci.conf.example";
                       P install_dir]);
                Cmd (S[A"mkdir"; A"-p"; P (install_dir / "lang")]);
                Cmd (S[A"cp"; Sh"lang/*.htbl";
                       P (install_dir / "lang")]);
(*                
                Cmd (S[A"mkdir"; A"-p"; P  conversion_decoder_dir]);
                Cmd (S[A"cp"; Sh"conversion/data/decoders/*.dec";
                       P conversion_decoder_dir]);
                Cmd (S[A"cp"; P"conversion/data/aliases.ini";
                       P conversion_aliases_ini]);
                Cmd (S[A"mkdir"; A"-p"; P (install_dir / "tlds")]);
                Cmd (S[A"cp"; P"tlds/tlds.db";
                       P (install_dir / "tlds")])
*)                  
               ]
        )


  | _ ->
      ()
end
