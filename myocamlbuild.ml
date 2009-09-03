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
      List.fold_left (fun acc a ->
                        if a.[0] = '#' then acc
                        else (P(a -.- ext)) :: acc
                     ) [] (string_list_of_file "plugins.list") in
      List.rev res
  in
    flag_and_dep ["ocaml"; "compile"; "native"; "use_plugins"] &
      S(plugins "cmx");
        
    flag_and_dep ["ocaml"; "compile"; "byte"; "use_plugins"] &
      S(plugins "cmo")

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
        
      flag ["ocaml"; "link"; "byte"; "use_cryptokit"] &
        S[A"nums.cma"];
      flag ["ocaml"; "link"; "native"; "use_cryptokit"] &
        S[A"nums.cmxa"];
      extern "cryptokit";
        
      extern "netsys";
      extern "pcre";
      extern "netclient";
      extern "equeue";
      extern "netstring";
      extern "sqlite3";

      extern "xml";
      extern "mltls";
      extern "treap";
      extern ~cma:"xMPP" "xmpp";
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
