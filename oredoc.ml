
(*M

Implementation of Oredoc
========================

Usual “OCaml-configuration” header:

M*)
open Nonstd
module Legacy_string = String
module String = Sosa.Native_string
let dbg fmt = Printf.(ksprintf (eprintf "# %s\n")) fmt


(*M

Yet Another Monad
-----------------

The module `Meta_result` provides a funny monad that allows one to keep track
of things to do “later” during a computation.

M*)
module Meta_result = struct

  type ('a, 'b) t = {
    result: 'a;
    more_things_todo: 'b list;
  }

  let return ?(more_things_todo=[]) result = {result; more_things_todo}
  let bind m ~f =
    let next = f m.result in
    { next with more_things_todo = next.more_things_todo @ m.more_things_todo}
  let (>>=) m f = bind m ~f

end

(*M

Identify Files By Extension 
---------------------------

This is used for both 1) deciding which treatment to apply to files and 2)
transforming relative links between them (in Markdown).

M*)
module File_kind = struct

  let check_and_remove_extension filename ~ext =
    if Filename.check_suffix filename ext
    then Some (Filename.chop_suffix filename ext)
    else None

  let identify_file filename =
    begin match check_and_remove_extension filename ~ext:".md" with
    | Some sub -> `Markdown sub
    | None ->
      begin match check_and_remove_extension filename ~ext:".ml" with
      | Some sub -> `Ocaml_implementation sub
      | None ->
        begin match check_and_remove_extension filename ~ext:".mli" with
        | Some sub -> `Ocaml_interface sub
        | None -> `Other
        end
      end
    end

end

(*M
Markdown To HTML
----------------

Our wrapper around [Omd](https://github.com/ocaml/omd), which adds a few
features/extensions:

- There is a special syntax to !overhighlight! stuff.
- Local links are transformed to be consistent between a repository view
(Github, Bitbucket) and the generated website.
- `some-command --help` will be transformed into a link too.

M*)
module Markdown = struct

  let code_url code =
    String.map code ~f:(function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' as c -> c
      | other -> '_')
    ^ ".html"

  let looks_like_module_path code =
    String.length code > 3
    &&
    String.split code ~on:(`Character '.')
    |> List.for_all ~f:(fun s ->
        String.length s > 0
        &&
        String.for_all s ~f:(function
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
          | other -> false))

  let url_of_module_path code =
    let is_value v =
      match v.[0] with
      | None -> false
      | Some c when Char.lowercase c = c -> true
      | Some c -> false in
    let rec loop acc =
      function
      | [] -> ""
      | ["t"]  -> acc ^ "html#TYPEt"
      | [one] when is_value one -> acc ^ "html#VAL" ^ one
      | [one] -> acc ^ one ^ ".html"
      | modul :: more -> loop (acc ^ modul ^ ".") more
    in
    loop "api/" (String.split code ~on:(`Character '.'))


  let preprocess content =
    let highligh t read_tokens = 
      let open Omd_representation in
      let highlight_style =
        "color: red; background-color: yellow; font-weight: bold" in
      (* dbg "highlight for: %s" (Omd_lexer.destring_of_tokens ~limit:5  read_tokens); *)
      function 
      | Exclamation :: Word w :: Exclamation :: more ->
        let in_red = sprintf  "<span style=%S>%s</span>" highlight_style w in
        Some (Html in_red :: t, read_tokens, more)
      | m ->
        (* dbg "none for: %s" (Omd_lexer.destring_of_tokens ~limit:4 m); *)
        None
    in
    let more_stuff_to_do = ref [] in
    let rec transform_links (t: Omd.element) : Omd.element  =
      let open Omd in
      match t with 
      | Paragraph  t -> Paragraph (List.map ~f:transform_links t)
      | Emph  t -> Emph (List.map ~f:transform_links t)
      | Bold t -> Bold (List.map ~f:transform_links t)
      | Ul  tl -> Ul  (List.map ~f:(List.map ~f:transform_links) tl)
      | Ol  tl -> Ol  (List.map ~f:(List.map ~f:transform_links) tl)
      | Ulp tl -> Ulp (List.map ~f:(List.map ~f:transform_links) tl)
      | Olp tl -> Olp (List.map ~f:(List.map ~f:transform_links) tl)
      | H1 t -> H1 (List.map ~f:transform_links t)
      | H2 t -> H2 (List.map ~f:transform_links t)
      | H3 t -> H3 (List.map ~f:transform_links t)
      | H4 t -> H4 (List.map ~f:transform_links t)
      | H5 t -> H5 (List.map ~f:transform_links t)
      | H6 t -> H6 (List.map ~f:transform_links t)
      | Blockquote  t -> Blockquote (List.map ~f:transform_links t)
      | Text _
      | Br
      | Hr
      | Img_ref _
      | Html _
      | Html_block _
      | Html_comment _
      | NL
      | X _
      | Raw _ | Raw_block _
      | Img _ as e -> e
      | Code (lang, code) when 
          String.sub code ~index:(String.length code - 6) ~length:6 
          = Some "--help" ->
        (* dbg "Code: %s %s" lang code; *)
        more_stuff_to_do := `Create_man_page code :: !more_stuff_to_do;
        Url (code_url code, [Code (lang, code)], code)
      | Code (lang, code) when looks_like_module_path code ->
        Url (url_of_module_path code, [Code (lang, code)], code)
      | Code (lang, code) -> Code (lang, code)
      | Code_block (lang, code) as code_block ->
        begin try
          let (_ : Higlo.lexer) = Higlo.get_lexer lang in
          Html_block (
            "<pre>"
            ^ (Higlo.to_xtmpl ~lang code |> Xtmpl.string_of_xmls)
            ^ "</pre>")
        with _ -> code_block
        end
      | Url (href, t, title) -> 
        begin match File_kind.identify_file href with
        | `Markdown m ->
          Url (sprintf "./%s.html" (Filename.basename m),
               List.map ~f:transform_links t, title)
        | `Ocaml_interface m ->
          let modname = Filename.basename m |> Legacy_string.capitalize in
          Url (sprintf "api/%s.html" modname,
               List.map ~f:transform_links t, title)
        | `Ocaml_implementation m ->
          Url (sprintf "%s.html" (Filename.basename m),
               List.map ~f:transform_links t, title)
        | `Other -> 
          Url (href, List.map ~f:transform_links t, title)
        end
      | Ref  (ref_container, name, string, _) as e -> e
    in
    let make_paragraphs =
      let module E = Omd_parser.Default_env(struct end) in
      let module Parser = Omd_parser.Make(E) in
      Parser.make_paragraphs in
    Meta_result.return ~more_things_todo:!more_stuff_to_do (
      Omd_parser.default_parse
        ~extensions:[highligh] (Omd_lexer.lex content)
      |> make_paragraphs
      |> List.map ~f:transform_links)

  let to_html content = 
    Meta_result.(
      preprocess content
      >>= fun p ->
      return Omd.(to_html p)
    )

  let to_toc content =
    Meta_result.(
      preprocess content
      >>= fun p ->
      return Omd.(to_html (toc ~start:[0] p))
    )

  let to_html_and_toc content = 
    Meta_result.(
      preprocess content
      >>= fun p ->
      return Omd.(to_html p, to_html (toc ~start:[1]  p))
    )
    (* Omd.(to_html p, to_html (toc  ~start:[1] p)) *)


end

(*M

Converting OCaml to HTML
------------------------

OCaml code with special Markdown comments will be transformed into HTML.

M*)
module Ocaml = struct

  open Meta_result

  let to_html code =
    let remove_comments s = 
      String.sub_exn s ~index:3 ~length:(String.length s - 6)
    in
    let open Higlo in
    let parsed = parse ~lang:"ocaml" code in
    let flush_tokens revtoklist =
      if List.for_all revtoklist 
          ~f:(function Text t when String.strip t = "" -> true | _ -> false)
      then ""
      else
        let html =
          Xtmpl.string_of_xmls
            (List.rev_map ~f:Higlo.token_to_xtmpl revtoklist) in
        "<pre>" ^ html ^ "</pre>"
    in
    let rec loop acc_tokens acc_html acc_toc tokens = 
      match tokens with
      | [] -> 
        List.rev acc_toc |> String.concat ~sep:"\n" |> Markdown.to_toc
        >>= fun toc ->
        return (List.rev (flush_tokens acc_tokens :: acc_html) 
         |> String.concat ~sep:"\n", toc)
      | one :: more ->
        begin match one with
        | Bcomment com
        | Lcomment com when String.sub com ~index:0 ~length:3 = Some "(*M" ->
          let html_code = flush_tokens acc_tokens in
          let comment_content = remove_comments com in
          Markdown.to_html comment_content
          >>= fun html_comment ->
          loop [] (html_comment :: html_code :: acc_html) 
            (comment_content :: acc_toc) more
        | tok -> 
          loop (tok :: acc_tokens) acc_html acc_toc more
        end
    in
    loop [] [] [] parsed

end

(*M

HTML Template
-------------

M*)
module Template = struct

  let make_page ~title ~stylesheets ~toc ~menu content =
    let link css =
      sprintf "<link rel=\"stylesheet\" href=%S type=\"text/css\">" css in
    Meta_result.return (
      " <!DOCTYPE html> <html> <head>"
      ^ String.concat ~sep:"\n" (List.map stylesheets ~f:link)
      ^ "<meta charset=\"utf-8\">"
      ^ sprintf "<title>%s</title>" title
      ^ "</head>"
      ^ "<body><div class=\"container\">" 
      ^ sprintf "<h1>%s</h1>" title
      ^ "<div class=\"row\">\n\
         <div class=\"col-md-3\">\n\
         <h2>Contents</h2>"
      ^ toc
      ^ "<h2>Menu</h2>"
      ^ menu
      ^ "</div><div class=\"col-md-9\">"
      ^ content
      ^ "</div></div></div></body><html>")

end

(*M

Some Utilities
--------------

M*)

module Utilities = struct
  let (//) = Filename.concat

  let env s =
    try Some (Sys.getenv s) with _ -> None

  let failwithf fmt = ksprintf failwith fmt

  let succeed s = 
    match Sys.command s with
    | 0 -> ()
    | other -> failwithf "Command %S did not succeed: %d" s other
  let succeedf fmt= ksprintf succeed fmt

  let all_files dir =
    Sys.readdir dir |> Array.to_list 
    |> List.filter_map ~f:(fun d ->
        let p = dir // d in 
        if Sys.is_directory p then None else Some p)

  let read_file f =
    let i = open_in f in
    let buf = Buffer.create 42 in
    let rec loop () =
      try Buffer.add_channel buf i 1; loop () with _ -> () in
    loop ();
    close_in i;
    Buffer.contents buf

  let write_file f ~content =
    let o = open_out f in
    output_string o content;
    close_out o

  let parse_list_of_substitutions s =
    let subs = String.split ~on:(`Character ',') s in
    List.filter_map subs ~f:(fun sub ->
        match String.split ~on:(`Character ':') (String.strip sub) with
        | [one; two] -> Some (one, two)
        | other -> None
      )

end
open Utilities
let say fmt = Printf.(ksprintf (printf "%s\n")) fmt


(*M

Configuration
-------------

M*)
let default_stylesheets = [
  "https://cdn.rawgit.com/hammerlab/ketrew/2d1c430cca52caa71e363a765ff8775a6ae14ba9/src/doc/code_style.css";
  "http://cdn.jsdelivr.net/bootstrap/3.1.1/css/bootstrap.min.css";
  "http://cdn.jsdelivr.net/bootstrap/3.1.1/css/bootstrap-theme.min.css";
  (* <link rel="stylesheet" href="code_style.css" type="text/css"> *)
]

let conf = 
  object (self)
    method output_directory =
      env "OUTPUT_DIR" |> Option.value ~default:"_doc"
    method input_files =
      env "INPUT" |> Option.value ~default:"oredoc.ml,src/doc,doc"
      |> String.split ~on:(`Character ',')
      |> List.map ~f:(fun path ->
          try
            if Sys.is_directory path
            then all_files path
            else [path]
          with _ -> [])
      |> List.concat
    method index_file =
      env "INDEX" |> Option.value ~default:"README.md"
    method stylesheets =
      env "CSS" |> Option.map ~f:(String.split ~on:(`Character ','))
      |> Option.value ~default:default_stylesheets
    method api_doc_directory =
      env "API"
    method title_prefix =
      env "TITLE_PREFIX" |> Option.value ~default:""
    method title_substitutions =
      env "TITLE_SUBSTITUTIONS"
      |> Option.value_map ~default:[] ~f:parse_list_of_substitutions
    method title ?(with_prefix=true) t = 
      let tt = 
        List.find_map self#title_substitutions ~f:(function
          | (a, b) when 
              a = t || (try Filename.chop_extension a = t with _ -> false) ->
            Some b
          | _ -> None)
        |> function
        | Some b -> b
        | None -> 
          String.map t ~f:(function '_' -> ' ' | c -> c)
      in
      sprintf "%s%s" (if with_prefix then self#title_prefix else "") tt
    method command_substitutions =
      env "COMMAND_SUBSTITUTIONS" 
      |> Option.value_map ~default:[] ~f:parse_list_of_substitutions
    method display =
      let list_of_paths l =
        (List.map l ~f:(sprintf "  - %S") |> String.concat ~sep:"\n") in
      let variable_note var =
        say "  (%S is %s)" var
          (match env var with None -> "empty" | Some s -> sprintf "%S" s) in
      say "Output directory: %s" self#output_directory;
      variable_note "OUTPUT_DIR";
      say "Input files:\n%s"
        (list_of_paths self#input_files);
      variable_note "INPUT";
      say "Style sheets:\n%s" (list_of_paths self#stylesheets);
      variable_note "CSS";
      begin match self#api_doc_directory with
      | Some s -> say "Getting API docs from: %S" s
      | None -> say "No getting API docs (*Warning*)"
      end;
      variable_note "API";
      say "Title prefix: %S" self#title_prefix;
      variable_note "TITLE_PREFIX";
      say "Command substitutions:";
      List.iter self#command_substitutions (fun (a, b) -> say "  - %s → %s" a b);
      variable_note "COMMAND_SUBSTITUTIONS";
      say "Index file: %s" self#index_file;
      variable_note "INDEX";
      ()
  end

(*M

Main Entry Point
----------------

M*)
let main () =
  let open Meta_result in
  succeedf "mkdir -p %s" conf#output_directory;
  begin match conf#api_doc_directory with
  | Some s -> succeedf "rsync -a %s/ %s/api" s conf#output_directory
  | None -> say "Warning, no API docs"
  end;
  let menu_md =
    sprintf "- [Home](index.html)\n" 
    :: (List.map conf#input_files ~f:(fun path ->
        match File_kind.identify_file path with
        | `Markdown m
        | `Ocaml_implementation m ->
          let base = Filename.basename m in
          let title = conf#title ~with_prefix:false base in
          sprintf "- [%s](%s.html)\n" title base
        | other -> ""))
    @ [sprintf "- [API Documentation](./api/index.html)\n"]
    |> String.concat ~sep:""
  in
  let first_pass_result : (unit, _) t =
    Markdown.to_html menu_md
    >>= fun menu ->
    Markdown.to_html_and_toc (read_file conf#index_file)
    >>= fun (content, toc) ->
    let title = conf#title "Home" in
    Template.make_page ~menu ~title ~stylesheets:conf#stylesheets ~toc content
    >>= fun markdown_index ->
    write_file (conf#output_directory // "index.html") ~content:markdown_index;
    List.fold ~init:(return ()) conf#input_files ~f:begin fun prev path ->
      prev >>= fun () ->
      match File_kind.identify_file path with
      | `Markdown m ->
        let base = Filename.basename m in
        let title = conf#title base in
        Markdown.to_html_and_toc (read_file path)
        >>= fun (content, toc) ->
        Template.make_page ~menu ~title ~stylesheets:conf#stylesheets ~toc content
        >>= fun content ->
        write_file (conf#output_directory // sprintf "%s.html" base) ~content;
        return ()
      | `Ocaml_implementation impl ->
        let base = Filename.basename impl in
        let title = conf#title base in
        Ocaml.to_html (read_file path)
        >>= fun (content, toc) ->
        Template.make_page ~title ~menu  ~stylesheets:conf#stylesheets ~toc content
        >>= fun content ->
        write_file (conf#output_directory // sprintf "%s.html" base) ~content;
        return ()
      | m -> 
        succeedf "cp %s %s/" (Filename.quote path) conf#output_directory;
        return ()
    end
  in
  List.dedup first_pass_result.more_things_todo |> List.iter ~f:begin function
  | `Create_man_page cmd ->
    let actual_cmd = 
      let stripped = String.strip cmd in
      List.find_map conf#command_substitutions ~f:(fun (left, right) ->
          match String.(sub stripped ~index:0 ~length:(length left)) with
          | Some prefix when prefix = left ->
            Some (right
                  ^ String.(sub_exn stripped ~index:(length left)
                              ~length:(length stripped - length left)))
          | _ -> None) |> Option.value ~default:stripped
    in
    let output_file = conf#output_directory // Markdown.code_url cmd in
    begin try 
      succeedf "set -o pipefail ; %s=groff | groff -Thtml -mandoc > %s" actual_cmd output_file;
    with
    | e -> 
      ignore (
        succeedf "(echo '```' ; %s ; echo '```') > %s" actual_cmd output_file;
        Markdown.to_html_and_toc (read_file output_file)
        >>= fun (content, toc) ->
        Markdown.to_html menu_md
        >>= fun menu ->
        Template.make_page ~menu ~title:cmd ~stylesheets:conf#stylesheets ~toc:"" content
        >>= fun content ->
        write_file (output_file) ~content;
        return ()
      );
    end;
  end;
  ()


let () =
  match Array.to_list Sys.argv with
  | [ _ ] | [] ->  main ()
  | exec :: "--help" :: _ 
  | exec :: "-h" :: _ ->
    say "Usage: [ENV_VAR=...] %s" exec;
    say "Current configuration:";
    conf#display
  | exec :: other ->
    say "Wrong command line";
    exit 1

