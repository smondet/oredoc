open Nonstd
module String = Sosa.Native_string
let dbg fmt = Printf.(ksprintf (eprintf "# %s\n")) fmt

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

module Markdown = struct


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
      | Code _
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
          Url (sprintf "%s.html" m, List.map ~f:transform_links t, title)
        | `Ocaml_implementation m ->
          Url (sprintf "%s.html" m, List.map ~f:transform_links t, title)
        | `Other -> 
          Url (href, List.map ~f:transform_links t, title)
        end
      | Ref  (ref_container, name, string, _) as e -> e
    in
    let make_paragraphs =
      let module E = Omd_parser.Default_env(struct end) in
      let module Parser = Omd_parser.Make(E) in
      Parser.make_paragraphs in
    Omd_parser.default_parse
      ~extensions:[highligh] (Omd_lexer.lex content)
    |> make_paragraphs
    |> List.map ~f:transform_links

  let to_html content = 
    Omd.to_html (preprocess content)

end

module Template = struct

  let make_page ~title ~stylesheets content =
    let link css =
      sprintf "<link rel=\"stylesheet\" href=%S type=\"text/css\">" css in
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
    ^ "<strong>TODO</strong>"
    ^ "</div><div class=\"col-md-9\">"
    ^ content
    ^ "</div></div></div></body><html>"

end

let say fmt = Printf.(ksprintf (eprintf "%s\n")) fmt

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
      say "Index file: %s" self#index_file;
      variable_note "INDEX";
      ()
  end

let main () =
  succeedf "mkdir -p %s" conf#output_directory;
  let index = read_file conf#index_file in
  let markdown_index =
    Markdown.to_html index
    |> Template.make_page ~title:"Home" ~stylesheets:conf#stylesheets
  in
  write_file (conf#output_directory // "index.html") ~content:markdown_index;
  List.iter conf#input_files ~f:begin fun path ->
    match File_kind.identify_file path with
    | `Markdown m ->
      let base = Filename.basename m in
      let title = String.map base ~f:(function '_' -> ' ' | c -> c) in
      let content =
        Markdown.to_html (read_file path)
        |> Template.make_page ~title ~stylesheets:conf#stylesheets in
      write_file (conf#output_directory // sprintf "%s.html" base) ~content
    | m -> (* TODO *) ()
  end;
  say "Done."


let () =
  match Array.to_list Sys.argv with
  | [ _ ] | [] ->  main ()
  | exec :: other ->
    say "Usage: [ENV_VAR=...] %s" exec;
    say "Current configuration:";
    conf#display

