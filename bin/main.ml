let read_program path =
  let file = open_in path in
  Parser.program Lexer.token (Lexing.from_channel file)

let () =
  let open Types in
  let open Inference in
  let program = read_program "source.iso2" in
  show_program program |> print_endline;
  let { t; ts } = program in
  let content =
    match infer_base (build_ctx ts) t with
    | Some t -> show_base_type t
    | None -> "unable to infer the type"
  in
  print_endline content
