let read_program path =
  let file = open_in path in
  let lexbuf = Lexing.from_channel file in
  Parser.program Lexer.token lexbuf

let () =
  let open Types in
  let open Inference in
  let open Eval in
  let program = read_program "source.iso2" in
  print_endline "AST:";
  show_program program |> print_endline;
  print_endline "\nInfered:";
  let { t; ts } = program in
  let ctx = build_ctx ts in
  begin
    match infer_base ctx t with
    | Some a ->
        show_base_type a |> print_endline;
        print_endline "\nEvaluated:";
        eval t |> show_term |> print_endline;
        print_endline "\nEvaluated (readable):";
        eval t |> p_term |> print_endline
    | None -> "unable to infer type" |> print_endline
  end
