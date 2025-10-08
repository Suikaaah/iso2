let read_program path =
  let lexbuf = open_in path |> Lexing.from_channel in
  try Ok (Parser.program Lexer.token lexbuf)
  with Parser.Error i -> Error (Messages.message i)

let () =
  let open Types in
  let open Inference in
  let open Eval in
  let program = read_program Sys.argv.(1) in
  match program with
  | Error e -> print_endline e
  | Ok program ->
      (* print_endline "AST:";
  show_program program |> print_endline;
  print_endline "\nInfered:"; *)
      let { t; ts } = program in
      let ctx = build_ctx ts in
      begin
        match infer_base ctx t with
        | Ok _ ->
            (*
        show_base_type a |> print_endline;
        print_endline "\nEvaluated:";
        eval t |> show_term |> print_endline;
        print_endline "\nEvaluated (readable):";
        *)
            eval ctx.psi t |> show_term |> print_endline
        | Error e -> print_endline e
      end
