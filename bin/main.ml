let read_program path =
  let lexbuf = open_in path |> Lexing.from_channel in
  try Ok (Parser.program Lexer.token lexbuf)
  with Parser.Error i -> Error (Messages.message i)

let () =
  let open Types in
  let open Inference in
  let open Util in
  let program = read_program Sys.argv.(1) in
  let report name e = boldred name ^ ": " ^ e in
  match program with
  | Error e -> report "Syntax Error" e |> print_string
  | Ok program ->
      let { t; ts } = program in
      let ctx = build_ctx ts in
      begin
        match infer_base ctx t with
        | Ok _ -> begin
            try Eval.eval ctx.psi t |> show_term |> print_endline
            with Failure e -> report "Runtime Error" e |> print_endline
          end
        | Error e -> report "Error" e |> print_endline
      end
