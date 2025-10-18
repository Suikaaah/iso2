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
      let gen = { i = 0 } in
      let ctx = build_ctx gen ts in
      let infered = Result.bind (infer_term t gen ctx) finalize in
      begin
        match infered with
        | Ok a -> begin
            try
              Eval.eval t |> show_term |> print_endline;
              print_string ": ";
              a |> base_of_any |> Result.get_ok |> show_base_type
              |> print_endline
            with Failure e -> report "Runtime Error" e |> print_endline
          end
        | Error e -> report "Error" e |> print_endline
      end
