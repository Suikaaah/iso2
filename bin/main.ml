let read_program path =
  let lexbuf = open_in path |> Lexing.from_channel in
  try Ok (Parser.program Lexer.token lexbuf)
  with Parser.Error i -> Error (Messages.message i)

let report name e = Util.boldred name ^ ": " ^ e

let () =
  let open Types in
  let open Inference in
  let open Util in
  let res =
    let** { t; ts } = read_program Sys.argv.(1) in
    let gen = { i = 0 } in
    let** ctx = build_ctx gen ts in
    let** inferred = infer_term t gen ctx in
    let** a = finalize inferred in
    let++ a = base_of_any a in
    "inferred: " ^ show_base_type a |> print_endline;
    try Eval.eval t |> show_term |> print_endline
    with Failure e | Invalid_argument e ->
      report "Runtime Error" e |> print_endline
  in
  match res with Ok () -> () | Error e -> report "Error" e |> print_endline
