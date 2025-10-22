let read_program path =
  let read_to_string path = open_in path |> In_channel.input_all in
  let stdlib = read_to_string "src/stdlib.iso2" in
  let read = read_to_string path in
  let lexbuf = stdlib ^ read |> Lexing.from_string in
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
    let** inferred = Result.bind (infer_term t gen ctx) finalize in
    (* let** base_type = base_of_any inferred in *)
    let++ evaluated = Eval.eval t in
    evaluated |> show_term |> print_endline;
    (* ": " ^ show_base_type base_type |> print_endline *)
    ": " ^ show_any inferred |> print_endline
  in
  match res with Ok () -> () | Error e -> report "Error" e |> print_endline
