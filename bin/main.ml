open Types
open Inference
open Util

type err = Syntax of string | Type of string | Runtime of string

let read_program path =
  let read_to_string path = open_in path |> In_channel.input_all in
  let stdlib = read_to_string "src/stdlib.iso2" |> Lexer.parse_stdlib in
  let read = read_to_string path in
  let++ { t; ts } = Lexer.parse read in
  { t; ts = ts @ stdlib }

let report name e = boldred name ^ ": " ^ e |> print_endline
let to_syntax r = Result.map_error (fun e -> Syntax e) r
let to_type r = Result.map_error (fun e -> Type e) r
let to_runtime r = Result.map_error (fun e -> Runtime e) r

let () =
  let res =
    let** { t; ts } = read_program Sys.argv.(1) |> to_syntax in
    (* show_term t |> print_endline; *)
    let gen = { i = 0 } in
    let** ctx = build_ctx gen ts |> to_type in
    let** inferred = Result.bind (infer_term t gen ctx) finalize |> to_type in
    (* let** base_type = base_of_any inferred in *)
    let++ evaluated = Eval.eval t |> to_runtime in
    evaluated |> show_term |> print_endline;
    (* ": " ^ show_base_type base_type |> print_endline *)
    ": " ^ show_any (tvar_map [ inferred ]) inferred |> print_endline
  in
  match res with
  | Ok () -> ()
  | Error (Syntax e) -> report "Syntax Error" e
  | Error (Type e) -> report "Error" e
  | Error (Runtime e) -> report "Runtime Error" e
