let read_program path =
  let file = open_in path in
  Parser.program Lexer.token (Lexing.from_channel file)

let () =
  let program = read_program "source.iso2" in
  Types.show_program program |> print_endline
