(* This file was auto-generated based on "parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
 fun s ->
  match s with
  | 166 -> "invalid state \"CTOR IN\""
  | 165 -> "invalid state \"LPAREN CTOR INVERT CTOR IN\""
  | 164 -> "invalid state \"LPAREN INVERT CTOR IN\""
  | 163 -> "invalid state \"LPAREN CTOR COMMA CTOR IN\""
  | 161 -> "invalid state \"LPAREN CTOR COMMA TYPE\""
  | 160 -> "invalid state \"LPAREN CTOR IN\""
  | 157 -> "invalid state \"LPAREN CTOR TYPE\""
  | 155 -> "invalid state \"LET VAR EQUAL CTOR IN TYPE\""
  | 154 -> "invalid state \"LET VAR EQUAL CTOR EOF\""
  | 153 -> "invalid state \"LET VAR EQUAL TYPE\""
  | 152 -> "invalid state \"LET VAR VAR\""
  | 150 ->
      "invalid state \"LET ISO VAR COLON VAR BIARROW VAR EQUAL CTOR BIARROW \
       CTOR IN TYPE\""
  | 149 ->
      "invalid state \"LET ISO VAR COLON VAR BIARROW VAR EQUAL CTOR BIARROW \
       CTOR RPAREN\""
  | 148 -> "invalid state \"LET ISO VAR COLON VAR BIARROW VAR EQUAL PIPE TYPE\""
  | 147 -> "invalid state \"LET ISO VAR COLON VAR BIARROW VAR EQUAL TYPE\""
  | 146 -> "invalid state \"LET ISO VAR COLON VAR BIARROW VAR RPAREN\""
  | 145 -> "invalid state \"LET ISO VAR COLON TYPE\""
  | 143 -> "invalid state \"LET ISO VAR EQUAL CTOR IN TYPE\""
  | 142 -> "invalid state \"LET ISO VAR EQUAL CTOR TYPE\""
  | 141 -> "invalid state \"LET ISO VAR EQUAL TYPE\""
  | 139 -> "invalid state \"LET ISO VAR VAR\""
  | 138 -> "invalid state \"LET ISO TYPE\""
  | 136 ->
      "invalid state \"LET REC VAR COLON VAR BIARROW VAR EQUAL CTOR IN TYPE\""
  | 135 -> "invalid state \"LET REC VAR COLON VAR BIARROW VAR EQUAL CTOR TYPE\""
  | 134 -> "invalid state \"CTOR INVERT CTOR IN\""
  | 132 -> "invalid state \"INVERT CTOR IN\""
  | 130 -> "invalid state \"CTOR TYPE\""
  | 129 ->
      "invalid state \"LET REC VAR COLON VAR BIARROW VAR EQUAL PIPE CTOR \
       BIARROW CTOR IN TYPE\""
  | 128 ->
      "invalid state \"LET REC VAR COLON VAR BIARROW VAR EQUAL PIPE CTOR \
       BIARROW CTOR RPAREN\""
  | 127 -> "invalid state \"LET REC VAR COLON VAR BIARROW VAR EQUAL PIPE TYPE\""
  | 126 -> "invalid state \"LET REC VAR COLON VAR BIARROW VAR EQUAL TYPE\""
  | 125 -> "invalid state \"LET REC VAR COLON VAR BIARROW VAR RPAREN\""
  | 124 -> "invalid state \"LET REC VAR COLON TYPE\""
  | 123 ->
      "invalid state \"LET REC VAR LPAREN VAR COLON VAR BIARROW VAR RPAREN \
       EQUAL\""
  | 121 ->
      "invalid state \"LET ISO VAR LPAREN VAR COLON VAR BIARROW VAR RPAREN \
       VAR\""
  | 119 ->
      "invalid state \"LET ISO VAR LPAREN VAR COLON VAR BIARROW VAR EQUAL\""
  | 118 -> "invalid state \"LET ISO VAR LPAREN VAR COLON TYPE\""
  | 117 -> "invalid state \"LET ISO VAR LPAREN VAR VAR\""
  | 116 -> "invalid state \"LET ISO VAR LPAREN TYPE\""
  | 115 -> "invalid state \"LET REC VAR VAR\""
  | 114 -> "invalid state \"LET REC TYPE\""
  | 113 -> "invalid state \"LET TYPE\""
  | 112 -> "invalid state \"LPAREN LPAREN TYPE\""
  | 110 -> "invalid state \"LPAREN VAR TYPE\""
  | 109 -> "invalid state \"LPAREN TYPE\""
  | 105 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR \
       EQUAL CTOR VAR IN TYPE\""
  | 104 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR \
       EQUAL CTOR VAR TRIANGLE\""
  | 103 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR \
       EQUAL CTOR TYPE\""
  | 100 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR \
       EQUAL VAR TRIANGLE CTOR IN TYPE\""
  | 99 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR \
       EQUAL VAR TRIANGLE CTOR TYPE\""
  | 98 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR \
       EQUAL VAR TRIANGLE TYPE\""
  | 97 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR \
       EQUAL VAR IN\""
  | 96 -> "invalid state \"INVERT CTOR TYPE\""
  | 94 -> "invalid state \"INVERT LPAREN CTOR TYPE\""
  | 93 -> "invalid state \"FIX VAR COLON VAR BIARROW VAR DOT CTOR TYPE\""
  | 92 -> "invalid state \"INVERT CTOR CTOR TYPE\""
  | 91 -> "invalid state \"BACKSLASH VAR COLON VAR BIARROW VAR DOT CTOR TYPE\""
  | 90 -> "invalid state \"BACKSLASH VAR COLON VAR BIARROW VAR DOT TYPE\""
  | 89 -> "invalid state \"BACKSLASH VAR COLON VAR BIARROW VAR RPAREN\""
  | 88 -> "invalid state \"BACKSLASH VAR COLON TYPE\""
  | 87 -> "invalid state \"BACKSLASH VAR VAR\""
  | 86 -> "invalid state \"BACKSLASH TYPE\""
  | 84 -> "invalid state \"FIX VAR COLON VAR BIARROW VAR DOT TYPE\""
  | 83 -> "invalid state \"FIX VAR COLON VAR BIARROW VAR RPAREN\""
  | 82 -> "invalid state \"FIX VAR COLON TYPE\""
  | 81 -> "invalid state \"FIX VAR VAR\""
  | 80 -> "invalid state \"FIX TYPE\""
  | 79 -> "invalid state \"INVERT LPAREN TYPE\""
  | 77 -> "invalid state \"INVERT TYPE\""
  | 75 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR BIARROW CTOR PIPE \
       TYPE\""
  | 74 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR BIARROW VAR TYPE\""
  | 72 -> "invalid state \"ISO COLON VAR BIARROW VAR DOT PIPE TYPE\""
  | 71 -> "invalid state \"ISO COLON VAR BIARROW VAR DOT TYPE\""
  | 70 -> "invalid state \"ISO COLON VAR BIARROW VAR RPAREN\""
  | 69 -> "invalid state \"ISO COLON TYPE\""
  | 68 -> "invalid state \"ISO VAR\""
  | 67 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR \
       EQUAL LPAREN TYPE\""
  | 66 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR \
       EQUAL VAR TYPE\""
  | 65 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR \
       EQUAL TYPE\""
  | 64 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR VAR\""
  | 63 -> "invalid state \"LET LPAREN VAR COMMA VAR VAR\""
  | 61 -> "invalid state \"LET LPAREN VAR COMMA TYPE\""
  | 60 -> "invalid state \"LET LPAREN VAR TRIANGLE\""
  | 57 -> "invalid state \"LET LPAREN TYPE\""
  | 55 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET TYPE\""
  | 54 -> "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR BIARROW TYPE\""
  | 53 -> "invalid state \"ISO COLON VAR BIARROW VAR DOT VAR VAR\""
  | 52 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT LPAREN CTOR COMMA VAR \
       VAR\""
  | 50 ->
      "invalid state \"ISO COLON VAR BIARROW VAR DOT LPAREN CTOR COMMA TYPE\""
  | 49 -> "invalid state \"ISO COLON VAR BIARROW VAR DOT LPAREN VAR VAR\""
  | 45 -> "invalid state \"ISO COLON VAR BIARROW VAR DOT CTOR TYPE\""
  | 43 -> "invalid state \"ISO COLON VAR BIARROW VAR DOT LPAREN TYPE\""
  | 41 -> "invalid state \"REC VAR COLON VAR BIARROW VAR DOT PIPE TYPE\""
  | 40 -> "invalid state \"REC VAR COLON VAR BIARROW VAR DOT TYPE\""
  | 39 -> "invalid state \"REC VAR COLON VAR BIARROW VAR RPAREN\""
  | 38 -> "invalid state \"ISO COLON LPAREN VAR VAR\""
  | 36 -> "invalid state \"ISO COLON VAR BIARROW TYPE\""
  | 35 -> "invalid state \"ISO COLON VAR VAR\""
  | 34 ->
      "invalid state \"ISO COLON VAR BIARROW VAR ARROW VAR BIARROW VAR VAR\""
  | 33 -> "invalid state \"ISO COLON VAR BIARROW VAR ARROW TYPE\""
  | 31 -> "invalid state \"ISO COLON LPAREN VAR BIARROW VAR EQUAL\""
  | 30 -> "invalid state \"ISO COLON LPAREN TYPE\""
  | 29 -> "invalid state \"REC VAR COLON TYPE\""
  | 28 -> "invalid state \"REC VAR VAR\""
  | 27 -> "invalid state \"REC TYPE\""
  | 26 -> "invalid state \"VAR TYPE\""
  | 19 -> "invalid state \"TYPE VAR EQUAL CTOR PIPE VAR\""
  | 18 -> "invalid state \"TYPE VAR EQUAL CTOR OF VAR TRIANGLE\""
  | 16 -> "invalid state \"ISO COLON LPAREN VAR COMMA VAR VAR\""
  | 14 -> "invalid state \"ISO COLON LPAREN VAR COMMA TYPE\""
  | 13 -> "invalid state \"ISO COLON VAR BIARROW LPAREN VAR VAR\""
  | 9 -> "invalid state \"ISO COLON VAR BIARROW LPAREN TYPE\""
  | 7 -> "invalid state \"TYPE VAR EQUAL CTOR OF TYPE\""
  | 6 -> "invalid state \"TYPE VAR EQUAL CTOR TRIANGLE\""
  | 5 -> "invalid state \"TYPE VAR EQUAL PIPE VAR\""
  | 3 -> "invalid state \"TYPE VAR EQUAL VAR\""
  | 2 -> "invalid state \"TYPE VAR VAR\""
  | 1 -> "invalid state \"TYPE TYPE\""
  | 0 -> "invalid state \"TRIANGLE\""
  | _ -> raise Not_found
