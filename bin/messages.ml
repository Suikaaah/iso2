
(* This file was auto-generated based on "parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 166 ->
        "CTOR IN\n"
    | 165 ->
        "LPAREN CTOR INVERT CTOR IN\n"
    | 164 ->
        "LPAREN INVERT CTOR IN\n"
    | 163 ->
        "LPAREN CTOR COMMA CTOR IN\n"
    | 161 ->
        "LPAREN CTOR COMMA TYPE\n"
    | 160 ->
        "LPAREN CTOR IN\n"
    | 157 ->
        "LPAREN CTOR TYPE\n"
    | 155 ->
        "LET VAR EQUAL CTOR IN TYPE\n"
    | 154 ->
        "LET VAR EQUAL CTOR EOF\n"
    | 153 ->
        "LET VAR EQUAL TYPE\n"
    | 152 ->
        "LET VAR VAR\n"
    | 150 ->
        "LET ISO VAR COLON VAR BIARROW VAR EQUAL CTOR BIARROW CTOR IN TYPE\n"
    | 149 ->
        "LET ISO VAR COLON VAR BIARROW VAR EQUAL CTOR BIARROW CTOR RPAREN\n"
    | 148 ->
        "LET ISO VAR COLON VAR BIARROW VAR EQUAL PIPE TYPE\n"
    | 147 ->
        "LET ISO VAR COLON VAR BIARROW VAR EQUAL TYPE\n"
    | 146 ->
        "LET ISO VAR COLON VAR BIARROW VAR RPAREN\n"
    | 145 ->
        "LET ISO VAR COLON TYPE\n"
    | 143 ->
        "LET ISO VAR EQUAL CTOR IN TYPE\n"
    | 142 ->
        "LET ISO VAR EQUAL CTOR TYPE\n"
    | 141 ->
        "LET ISO VAR EQUAL TYPE\n"
    | 139 ->
        "LET ISO VAR VAR\n"
    | 138 ->
        "LET ISO TYPE\n"
    | 136 ->
        "LET REC VAR COLON VAR BIARROW VAR EQUAL CTOR IN TYPE\n"
    | 135 ->
        "LET REC VAR COLON VAR BIARROW VAR EQUAL CTOR TYPE\n"
    | 134 ->
        "CTOR INVERT CTOR IN\n"
    | 132 ->
        "INVERT CTOR IN\n"
    | 130 ->
        "CTOR TYPE\n"
    | 129 ->
        "LET REC VAR COLON VAR BIARROW VAR EQUAL PIPE CTOR BIARROW CTOR IN TYPE\n"
    | 128 ->
        "LET REC VAR COLON VAR BIARROW VAR EQUAL PIPE CTOR BIARROW CTOR RPAREN\n"
    | 127 ->
        "LET REC VAR COLON VAR BIARROW VAR EQUAL PIPE TYPE\n"
    | 126 ->
        "LET REC VAR COLON VAR BIARROW VAR EQUAL TYPE\n"
    | 125 ->
        "LET REC VAR COLON VAR BIARROW VAR RPAREN\n"
    | 124 ->
        "LET REC VAR COLON TYPE\n"
    | 123 ->
        "LET REC VAR LPAREN VAR COLON VAR BIARROW VAR RPAREN EQUAL\n"
    | 121 ->
        "LET ISO VAR LPAREN VAR COLON VAR BIARROW VAR RPAREN VAR\n"
    | 119 ->
        "LET ISO VAR LPAREN VAR COLON VAR BIARROW VAR EQUAL\n"
    | 118 ->
        "LET ISO VAR LPAREN VAR COLON TYPE\n"
    | 117 ->
        "LET ISO VAR LPAREN VAR VAR\n"
    | 116 ->
        "LET ISO VAR LPAREN TYPE\n"
    | 115 ->
        "LET REC VAR VAR\n"
    | 114 ->
        "LET REC TYPE\n"
    | 113 ->
        "LET TYPE\n"
    | 112 ->
        "LPAREN LPAREN TYPE\n"
    | 110 ->
        "LPAREN VAR TYPE\n"
    | 109 ->
        "LPAREN TYPE\n"
    | 105 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR EQUAL CTOR VAR IN TYPE\n"
    | 104 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR EQUAL CTOR VAR TRIANGLE\n"
    | 103 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR EQUAL CTOR TYPE\n"
    | 100 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR EQUAL VAR TRIANGLE CTOR IN TYPE\n"
    | 99 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR EQUAL VAR TRIANGLE CTOR TYPE\n"
    | 98 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR EQUAL VAR TRIANGLE TYPE\n"
    | 97 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR EQUAL VAR IN\n"
    | 96 ->
        "INVERT CTOR TYPE\n"
    | 94 ->
        "INVERT LPAREN CTOR TYPE\n"
    | 93 ->
        "FIX VAR COLON VAR BIARROW VAR DOT CTOR TYPE\n"
    | 92 ->
        "INVERT CTOR CTOR TYPE\n"
    | 91 ->
        "BACKSLASH VAR COLON VAR BIARROW VAR DOT CTOR TYPE\n"
    | 90 ->
        "BACKSLASH VAR COLON VAR BIARROW VAR DOT TYPE\n"
    | 89 ->
        "BACKSLASH VAR COLON VAR BIARROW VAR RPAREN\n"
    | 88 ->
        "BACKSLASH VAR COLON TYPE\n"
    | 87 ->
        "BACKSLASH VAR VAR\n"
    | 86 ->
        "BACKSLASH TYPE\n"
    | 84 ->
        "FIX VAR COLON VAR BIARROW VAR DOT TYPE\n"
    | 83 ->
        "FIX VAR COLON VAR BIARROW VAR RPAREN\n"
    | 82 ->
        "FIX VAR COLON TYPE\n"
    | 81 ->
        "FIX VAR VAR\n"
    | 80 ->
        "FIX TYPE\n"
    | 79 ->
        "INVERT LPAREN TYPE\n"
    | 77 ->
        "INVERT TYPE\n"
    | 75 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR BIARROW CTOR PIPE TYPE\n"
    | 74 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR BIARROW VAR TYPE\n"
    | 72 ->
        "ISO COLON VAR BIARROW VAR DOT PIPE TYPE\n"
    | 71 ->
        "ISO COLON VAR BIARROW VAR DOT TYPE\n"
    | 70 ->
        "ISO COLON VAR BIARROW VAR RPAREN\n"
    | 69 ->
        "ISO COLON TYPE\n"
    | 68 ->
        "ISO VAR\n"
    | 67 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR EQUAL LPAREN TYPE\n"
    | 66 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR EQUAL VAR TYPE\n"
    | 65 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR EQUAL TYPE\n"
    | 64 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET VAR VAR\n"
    | 63 ->
        "LET LPAREN VAR COMMA VAR VAR\n"
    | 61 ->
        "LET LPAREN VAR COMMA TYPE\n"
    | 60 ->
        "LET LPAREN VAR TRIANGLE\n"
    | 57 ->
        "LET LPAREN TYPE\n"
    | 55 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR BIARROW LET TYPE\n"
    | 54 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR BIARROW TYPE\n"
    | 53 ->
        "ISO COLON VAR BIARROW VAR DOT VAR VAR\n"
    | 52 ->
        "ISO COLON VAR BIARROW VAR DOT LPAREN CTOR COMMA VAR VAR\n"
    | 50 ->
        "ISO COLON VAR BIARROW VAR DOT LPAREN CTOR COMMA TYPE\n"
    | 49 ->
        "ISO COLON VAR BIARROW VAR DOT LPAREN VAR VAR\n"
    | 45 ->
        "ISO COLON VAR BIARROW VAR DOT CTOR TYPE\n"
    | 43 ->
        "ISO COLON VAR BIARROW VAR DOT LPAREN TYPE\n"
    | 41 ->
        "REC VAR COLON VAR BIARROW VAR DOT PIPE TYPE\n"
    | 40 ->
        "REC VAR COLON VAR BIARROW VAR DOT TYPE\n"
    | 39 ->
        "REC VAR COLON VAR BIARROW VAR RPAREN\n"
    | 38 ->
        "ISO COLON LPAREN VAR VAR\n"
    | 36 ->
        "ISO COLON VAR BIARROW TYPE\n"
    | 35 ->
        "ISO COLON VAR VAR\n"
    | 34 ->
        "ISO COLON VAR BIARROW VAR ARROW VAR BIARROW VAR VAR\n"
    | 33 ->
        "ISO COLON VAR BIARROW VAR ARROW TYPE\n"
    | 31 ->
        "ISO COLON LPAREN VAR BIARROW VAR EQUAL\n"
    | 30 ->
        "ISO COLON LPAREN TYPE\n"
    | 29 ->
        "REC VAR COLON TYPE\n"
    | 28 ->
        "REC VAR VAR\n"
    | 27 ->
        "REC TYPE\n"
    | 26 ->
        "VAR TYPE\n"
    | 19 ->
        "TYPE VAR EQUAL CTOR PIPE VAR\n"
    | 18 ->
        "TYPE VAR EQUAL CTOR OF VAR TRIANGLE\n"
    | 16 ->
        "ISO COLON LPAREN VAR COMMA VAR VAR\n"
    | 14 ->
        "ISO COLON LPAREN VAR COMMA TYPE\n"
    | 13 ->
        "ISO COLON VAR BIARROW LPAREN VAR VAR\n"
    | 9 ->
        "ISO COLON VAR BIARROW LPAREN TYPE\n"
    | 7 ->
        "TYPE VAR EQUAL CTOR OF TYPE\n"
    | 6 ->
        "TYPE VAR EQUAL CTOR TRIANGLE\n"
    | 5 ->
        "TYPE VAR EQUAL PIPE VAR\n"
    | 3 ->
        "TYPE VAR EQUAL VAR\n"
    | 2 ->
        "TYPE VAR VAR\n"
    | 1 ->
        "TYPE TYPE\n"
    | 0 ->
        "TRIANGLE\n"
    | _ ->
        raise Not_found
