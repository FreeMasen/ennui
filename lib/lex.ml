
(** Test if the provided character is a name character after the first *)
let name_cont ch =
  match ch with
  | ('a'..'z' | 'A'..'Z' | '_' | '0'..'9') -> true
  | _ -> false

(** Test if the provided character is a first name character *)
let name_start ch =
  match ch with
  | ('a'..'z' | 'A'..'Z' | '_') -> true
  | _ -> false

(** Test if the provided character is a decimal digit *)
let dec ch =
  match ch with 
  | ('0' .. '9') -> true
  | _ -> false

(** Test if the provided character is a hexadecimal digit *)
let hex ch =
  match ch with
  | ('0' .. '9' | 'a'..'f' | 'A' .. 'F') -> true
  | _ -> false

(** Append a single character to the end of an existing string *)
let append_char s c =
  s ^ String.make 1 c

(* A multi-line string or comment has been started, count the equal signs between
the opening '[' and then consume the input until the matching end is found  *)
let rec long_bracket_start eq_ct input ctor =
  match input() with
  | Seq.Cons('=', tail) -> long_bracket_start (eq_ct + 1) tail ctor
  | Seq.Cons('[', tail) -> long_bracket_string eq_ct "" tail ctor
  | Seq.Cons(ch, tail) -> 
    let s = ("[" ^ String.make eq_ct '=') in
    let t = Tok.Unknown s in
    let cont = Seq.cons ch tail in
    (t, cont)
  | Seq.Nil -> 
    let s = "[" ^ (String.make eq_ct '=') in
    let t = Tok.Unknown s in
    (t, Seq.empty)
and long_bracket_string eq_ct contents input ctor =
  match input () with
  | Seq.Cons(']', tail) -> long_bracket_end eq_ct 0 contents tail ctor
  | Seq.Cons(ch, tail) -> long_bracket_string eq_ct (append_char contents ch) tail ctor
  | Seq.Nil -> ((Tok.Unknown ("[" ^ String.make eq_ct '=' ^ "[" ^ "..." ^ contents)), input)
and long_bracket_end target_eq eq_ct contents input ctor =
match input() with
| Seq.Cons(']', tail) when target_eq = eq_ct -> (complete contents tail eq_ct ctor) 
| Seq.Cons('=', tail) -> long_bracket_end target_eq (eq_ct+1) contents tail ctor
| Seq.Cons(ch, tail) -> long_bracket_string target_eq (contents ^ "]" ^ (String.make eq_ct '=') ^ (String.make 1 ch)) tail ctor
| Seq.Nil -> ((Tok.Unknown ("[" ^ String.make target_eq '=' ^ "[" ^ "..." ^ contents ^ "]" ^ String.make eq_ct '=')), input)
and complete contents input eq_ct ctor =
  let t = ctor eq_ct contents in
  (t, input)

(** Construct a name or keyword token depending on the strings contents *)
let name_or_kw content =
  match content with
  | "and" -> Tok.Keyword Tok.And
  | "break" -> Tok.Keyword Tok.Break
  | "do" -> Tok.Keyword Tok.Do
  | "else" -> Tok.Keyword Tok.Else
  | "elseif" -> Tok.Keyword Tok.ElseIf
  | "end" -> Tok.Keyword Tok.End
  | "false" -> Tok.Keyword Tok.False
  | "for" -> Tok.Keyword Tok.For
  | "function" -> Tok.Keyword Tok.Function
  | "goto" -> Tok.Keyword Tok.GoTo
  | "if" -> Tok.Keyword Tok.If
  | "in" -> Tok.Keyword Tok.In
  | "local" -> Tok.Keyword Tok.Local
  | "nil" -> Tok.Keyword Tok.Nil
  | "not" -> Tok.Keyword Tok.Not
  | "or" -> Tok.Keyword Tok.Or
  | "repeat" -> Tok.Keyword Tok.Repeat
  | "return" -> Tok.Keyword Tok.Return
  | "then" -> Tok.Keyword Tok.Then
  | "true" -> Tok.Keyword Tok.True
  | "until" -> Tok.Keyword Tok.Until
  | "while" -> Tok.Keyword Tok.While
  | _ -> Tok.Name content

let rec name prefix input =
  match input () with
  | Seq.Cons(ch, tail) when name_cont ch -> name (append_char prefix ch) tail
  | Seq.Cons(_, _) -> (name_or_kw (prefix), input)
  | Seq.Nil -> (name_or_kw prefix, Seq.empty)

let rec tokenize_ input =
  match input () with
  | Seq.Cons('&', tail) ->   punct Tok.Ampersand tail
  | Seq.Cons('|', tail) ->   punct Tok.Pipe tail
  | Seq.Cons('^', tail) ->   punct Tok.Caret tail
  | Seq.Cons('(', tail) ->   punct Tok.OpenParen tail
  | Seq.Cons(')', tail) ->   punct Tok.CloseParen tail
  | Seq.Cons('{', tail) ->   punct Tok.OpenBrace tail
  | Seq.Cons('}', tail) ->   punct Tok.CloseBrace tail
  | Seq.Cons(']', tail) ->   punct Tok.CloseBracket tail
  | Seq.Cons('*', tail) ->   punct Tok.Star tail
  | Seq.Cons('%', tail) ->   punct Tok.Percent tail
  | Seq.Cons('#', tail) ->   punct Tok.Hash tail
  | Seq.Cons(',', tail) ->   punct Tok.Comma tail
  | Seq.Cons('+', tail) ->  punct Tok.Plus tail
  | Seq.Cons(';', tail) ->  punct Tok.SemiColon tail
  | Seq.Cons('[', tail) ->   open_bracket tail
  | Seq.Cons('.', tail) ->   dot tail
  | Seq.Cons('/', tail) ->   slash tail
  | Seq.Cons('>', tail) ->   gt tail
  | Seq.Cons('<', tail) ->   lt tail
  | Seq.Cons('=', tail) ->   eq tail
  | Seq.Cons('~', tail) ->   tilde tail
  | Seq.Cons(':', tail) ->   colon tail
  | Seq.Cons('-', tail) ->  dash tail
  | Seq.Cons((' ' | '\n' | '\t'), tail) -> tokenize_ tail
  | Seq.Cons('\'', tail) -> single_quote tail
  | Seq.Cons('"', tail) -> double_quote tail
  | Seq.Cons(ch, tail) when name_start ch -> ident (String.make 1 ch) tail
  | Seq.Cons('0', tail) -> zero tail
  | Seq.Cons(ch, tail) when dec ch -> dec_num (String.make 1 ch) tail
  | Seq.Cons(ch, tail) -> unknown (String.make 1 ch) tail
  | Seq.Nil -> Seq.empty
and punct p tail =
  next (Tok.Punct p) tail
and unknown s input =
  next (Tok.Unknown s) input
and next tok tail =
  Seq.cons tok (tokenize_ tail)
and open_bracket input =
  match input() with
  | Seq.Cons('[', tail) -> let (t, tail) = long_bracket_string 0 "" tail (fun eq_ct s -> Tok.MultiString (eq_ct, s)) in
    next t tail
  | Seq.Cons('=', tail) -> let (t, tail) = long_bracket_start 1 tail (fun eq_ct s -> Tok.MultiString (eq_ct, s)) in
    next t tail
  | Seq.Cons(_, _) -> punct Tok.OpenBracket input
  | Seq.Nil -> punct Tok.OpenBracket input
and dot input =
  match input () with
  | Seq.Cons('.', tail) -> double_dot tail
  | Seq.Cons(ch, tail) when dec ch -> fract (append_char "." ch) tail
  | _ -> punct Tok.Dot input
and double_dot input =
  match input () with
  | Seq.Cons('.', tail) -> punct Tok.Ellipses tail
  | _ -> punct Tok.DoubleDot input
and slash input =
  match input () with
  | Seq.Cons('/', tail) -> punct Tok.DoubleSlash tail
  | _ -> punct Tok.Slash input
and gt input =
  match input () with
  | Seq.Cons('>', tail) -> punct Tok.DoubleGreater tail
  | Seq.Cons('=', tail) -> punct Tok.GreaterEq tail
  | _ -> punct Tok.Greater input
and lt input =
  match input () with
  | Seq.Cons('<', tail) -> punct Tok.DoubleLess tail
  | Seq.Cons('=', tail) -> punct Tok.LessEq tail
  | _ -> punct Tok.Less input
and eq input =
  match input () with
  | Seq.Cons('=', tail) -> punct Tok.DoubleEq tail
  | _ -> punct Tok.Eq input
and tilde input =
  match input () with
  | Seq.Cons('=', tail) -> punct Tok.TildeEq tail
  | _ -> punct Tok.Tilde input
and colon input =
  match input () with
  | Seq.Cons(':', tail) -> punct Tok.DoubleColon tail
  | _ -> punct Tok.Colon input
and dash input =
  match input () with
  | Seq.Cons('-', tail) -> double_dash tail
  | _ -> punct Tok.Minus input
and ident prefix input =
  let tok,tail = name prefix input in
  next tok tail
and double_dash input =
  match input() with
  | Seq.Cons('[', tail) -> double_dash_open_bracket tail
  | _ -> line_comment "" input
and double_dash_open_bracket input =
  match input() with
  | Seq.Cons('[', tail) -> let (t, tail) = long_bracket_string 0 "" tail (fun eq_ct s -> Tok.MultiComment (eq_ct, s)) in
    next t tail
  | Seq.Cons('=', tail) -> let (t, tail) = long_bracket_start 1 tail (fun eq_ct s -> Tok.MultiComment (eq_ct, s)) in
    next t tail
  | _ -> line_comment "" input
and line_comment contents input =
  match input () with
  | Seq.Cons('\n', tail) -> next (Tok.Comment contents) tail
  | Seq.Cons(ch, tail) -> line_comment (append_char contents ch) tail
  | Seq.Nil -> next (Tok.Comment contents) Seq.empty
and single_quote input =
  match input () with
  | Seq.Cons(ch, tail) -> string_contents '\'' (String.make 1 ch) tail
  | Seq.Nil -> next (Tok.Unknown "'") Seq.empty
and double_quote input =
  match input () with
  | Seq.Cons(ch, tail) -> string_contents '"' (String.make 1 ch) tail
  | Seq.Nil -> next (Tok.Unknown "\"") Seq.empty
and string_contents delim contents input =
  match input () with
  | Seq.Cons(ch, tail) when ch = delim -> next (Tok.LiteralString (delim, contents)) tail
  | Seq.Cons(ch, tail) -> string_contents delim (append_char contents ch) tail
  | Seq.Nil -> next (Tok.Unknown contents) Seq.empty
and zero input =
  match input() with 
  | Seq.Cons(('x' | 'X'), tail) -> hex_num "0x" tail
  | Seq.Cons('.', tail) -> fract "0." tail
  | _ -> next (Tok.Numeral "0") input
and fract content input =
  match input () with
  | Seq.Cons('0'..'9' as ch, tail) -> fract (append_char content ch) tail
  | Seq.Cons(('e' | 'E') as ch, tail) -> dec_exp (append_char content ch) tail
  | _ -> next (Tok.Numeral content) input
and dec_num content input =
  match input () with
  | Seq.Cons(ch, tail) when dec ch -> dec_num (append_char content ch) tail
  | Seq.Cons('.', tail) -> fract (content ^ ".") tail
  | Seq.Cons(('e' | 'E') as ch, tail) -> dec_exp (append_char content ch) tail
  | Seq.Cons(('p' | 'P') as ch, tail) -> hex_exponent (append_char content ch) tail
  | _ -> next (Tok.Numeral content) input
and dec_exp content input =
  match input () with
  | Seq.Cons(('-' | '+') as ch, tail) -> dec_exp_after_sign (content ^ String.make 1 ch) tail
  | Seq.Cons(ch, tail) when dec ch -> dec_exp_after_sign (content ^ String.make 1 ch) tail
  | _ -> next (Tok.Numeral content) input
and dec_exp_after_sign content input =
  match input () with
  | Seq.Cons(ch, tail) when dec ch -> dec_exp_after_sign (append_char content ch) tail
  | _ -> next (Tok.Numeral content) input
and hex_num content input =
  match input() with
  | Seq.Cons(ch, tail) when hex ch -> hex_num (append_char content ch) tail
  | Seq.Cons(('p' | 'P') as ch, tail) -> hex_exponent (append_char content ch) tail
  | _ -> next (Tok.Numeral content) input
and hex_exponent content input =
  match input() with
  | Seq.Cons(('+' | '-') as ch, tail) -> hex_exponent_cont (append_char content ch) tail
  | _ -> next (Tok.Numeral content) input
and hex_exponent_cont content input =
  match input() with
  | Seq.Cons(ch, tail) when hex ch -> hex_exponent_cont (append_char content ch) tail
  | _ -> next (Tok.Numeral content) input

(** Tokenize a `Seq` of `char`s into a `Seq` of `Ennui.Tok.token`s 

The provided `Seq` will be `memoized` before it starts consuming *)
let tokenize input =
    tokenize_ (Seq.memoize input)
