open Tok

(** Test if the provided character is a name character after the first *)
let name_cont ch =
  match ch with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
  | _ -> false

(** Test if the provided character is a first name character *)
let name_start ch =
  match ch with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

(** Test if the provided character is a decimal digit *)
let dec ch = match ch with '0' .. '9' -> true | _ -> false

(** Test if the provided character is a hexadecimal digit *)
let hex ch =
  match ch with '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

(** Append a single character to the end of an existing string *)
let append_char s c = s ^ String.make 1 c

let make_spanned_token start_idx end_idx tok = { start_idx; end_idx; tok }

(* A multi-line string or comment has been started, count the equal signs between
  the opening '[' and then consume the input until the matching end is found  *)
let rec long_bracket_start start_idx eq_ct input ctor =
  match input () with
  | Seq.Cons ((_, '='), tail) ->
      long_bracket_start start_idx (eq_ct + 1) tail ctor
  | Seq.Cons ((_, '['), tail) ->
      long_bracket_string start_idx eq_ct "" tail ctor
  | Seq.Cons ((end_idx, _), _) ->
      let s = "[" ^ String.make eq_ct '=' in
      let t = Tok.Unknown s in
      (make_spanned_token start_idx end_idx t, input)
  | Seq.Nil ->
      let s = "[" ^ String.make eq_ct '=' in
      let t = Tok.Unknown s in
      let len = String.length s in
      let end_idx = start_idx + len in
      (make_spanned_token start_idx end_idx t, Seq.empty)

and long_bracket_string start_idx eq_ct contents input ctor =
  match input () with
  | Seq.Cons ((_, ']'), tail) ->
      long_bracket_end start_idx eq_ct 0 contents tail ctor
  | Seq.Cons ((_, ch), tail) ->
      long_bracket_string start_idx eq_ct (append_char contents ch) tail ctor
  | Seq.Nil ->
      let s = "[" ^ String.make eq_ct '=' ^ "[" ^ "..." ^ contents in
      let len = String.length s in
      let end_idx = start_idx + len in
      let t = Tok.Unknown s in
      (make_spanned_token start_idx end_idx t, input)

and long_bracket_end start_idx target_eq eq_ct contents input ctor =
  match input () with
  | Seq.Cons ((end_idx, ']'), tail) when target_eq = eq_ct ->
      complete start_idx (end_idx + 1) contents tail eq_ct ctor
  | Seq.Cons ((_, '='), tail) ->
      long_bracket_end start_idx target_eq (eq_ct + 1) contents tail ctor
  | Seq.Cons ((_, ch), tail) ->
      let s = contents ^ "]" ^ String.make eq_ct '=' ^ String.make 1 ch in
      long_bracket_string start_idx target_eq s tail ctor
  | Seq.Nil ->
      let s =
        "[" ^ String.make target_eq '=' ^ "[" ^ "..." ^ contents ^ "]"
        ^ String.make eq_ct '='
      in
      let len = String.length s in
      let end_idx = start_idx + len in
      let t = Tok.Unknown s in
      (make_spanned_token start_idx end_idx t, input)

and complete start_idx end_idx contents input eq_ct ctor =
  let t = ctor eq_ct contents in
  (make_spanned_token start_idx end_idx t, input)

(** Construct a name or keyword token depending on the strings contents *)
let name_or_kw start_idx end_idx content =
  match content with
  | "and" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.And)
  | "break" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.Break)
  | "do" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.Do)
  | "else" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.Else)
  | "elseif" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.ElseIf)
  | "end" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.End)
  | "false" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.False)
  | "for" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.For)
  | "function" ->
      make_spanned_token start_idx end_idx (Tok.Keyword Tok.Function)
  | "goto" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.GoTo)
  | "if" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.If)
  | "in" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.In)
  | "local" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.Local)
  | "nil" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.Nil)
  | "not" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.Not)
  | "or" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.Or)
  | "repeat" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.Repeat)
  | "return" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.Return)
  | "then" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.Then)
  | "true" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.True)
  | "until" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.Until)
  | "while" -> make_spanned_token start_idx end_idx (Tok.Keyword Tok.While)
  | _ -> make_spanned_token start_idx end_idx (Tok.Name content)

let rec name start_idx prefix input =
  match input () with
  | Seq.Cons ((_, ch), tail) when name_cont ch ->
      name start_idx (append_char prefix ch) tail
  | Seq.Cons ((end_idx, _), _) -> (name_or_kw start_idx end_idx prefix, input)
  | Seq.Nil ->
      (name_or_kw start_idx (start_idx + String.length prefix) prefix, Seq.empty)

let rec tokenize_ input =
  match input () with
  | Seq.Cons ((idx, '&'), tail) -> punct idx (idx + 1) Tok.Ampersand tail
  | Seq.Cons ((idx, '|'), tail) -> punct idx (idx + 1) Tok.Pipe tail
  | Seq.Cons ((idx, '^'), tail) -> punct idx (idx + 1) Tok.Caret tail
  | Seq.Cons ((idx, '('), tail) -> punct idx (idx + 1) Tok.OpenParen tail
  | Seq.Cons ((idx, ')'), tail) -> punct idx (idx + 1) Tok.CloseParen tail
  | Seq.Cons ((idx, '{'), tail) -> punct idx (idx + 1) Tok.OpenBrace tail
  | Seq.Cons ((idx, '}'), tail) -> punct idx (idx + 1) Tok.CloseBrace tail
  | Seq.Cons ((idx, ']'), tail) -> punct idx (idx + 1) Tok.CloseBracket tail
  | Seq.Cons ((idx, '*'), tail) -> punct idx (idx + 1) Tok.Star tail
  | Seq.Cons ((idx, '%'), tail) -> punct idx (idx + 1) Tok.Percent tail
  | Seq.Cons ((idx, '#'), tail) -> punct idx (idx + 1) Tok.Hash tail
  | Seq.Cons ((idx, ','), tail) -> punct idx (idx + 1) Tok.Comma tail
  | Seq.Cons ((idx, '+'), tail) -> punct idx (idx + 1) Tok.Plus tail
  | Seq.Cons ((idx, ';'), tail) -> punct idx (idx + 1) Tok.SemiColon tail
  | Seq.Cons ((idx, '['), tail) -> open_bracket idx tail
  | Seq.Cons ((idx, '.'), tail) -> dot idx tail
  | Seq.Cons ((idx, '/'), tail) -> slash idx tail
  | Seq.Cons ((idx, '>'), tail) -> gt idx tail
  | Seq.Cons ((idx, '<'), tail) -> lt idx tail
  | Seq.Cons ((idx, '='), tail) -> eq idx tail
  | Seq.Cons ((idx, '~'), tail) -> tilde idx tail
  | Seq.Cons ((idx, ':'), tail) -> colon idx tail
  | Seq.Cons ((idx, '-'), tail) -> dash idx tail
  | Seq.Cons ((_, (' ' | '\n' | '\t')), tail) -> tokenize_ tail
  | Seq.Cons ((idx, '\''), tail) -> single_quote idx tail
  | Seq.Cons ((idx, '"'), tail) -> double_quote idx tail
  | Seq.Cons ((idx, ch), tail) when name_start ch ->
      ident idx (String.make 1 ch) tail
  | Seq.Cons ((idx, '0'), tail) -> zero idx tail
  | Seq.Cons ((idx, ch), tail) when dec ch ->
      dec_num idx (String.make 1 ch) tail
  | Seq.Cons ((idx, ch), tail) -> unknown idx (idx + 1) (String.make 1 ch) tail
  | Seq.Nil -> Seq.empty

and punct start_idx end_idx p tail =
  next (make_spanned_token start_idx end_idx (Tok.Punct p)) tail

and unknown start_idx end_idx s input =
  next (make_spanned_token start_idx end_idx (Tok.Unknown s)) input

and next tok tail = Seq.cons tok (tokenize_ tail)

and open_bracket start_idx input =
  match input () with
  | Seq.Cons ((_, '['), tail) ->
      let t, tail =
        long_bracket_string start_idx 0 "" tail (fun eq_ct s ->
            Tok.MultiString (eq_ct, s))
      in
      next t tail
  | Seq.Cons ((_, '='), tail) ->
      let t, tail =
        long_bracket_start start_idx 1 tail (fun eq_ct s ->
            Tok.MultiString (eq_ct, s))
      in
      next t tail
  | Seq.Cons ((idx, _), _) -> punct start_idx idx Tok.OpenBracket input
  | Seq.Nil -> punct start_idx (start_idx + 1) Tok.OpenBracket input

and dot start_idx input =
  match input () with
  | Seq.Cons ((_, '.'), tail) -> double_dot start_idx tail
  | Seq.Cons ((_, ch), tail) when dec ch ->
      fract start_idx (append_char "." ch) tail
  | _ -> punct start_idx (start_idx + 1) Tok.Dot input

and double_dot start_idx input =
  match input () with
  | Seq.Cons ((idx, '.'), tail) -> punct start_idx (idx + 1) Tok.Ellipses tail
  | _ -> punct start_idx (start_idx + 1) Tok.DoubleDot input

and slash start_idx input =
  match input () with
  | Seq.Cons ((idx, '/'), tail) ->
      punct start_idx (idx + 1) Tok.DoubleSlash tail
  | _ -> punct start_idx (start_idx + 1) Tok.Slash input

and gt start_idx input =
  match input () with
  | Seq.Cons ((idx, '>'), tail) ->
      punct start_idx (idx + 1) Tok.DoubleGreater tail
  | Seq.Cons ((idx, '='), tail) -> punct start_idx (idx + 1) Tok.GreaterEq tail
  | _ -> punct start_idx (start_idx + 1) Tok.Greater input

and lt start_idx input =
  match input () with
  | Seq.Cons ((idx, '<'), tail) -> punct start_idx (idx + 1) Tok.DoubleLess tail
  | Seq.Cons ((idx, '='), tail) -> punct start_idx (idx + 1) Tok.LessEq tail
  | _ -> punct start_idx (start_idx + 1) Tok.Less input

and eq start_idx input =
  match input () with
  | Seq.Cons ((idx, '='), tail) -> punct start_idx (idx + 1) Tok.DoubleEq tail
  | _ -> punct start_idx (start_idx + 1) Tok.Eq input

and tilde start_idx input =
  match input () with
  | Seq.Cons ((idx, '='), tail) -> punct start_idx (idx + 1) Tok.TildeEq tail
  | _ -> punct start_idx (start_idx + 1) Tok.Tilde input

and colon start_idx input =
  match input () with
  | Seq.Cons ((idx, ':'), tail) ->
      punct start_idx (idx + 1) Tok.DoubleColon tail
  | _ -> punct start_idx (start_idx + 1) Tok.Colon input

and dash start_idx input =
  match input () with
  | Seq.Cons ((_, '-'), tail) -> double_dash start_idx tail
  | _ -> punct start_idx (start_idx + 1) Tok.Minus input

and ident start_idx prefix input =
  let tok, tail = name start_idx prefix input in
  next tok tail

and double_dash start_idx input =
  match input () with
  | Seq.Cons ((_, '['), tail) -> double_dash_open_bracket start_idx tail
  | _ -> line_comment start_idx "" input

and double_dash_open_bracket start_idx input =
  match input () with
  | Seq.Cons ((_, '['), tail) ->
      let t, tail =
        long_bracket_string start_idx 0 "" tail (fun eq_ct s ->
            Tok.MultiComment (eq_ct, s))
      in
      next t tail
  | Seq.Cons ((_, '='), tail) ->
      let t, tail =
        long_bracket_start start_idx 1 tail (fun eq_ct s ->
            Tok.MultiComment (eq_ct, s))
      in
      next t tail
  | _ -> line_comment start_idx "" input

and line_comment start_idx contents input =
  match input () with
  | Seq.Cons ((idx, '\n'), tail) ->
      next (make_spanned_token start_idx (idx + 1) (Tok.Comment contents)) tail
  | Seq.Cons ((_, ch), tail) ->
      line_comment start_idx (append_char contents ch) tail
  | Seq.Nil ->
      let len = String.length contents in
      let end_idx = start_idx + len in
      let t = Tok.Comment contents in
      next (make_spanned_token start_idx end_idx t) Seq.empty

and single_quote start_idx input =
  match input () with
  | Seq.Cons ((_, ch), tail) ->
      string_contents start_idx '\'' (String.make 1 ch) tail
  | Seq.Nil ->
      let t = Tok.Unknown "'" in
      next (make_spanned_token start_idx (start_idx + 1) t) Seq.empty

and double_quote start_idx input =
  match input () with
  | Seq.Cons ((_, ch), tail) ->
      string_contents start_idx '"' (String.make 1 ch) tail
  | Seq.Nil ->
      let t = Tok.Unknown "\"" in
      next (make_spanned_token start_idx (start_idx + 1) t) Seq.empty

and string_contents start_idx delim contents input =
  match input () with
  | Seq.Cons ((_, '\\'), tail) -> escape start_idx delim contents tail
  | Seq.Cons ((idx, ch), tail) when ch = delim ->
      let t = Tok.LiteralString (delim, contents) in
      next (make_spanned_token start_idx (idx + 1) t) tail
  | Seq.Cons ((_, ch), tail) ->
      string_contents start_idx delim (append_char contents ch) tail
  | Seq.Nil ->
      let t = Tok.Unknown contents in
      let len = String.length contents in
      let end_idx = start_idx + len in
      next (make_spanned_token start_idx end_idx t) Seq.empty

and escape start_idx delim contents input =
  match input () with
  | Seq.Cons ((_, ch), tail) ->
      let with_char = append_char contents ch in
      string_contents start_idx delim with_char tail
  | Seq.Nil ->
      let len = String.length contents in
      let end_idx = start_idx + len in
      let t = Tok.Unknown contents in
      next (make_spanned_token start_idx end_idx t) Seq.empty

and zero start_idx input =
  match input () with
  | Seq.Cons ((_, ('x' | 'X')), tail) -> hex_num start_idx "0x" tail
  | Seq.Cons ((_, '.'), tail) -> fract start_idx "0." tail
  | Seq.Cons((_, ('e' | 'E' as ch)), tail) -> dec_exp start_idx (append_char "0" ch) tail
  | _ ->
      let t = Tok.Numeral "0" in
      next (make_spanned_token start_idx (start_idx + 1) t) input

and fract start_idx content input =
  match input () with
  | Seq.Cons ((_, ('0' .. '9' as ch)), tail) ->
      fract start_idx (append_char content ch) tail
  | Seq.Cons ((_, (('e' | 'E') as ch)), tail) ->
      dec_exp start_idx (append_char content ch) tail
  | _ ->
      let t = Tok.Numeral content in
      let len = String.length content in
      let end_idx = start_idx + len in
      next (make_spanned_token start_idx end_idx t) input

and dec_num start_idx content input =
  match input () with
  | Seq.Cons ((_, ch), tail) when dec ch ->
      dec_num start_idx (append_char content ch) tail
  | Seq.Cons ((_, '.'), tail) -> fract start_idx (content ^ ".") tail
  | Seq.Cons ((_, (('e' | 'E') as ch)), tail) ->
      dec_exp start_idx (append_char content ch) tail
  | Seq.Cons ((_, (('p' | 'P') as ch)), tail) ->
      hex_exponent start_idx (append_char content ch) tail
  | _ ->
      let t = Tok.Numeral content in
      let len = String.length content in
      let end_idx = start_idx + len in
      next (make_spanned_token start_idx end_idx t) input

and dec_exp start_idx content input =
  match input () with
  | Seq.Cons ((_, (('-' | '+') as ch)), tail) ->
      dec_exp_after_sign start_idx (content ^ String.make 1 ch) tail
  | Seq.Cons ((_, ch), tail) when dec ch ->
      dec_exp_after_sign start_idx (content ^ String.make 1 ch) tail
  | _ ->
      let t = Tok.Numeral content in
      let len = String.length content in
      let end_idx = start_idx + len in
      next (make_spanned_token start_idx end_idx t) input

and dec_exp_after_sign start_idx content input =
  match input () with
  | Seq.Cons ((_, ch), tail) when dec ch ->
      dec_exp_after_sign start_idx (append_char content ch) tail
  | _ ->
      let t = Tok.Numeral content in
      let len = String.length content in
      let end_idx = start_idx + len in
      next (make_spanned_token start_idx end_idx t) input

and hex_num start_idx content input =
  match input () with
  | Seq.Cons ((_, ch), tail) when hex ch ->
      hex_num start_idx (append_char content ch) tail
  | Seq.Cons ((_, '.'), tail) -> hex_fract start_idx (append_char content '.') tail
  | Seq.Cons ((_, (('p' | 'P') as ch)), tail) ->
      hex_exponent start_idx (append_char content ch) tail
  | _ ->
      let t = Tok.Numeral content in
      let len = String.length content in
      let end_idx = start_idx + len in
      next (make_spanned_token start_idx end_idx t) input
and hex_fract start_idx content input =
  match input () with
  | Seq.Cons ((_, ch), tail) when hex ch ->
    hex_num start_idx (append_char content ch) tail
  | Seq.Cons ((_, (('p' | 'P') as ch)), tail) ->
    hex_exponent start_idx (append_char content ch) tail
  | _ ->
    let t = Tok.Numeral content in
    let len = String.length content in
    let end_idx = start_idx + len in
    next (make_spanned_token start_idx end_idx t) input
and hex_exponent start_idx content input =
  match input () with
  | Seq.Cons ((_, (('+' | '-') as ch)), tail) ->
      hex_exponent_cont start_idx (append_char content ch) tail
  | _ ->
      let t = Tok.Numeral content in
      let len = String.length content in
      let end_idx = start_idx + len in
      next (make_spanned_token start_idx end_idx t) input

and hex_exponent_cont start_idx content input =
  match input () with
  | Seq.Cons ((_, ch), tail) when hex ch ->
      hex_exponent_cont start_idx (append_char content ch) tail
  | _ ->
      let t = Tok.Numeral content in
      let len = String.length content in
      let end_idx = start_idx + len in
      next (make_spanned_token start_idx end_idx t) input

let tokenize_spanned_ input =
  let with_i = Seq.mapi (fun i c -> (i, c)) input in
  tokenize_ (Seq.memoize with_i)

(** Tokenize a `Seq` of `char`s into a `Seq` of `Ennui.Tok.token`s

    The provided `Seq` will be `memoized` before it starts consuming *)
let tokenize_spanned input = tokenize_spanned_ input

(** Tokenize a `Seq` of `char`s into a `Seq` of `Ennui.Tok.token`s but raise a
    Failure if an Unknown token is found.

    The provided `Seq` will be `memoized` before it starts consuming *)
let tokenize_spanned_all input =
  Seq.map
    (fun tok ->
      match tok.tok with
      | Tok.Unknown contents ->
          raise
            (Failure
               (Format.sprintf "Found Unknown token: %S (%i:%i)" contents
                  tok.start_idx tok.end_idx))
      | _ -> tok)
    (tokenize_spanned input)
