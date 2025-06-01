(** A discrete punctuation mark *)
type punct = 
  | Ampersand
  | Caret
  | CloseBrace
  | CloseBracket
  | CloseParen
  | Colon
  | Comma
  | Dot
  | DoubleColon
  | DoubleDot
  | DoubleEq
  | DoubleGreater
  | DoubleLess
  | DoubleSlash
  | Ellipses
  | Eq
  | Greater
  | GreaterEq
  | Hash
  | Less
  | LessEq
  | Minus
  | OpenBrace
  | OpenBracket
  | OpenParen
  | Percent
  | Pipe
  | Plus
  | SemiColon
  | Slash
  | Star
  | Tilde
  | TildeEq
(** Reserved identifiers *)
type keyword = 
  | And
  | Break
  | Do
  | Else
  | ElseIf
  | End
  | False
  | For
  | Function
  | GoTo
  | If
  | In
  | Local
  | Nil
  | Not
  | Or
  | Repeat
  | Return
  | Then
  | True
  | Until
  | While

(** All tokens *)
type token =
  | Punct of punct
  | Keyword of keyword
  | Name of string
  | LiteralString of (char * string)
  | MultiString of (int * string)
  | Comment of string
  | MultiComment of (int * string)
  | Numeral of string
  | Unknown of string

let pp_punct ppf p =
  Format.fprintf ppf "%s" (match p with
  | Plus -> "Punct->Plus"
  | Minus -> "Punct->Minus"
  | Hash -> "Punct->Hash"
  | Star -> "Punct->Star"
  | Slash -> "Punct->Slash"
  | DoubleSlash -> "Punct->DoubleSlash"
  | Caret -> "Punct->Caret"
  | Percent -> "Punct->Percent"
  | Ampersand -> "Punct->Ampersand"
  | Tilde -> "Punct->Tilde"
  | Pipe -> "Punct->Pipe"
  | Greater -> "Punct->Greater"
  | DoubleGreater -> "Punct->DoubleGreater"
  | Less -> "Punct->Less"
  | DoubleLess -> "Punct->DoubleLess"
  | Dot -> "Punct->Dot"
  | DoubleDot -> "Punct->DoubleDot"
  | Eq -> "Punct->Eq"
  | LessEq -> "Punct->LessEq"
  | GreaterEq -> "Punct->GreaterEq"
  | DoubleEq -> "Punct->DoubleEq"
  | TildeEq -> "Punct->TildeEq"
  | Comma -> "Punct->Comma"
  | SemiColon -> "Punct->SemiColon"
  | OpenBracket -> "Punct->OpenBracket"
  | CloseBracket -> "Punct->CloseBracket"
  | OpenBrace -> "Punct->OpenBrace"
  | CloseBrace -> "Punct->CloseBrace"
  | OpenParen -> "Punct->OpenParen"
  | CloseParen -> "Punct->CloseParen"
  | Colon -> "Punct->Colon"
  | DoubleColon -> "Punct->DoubleColon"
  | Ellipses -> "Punct->Ellipses")
let pp_kw ppf kw =
  Format.fprintf ppf "%s" (match kw with
  | And -> "Keyword->And"
  | Or -> "Keyword->Or"
  | Not -> "Keyword->Not"
  | Function -> "Keyword->Function"
  | End -> "Keyword->End"
  | Nil -> "Keyword->Nil"
  | False -> "Keyword->False"
  | True -> "Keyword->True"
  | Return -> "Keyword->Return"
  | Local -> "Keyword->Local"
  | For -> "Keyword->For"
  | While -> "Keyword->While"
  | Repeat -> "Keyword->Repeat"
  | Until -> "Keyword->Until"
  | If -> "Keyword->If"
  | Else -> "Keyword->Else"
  | ElseIf -> "Keyword->ElseIf"
  | Do -> "Keyword->Do"
  | Then -> "Keyword->Then"
  | In -> "Keyword->In"
  | GoTo -> "Keyword->GoTo"
  | Break -> "Keyword->Break")

let pp ppf tok =
  match tok with
  | Punct p -> pp_punct ppf p
  | Keyword kw -> pp_kw ppf kw
  | Comment c -> Format.fprintf ppf "Comment(%S)" c
  | MultiComment(eq, c) -> Format.fprintf ppf "Comment(--[%s[,%S)" (String.make eq '=') c
  | LiteralString(q, s) -> Format.fprintf ppf "String(%C, %S)" q s
  | MultiString(eq, s) -> Format.fprintf ppf "String([%s[,%S)" (String.make eq '=') s
  | Name n -> Format.fprintf ppf "Name(%S)" n
  | Numeral n -> Format.fprintf ppf "Numeral(%S)" n
  | Unknown v -> Format.fprintf ppf "Unknown(%S)" v

let to_string tok =
  Format.asprintf "%a" pp tok

type spanned_token = { start_idx : int; end_idx : int; tok : token }
