
open Tok

(** Tokenize a `Seq` of `char`s into a `Seq` of `Ennui.Tok.token`s 

The provided `Seq` will be `memoized` before it starts consuming *)
let tokenize input =
  let spanned_seq = Lex_spanned.tokenize_spanned input in
  Seq.map (fun spanned_tok -> spanned_tok.tok) spanned_seq
    

(** Tokenize a `Seq` of `char`s into a `Seq` of `Ennui.Tok.token`s but raise a Failure
if an Unknown token is found.

The provided `Seq` will be `memoized` before it starts consuming *)
let tokenize_all input =
  let spanned_seq = Lex_spanned.tokenize_spanned_all input in
  Seq.map (fun spanned_tok -> spanned_tok.tok) spanned_seq
