open Ennui.Tok
let usage_msg = "append <file1>"
let input_file = ref None

let () =
  Arg.parse [] (fun arg -> input_file := Some arg) usage_msg;

let file = match !input_file with
  | Some path -> In_channel.open_text path
  | None -> In_channel.stdin in
let read f = 
  match In_channel.input_char f with
  | Some(ch) -> Some (ch, f)
  | None -> Option.none in
let seq = Seq.unfold read file
in Seq.iter (fun tok -> 
  let () = Format.printf "% 5i..%i " tok.start_idx tok.end_idx in
  Format.printf "%a\n" Ennui.Tok.pp tok.tok)
  (Ennui.Lex_spanned.tokenize_spanned_all seq)
