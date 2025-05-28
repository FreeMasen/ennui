let usage_msg = "append <file1>"
let input_file = ref None

let () =
  Arg.parse [] (fun arg -> input_file := Some arg) usage_msg;
let file = In_channel.open_text (Option.get (!input_file)) in 
let read f = 
  match In_channel.input_char f with
  | Some(ch) -> Some (ch, f)
  | None -> Option.none in
let seq = Seq.unfold read file
in Seq.iter (fun tok -> Format.printf "%a\n" Ennui.Tok.pp tok) (Ennui.Lex.tokenize seq)
