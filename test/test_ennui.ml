open OUnit2

let line_comment = "--single line comment\n"
let multi_line_comment_simple = "--[[
multi line comment
]]"
let multi_line_comment_escaped = "--[=[
[[multi line comment]]
]=]"

let tok_list_to_string lst =
  (List.fold_left (fun acc tok -> acc ^ (Ennui.Tok.to_string tok) ^ ", ") "[" lst) ^ "]"

let assert_lex target text =
  assert_equal 
    ~printer: (fun lst -> tok_list_to_string lst)
    target (List.of_seq (Ennui.Lex.tokenize (String.to_seq text)))

let test_line_comment _ =
  assert_lex [(Ennui.Tok.Comment "single line comment")] line_comment

let test_multi_line_comment_simple _ =
  assert_lex [Ennui.Tok.MultiComment (0, "\nmulti line comment\n")] multi_line_comment_simple 

let test_multi_line_comment_escaped _ =
  assert_lex [Ennui.Tok.MultiComment (1, "\n[[multi line comment]]\n")] multi_line_comment_escaped


let all_puncts = "& ^ } ] ) : , . :: .. == >> << // ... = > >= # < <= - { [ ( % | + ; / * ~ ~="
let puncts _ =
  assert_lex [
    Ennui.Tok.Punct Ennui.Tok.Ampersand;
    Ennui.Tok.Punct Ennui.Tok.Caret;
    Ennui.Tok.Punct Ennui.Tok.CloseBrace;
    Ennui.Tok.Punct Ennui.Tok.CloseBracket;
    Ennui.Tok.Punct Ennui.Tok.CloseParen;
    Ennui.Tok.Punct Ennui.Tok.Colon;
    Ennui.Tok.Punct Ennui.Tok.Comma;
    Ennui.Tok.Punct Ennui.Tok.Dot;
    Ennui.Tok.Punct Ennui.Tok.DoubleColon;
    Ennui.Tok.Punct Ennui.Tok.DoubleDot;
    Ennui.Tok.Punct Ennui.Tok.DoubleEq;
    Ennui.Tok.Punct Ennui.Tok.DoubleGreater;
    Ennui.Tok.Punct Ennui.Tok.DoubleLess;
    Ennui.Tok.Punct Ennui.Tok.DoubleSlash;
    Ennui.Tok.Punct Ennui.Tok.Ellipses;
    Ennui.Tok.Punct Ennui.Tok.Eq;
    Ennui.Tok.Punct Ennui.Tok.Greater;
    Ennui.Tok.Punct Ennui.Tok.GreaterEq;
    Ennui.Tok.Punct Ennui.Tok.Hash;
    Ennui.Tok.Punct Ennui.Tok.Less;
    Ennui.Tok.Punct Ennui.Tok.LessEq;
    Ennui.Tok.Punct Ennui.Tok.Minus;
    Ennui.Tok.Punct Ennui.Tok.OpenBrace;
    Ennui.Tok.Punct Ennui.Tok.OpenBracket;
    Ennui.Tok.Punct Ennui.Tok.OpenParen;
    Ennui.Tok.Punct Ennui.Tok.Percent;
    Ennui.Tok.Punct Ennui.Tok.Pipe;
    Ennui.Tok.Punct Ennui.Tok.Plus;
    Ennui.Tok.Punct Ennui.Tok.SemiColon;
    Ennui.Tok.Punct Ennui.Tok.Slash;
    Ennui.Tok.Punct Ennui.Tok.Star;
    Ennui.Tok.Punct Ennui.Tok.Tilde;
    Ennui.Tok.Punct Ennui.Tok.TildeEq;
  ] all_puncts

let all_keywords = "and break do else elseif end false for function goto if in local nil not or repeat return then true until while"
let keywords _ =
  assert_lex
    [
      Ennui.Tok.Keyword Ennui.Tok.And;
      Ennui.Tok.Keyword Ennui.Tok.Break;
      Ennui.Tok.Keyword Ennui.Tok.Do;
      Ennui.Tok.Keyword Ennui.Tok.Else;
      Ennui.Tok.Keyword Ennui.Tok.ElseIf;
      Ennui.Tok.Keyword Ennui.Tok.End;
      Ennui.Tok.Keyword Ennui.Tok.False;
      Ennui.Tok.Keyword Ennui.Tok.For;
      Ennui.Tok.Keyword Ennui.Tok.Function;
      Ennui.Tok.Keyword Ennui.Tok.GoTo;
      Ennui.Tok.Keyword Ennui.Tok.If;
      Ennui.Tok.Keyword Ennui.Tok.In;
      Ennui.Tok.Keyword Ennui.Tok.Local;
      Ennui.Tok.Keyword Ennui.Tok.Nil;
      Ennui.Tok.Keyword Ennui.Tok.Not;
      Ennui.Tok.Keyword Ennui.Tok.Or;
      Ennui.Tok.Keyword Ennui.Tok.Repeat;
      Ennui.Tok.Keyword Ennui.Tok.Return;
      Ennui.Tok.Keyword Ennui.Tok.Then;
      Ennui.Tok.Keyword Ennui.Tok.True;
      Ennui.Tok.Keyword Ennui.Tok.Until;
      Ennui.Tok.Keyword Ennui.Tok.While;
    ]
    all_keywords


let some_idents = "a z _ A1 A_Z"
let idents _ =
  assert_lex [
    Ennui.Tok.Name "a";
    Ennui.Tok.Name "z";
    Ennui.Tok.Name "_";
    Ennui.Tok.Name "A1";
    Ennui.Tok.Name "A_Z";
  ] some_idents

let some_strings = {|"double quoted" "escaped \" double quoted" 'single quoted' 'escaped \' single quoted' [[
multi line
]] [=[
escaped multi line]]]=]
[===[
[]
[[]]
[=[]=]
[==[]==]
]===]
|}

let strings _ =
  assert_lex [
  Ennui.Tok.LiteralString ('"', "double quoted");
  Ennui.Tok.LiteralString ('"', "escaped \" double quoted");
  Ennui.Tok.LiteralString ('\'', "single quoted");
  Ennui.Tok.LiteralString ('\'', "escaped ' single quoted");
  Ennui.Tok.MultiString (0, "\nmulti line\n");
  Ennui.Tok.MultiString (1, "\nescaped multi line]]");
  Ennui.Tok.MultiString (3, "
[]
[[]]
[=[]=]
[==[]==]
");
  ] some_strings

let some_numbers = "0 0.1 .1 0e1 0E1 0E+1 0E-1 0x0 0xef 0xEF 0xEp+F 0xEp-F 0xep+f
0x0.ef1"

let numbers _ =
  assert_lex
  [
    Ennui.Tok.Numeral "0";
    Ennui.Tok.Numeral "0.1";
    Ennui.Tok.Numeral ".1";
    Ennui.Tok.Numeral "0e1";
    Ennui.Tok.Numeral "0E1";
    Ennui.Tok.Numeral "0E+1";
    Ennui.Tok.Numeral "0E-1";
    Ennui.Tok.Numeral "0x0";
    Ennui.Tok.Numeral "0xef";
    Ennui.Tok.Numeral "0xEF";
    Ennui.Tok.Numeral "0xEp+F";
    Ennui.Tok.Numeral "0xEp-F";
    Ennui.Tok.Numeral "0xep+f";
    Ennui.Tok.Numeral "0x0.ef1";
  ] some_numbers
let suite =
   "TokenTests" >::: [
     "test_line_comment" >:: test_line_comment;
     "test_multi_line_comment_simple" >:: test_multi_line_comment_simple;
     "test_multi_line_comment_escaped" >:: test_multi_line_comment_escaped;
     "puncts" >:: puncts;
     "keywords" >:: keywords;
     "idents" >:: idents;
     "strings" >:: strings;
     "numbers" >:: numbers;
   ]

let () =
   run_test_tt_main suite
