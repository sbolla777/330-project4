open Types

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let re_whitespace = Str.regexp "[ \t\n]+"
let re_bool = Str.regexp "true\\|false"
let re_int = Str.regexp "[0-9]+"
let re_string = Str.regexp "\"[^\"]*\""
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
let re_lcurly = Str.regexp "{" 
let re_rcurly = Str.regexp "}"  
let re_dot = Str.regexp "\\."
let re_equal = Str.regexp "="
let re_notequal = Str.regexp "<>"
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_greaterequal = Str.regexp ">="
let re_lessequal = Str.regexp "<="
let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "\\bnot\\b"
let re_if = Str.regexp "\\bif\\b"
let re_then = Str.regexp "\\bthen\\b"
let re_else = Str.regexp "\\belse\\b"  
let re_add = Str.regexp "\\+"
let re_sub = Str.regexp "-"
let re_mult = Str.regexp "\\*"
let re_div = Str.regexp "/"
let re_concat = Str.regexp "\\^"
let re_let = Str.regexp "\\blet\\b"
let re_def = Str.regexp "\\bdef\\b"
let re_in = Str.regexp "\\bin\\b"
let re_rec = Str.regexp "\\brec\\b"
let re_fun = Str.regexp "\\bfun\\b"
let re_arrow = Str.regexp "->"
let re_doublesemi = Str.regexp ";;"
let re_semi = Str.regexp ";"

let tokenize input =
  let rec helper index lst =
    if index >= String.length input then
      List.rev lst
    else if Str.string_match re_whitespace input index then
      helper (index + String.length (Str.matched_string input)) lst
    else if Str.string_match re_if input index then
      helper (index + 2) (Tok_If :: lst)
    else if Str.string_match re_then input index then
      helper (index + 4) (Tok_Then :: lst)
    else if Str.string_match re_else input index then
      helper (index + 4) (Tok_Else :: lst)
    else if Str.string_match re_not input index then
      helper (index + 3) (Tok_Not :: lst)
    else if Str.string_match re_or input index then
      helper (index + 2) (Tok_Or :: lst)
    else if Str.string_match re_and input index then
      helper (index + 2) (Tok_And :: lst)
    else if Str.string_match re_lparen input index then
      let nextindex = index + 1 in
      if nextindex < String.length input && input.[nextindex] = '-' then
        let _ = Str.string_match re_int input (nextindex + 1) in
        let value = int_of_string ("-" ^ Str.matched_string input) in
        helper (nextindex + 1 + String.length (Str.matched_string input)) (Tok_Int value :: lst)
      else
        helper (index + 1) (Tok_LParen :: lst)
    else if Str.string_match re_rparen input index then
      helper (index + 1) (Tok_RParen :: lst)
    else if Str.string_match re_lcurly input index then
      helper (index + 1) (Tok_LCurly :: lst)
    else if Str.string_match re_rcurly input index then
      helper (index + 1) (Tok_RCurly :: lst)
    else if Str.string_match re_dot input index then
      helper (index + 1) (Tok_Dot :: lst)
    else if Str.string_match re_add input index then
      helper (index + 1) (Tok_Add :: lst)
    else if Str.string_match re_arrow input index then
      helper (index + 2) (Tok_Arrow :: lst)
    else if Str.string_match re_sub input index then
      helper (index + 1) (Tok_Sub :: lst)
    else if Str.string_match re_mult input index then
      helper (index + 1) (Tok_Mult :: lst)
    else if Str.string_match re_div input index then
      helper (index + 1) (Tok_Div :: lst)
    else if Str.string_match re_int input index then
      let value = int_of_string (Str.matched_string input) in
      helper (index + String.length (Str.matched_string input)) (Tok_Int value :: lst)
    else if Str.string_match re_bool input index then
      let value = Str.matched_string input = "true" in
      helper (index + String.length (Str.matched_string input)) (Tok_Bool value :: lst)
    else if Str.string_match re_string input index then
      let value = Str.matched_string input in
      if String.length value < 2 then
        raise (InvalidInputException "invalid input")
      else
        let newstring = String.sub value 1 (String.length value - 2) in
        helper (index + String.length value) (Tok_String newstring :: lst)
    else if Str.string_match re_let input index then
      helper (index + 3) (Tok_Let :: lst)
    else if Str.string_match re_rec input index then
      helper (index + 3) (Tok_Rec :: lst)
    else if Str.string_match re_fun input index then
      helper (index + 3) (Tok_Fun :: lst)
    else if Str.string_match re_in input index then
      helper (index + 2) (Tok_In :: lst)
    else if Str.string_match re_def input index then
      helper (index + 3) (Tok_Def :: lst)
    else if Str.string_match re_equal input index then
      helper (index + 1) (Tok_Equal :: lst)
    else if Str.string_match re_notequal input index then
      helper (index + 2) (Tok_NotEqual :: lst)
    else if Str.string_match re_greaterequal input index then
      helper (index + 2) (Tok_GreaterEqual :: lst)
    else if Str.string_match re_lessequal input index then
      helper (index + 2) (Tok_LessEqual :: lst)
    else if Str.string_match re_greater input index then
      helper (index + 1) (Tok_Greater :: lst)
    else if Str.string_match re_less input index then
      helper (index + 1) (Tok_Less :: lst)
    else if Str.string_match re_concat input index then
      helper (index + 1) (Tok_Concat :: lst)
    else if Str.string_match re_doublesemi input index then
      helper (index + 2) (Tok_DoubleSemi :: lst)
    else if Str.string_match re_semi input index then
      helper (index + 1) (Tok_Semi :: lst)
    else if Str.string_match re_id input index then
      let value = Str.matched_string input in
      helper (index + String.length value) (Tok_ID value :: lst)
    else
      raise (InvalidInputException ("invalid input"))
  in
  helper 0 []
