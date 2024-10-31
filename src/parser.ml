open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = match lookahead toks with
  | Some Tok_Let -> parse_let toks
  | Some Tok_If -> parse_if toks
  | Some Tok_Fun -> parse_fun toks
  | _ -> parse_or toks

and parse_let toks =
  let toks = match_token toks Tok_Let in
  let (isrec, toks) = match lookahead toks with
    | Some Tok_Rec -> (true, match_token toks Tok_Rec)
    | _ -> (false, toks)
  in match lookahead toks with
  | Some (Tok_ID id) ->
      let toks = match_token toks (Tok_ID id) in
      let toks = match_token toks Tok_Equal in
      let (toks, e1) = parse_expr toks in
      let toks = match_token toks Tok_In in
      let (toks, e2) = parse_expr toks in
      (toks, Let (id, isrec, e1, e2))
  | _ -> raise (InvalidInputException "invalid input")

and parse_if toks =
  let toks = match_token toks Tok_If in
  let (toks, cond) = parse_expr toks in
  let toks = match_token toks Tok_Then in
  let (toks, e1) = parse_expr toks in
  let toks = match_token toks Tok_Else in
  let (toks, e2) = parse_expr toks in
  (toks, If (cond, e1, e2))

and parse_fun toks =
  let toks = match_token toks Tok_Fun in match lookahead toks with
  | Some (Tok_ID id) ->
      let toks = match_token toks (Tok_ID id) in
      let toks = match_token toks Tok_Arrow in
      let (toks, body) = parse_expr toks in
      (toks, Fun (id, body))
  | _ -> raise (InvalidInputException "invalid input")

and parse_or toks =
  let (toks, e1) = parse_and toks in match lookahead toks with
  | Some Tok_Or ->
      let toks = match_token toks Tok_Or in
      let (toks, e2) = parse_or toks in
      (toks, Binop (Or, e1, e2))
  | _ -> (toks, e1)

and parse_and toks =
  let (toks, e1) = parse_equality toks in match lookahead toks with
  | Some Tok_And ->
      let toks = match_token toks Tok_And in
      let (toks, e2) = parse_and toks in
      (toks, Binop (And, e1, e2))
  | _ -> (toks, e1)

and parse_equality toks =
  let (toks, e1) = parse_relational toks in match lookahead toks with
  | Some Tok_Equal ->
      let toks = match_token toks Tok_Equal in
      let (toks, e2) = parse_equality toks in
      (toks, Binop (Equal, e1, e2))
  | Some Tok_NotEqual ->
      let toks = match_token toks Tok_NotEqual in
      let (toks, e2) = parse_equality toks in
      (toks, Binop (NotEqual, e1, e2))
  | _ -> (toks, e1)

and parse_relational toks =
  let (toks, e1) = parse_additive toks in
  match lookahead toks with
  | Some Tok_Less ->
      let toks = match_token toks Tok_Less in
      let (toks, e2) = parse_relational toks in
      (toks, Binop (Less, e1, e2))
  | Some Tok_Greater ->
      let toks = match_token toks Tok_Greater in
      let (toks, e2) = parse_relational toks in
      (toks, Binop (Greater, e1, e2))
  | _ -> (toks, e1)

and parse_additive toks =
  let (toks, e1) = parse_multiplicative toks in match lookahead toks with
  | Some Tok_Add ->
      let toks = match_token toks Tok_Add in
      let (toks, e2) = parse_additive toks in
      (toks, Binop (Add, e1, e2))
  | Some Tok_Sub ->
      let toks = match_token toks Tok_Sub in
      let (toks, e2) = parse_additive toks in
      (toks, Binop (Sub, e1, e2))
  | _ -> (toks, e1)

and parse_multiplicative toks =
  let (toks, e1) = parse_concat toks in match lookahead toks with
  | Some Tok_Mult ->
      let toks = match_token toks Tok_Mult in
      let (toks, e2) = parse_multiplicative toks in
      (toks, Binop (Mult, e1, e2))
  | Some Tok_Div ->
      let toks = match_token toks Tok_Div in
      let (toks, e2) = parse_multiplicative toks in
      (toks, Binop (Div, e1, e2))
  | _ -> (toks, e1)

and parse_concat toks =
  let (toks, e1) = parse_unary toks in match lookahead toks with
  | Some Tok_Concat ->
      let toks = match_token toks Tok_Concat in
      let (toks, e2) = parse_concat toks in
      (toks, Binop (Concat, e1, e2))
  | _ -> (toks, e1)

and parse_unary toks = match lookahead toks with
  | Some Tok_Not ->
      let toks = match_token toks Tok_Not in
      let (toks, expr) = parse_unary toks in (toks, Not expr)
  | _ -> parse_app toks

and parse_app toks =
  let (toks, e1) = parse_select toks in
  parse_more_apps toks e1

and parse_more_apps toks e1 = match lookahead toks with
  | Some Tok_LParen -> 
      let toks = match_token toks Tok_LParen in
      let (toks, arg) = parse_expr toks in
      let toks = match_token toks Tok_RParen in
      parse_more_apps toks (App (e1, arg))
  | Some (Tok_ID _ | Tok_Int _ | Tok_Bool _ | Tok_String _) ->
      let (toks, e2) = parse_select toks in
      parse_more_apps toks (App (e1, e2))
  | _ -> (toks, e1)

and parse_select toks =
  let (toks, expr) = parse_primary toks in
  match lookahead toks with
  | Some Tok_Dot ->
      let toks = match_token toks Tok_Dot in
      let (id, toks) = match lookahead toks with
        | Some (Tok_ID id) -> (id, match_token toks (Tok_ID id))
        | _ -> raise (InvalidInputException "invalid input")
      in (toks, Select (Lab id, expr))
  | _ -> (toks, expr)

and parse_primary toks = match lookahead toks with
  | Some (Tok_Int n) -> (match_token toks (Tok_Int n), Int n)
  | Some (Tok_Bool b) -> (match_token toks (Tok_Bool b), Bool b)
  | Some (Tok_String s) -> (match_token toks (Tok_String s), String s)
  | Some (Tok_ID id) -> (match_token toks (Tok_ID id), ID id)
  | Some Tok_LParen ->
      let toks = match_token toks Tok_LParen in
      let (toks, expr) = parse_expr toks in
      let toks = match_token toks Tok_RParen in (toks, expr)
  | Some Tok_LCurly -> parse_record toks
  | _ -> raise (InvalidInputException "invalid input")

and parse_record toks =
  let toks = match_token toks Tok_LCurly in match lookahead toks with
  | Some Tok_RCurly -> (match_token toks Tok_RCurly, Record [])
  | _ ->
      let (toks, fields) = parse_record_body toks in
      let toks = match_token toks Tok_RCurly in
      (toks, Record fields)

and parse_record_body toks =
  let (id, toks) = match lookahead toks with
    | Some (Tok_ID id) -> (id, match_token toks (Tok_ID id))
    | _ -> raise (InvalidInputException "invalid input")
  in
  let toks = match_token toks Tok_Equal in
  let (toks, expr) = parse_expr toks in match lookahead toks with
  | Some Tok_Semi ->
      let toks = match_token toks Tok_Semi in
      let (toks, rest) = parse_record_body toks in
      (toks, (Lab id, expr) :: rest)
  | _ -> (toks, [(Lab id, expr)])

(* Part 3: Parsing mutop *)

let rec parse_mutop toks =
  match lookahead toks with
  | Some Tok_Def -> parse_def toks
  | Some Tok_DoubleSemi -> (match_token toks Tok_DoubleSemi, NoOp)
  | _ ->
      let (toks, expr) = parse_expr toks in
      let toks = match_token toks Tok_DoubleSemi in (toks, Expr expr)

and parse_def toks =
  let toks = match_token toks Tok_Def in
  let (id, toks) = match lookahead toks with
    | Some (Tok_ID id) -> (id, match_token toks (Tok_ID id))
    | _ -> raise (InvalidInputException "invalid input")
  in
  let toks = match_token toks Tok_Equal in
  let (toks, expr) = parse_expr toks in
  let toks = match_token toks Tok_DoubleSemi in
  (toks, Def (id, expr))
