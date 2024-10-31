open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

let rec eval_expr env e = match e with
  | Int _ | Bool _ | String _ | Closure _ -> e
  | ID x -> lookup env x
  | Not e1 -> (match eval_expr env e1 with
    | Bool b -> Bool (not b)
    | _ -> raise (TypeError "type error"))
  | Binop (op, e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in (match op with
      | Add | Sub | Mult | Div -> (match v1, v2 with
          | Int i1, Int i2 -> 
            if op = Div && i2 = 0 then 
              raise DivByZeroError
            else Int (match op with
              | Add -> i1 + i2 
              | Sub -> i1 - i2
              | Mult -> i1 * i2
              | Div -> i1 / i2
              | _ -> 0)
          | _ -> raise (TypeError "type error"))
      | Greater | Less | GreaterEqual | LessEqual -> (match v1, v2 with
        | Int i1, Int i2 -> 
          Bool (match op with
            | Greater -> i1 > i2
            | Less -> i1 < i2
            | GreaterEqual -> i1 >= i2
            | LessEqual -> i1 <= i2
            | _ -> false)
        | _ -> raise (TypeError "type error"))
      | Equal | NotEqual -> (match v1, v2 with
        | Closure _, _ | _, Closure _ -> 
          raise (TypeError "type error")
        | _ -> Bool (if op = Equal then v1 = v2 else v1 <> v2))
      | Concat -> (match v1, v2 with
        | String s1, String s2 -> String (s1 ^ s2)
        | _ -> raise (TypeError "type error"))
      | And | Or -> (match v1, v2 with
        | Bool b1, Bool b2 -> 
          Bool (if op = And then b1 && b2 else b1 || b2)
        | _ -> raise (TypeError "type error")))
  | If (cond, e1, e2) -> (match eval_expr env cond with
    | Bool true -> eval_expr env e1
    | Bool false -> eval_expr env e2
    | _ -> raise (TypeError "type error"))
  | Let (x, is_rec, e1, e2) ->
    if is_rec then
      let env' = extend_tmp env x in
      let v1 = eval_expr env' e1 in
      let _ = update env' x v1 in
      eval_expr env' e2
    else
      let v1 = eval_expr env e1 in
      eval_expr (extend env x v1) e2
  | Fun (x, body) -> Closure (env, x, body)
  | App (f, arg) -> (match eval_expr env f with
    | Closure (env', x, body) ->
      let v = eval_expr env arg in
      eval_expr (extend env' x v) body
    | _ -> raise (TypeError "type error"))
  | Record fields ->
    Record (List.map (fun (l, e) -> (l, eval_expr env e)) fields)
  | Select (l, e) -> (match eval_expr env e with
    | Record fields -> (match List.assoc_opt l fields with
      | Some v -> v
      | None -> raise (SelectError "select error"))
    | _ -> raise (TypeError "type error"))

(* Part 2: Evaluating mutop directive *)

let eval_mutop env m = match m with
  | Def (x, e) ->
      let env' = extend_tmp env x in
      let result = eval_expr env' e in
      update env' x result;
      (env', Some result)
  | Expr e ->
      let result = eval_expr env e in
      (env, Some result)
  | NoOp ->
      (env, None)
