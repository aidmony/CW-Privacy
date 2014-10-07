type value = 
    | Num of int
    | Bool of bool
    | Err of string

type expr = 
    | Var of string
    | Val of value
    | Pred of expr
    | Succ of expr
    | Plus of expr * expr
    | Mult of expr * expr
    | Div of expr * expr
    | If of expr * expr * expr
    | Eq of expr * expr

let rec langB_expr = function
  | Val(Num(i)) -> Printf.printf "%d" i
  | Val(Bool(b)) -> Printf.printf "%b" b
  | Val(Err(e)) -> Printf.printf "%s" e
  | Eq(e1,e2) -> langB_expr e1; Printf.printf " = "; langB_expr e2
  | If(c,e1,e2) -> Printf.printf "if("; langB_expr c; Printf.printf ") then "; langB_expr e1; Printf.printf "else "; langB_expr e2
  | Div(e1,e2) -> langB_expr e1; Printf.printf "/"; langB_expr e2
  | Pred(e) -> Printf.printf "pred("; langB_expr e; Printf.printf ")"
  | Succ(e) -> Printf.printf "succ("; langB_expr e; Printf.printf ")"
  | Plus(e1,e2) -> langB_expr e1; Printf.printf "+"; langB_expr e2
  | Mult(e1,e2) -> langB_expr e1; Printf.printf "*"; langB_expr e2
  | Var(s) -> Printf.printf "%s" s

let rec one_step e env = match e with

  | Eq(Val(Err(s)),e2) -> Val(Err(s))
  | Eq(Val(_),Val(Err(s))) -> Val(Err(s))
  | Eq(Val(Num(i)),Val(Num(j))) -> Val(Bool(i=j))
  | Eq(Val(Num(_)),Val(_)) -> Val(Err("eq1"))
  | Eq(Val(_), Val(Num(_))) -> Val(Err("eq2"))
  | Eq(Val(_), Val(_)) -> Val(Err("eq1"))
  | Eq(Val(Num(i)),e2) -> let e2' = one_step e2 env in Eq(Val(Num(i)), e2')
  | Eq(e1,e2) ->let e1' = one_step e1 env in Eq(e1',e2)

  | Val(_) as s -> s

  | Pred(Val(Num(i))) -> Val(Num(i-1))
  | Pred(Val(Err(s))) -> Val(Err(s))
  | Pred(Val(_)) -> Val(Err("pred"))
  | Pred(e1) -> let e1' = one_step e1 env in Pred(e1')

  | Succ(Val(Num(i))) -> Val(Num((i+1)))
  | Succ(Val(Err(s))) -> Val(Err(s))
  | Succ(Val(_)) -> Val(Err("succ"))
  | Succ(e1) -> let e1' = one_step e1 env in Succ(e1')

  | If(Val(Err(s)), e1, e2) -> Val(Err(s))
  | If(Val(Bool(true)),e1,e2) -> e1
  | If(Val(Bool(false)),e1,e2) -> e2
  | If(Val(_),_,_) -> Val(Err("if"))
  | If(c,e1,e2) ->  let c' = one_step c env in If(c',e1,e2)

  | Plus(Val(Err(s)),e2) -> Val(Err(s))
  | Plus(Val(_),Val(Err(s))) -> Val(Err(s))
  | Plus(Val(Num((i))),Val(Num((j)))) -> Val(Num(i+j))
  | Plus(Val(Num(_)),Val(_)) -> Val(Err("plus2"))
  | Plus(Val(_),Val(Num(_))) -> Val(Err("plus1"))
  | Plus(Val(Num((i))),e2) -> let e2' = one_step e2 env in Plus(Val(Num((i))), e2')
  | Plus(e1,e2) ->  let e1' = one_step e1 env in Plus(e1', e2)

  | Div(Val(Err(s)),e2) -> Val(Err(s))
  | Div(Val(_),Val(Err(s))) -> Val(Err(s))
  | Div(Val(Num(i)),Val(Num(0))) -> Val(Err("div0"))
  | Div(Val(Num(i)),Val(Num(j))) -> Val(Num(i/j)) 
  | Div(Val(Num(_)),Val(_)) -> Val(Err("div2"))
  | Div(Val(_),Val(Num(_))) -> Val(Err("div1"))
  | Div(Val(Num((i))),e2) ->  let e2' = one_step e2 env in Div(Val(Num(i)), e2')
  | Div(e1,e2) -> let e1' = one_step e1 env in Div(e1', e2)

  | Mult(Val(Err(s)),e2) -> Val(Err(s))
  | Mult(Val(_),Val(Err(s))) -> Val(Err(s))
  | Mult(Val(Num((i))),Val(Num((j)))) -> Val(Num(i*j))
  | Mult(Val(_),Val(Num(_))) -> Val(Err("mult1"))
  | Mult(Val(Num(_)),Val(_)) -> Val(Err("mult2"))
  | Mult(Val(Num((i))),e2) -> let e2' = one_step e2 env in Mult(Val(Num((i))), e2')
  | Mult(e1, e2) -> let e1' = one_step e1 env in Mult(e1', e2)


  | Var(s) -> try Hashtbl.find env s with Not_found -> Val(Err(s))

let rec eval e env =
  let e' = one_step e env in
    langB_expr e; Printf.printf " -> "; langB_expr e'; Printf.printf "\n";
    match e' with
      | Val(i) -> i
      | _ -> eval e' env

let langB_val = function
  | Num(i) -> Printf.printf "%d\n" i
  | Bool(b) -> Printf.printf "%b\n" b
  | Err(s) -> Printf.printf "err: %s\n" s

let test_eval term env = 
  Printf.printf "eval ";
  langB_expr term;
  Printf.printf "\n";
  langB_val(eval term env);
  Printf.printf "\n"

;;

let t1 = Val(Num(1)) in
let t2 = Eq(Val(Num((1))),Val(Num((1)))) in
let t3 = Eq(Pred(Val(Num((2)))),Val(Num((1)))) in
let t4 = Eq(Succ(Val(Num((2)))),Val(Num((1)))) in
let t5 = Eq(Succ(Val(Num((2)))),Div(Val(Num((1))),Val(Num((0))))) in
let t6 = Eq(Val(Bool((true))),Val(Bool((true)))) in
let t7 = Plus(Val(Bool(false)),Val(Num(8))) in
let env = Hashtbl.create 10 in
  Hashtbl.add env "x" (Val(Num((1))));
  let t8 = Eq(Var("x"),Val(Num((2)))) in
  let t9 = Eq(Var("y"),Val(Num((2)))) in
  let t10 = If(t8, Val(Num(1)), Val(Num(2))) in
    test_eval t1 env;
    test_eval t2 env;
    test_eval t3 env;
    test_eval t4 env;
    test_eval t5 env;
    test_eval t6 env;
    test_eval t7 env;
    test_eval t8 env;
    test_eval t9 env;
    test_eval t10 env;
    Printf.printf "Complete!"

