
(* Additional Test Cases *)

let accept_new = fun d -> function | "*"::t -> Some (d,"*"::t) | _ -> None
let accept_all derivation string = Some (derivation, string)

type my_nonterminals =
  | Expr | Term | Binop | Num

let my_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
       [[N Num];
        [T"("; N Expr; T")"]]
     | Binop ->
       [[T"+"];
        [T"-"];
          [T"*"]]
     | Num ->
       [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
        [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])
   
let test_1 = ((parse_prefix my_grammar accept_all ["1";"*";"2";"+";"3"]) = Some
  ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "1"]);
   (Binop, [T "*"]); (Expr, [N Term; N Binop; N Expr]); (Term, [N Num]);
   (Num, [T "2"]); (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]);
   (Num, [T "3"])],
  []))
  
let test_2 = ((parse_prefix my_grammar accept_new ["1";"*";"2";"+";"3"]) = Some
 ([(Expr, [N Term]); (Term, [N Num]); (Num, [T "1"])], ["*"; "2"; "+"; "3"]))