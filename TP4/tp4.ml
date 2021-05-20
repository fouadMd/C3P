(* Exercice 1 *)

type matL = Value of int 
				| BinOp of string * matL * matL 
				| UniOp of string * matL 
				| VarL of  string  ;;
				
type matL_S = Expression of matL 
                         | Affectation of string * matL ;;


let x = BinOp ("+", Value 1 , BinOp ("+",Value 2,UniOp("-",Value 3) ) ) ;;

let y = BinOp ("^",Value 5,Value 2) ;;


let Z = VarL ("a" , BinOp ("*" , Value 4, BinOp ("+" , Value 5 , Value 6 ) ) ) ;;

let rec evaluate_expr m = match m with
    Value x -> x
    | UniOp ("+",y) -> evaluate_expr y
    | UniOp ("-",y) -> - (evaluate_expr y)
    | UniOp (o , _) -> failwith "operation inconnue"
    | BinOp  ("+",y,z) ->  (evaluate_expr y) + (evaluate_expr z)
    | BinOp  ("-",y,z) ->  (evaluate_expr y) - (evaluate_expr z)
    | BinOp  ("*",y,z) ->  (evaluate_expr y) * (evaluate_expr z)
    | BinOp  ("/",y,z) ->  (evaluate_expr y) / (evaluate_expr z)
    | BinOp  (o,_,_) ->  failwith "operation inconnue" ;;
    


(* Exercice 2 *)
    
let rec exp x y = match y with
	 0 -> 1
	|1 -> x
	|n -> exp (x*x) (n-1) ;;
	
let recherche n = 
    let rec recherche' n l = 
        match l with
        [] -> failwith "operation inconnue"
        | (x,y) :: xs -> if x = n then y
                               else recherche' n xs 
        in recherche' n  [("+",fun x y -> x + y) ; ("-",fun x y -> x - y ) ; ("*",fun x y -> x * y ) ;
         ("/", fun x y -> x / y ) ; ("^",exp) ]  ;;
        

let rec evaluate_expr' m = match m with
    Value x -> x
    | UniOp (o,y) ->(recherche o) 0 (evaluate_expr' y) 
    | BinOp  (n,y,z) ->  (recherche n)  (evaluate_expr' y) (evaluate_expr' z) ;;

evaluate_expr' y;; 

(*Exercice 3*)

let a = Affectation ( "a =" , BinOp ( "+" , Value 6, BinOp ("*" , Value 4 , Value 5) ) ) ;; 


let search_variable var =
    let rec search_variable_aux var lst = match lst with
                          | [] -> failwith "empty list"
                          | (c,v)::xs -> if var = c then v
                                          else search_variable_aux var xs
    in search_variable_aux var listOp;;


let evaluate_statement stat = match stat with
                        | expr = evaluate_expr_sndVersion expr
                        | Affectation (v,expr) = v ^ string_of_int(evaluate_expr_sndVersion expr);;
   
