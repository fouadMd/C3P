(* Exercice 1 *)

(* question 1 *)
1 + 2 ;;

(* question 3 *)
2.0 ;;
2,0 ;;
'a' ;;
"a" ;;
true ;;
() ;;
[] ;;
[1] ;;

(* Exercice 2 *)

(* q1 *)
let max a b = 
    if a < b then b
    else a ;;
    
let min a b = 
    if a < b then a
    else b ;;
    
(* q2 *)
let minimun a b c = 
    min (min a b) c ;;
    
minimun 5 2 9 ;;

(*q3*)
let pair a =
    if a mod 2 = 0 then string_of_int a
    else "odd" ;;
    
pair 6 ;;
pair 5 ;;

(*q4*)

(*q5*)
let fonction a =
    if a<10
        then "small"  
        else "large";;

let b = fonction 2 ;;

(* Exercice 3 *)

let avg a b c = (a +. b +. c) /. 3.0 ;;
avg 11.0 12.0 15.0 ;;

let implies a b = 
    a & b ;;
true & false ;;


let inv x = match x with
    (a,b) -> (b,a) ;;
    
let inv2 x = (snd x,fst x) ;;
    
inv2 (2,3) ;;

(* Exercice 4 *)

let rec fib n = match n with
    0 -> 0
    | 1 -> 1
    | n -> fib (n-2) +fib (n-1) ;;
    
fib 5 ;;

(* Exercice 5 *)

(*let rec pgcd m n = 
    if m = 0 then n
    else if m > n then pgcd(n,m)
        else pgcd(n mod m,m) ;;*)
        
(*exercice 6*)

let rec pair x = 
    if x = 0 then true 
    else impair (x-1)
    and
    impair x =
    if x = 0 then false 
    else pair (x-1) ;;
    
pair 6 ;;
        


(* Exercice 7 *)

let rec fact n =
    if  n < 2 then 1
    else fact2 n 1
    and fact2 n m = 
    if n < 2 then m
    else fact2 (n-1) (m*n) ;;
    
    
fact 3 1 ;;

(* Exercice 8 *)

let rec fib n = match n with
    0 -> 0
    | 1 -> 1
    | n -> fib2 n 0 1
    and fib2 n m1 m2 =
    if n = 0 then m1
    else if n = 1 then m2
            else fib2 (n-1) m2 (m1+m2) ;;
            
(* Exercice 9 *)

let rec exp x n = match n with
    0 -> 1
    |1 -> x
    | n -> x* exp x (n-1) ;;
    
let rec expTerminale x n = match n with
    0 -> 1
    |1 -> x
    | n -> expAide x n 1
    and expAide x n res = match n with
    0 -> res
    | n -> expAide x (n-1) (res*x) ;;
    
let rec expCpt x n = match n with
    0 -> (1,0)
    |1 -> (x,0)
    | n -> exp2 x n 1 0
    and exp2 x n res cpt = match n with
    0 -> (res,cpt)
    | n -> exp2 x (n-1) (res*x) (cpt+1) ;;
    
(* Exercice 10 *)

let sum1 n m 


    
    
    
    
    
    
