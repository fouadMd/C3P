(* Exercice 1 *)

let rec integers n = match n with
    0 -> [0]
    | n -> (integers (n-1)) @ [n] ;;
    
let rec integers'' n = List.rev (integers2 n)
    and integers2 n = match n with
    0 -> [0]
    | n -> n :: integers2 (n-1) ;;
    
let integers'' n = 
    let rec integers2 n = match n with
    0 -> [0]
    | n -> n :: integers2 (n-1) in List.rev (integers2 n);;
    
    
let integers3 n = let rec aux n m =
    if  n = m then [n]
    else m :: aux n (m+1) in aux n 0 ;;
    
let time f x =
    let t = Sys.time() in
        let resultat = f x in
            (Sys.time() -. t) *. 1000., resultat;;
    
(* Exercice 2 *)

let rec size l = match l with
    [] -> 0
    | x :: xs -> 1 + size xs ;;
    
let rec three_or_more l = match l with
    x :: y :: z :: xs -> true
    | l -> false ;;

let rec last l = match l with
    [] -> failwith "aucun élement"
    | x :: [] -> x
    | x :: xs -> last xs ;;
    
let rec sum l = match l with
    [] -> 0
    | x :: xs  -> x + sum xs ;;
    
let rec find e l = match l with
    [] -> false
    | x :: xs -> if x = e then true
                     else find e xs ;;
                     
let rec nth n l = match n,l with
    _,[] -> failwith "index out of bounds"
    | 0,(x :: xs) -> x
    | n,(x :: xs) -> nth (n-1) xs ;;
    
let rec is_increasing l = match l with
    [] -> true
    | [x] -> true
    | x :: y :: xs -> if x <= y then is_increasing (y :: xs)
                           else false ;;
                           
(* Exercice 3 *)

let rec list_copy l = match l with
      [] -> []
      | x :: xs -> x :: (list_copy xs) ;;
      
let rec reverse l = match l with
    [] -> []
    | x :: xs -> (reverse xs) @ [x] ;;

let rec without_duplicates l = match l with
    [] -> []
    | x :: y :: xs -> if x = y then without_duplicates (x :: xs)
                           else x :: (without_duplicates (y :: xs)) 
    | l -> l ;;
    
(* Exercice 4 *)

let rec filter f l = match l with
    [] -> []
    | x :: xs -> if f x then x :: (filter f xs)
                     else filter f xs ;;
                     
let rec collect f l = match l with
    [] -> []
    | x :: xs -> (f x) :: (collect f xs) ;;
    
let rec reject f l = match l with
    [] -> []
    | x :: xs -> if f x then reject f xs
                     else x :: (reject f xs)  ;;
                     

let rec including l1 l2 = 
let rec find e l = match l with
    [] -> false
    | x :: xs -> if x = e then true
                     else find e xs 
    in match l1 with
    [] -> true
    | x :: xs -> (&&) (find x l2) (including xs l2) ;;


let rec excluding l1 l2 = match l1 with
    [] -> true
    | x :: xs ->
    let rec excludes e l = match l with
        [] -> true
        |  x :: xs -> if x = e then false
                    else excludes e xs 
        in (&&) (excludes x l2) (excluding xs l2) ;;
        
let rec zip l1 l2 = match l1,l2 with
    [],_ -> []
    | _,[] -> []
    | (x :: xs) , (y :: ys) -> (x,y) :: (zip xs ys) ;;
        

(* Exercice 5 *)

let  paireInv l = l |> List.filter (fun x-> x mod 2 = 0) |> without_duplicates |> reverse ;;

let impairNbre l = l |> List.filter (fun x-> x mod 2 != 0) |> size ;;

let rec carac l = match l with
        [] -> []
        | x :: xs -> (x |> string_of_int ) :: carac xs ;; 
        
let rec carreL l = match l with
        [] -> []
        | x :: xs -> (x*x) :: carreL xs ;; 
    
    
    
    
