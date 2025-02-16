(*
    Assignment 3 exercises
    3.1 - 3.8 easy
    3.9 - 3.11 medium
    3.12 - 3.14 hard
*)

// 3.1
let add5 x = x + 5

let mul3 x = x * 3

let add5mul3 x = x |> add5 |> mul3

let add5mul3_2 = add5 >> mul3

// 3.2
(* 
    Usage: add5_2 (fun x -> x * 3) 7 
    outputs: int = 26 
*)
let add5_2 f x = f x + 5

let mul3_2 f x = f x * 3

// 3.3
let rec downto4 f n e = 
    match n with
    | n when n > 0 -> downto4 f (n - 1) (f n e)
    | n -> e;;

let fac n = downto4 (fun x acc -> x * acc) n 1 // Using accumulator (Folds concept)

let range g n = downto4 (fun n acc -> (g n)::acc) n []

// 3.4
let rec double lst = 
    match lst with 
    | [] -> []
    | x::lst -> (x * 2)::(double lst);;

let double_2 lst = List.map (fun x -> x * 2) lst

// 3.5
let rec stringLength

// 3.6

// 3.7 

// 3.8

// 3.9

// 3.10

// 3.11

// 3.12

// 3.13

// 3.14