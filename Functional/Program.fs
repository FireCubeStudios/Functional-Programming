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
    Usage: 
        add5_2 (fun x -> x * 3) 7 
        output: int = 26 
*)
let add5_2 f x = f x + 5

let mul3_2 f x = f x * 3

// 3.3
let rec downto4 f n e = 
    match n with
    | n when n > 0 -> downto4 f (n - 1) (f n e)
    | n -> e;;

let fac n = downto4 (fun acc x -> x * acc) n 1 // Using accumulator (Folds concept) where acc = accumulated value

let range g n = downto4 (fun n acc -> (g n)::acc) n []

(*
    Usage:
        let double x = x * 2;;
        range double 9;;

        output: [2; 4; 6; 8; 10; 12; 14; 16; 18] aka [g 0; g 1; ... g n]
*)

// 3.4
let rec double lst = 
    match lst with 
    | [] -> []
    | x::lst -> (x * 2)::(double lst);;

let double_2 lst = List.map (fun x -> x * 2) lst

// 3.5
let rec stringLength (lst: string list) =
    match lst with
    | [] -> []
    | s::lst -> (s.Length)::(stringLength lst)

let stringLength_2 lst = List.map (fun (s: string) -> s.Length) lst

// 3.6
let rec keepEven lst = 
    match lst with
    | [] -> []
    | x::lst when x % 2 = 0 -> x::(keepEven lst)
    | x::lst -> keepEven lst;;

let keepEven_2 lst = List.filter (fun x -> x % 2 = 0) lst

// 3.7 
let rec keepLengthGT5 (lst: string list) =
    match lst with
    | [] -> []
    | s::lst when s.Length > 5 -> s::(keepLengthGT5 lst)
    | s::lst -> keepLengthGT5 lst;;

let keepLengthGT5_2 lst = List.filter (fun (s: string) -> s.Length > 5) lst

// 3.8
let rec sumPositive lst = 
    match lst with
    | [] -> 0
    | x::lst when x > 0 -> x + (sumPositive lst)
    | x::lst -> sumPositive lst;;

let sumPositive_2 lst = List.fold (fun acc x -> if x > 0 then x + acc else acc) 0 lst // Remember accumulator is first param in fold function

let sumPositive_3 lst = lst |> List.filter (fun x -> x > 0) |> List.fold (fun acc x -> x + acc) 0

// 3.9
let add5mul3_3 f x =  f x |> add5_2 id |> mul3_2 id // id is the built-in function fun x -> x

// 3.10
let mergeFuns fs x = List.fold (fun acc f -> f acc) x fs

// 3.11
let removeOddIdx xs = 
    let (index, list) = 
        List.foldBack (fun x (i, acc) -> if i % 2 = 0 then (i + 1, x::acc) else (i + 1, acc)) xs (xs.Length - 1, [])
    list;;

(*
    List.foldback reverses a List.fold by starting at the end of a list
    We do this because preprending items when using List.fold will reverse the final list
    List.foldback has to have parameters reversed compared to List.fold
*)

// 3.12
let rec facFuns x = 
    match x with
    | 1 -> [(fun y -> 1 * y)]
    | x -> (fun y -> x * y)::(facFuns (x - 1));;

let fac_2 x = mergeFuns (facFuns x) 1 // Pass in 1 so the calculation is correctly applied

// 3.13
let weird (strs: string list) =
    strs |> List.filter (fun (s: string) -> s.Length % 2 = 0) 
    |> List.map (fun (s: string) -> s.Length) 
    |> List.fold (fun acc x -> acc + x.ToString()) ""


// 3.14
let rec insert x xs =    
    match xs with
    | [] -> [[x]]
    | n::list -> (x::xs)::(List.map (fun lst -> n::lst) (insert x list))

let rec permutations lst =
    match lst with
    | [] -> [[]]
    | x::lst -> List.collect (fun permutation -> insert x permutation) (permutations lst)