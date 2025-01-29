(*
    Assignment 1 exercises
    1.1 - 1.8 easy
    1.9 - 1.10 medium
    1.11 hard
*)

// 1.1
let sqr x : int = x * x;

// 1.2
let pow x n : float = System.Math.Pow(x, n)

// 1.3
let rec sum n : int = 
    match n with
    | 0 -> 0
    | n when n > 0 -> n + sum(n - 1)
    | _ -> failwith "Number was negative";;

// 1.4
let rec fib n: int =
    match n with
    | 0 -> 0
    | 1 -> 1
    | n when n > 1 -> fib(n - 1) + fib(n - 2)
    | _ -> failwith "Number was negative";;

// 1.5
let dup s : string = s + s

// 1.6
let rec dupn (s: string) (n: int) = 
    if n > 0 then 
        s + dupn s (n - 1)
    else
        ""

// 1.7
let rec bin = 
    function
    | (n, 0) -> 1
    | (n, k) when n = k -> 1
    | (n, k) when n = 0 || n < k -> failwith "Something went wrong";
    | (n, k) -> bin(n - 1, k - 1) + bin(n - 1, k);;

// 1.8
