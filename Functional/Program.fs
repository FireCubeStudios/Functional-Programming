// For more information see https://aka.ms/fsharp-console-apps

// 1.1
let sqr x : int = x * x;

printfn "1.1: Square 4 = %d" (sqr(4))

// 1.2
let pow x n : float = System.Math.Pow(x, n)

printfn "1.2: 4^2 = %d"
// 1.3


// more
let rec fact = 
    function
    | 0 -> 1
    | x when x > 0 -> x * fact(x - 1)
    | _ -> failwith "";