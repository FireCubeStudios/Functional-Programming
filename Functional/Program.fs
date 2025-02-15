(*
    Assignment 2 exercises
    2.1 - 2.8 easy
*)

// 2.1
let rec downto1 n = if n > 0 then n::downto1(n - 1) else []

let rec downto2 n = 
    match n with
    | n when n <= 0 -> []
    | n -> n::downto2(n - 1)

let downto3 =
    function
    | n when n <= 0 -> []
    | n -> n::downto2(n - 1)

// 2.2
(*let removeOddIdx xs = 
    let list = []
    let rec test index = 
        match index % 2 = 0 with
        | true -> test index + 1
        | false -> 
            let y = xs[index]::list
            test index + 1*)

// 2.3

// 2.4
type complex = float * float

let mkComplex a b = complex(a, b)

let complexToPair ((a, b): complex) = (a, b) // ??? not exactly complex -> float * float

let (|+|) = fun ((a, b): complex) ((c, d): complex) -> complex(a + c, b + d)

let (|*|) = fun ((a, b): complex) ((c, d): complex) -> complex((a * c) - (b * d), (b * c) + (a * d))

let (|-|) = fun ((a, b): complex) ((c, d): complex) -> (a, b) |+| (-c, -d)

let (|/|) = fun ((a, b): complex) ((c, d): complex) -> 
    if c = 0 || d = 0 then 
        failwith "Can't divide by 0" 
    else 
        (a, b) |*| ((a / (a * a) + (b * b)), (-b / (a * a) + (b * b)))

// 2.5

// 2.6

// 2.7

// 2.8

let ack (m ,n) = 


(*
    Assignment 3 exercises
    3.1 - 3.8 easy
    3.9 - 3.11 medium
    3.12 - 3.14 hard
*)

// 3.1

// 3.2

// 3.3

// 3.4

// 3.5

// 3.6

// 3.7 

// 3.8

// 3.9

// 3.10

// 3.11

// 3.12

// 3.13

// 3.14
