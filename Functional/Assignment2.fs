module Assignment2
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
    let rec removeOddIdx xs = 
        match xs with
        | [] -> []
        | x::y::xs -> x::(removeOddIdx xs)
        | x::xs -> x::(removeOddIdx xs);; 

    // 2.3
    let rec combinePair xs = 
        match xs with
        | [] -> []
        | x::y::xs -> (x, y)::(combinePair xs)
        | x::xs -> [];; // This ignores the last item in an odd list

    // 2.4
    type complex = float * float

    let mkComplex a b = complex(a, b)

    (* 
        Could not make the following functions exactly complex -> ...
        instead F# shows it as float * float -> ...

        Also for the infix operators the type is not needed and I can do 
        let (|+|) = fun (a, b) (c, d) -> complex(a + c, b + d)
    *)
    let complexToPair ((a, b): complex) = (a, b) 

    let (|+|) = fun ((a, b): complex) ((c, d): complex) -> complex(a + c, b + d)

    let (|*|) = fun ((a, b): complex) ((c, d): complex) -> complex((a * c) - (b * d), (b * c) + (a * d))

    let (|-|) = fun ((a, b): complex) ((c, d): complex) -> (a, b) |+| (-c, -d)

    let (|/|) = fun ((a, b): complex) ((c, d): complex) -> 
        let divisor a b = (a * a) + (b * b)
        let inverse a b = ((a / divisor a b), (-b / divisor a b))
        if c = 0 && d = 0 then 
            failwith "Can't divide by 0" 
        else 
            (a, b) |*| inverse c d
    (*
        Usage:
        let x = complex(a, b)
        let y = complex(c, d)

        x |+| y // Addition
        x |-| y // Subtraction...
        x |*| y // Multiplication
        x |/| y // Division
    *)

    // 2.5
    let explode1 (s: string) = List.ofArray(s.ToCharArray())

    let rec explode2 (s: string): char list = 
        if s.Length = 0 then []
        else s.[0]::(explode2 (s.Remove(0, 1)))

    // 2.6
    let rec implode (cs: char list) =
        match cs with 
        | [] -> ""
        | c::cs -> c.ToString() + implode cs;;

    let rec implodeRev (cs: char list) = 
        match cs with 
        | [] -> ""
        | c::cs ->  implodeRev cs + c.ToString();;

    // 2.7
    (* 
        Unable to find a way to use one line pipeline/function composition 
        without for loop, higher order functions and other restrictions

        This is why I made an extra function char: list -> char: list 
        which resurively makes every char in the list uppercase
    *)
    let rec listUpper (cs: char list) =
        match cs with 
        | [] -> []
        | c::cs -> (System.Char.ToUpper c)::listUpper(cs);;

    let toUpper s = s |> explode2 |> listUpper |> implode

    let toUpper2 = explode2 >> listUpper >> implode

    // 2.8
    let rec ack = 
        function 
        | (m, n) when m = 0 -> n + 1
        | (m, n) when m > 0 && n = 0 -> ack(m - 1, 1)
        | (m, n) when m > 0 && n > 0 -> ack(m - 1, ack(m, n - 1))
        | _ -> failwith "Something went wrong";;