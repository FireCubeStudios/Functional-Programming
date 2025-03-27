module Rational

    type rational = 
        R of int * int // Internal representation of rational number, Usage: R(1, 2)
            override q.ToString (): string = // Override ToString method for better printing (Polymorphism)
                match q with
                | R(a, b) -> sprintf "(%d / %d)" a b

    let rec gcd a b = if b = 0 then a else gcd b (a % b) // Euclid algorithm (private)

    // Create rational number for clients to use while hiding internqal "rational" R representation (encapsulation)
    let initRational a b =
        let g = gcd a b
        R(a / g, b / g)

    let (.+.) (R(a, b), R(c, d)) = R(a * d + b * c, c * d)
    let (.-.) (R(a, b), R(c, d)) = R(a * d - b * c, c * d)
    let (.*.) (R(a, b), R(c, d)) = R(a * c, b * d)
    let (./.) (R(a, b), R(c, d)) = R(a * d, b * c)

