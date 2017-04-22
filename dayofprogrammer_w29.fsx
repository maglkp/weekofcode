open System

let year = Int32.Parse(Console.ReadLine())

let (|Pre1918|In1918|Post1918|) year = if year < 1918 then Pre1918 elif year > 1918 then Post1918 else In1918

let divides num div =
    num % div = 0

let dayOfTheProgrammer =
    match year with
        | In1918 -> "25.09.1918"
        | Pre1918 -> if (divides year 100 || (divides year 4 && not(divides year 100))) then "12.09." + year.ToString() else "13.09." + year.ToString()
        | Post1918 -> if (divides year 4) then "12.09." + year.ToString() else "13.09." + year.ToString()

printfn "%s" dayOfTheProgrammer