open System

let (n, c, m, ps) = 
    let firstLine = Console.ReadLine().Split(' ')
    let (n, c, m) = (Int32.Parse(firstLine.[0]), Int32.Parse(firstLine.[1]), Int32.Parse(firstLine.[2]))
    let secondLine = Console.ReadLine()
    let ps = secondLine.Split(' ') |> Array.map Int32.Parse |> List.ofArray
    (n, c, m, ps)

let cap = c * m
if List.forall (fun e -> e <= cap) ps then 
    printfn "%s" "Yes"
else 
    printfn "%s" "No"
