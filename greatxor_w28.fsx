open System

let (q, xs) = 
    let q = Int32.Parse(Console.ReadLine())
    let xs = List.init q (fun _ -> Convert.ToString(Int64.Parse(Console.ReadLine()), 2))    
    (q, xs)

let getNumberOfGreatXors (binStr: string) = 
    binStr.ToCharArray()
    |> Array.rev 
    |> Array.mapi (fun i c -> if c = '1' then 0. else Math.Pow(2., float(i)))
    |> Array.sum
    |> int

xs 
|> List.map getNumberOfGreatXors
|> List.iter (fun e -> printfn "%i" e)