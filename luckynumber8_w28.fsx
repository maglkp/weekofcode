open System

let (digits, n) = 
    (Int32.Parse(Console.ReadLine()), Console.ReadLine().ToCharArray())

let ones = 
    n 
    |> Array.filter (fun d -> d = '0' || d = '8') 
    |> Array.length

let twos = 
    n
    |> Array.pairwise
    |> Array.filter (fun (d1, d2) -> Int32.Parse(string(d1) + string(d2)) % 8 = 0)
    |> Array.length

let threes =
    n
    |> Array.windowed 3
    |> Array.mapi (fun i ddd -> 
                match ddd with
                | [|d1; d2; d3|] -> if Int32.Parse(string(d1) + string(d2) + string(d3)) % 8 > 0 then 0 else 1 + i
                | _ -> failwith "Array.windowed")
    |> Array.sum

let genComb ls comLen = 
    let rec gen acc ls comLen =
        if List.length acc = comLen then printfn "gen= %A" (List.rev acc)
        else 
            [0..((List.length ls) - 1 - ((comLen - 1) - List.length acc))]               
            |> List.iter (fun i -> gen (ls.[i]::acc) (List.skip (i+1) ls) comLen) 
    gen [] ls comLen
    
    //ls |> List.iteri (fun i l -> gen (l::acc) (ls.[i+1, ))


//printfn "%i" ((ones + twos + threes) % (1000000007))
//printfn "%A" (ones)
//printfn "%A" (twos)
//printfn "%A" (threes)