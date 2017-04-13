open System

let arrays = 
    let cnt = Int32.Parse(Console.ReadLine())
    List.init cnt (fun i -> 
                            let _ = Console.ReadLine()
                            Console.ReadLine().Split(' ') |> Array.map Int32.Parse |> List.ofArray) 

let rec shorten (xs: int list) =
    //printfn "%A" xs
    let except (xs: int list list) (ix:int) = 
        xs.[0..(ix-1)] @ xs.[(ix+1).. ((List.length xs) - 1)]
    let reclaim (ws: int list list) (xs: int list) = 
        if (List.length ws = 0) then 
            []
        else
            let middles = ws |> List.map (fun e-> e.[1]) 
            let l = List.last xs
            let f = xs.[0]
            let r = f :: (middles @ [l])                    
            //printfn "reclaim=%A" r
            r

    let ws = List.windowed 3 xs
    
    let toRemoveIxOpt = ws |> List.tryFindIndex (fun w -> w.[0] = 0 && w.[2] = 0)
    match toRemoveIxOpt with
        | Some ix -> 
                let exc = except ws ix
                //printfn "except=%A" exc
                shorten (reclaim exc xs)
        | None -> xs
        

arrays
|> List.iter (fun xs -> 
                let diff = (List.length xs) - List.length (shorten xs)
                if diff % 2 = 0 then printfn "%s" "Bob" else printfn "%s" "Alice")