open System

let arrays = 
    let cnt = Int32.Parse(Console.ReadLine())
    List.init cnt (fun i -> 
                            let _ = Console.ReadLine()
                            Console.ReadLine().Split(' ') |> Array.map Int32.Parse |> List.ofArray) 

let cut n m xs =    
    xs 
        |> List.skip n
        |> List.take (m - n + 1)
 
arrays 
|> List.map (fun xs -> 
    xs 
    |> List.indexed 
    |> List.exists (fun (x, ix) -> 
        if x <= ix then 
            false
        else 
            xs 
            |> cut ix x            
            |> List.exists (fun a -> Math.Abs(a - x) > 1)
    )
)
|> List.iter (fun cannotSort -> if cannotSort then 
                                    printfn "%s" "No"
                                else 
                                    printfn "%s" "Yes")