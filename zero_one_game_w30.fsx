open System

let arrays = 
    let cnt = Int32.Parse(Console.ReadLine())
    List.init cnt (fun i -> 
                            let _ = Console.ReadLine()
                            Console.ReadLine().Split(' ') |> Array.map Int32.Parse |> List.ofArray) 
let rec shorten xs =
    let except (xs: int list) (ix:int) = 
        xs.[0..(ix-1)] @ xs.[(ix+1).. ((List.length xs) - 1)]
    let reclaim xs = 
        xs.[0].[1] :: xsmapped @@ [xs.[last].[1]]
    let ws = List.windowed 3 xs
    let toRemoveIxOpt = ws |> List.tryFindIndex (fun (a,b,c) -> a = 0 && b = 0)
    match toRemoveIxOpt with
        | Some ix -> shorten (reclaim(except ws ix))
        | None -> xs
        




