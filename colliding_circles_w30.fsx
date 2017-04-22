open System

let (n, k, circles) = 
    let nk = Console.ReadLine().Split(' ') |> Array.map Int32.Parse |> List.ofArray
    let circles = Console.ReadLine().Split(' ') |> Array.map Int32.Parse |> List.ofArray
    (nk.[0], nk.[1], circles)

let rec getCombination (xs: int list) =
    //List.mapi (fun i x -> let tail List.skip)
    match xs with 
        | x1 :: x2 :: tail -> (List.map (fun x -> (x1, x)) (x2::tail)) @ (getCombination (x2::tail))
        | _ -> []


