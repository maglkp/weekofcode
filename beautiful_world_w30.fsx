open System
   
let vowels = ['a'; 'e'; 'i'; 'o'; 'u'; 'y']
let word = Console.ReadLine().ToCharArray()

let beautiful = 
    word 
        |> Array.pairwise
        |> Array.tryFind (fun (a, b) -> a = b || (List.contains a vowels && List.contains b vowels))

match beautiful with
    | Some _ -> printfn "%s" "No"
    | None -> printfn "%s" "Yes"