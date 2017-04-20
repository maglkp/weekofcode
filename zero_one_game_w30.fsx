open System

let arrays = 
    let cnt = Int32.Parse(Console.ReadLine())
    List.init cnt (fun i -> 
                            let _ = Console.ReadLine()
                            Console.ReadLine().Split(' ') |> Array.map Int32.Parse |> List.ofArray) 

let IX_NOT_SET = -1
let rec cntRems (xs: int list) acc fragmentIx ix = 
    match xs with
    | [a; b; c] -> if fragmentIx = IX_NOT_SET then                     
                        if a = 0 && c = 0 then 
                            //printfn "end %A %A %A" ix fragmentIx 1
                            acc + 1
                        else
                            acc
                   else 
                       if c = 0 then 
                            let increase = (ix - fragmentIx + 1) // 100, 010, 000
                            //printfn "end %A %A %A" ix fragmentIx increase
                            acc + increase
                       else if b = 0 then 
                            let increase = (ix - fragmentIx) // 001, 101
                            //printfn "end %A %A %A" ix fragmentIx increase
                            acc + increase
                       else 
                            let increase = (ix - fragmentIx - 1) // 011
                            //printfn "end %A %A %A" ix fragmentIx increase
                            acc + increase
    | a :: b :: c :: tail -> if fragmentIx = IX_NOT_SET then
                                if a = 0 && (b <> 1 || c <> 1) then
                                    //printfn "start %A %A %A %A" ix a b c                                    
                                    // start of fragment to eat, just set fragmentIx to current ix
                                    cntRems (b::c::tail) acc ix (ix+1)
                                else 
                                    // not in fragment and not starting one, going through badlands, keep going...
                                    cntRems (b::c::tail) acc IX_NOT_SET (ix+1)                                                             
                             elif b <> 0 && c <> 0 then                                
                                let increase = if ix - fragmentIx <= 1 then 0 else (ix - fragmentIx + 1) - 2
                                //printfn "removing %A %A %A" ix fragmentIx increase
                                // end of fragment, increase the removed elements acc accordingly
                                cntRems (b::c::tail) (acc + increase) IX_NOT_SET (ix+1)
                             else 
                                // continue going through fragment
                                cntRems (b::c::tail) acc fragmentIx (ix+1)
    | _ -> failwith "cntRems list shorter than 3 elements"

let rec shorten2 (xs: int list) = 
    match xs with
    | a :: b :: c :: tail -> if a = 0 && c = 0 then shorten2 (a :: c :: tail) else a :: shorten2 (b :: c :: tail)
    | _ -> xs


arrays
|> List.iter (fun xs -> 
                match xs with
                | [] ->  printfn "%s" "Bob"
                | [a] -> printfn "%s" "Bob"
                | [a; b] -> printfn "%s" "Bob"
                | [a; b; c] -> if a = 0 && c = 0 then printfn "%s" "Alice" else printfn "%s" "Bob"
                | _ ->
                    let cnt = cntRems xs 0 IX_NOT_SET 0
                    //printfn "%A" cnt
                    if cnt % 2 = 0 then printfn "%s" "Bob" else printfn "%s" "Alice")

