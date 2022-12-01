open System
open System.IO

let splitter (acc : list<int>) (line : string) : list<int> =
    match line, acc with
    | _, [] | "", _  -> 0::acc
    | s, x::xs -> (x + int s) :: xs

//let splitter (acc : list<int>) (line : string) : list<int> =
//    match line, acc with
//    | _, []    -> [0]
//    | "", acc  -> 0::acc
//    | s, x::xs -> (x + int s) :: xs

let readFile (filename : string) =
    filename
    |> File.ReadLines
    |> Seq.toList
    |> List.fold splitter []
    |> List.sort
    |> List.rev

printfn "%A" <| (readFile "inputSimple.txt").[0]
printfn "%A" <| List.sum ((readFile "inputSimple.txt").[0..2])

printfn "%A" <| (readFile "input.txt").[0]
printfn "%A" <| List.sum ((readFile "input.txt").[0..2])
