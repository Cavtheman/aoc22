open System.IO

let readFile (filename : string) : int [,] =
    filename
    |> File.ReadLines
    |> Seq.map (Seq.map (string >> int) >> Seq.toArray)
    |> Seq.toList
    |> array2D

let isVisible (forest : int [,]) (x : int) (y : int) =
    let shape1, shape2 = Array2D.length1 forest, Array2D.length2 forest
    let above = [ for j in 0..x-1        do yield j,y ]
    let below = [ for j in x+1..shape1-1 do yield j,y ]
    let left  = [ for i in 0..y-1        do yield x,i ]
    let right = [ for i in y+1..shape2-1 do yield x,i ]
    let folder acc (i,j) = forest.[i,j] < forest.[x,y] && acc

    [above;below;left;right]
    |> List.map (List.fold folder true)
    |> List.reduce (||)

let findView (forest : int [,]) (x : int) (y : int) =
    let shape1, shape2 = Array2D.length1 forest, Array2D.length2 forest
    let above = List.rev [ for j in 0..x-1 do yield j,y ]
    let left  = List.rev [ for i in 0..y-1 do yield x,i ]
    let below = [ for j in x+1..shape1 - 1 do yield j,y ]
    let right = [ for i in y+1..shape2 - 1 do yield x,i ]

    let rec viewHelper (adjacent : list<int * int>) =
        match adjacent with
        | [] -> 0
        | (i,j)::xs when forest.[i,j] < forest.[x,y] -> 1 + viewHelper xs
        | _ -> 1

    [above;below;left;right]
    |> List.map viewHelper
    |> List.reduce (*)

let forest = readFile "input.txt"
let shape1, shape2 = Array2D.length1 forest, Array2D.length2 forest
let resultArr = Array.init shape1 (fun i ->
                                   Array.init shape2 (fun j ->
                                                      if isVisible forest i j then 1 else 0))
let numVisible = Array.map Array.sum resultArr |> Array.sum
let views = Array.init shape1 (fun i ->
                               Array.init shape2 (fun j ->
                                                  findView forest i j))
let bestView = views |> Array.map (Array.max) |> Array.max
printfn "%A" numVisible
printfn "%A" bestView
