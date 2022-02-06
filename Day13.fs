module Day13

open System.IO

type Dir =
    | X
    | Y

let parse (input: string) =
    let (rawDots, rawFolds) =
        input.Split "\n\n"
        |> function
            | [| dots; foldInstructions |] -> dots, foldInstructions
            | x -> failwithf "invalid match %A" x

    let foldInstructions =
        rawFolds.Split "\n"
        |> Array.map (fun l ->
            (l.[11]
             |> function
                 | 'x' -> X
                 | 'y' -> Y
                 | x -> failwithf "%A not a valid Dir" x),
            l.Substring 13 |> int)

    let maxX =
        foldInstructions
        |> Array.filter (fun x -> fst x = X)
        |> Array.maxBy snd
        |> snd
        |> ((*) 2)

    let maxY =
        foldInstructions
        |> Array.filter (fun x -> fst x = Y)
        |> Array.maxBy snd
        |> snd
        |> ((*) 2)

    let dots =
        let coords =
            rawDots.Split "\n"
            |> Array.map (fun l ->
                l.Split ","
                |> Array.map int
                |> function
                    | [| x; y |] -> x, y
                    | x -> failwithf "invalid match %A" x)


        let mutable newDots =
            Array2D.create (maxX + 1) (maxY + 1) false

        for (x, y) in coords do
            newDots.[x, y] <- true

        newDots

    dots, foldInstructions

let input =
    File.ReadAllText("puzzle_input/day_13") |> parse

let folder input =
    let (init, folds) = input
    let mutable curr: bool [,] = init


    folds
    |> Array.iter (function
        | (X, at) ->
            let mutable next = curr.[0..(at - 1), *]
            let fold = curr.[at + 1..curr.GetLength(0), *]

            next
            |> Array2D.iteri (fun x y v -> next.[x, y] <- v || fold.[at - 1 - x, y])

            curr <- next
        | (Y, at) ->
            let mutable next = curr.[*, 0..(at - 1)]
            let fold = curr.[*, at + 1..curr.GetLength(1)]

            next
            |> Array2D.iteri (fun x y v -> next.[x, y] <- v || fold.[x, at - 1 - y])

            curr <- next)

    curr

let part_1_ (input: bool [,] * (Dir * int) []) =
    let curr = folder input

    curr
    |> Seq.cast<bool>
    |> Seq.filter id
    |> Seq.length

let part_2_ (input) =
    let curr = folder input

    for y in 0 .. curr.GetLength(1) - 1 do
        printfn
            "%A"
            (curr.[*, y]
             |> Array.map (function
                 | true -> "#"
                 | false -> ".")
             |> String.concat "")

let part_1 () =
    input
    |> (fun (dots, folds) -> (dots, [| folds.[0] |]))
    |> part_1_
    |> printfn "%A"

let part_2 () = input |> part_2_ |> printfn "%A"

let parts = (13, part_1, part_2)

module Tests =
    let testInput =
        ("6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")
        |> parse

    let ``part 1 sample data`` () =
        testInput
        |> (fun (dots, folds) -> (dots, [| folds.[0] |]))
        |> part_1_
        |> (fun result ->
#if DEBUG
            printfn "Part 1 sample data result: %A" result
#endif
            assert (result = 17))

    let ``part 2 sample data`` () =
        testInput
        |> part_2_
        |> (fun result ->
#if DEBUG
            printfn "Part 2 sample data result: %A" result
#endif
            assert (result = ()))

Tests.``part 1 sample data`` ()
Tests.``part 2 sample data`` ()
