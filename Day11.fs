module Day11

open System.IO
open System.Collections.Generic

let parse (input: string seq) =
    input
    |> Seq.map (fun line ->
        line.ToCharArray()
        |> Seq.map (fun c -> (int c) - int '0'))
    |> array2D

let input =
    File.ReadLines("puzzle_input/day_11") |> parse

let generateAdjacent row col =
    let offset = [ -1; 0; 1 ]

    seq {
        for xOffset in offset do
            for yOffset in offset do
                yield
                    if xOffset = yOffset && xOffset = 0 then
                        None
                    else
                        (row + xOffset, col + yOffset) |> Some
    }
    |> Seq.choose id

let step (grid: int [,]) =
    let mutable currentGrid = grid
    let bumped: Stack<int * int> = Stack()

    // first pass
    currentGrid <-
        currentGrid
        |> Array2D.mapi (fun row col curr ->
            match curr with
            | 9 ->
                generateAdjacent row col
                |> Seq.iter (fun (row, col) ->
                    let inGridAndNotFlashed =
                        try
                            currentGrid.[row, col] <= 9
                        with
                        | :? System.IndexOutOfRangeException -> false

                    if inGridAndNotFlashed then
                        bumped.Push(row, col))

                10
            | x when x > 9 -> 10
            | x -> x + 1)

    while bumped.Count <> 0 do
        let (row, col) = bumped.Pop()

        match currentGrid.[row, col] with
        | 9 ->
            currentGrid.[row, col] <- 10

            generateAdjacent row col
            |> Seq.iter (fun (row, col) ->
                let inGridAndNotFlashed =
                    try
                        currentGrid.[row, col] <= 9
                    with
                    | :? System.IndexOutOfRangeException -> false

                if inGridAndNotFlashed then
                    bumped.Push(row, col))

        | x when x > 9 -> ()
        | x -> currentGrid.[row, col] <- x + 1

    currentGrid <-
        currentGrid
        |> Array2D.map (function
            | x when x > 9 -> 0
            | x -> x)

    currentGrid


let part_1_ (input) =
    let mutable grid: int [,] = input

    let mutable totalFlashes = 0uL

    for _ in 1 .. 100 do
        let next = step grid

        totalFlashes <-
            (next
             |> Seq.cast<int>
             |> Seq.sumBy (function
                 | 0 -> 1uL
                 | _ -> 0uL))
            + totalFlashes

        grid <- next

    totalFlashes


let rec part_2_ stepCount (input: int [,]) =
    if (input |> Seq.cast<int> |> Seq.forall ((=) 0)) then
        stepCount
    else
        (part_2_ (stepCount + 1uL) (step input))

let part_1 () = input |> part_1_ |> printfn "%A"

let part_2 () = input |> (part_2_ 0uL) |> printfn "%A"

let parts = (11, part_1, part_2)

module Tests =
    let testInput =
        ("5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")
            .Split '\n'
        |> parse

    let ``part 1 sample data`` () =
        testInput
        |> part_1_
        |> (fun result ->
#if DEBUG
            printfn "Part 1 sample data result: %A" result
#endif
            assert (result = 1656uL))

    let ``part 2 sample data`` () =
        testInput
        |> (part_2_ 0uL)
        |> (fun result ->
#if DEBUG
            printfn "Part 2 sample data result: %A" result
#endif
            assert (result = 195uL))

Tests.``part 1 sample data`` ()
Tests.``part 2 sample data`` ()
