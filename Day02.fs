module Day02

open System.IO

type Dir =
    | Forward of int
    | Up of int
    | Down of int

let parse (line: string) =
    match line.Trim().Split ' ' with
    | [| "forward"; x |] -> Forward(int x)
    | [| "down"; x |] -> Down(int x)
    | [| "up"; x |] -> Up(int x)
    | x -> failwithf "%A is not a valid pattern" x

let processPosition (dirs: Dir []) =
    dirs
    |> Array.fold
        (fun (x, y) ->
            fun dir ->
                match dir with
                | Forward (by) -> (x + by, y)
                | Up (by) -> (x, y - by)
                | Down (by) -> (x, y + by)

            )
        (0, 0)

let processPositionWithAim (dirs: Dir []) =
    dirs
    |> Array.fold
        (fun (hPos, depth, aim) ->
            fun dir ->
                match dir with
                | Down (x) -> (hPos, depth, aim + x)
                | Up (x) -> (hPos, depth, aim - x)
                | Forward (x) -> (hPos + x, depth + (aim * x), aim))
        (0, 0, 0)


let input =
    File.ReadAllLines "puzzle_input/day_02"
    |> Array.ofSeq
    |> Array.map parse

let part_1 () =
    let (x, y) = input |> processPosition
    printfn "%A" (x * y)

let part_2 () =
    let (x, y, _) = input |> processPositionWithAim
    printfn "%A" (x * y)

let parts = (2, part_1, part_2)

module Tests =
    let testInput =
        ("forward 5
        down 5
        forward 8
        up 3
        down 8
        forward 2")
            .Split '\n'
        |> Array.map parse

    let ``part 1 sample data`` () =
        let result = testInput |> processPosition
        assert (result = (15, 10))

    let ``part 2 sample data`` () =
        let (depth, hPos, _) = testInput |> processPositionWithAim
        assert ((depth, hPos) = (15, 60))

Tests.``part 1 sample data`` ()
Tests.``part 2 sample data`` ()
