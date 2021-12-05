module Day05

open System.IO
open System.Text.RegularExpressions

type Point = (int * int)
type Line = Point * Point

type Dim =
    | X
    | Y

let maxPoint (dim: Dim) (line: Line) =
    let accessWith =
        match dim with
        | X -> fst
        | Y -> snd

    match ((fst >> accessWith) line), ((snd >> accessWith) line) with
    | (d1, d2) when d1 > d2 -> d1
    | (d1, d2) when d1 < d2 -> d2
    | (d1, _) -> d1


let linePattern = Regex "^(\d+),(\d+) +-> +(\d+),(\d+)$"

let parse (line: string) : Line =
    let match_ = linePattern.Match line

    match match_.Groups
          |> List.ofSeq
          // first item matches the whole line
          |> List.skip 1
          |> List.map (fun x -> x.Value.Trim() |> int)
        with
    | [ x1; y1; x2; y2 ] -> (x1, y1), (x2, y2)
    | x -> failwithf "No match %A" x

let input =
    File.ReadLines("puzzle_input/day_05")
    |> Array.ofSeq
    |> Array.map parse

let twoWayRange a b =
    if a < b then
        [ a .. b ]
    else
        [ a .. -1 .. b ]

let genIntermediatePoints (allowDiagonal: bool) (line: Line) =
    match line with
    | (x1, y1), (x2, y2) when x1 = x2 -> twoWayRange y1 y2 |> List.map (fun y -> x1, y)
    | (x1, y1), (x2, y2) when y1 = y2 -> twoWayRange x1 x2 |> List.map (fun x -> x, y1)
    | (x1, y1), (x2, y2) when allowDiagonal -> List.zip (twoWayRange x1 x2) (twoWayRange y1 y2)
    | skip ->
        // printfn "skipping %A" skip
        []

let countOverlap (allowDiagonal: bool) (lines: Line []) =
    let maxX =
        lines |> Array.maxBy (maxPoint X) |> maxPoint X

    let maxY =
        lines |> Array.maxBy (maxPoint Y) |> maxPoint Y

    let mutable oceanFloor = Array2D.zeroCreate (maxX + 1) (maxY + 1)

    for line in lines do
        let intermediatePoints = genIntermediatePoints allowDiagonal line

        for (xi, yi) in intermediatePoints do
            oceanFloor.[xi, yi] <- oceanFloor.[xi, yi] + 1

    // https://stackoverflow.com/a/70235461/1924257
    // for y in 0 .. oceanFloor.GetLength(1) - 1 do
    //     printfn
    //         "%s"
    //         (oceanFloor.[*, y]
    //          |> Array.map (fun x -> if x = 0 then "." else string x)
    //          |> String.concat "")

    oceanFloor
    |> Seq.cast<int>
    |> Seq.filter (fun x -> x >= 2)
    |> Seq.length

let part_1 () =
    input |> countOverlap false |> printfn "%A"

let part_2 () =
    input |> countOverlap true |> printfn "%A"

let parts = (5, part_1, part_2)

module Tests =
    let testInput =
        ("0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")
            .Split '\n'
        |> Array.map parse

    let ``part 1 sample data`` () =
        let overlapCount = countOverlap false testInput
        assert (overlapCount = 5)

    let ``part 2 sample data`` () =
        let overlapCount = countOverlap true testInput
        assert (overlapCount = 12)

Tests.``part 1 sample data`` ()
Tests.``part 2 sample data`` ()
