module Day01

open System.IO

let parse (x: string) = x.TrimEnd() |> uint

let count_increase (data: uint []) =
    data
    |> Array.pairwise
    |> Array.sumBy (fun (first, second) -> if first < second then 1 else 0)

let triple_sum_increase (data: uint []) =
    data
    |> Array.windowed 3
    |> Array.map (fun x -> Array.sum x)
    |> Array.pairwise
    |> Array.sumBy (fun (first, second) -> if first < second then 1 else 0)

let input =
    File.ReadLines("puzzle_input/day_01/part1")
    |> Array.ofSeq
    |> Array.map parse

let part_1 () = input |> count_increase |> printfn "%A"

let part_2 () =
    input |> triple_sum_increase |> printfn "%A"

let parts = (1, part_1, part_2)

module Tests =
    let testInput =
        ("199
    200
    208
    210
    200
    207
    240
    269
    260
    263")
            .Split '\n'
        |> Array.map parse

    let ``Example data works with count_increase`` () =

        let result = testInput |> count_increase

        assert (result = 7)

    let ``Example data works with triple_sum_increase`` () =
        let result = triple_sum_increase testInput
        assert (result = 5)

Tests.``Example data works with count_increase`` ()
Tests.``Example data works with triple_sum_increase`` ()
