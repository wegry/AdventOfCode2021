module DayXX

open System.IO

let parse (input) = input |> Array.ofSeq

let input =
    File.ReadLines("puzzle_input/day_XX") |> parse

let part_1_ (input) = "stub"

let part_2_ (input) = "stub"

let part_1 () = input |> part_1_ |> printfn "%A"

let part_2 () = input |> part_2_ |> printfn "%A"

let parts = (System.Int32.MaxValue, part_1, part_2)

module Tests =
    let testInput = ("").Split '\n' |> parse

    let ``part 1 sample data`` () =
        testInput
        |> part_1_
        |> (fun result ->
#if DEBUG
            printfn "Part 1 sample data result: %A" result
#endif
            assert (result = "stub"))

    let ``part 2 sample data`` () =
        testInput
        |> part_2_
        |> (fun result ->
#if DEBUG
            printfn "Part 2 sample data result: %A" result
#endif
            assert (result = "stub"))

Tests.``part 1 sample data`` ()
Tests.``part 2 sample data`` ()
