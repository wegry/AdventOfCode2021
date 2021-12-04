module DayXX

open System.IO

let input =
    File.ReadLines("puzzle_input/day_XX")
    |> Array.ofSeq

let part_1 () = printfn "not hooked up"

let part_2 () = printfn "not hooked up"

let parts = (System.Int32.MaxValue, part_1, part_2)

module Tests =
    let testInput = ("").Split '\n'

    let ``part 1 sample data`` () = printfn "not hooked up"

    let ``part 2 sample data`` () = printfn "not hooked up"

Tests.``part 1 sample data`` ()
Tests.``part 2 sample data`` ()
