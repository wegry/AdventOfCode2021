module Day07

open System.IO

let parse (s: string) =
    s.Split ',' |> Array.map int |> Array.sort

let bestFuelEfficiency (positionScaling: int -> int) (crabs: int []) =
    // assume sorted
    let max = crabs |> Array.last
    let min = crabs |> Array.head

    // assume non-negative
    let mutable tracking = Array.create (max + 1) None

    for currentPosition in [ min .. max ] do
        tracking.[currentPosition] <-
            crabs
            |> Array.sumBy (fun (pos: int) -> abs (pos - currentPosition) |> positionScaling)
            |> Some

    tracking
    |> Array.mapi (fun i value -> i, value)
    |> Array.collect (fun (i, opt) ->
        match opt with
        | None -> [||]
        | Some (x) -> [| i, x |])
    |> Array.minBy snd
    |> snd

let mutable scaling2: Map<int, int> = Map.ofList []

open System.Collections.Generic

// https://stackoverflow.com/a/3461907/1924257
let cache = Dictionary<int, int>()

let rec part2Scaling (x: int) =
    match x with
    | 0
    | 1 -> x
    | _ ->
        match cache.TryGetValue(x) with
        | true, value -> value
        | _, _ ->
            let intermediate = x + part2Scaling (x - 1)
            cache.[x] <- intermediate
            intermediate


let input =
    File.ReadAllText("puzzle_input/day_07") |> parse

let part_1 () =
    input |> bestFuelEfficiency id |> printfn "%A"

let part_2 () =
    input
    |> bestFuelEfficiency part2Scaling
    |> printfn "%A"


let parts = (System.Int32.MaxValue, part_1, part_2)

module Tests =
    let testInput = ("16,1,2,0,4,2,7,1,2,14") |> parse

    let ``part 1 sample data`` () =
        let best = bestFuelEfficiency id testInput

        assert (best = 37)

    let ``part 2 sample data`` () =
        let best =
            bestFuelEfficiency part2Scaling testInput

        assert (best = 168)

Tests.``part 1 sample data`` ()
Tests.``part 2 sample data`` ()
