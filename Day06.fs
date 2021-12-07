module Day06

open System.IO

let parse (s: string) = s.Split ',' |> Array.map uint64

let input =
    File.ReadAllText("puzzle_input/day_06") |> parse

let fishSpawner (input: uint64 []) (days: int) =
    let mutable compressed =
        input
        |> Array.countBy uint64
        |> Map.ofArray
        |> (fun x ->
            [ 0UL .. 8UL ]
            |> List.map (fun i ->
                Map.tryFind i x
                |> Option.map (uint64)
                |> Option.defaultValue 0UL))
        |> Array.ofList

    for _ in 1 .. days do
        let mutable zeroed = compressed.[0]

        for key in [ 0 .. 8 ] do
            if key = 0 then
                ()
            else
                compressed.[key - 1] <- compressed.[key]
                ()
            |> ignore

        compressed.[6] <- compressed.[6] + zeroed
        compressed.[8] <- zeroed

    compressed |> Array.sum


let part_1 () = fishSpawner input 80 |> printfn "%A"

let part_2 () = fishSpawner input 256 |> printfn "%A"

let parts = (6, part_1, part_2)

module Tests =
    let testInput = ("3,4,3,1,2") |> parse

    let ``part 1 sample data`` () =
        let fishCount = fishSpawner testInput 18
        assert (fishCount = 26UL)

    let ``part 2 sample data`` () =
        let fishCount = fishSpawner testInput 256
        assert (fishCount = 26984457539UL)


Tests.``part 1 sample data`` ()
Tests.``part 2 sample data`` ()
