module Day03

open System
open System.IO

let input =
    File.ReadAllLines("puzzle_input/day_03")
    |> Array.map (fun x -> x.Trim())

let calculateRates (a: string []) =
    let bitCounts = Array.zeroCreate (a.[0].Trim().Length)

    a
    |> Array.fold
        (fun (state) ->
            fun (current) ->
                current.ToCharArray()
                |> Array.mapi (fun i char -> state.[i] + (string >> int) char))
        bitCounts
    |> (fun (bitCounts) ->
        let inputLength = a.Length

        bitCounts
        |> Array.mapi (fun i x -> (i, x))
        |> Array.fold
            (fun (eps: int []) ->
                fun ((i, curr)) ->
                    let mutable epsEdit = eps
                    let mostCommon = (float curr) / (float inputLength)

                    epsEdit.[i] <- if mostCommon >= 0.5 then 1 else 0

                    epsEdit)
            bitCounts)

    |> (fun (eps) ->
        let flip (x: int []) =
            x |> Array.map (fun x -> if x = 1 then 0 else 1)

        let convert (x: int []) =
            x
            |> Array.map string
            |> String.concat ""
            |> (fun asString -> Convert.ToInt32(asString, 2))

        (convert eps, (flip >> convert) eps))

let calculateMostCommonBitAt (a: string []) (at: int) =
    a
    |> Array.map (fun line -> line.[at])
    |> Array.countBy id
    |> Map.ofArray
    |> fun map ->
        match Map.tryFind '0' map, Map.tryFind '1' map with
        | Some (_0), Some (_1) when _0 = _1 -> 1
        | Some (_0), Some (_1) -> if _0 > _1 then 0 else 1
        | Some (_0), None -> 0
        | None, Some (_1) -> 1
        | x -> failwithf "wtf %A" x

type Mode =
    | OxygenGenerator
    | C02Scrubber

let calcLifeSupportRating (a: string []) (common: Mode) =
    let mutable remaining = a
    let mutable currentBit = 0
    let bitCount = a.[0].Length

    while remaining.Length > 1 && currentBit < bitCount do
        let mostCommonBit =
            calculateMostCommonBitAt remaining currentBit
            |> (fun x ->
                if common = OxygenGenerator then
                    x
                else
                    (if x = 1 then 0 else 1))

            |> string
            |> fun x -> x.[0]

        remaining <-
            remaining
            |> Array.filter (fun line -> line.[currentBit] = mostCommonBit)

        currentBit <- currentBit + 1

    let rating = Convert.ToInt32(remaining.[0], 2)
    rating


let part_1 () =
    input
    |> calculateRates
    |> (fun (gamma, epsilon) -> gamma * epsilon)
    |> printfn "%A"

let part_2 () =
    input
    |> (fun x -> calcLifeSupportRating x OxygenGenerator, calcLifeSupportRating x C02Scrubber)
    |> (fun (oxygenGeneratorRating, c02ScrubberRating) -> oxygenGeneratorRating * c02ScrubberRating)
    |> printfn "%A"

let parts = (3, part_1, part_2)

module Test =
    let testInput =
        ("00100
        11110
        10110
        10111
        10101
        01111
        00111
        11100
        10000
        11001
        00010
        01010")
            .Split '\n'
        |> Array.map (fun x -> x.Trim())

    let ``part 1 sample data`` () =
        let (gammaRate, epsilonRate) = calculateRates testInput
        assert (gammaRate = 22)
        assert (epsilonRate = 9)
        assert (gammaRate * epsilonRate = 198)

    let ``part 2 sample data`` () =
        let oxygenGeneratorRating =
            calcLifeSupportRating testInput OxygenGenerator

        let c02ScrubberRating =
            calcLifeSupportRating testInput C02Scrubber

        assert (oxygenGeneratorRating = 23)
        assert (c02ScrubberRating = 10)
        assert (oxygenGeneratorRating * c02ScrubberRating = 230)

    let ``most common bit part 2 oxygen`` () =
        let result =
            calculateMostCommonBitAt [| "10110"; "10111" |] 4

        assert (result = 1)



Test.``part 1 sample data`` ()
Test.``most common bit part 2 oxygen`` ()
Test.``part 2 sample data`` ()
