module Day08

open System.IO

let parse (line: string) =
    let (signalPattern, outputValue) =
        match line.Split " | " with
        | [| signalPattern; outputValue |] -> signalPattern.Trim(), outputValue.Trim()
        | invalid -> failwithf "invalid line %A" invalid

    let decompose (pattern: string) =
        pattern.Split ' '
        |> Array.map (fun x -> x.ToCharArray() |> Set.ofArray)

    decompose signalPattern, decompose outputValue


let part1Patterns =
    [ 0, "abc efg"
      1, "  c  f " // 2
      2, "a cde g"
      3, "a cd fg"
      4, " bcd f " // 4
      5, "ab d fg"
      6, "ab defg"
      7, "a c  f " // 3
      8, "abcdefg" // 7
      9, "abcd fg" ]
    |> List.map (fun (n, seg) ->
        seg.ToCharArray()
        |> Set.ofSeq
        |> (fun x -> Set.remove ' ' x),
        n)
    |> Map.ofList

let part2Patterns =
    part1Patterns
    |> Map.toArray
    |> Array.map (fun t -> snd t, fst t)
    |> Map.ofArray

type Wire = Set<char>
type Wires = Wire []
type Input = (Wires * Wires) []

let part_1_ (input: Input) =
    let lookingFor = [ 1; 4; 7; 8 ] |> Set.ofList

    input
    |> Array.map (fun (signals, output) ->
        let sum =
            output
            |> Array.sumBy (fun obfuscated ->
                match obfuscated.Count with
                | 2
                | 3
                | 4
                | 7 -> 1
                | _ -> 0)

        sum)
    |> Array.sum

let findUniqueLengths (wires: Wires) =
    wires
    |> Array.map (fun wire ->
        wire,
        match wire.Count with
        | 2 -> Some(1)
        | 3 -> Some(7)
        | 4 -> Some(4)
        | 7 -> Some(8)
        | _ -> None)
    |> Array.collect (fun (scrambled, maybeDecoded) ->
        maybeDecoded
        |> Option.map (fun n -> [| (n, scrambled) |])
        |> Option.defaultValue [||])

let mapSignals (input: Map<int, Wire>) =
    match input.TryFind 1, input.TryFind 4, input.TryFind 7, input.TryFind 8 with
    | Some (_1), Some (_4), Some (_7), Some (_8) ->
        let a =
            _7 |> Set.difference _4 |> Set.toSeq |> Seq.head

        let d =
            _4
            |> Set.difference _1
            |> Set.difference _7
            |> Set.toSeq
            |> Seq.head

        [ 'a', a; 'd', d ]
    | x -> failwithf "Map with nothing...%A" x



// Needed a hint on reddit...
let part_2_ (input: Input) = None
// |> Array.map (fun (signals, output) -> signals |> findUniqueLengths |> mapSignals)

let input =
    File.ReadLines("puzzle_input/day_08")
    |> Array.ofSeq
    |> Array.map parse

let part_1 () = input |> part_1_ |> printfn "%A"

let part_2 () =
    input |> part_2_ |> printfn "Not finished %A"

let parts = (8, part_1, part_2)

module Tests =
    let testInput =
        ("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")
            .Split '\n'
        |> Array.map parse

    let ``part 1 sample data`` () =
        let result = part_1_ testInput
        printfn "%A" result
        assert (result = 26)

    let ``part 2 sample data`` () = printfn "not hooked up"

Tests.``part 1 sample data`` ()
Tests.``part 2 sample data`` ()
