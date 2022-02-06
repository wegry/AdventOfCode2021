module Day12

open System.IO

type Size =
    | Large
    | Small

type CaveT = { name: string; size: Size }

type Node =
    | Start
    | End
    | Cave of CaveT
    override this.ToString() =
        match this with
        | Start
        | End -> sprintf "%A" this
        | Cave (c) -> c.name

    static member parse =
        function
        | "start" -> Start
        | "end" -> End
        | cave ->
            let firstUppercase = cave.[0] <= 'Z' && cave.[0] >= 'A'

            Cave
                { name = cave
                  size = if firstUppercase then Large else Small }


let parse (input: string seq) =
    let parsed =
        input
        |> Seq.map (fun line ->
            match line.Split '-' with
            | [| start; end_ |] -> (Node.parse start, Node.parse end_)
            | x -> failwithf "invalid line %A" x)

    seq {
        yield! (parsed |> Seq.filter (fun x -> snd x <> Start))

        yield!
            (parsed
             |> Seq.map (fun (x, y) -> y, x)
             |> Seq.filter (fun x -> fst x <> End))
    }
    |> Seq.groupBy fst
    |> Array.ofSeq
    |> Array.map (fun (k, v) -> k, v |> Seq.map snd |> Set.ofSeq)
    |> Map.ofSeq

let input =
    File.ReadLines("puzzle_input/day_12") |> parse

type Graph = Map<Node, Set<Node>>

let rec getPossiblePaths (graph: Graph) (ancestors: List<Node>) (current: Node) : Option<Set<List<Node>>> =
    let currentPath = List.append ancestors [ current ]

    let cannotVisitAgain =
        match current with
        | Cave (c) -> c.size = Small
        | Start
        | End -> true

    if current = End then
        [ currentPath ] |> Set.ofList |> Some
    else if cannotVisitAgain
            && List.contains current ancestors then
        None
    else
        graph.TryFind current
        |> Option.defaultValue Set.empty
        |> Set.map (fun x ->
            let res = getPossiblePaths graph currentPath x
            let unwrapped = res |> Option.defaultValue Set.empty
            unwrapped)
        |> Set.unionMany
        |> Some

let part_1_ (input: Graph) =
    let possiblePaths = getPossiblePaths input [] Start

    possiblePaths
    |> Option.defaultValue Set.empty
    |> Set.count

let tee x =
    printfn "%A" x
    x


type PartII = Set<List<Node>>
type PartIIResult = Option<PartII>
// https://stackoverflow.com/a/3461907/1924257

let mutable invalidAncestors: PartII = Set.empty

let rec getPossiblePaths2 (graph: Graph) (ancestors: List<Node>) (current: Node) : PartIIResult =
    let currentPath = List.append ancestors [ current ]

    if (invalidAncestors |> Set.contains currentPath) then
        None

    else
        (let cannotVisitAgain =
            match current with
            | Cave (c) when
                c.size = Small
                && currentPath.Length > 2
                && (currentPath
                    |> List.choose (function
                        | Cave (ic) when ic.size = Small -> Some ic.name
                        | _ -> None)
                    |> List.countBy id
                    |> Map.ofList
                    |> Map.partition (fun _ v -> v >= 2)
                    |> fst
                    |> Map.count
                    |> (fun x -> x > 1))
                ->
                true
            | Start when currentPath.Length > 1 -> true
            | End -> true
            | _ -> false

         if current = End then
             [ currentPath ] |> Set.ofList |> Some
         else if cannotVisitAgain then
             invalidAncestors <- Set.add currentPath invalidAncestors
             None
         else
             current
             |> graph.TryFind
             |> Option.defaultValue Set.empty
             |> Set.map (fun x ->
                 let res = getPossiblePaths2 graph currentPath x
                 let unwrapped = res |> Option.defaultValue Set.empty
                 unwrapped)
             |> Set.unionMany
             |> Some)

let part_2_ (input) =
    // let possiblePaths = getPossiblePaths2 input [] Start

    // possiblePaths
    // |> Option.defaultValue Set.empty
    // |> Set.count
    printfn "Not implemented"
    0


let part_1 () = input |> part_1_ |> printfn "%A"

let part_2 () = input |> part_2_ |> printfn "%A"

let parts = (12, part_1, part_2)

module Tests =
    let rawInput =
        "start-A
start-b
A-c
A-b
b-d
A-end
b-end"

    let testInput = rawInput.Split '\n' |> parse

    let ``part 1 sample data`` () =
        testInput
        |> part_1_
        |> (fun result ->
#if DEBUG
            printfn "Part 1 sample data result: %A" result
#endif
            assert (result = 10))

    let ``part 2 sample data`` () = printfn "Not implemented"
//         testInput
//         |> part_2_
//         |> (fun result ->
// #if DEBUG
//             printfn "Part 2 sample data result: %A" result
// #endif
//             assert (result = 36))

Tests.``part 1 sample data`` ()
Tests.``part 2 sample data`` ()
