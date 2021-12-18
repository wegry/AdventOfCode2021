module Day10

open System.IO
open System.Collections.Generic

let parse (input) = input |> Array.ofSeq

type Mode =
    | Open
    | Close

type Token =
    | Paren
    | Square
    | Curly
    | Angle

type Status =
    | Corrupted of int * Token
    | Incomplete
    | Parsing
    | Complete
    | Incomplete2 of Stack<Mode * Token>


let tokenize (s: string) =
    s.ToCharArray()
    |> Seq.toArray
    |> Array.map (fun c ->
        match c with
        | '(' -> Open, Paren
        | ')' -> Close, Paren
        | '[' -> Open, Square
        | ']' -> Close, Square
        | '{' -> Open, Curly
        | '}' -> Close, Curly
        | '<' -> Open, Angle
        | '>' -> Close, Angle
        | x -> failwithf "%A not a valid token" x)

let input =
    File.ReadLines("puzzle_input/day_10") |> parse

let part_1_ (input) =
    input
    |> Array.map (fun line ->
        let stack = new Stack<(Mode * Token)>()

        seq {
            for (i, token) in (line |> tokenize |> (Array.mapi (fun i x -> i, x))) do
                let maybeTop =
                    if stack.Count > 0 then
                        stack.Peek() |> Some
                    else
                        None

                yield
                    match token, maybeTop with
                    | (Close, x), Some (Open, y) when x = y ->
                        stack.Pop() |> ignore
                        Parsing
                    | (Close, x), Some (Open, y) when x <> y -> Corrupted(i, x)
                    | _ ->
                        stack.Push(token)
                        Parsing

            yield Incomplete
        }
        |> Seq.find (fun x ->
            match x with
            | Corrupted (_, _) -> true
            | Incomplete -> true
            | _ -> false))
    |> Array.mapi (fun i x -> i, x)
    |> Array.sumBy (fun (i, x) ->
        match x with
        | Corrupted (_, token) ->
            match token with
            | Paren -> 3
            | Square -> 57
            | Curly -> 1197
            | Angle -> 25137
        | _ -> 0)


let part_2_ (input) =
    input
    |> Array.map (fun line ->
        let stack = new Stack<(Mode * Token)>()

        seq {
            for (i, token) in (line |> tokenize |> (Array.mapi (fun i x -> i, x))) do
                let maybeTop =
                    if stack.Count > 0 then
                        stack.Peek() |> Some
                    else
                        None

                yield
                    match token, maybeTop with
                    | (Close, x), Some (Open, y) when x = y ->
                        stack.Pop() |> ignore
                        Parsing
                    | (Close, x), Some (Open, y) when x <> y -> Corrupted(i, x)
                    | _ ->
                        stack.Push(token)
                        Parsing

            yield
                if stack.Count = 0 then
                    Complete
                else
                    Incomplete2 stack
        }
        |> Seq.tryFind (fun x ->
            match x with
            | Corrupted (_) -> true
            | Incomplete2 (s) -> true
            | _ -> false))
    |> Seq.choose (fun x ->
        match x with
        | Some (Incomplete2 (_)) -> x
        | _ -> None)
    |> Seq.toArray
    |> Array.map (fun status ->
        match status with
        | Incomplete2 (s) ->
            seq {
                while s.Count > 0 do
                    match s.Pop() with
                    | (_, token) ->
                        match token with
                        | Paren -> 1uL
                        | Square -> 2uL
                        | Curly -> 3uL
                        | Angle -> 4uL
            }
            |> Seq.reduce (fun acc curr -> (acc * 5uL) + curr)
        | _ -> 0uL)
    |> Array.sort
    |> (fun a -> a.[(a.Length - 1) / 2])

let part_1 () = input |> part_1_ |> printfn "%A"

let part_2 () = input |> part_2_ |> printfn "%A"

let parts = (10, part_1, part_2)

module Tests =
    let testInput =
        ("[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")
            .Split '\n'

    let ``part 1 sample data`` () =
        testInput
        |> part_1_
        |> (fun result ->
#if DEBUG
            printfn "%A" result
#endif
            assert (result = 26397))

    let ``part 2 sample data`` () =
        testInput
        |> part_2_
        |> (fun result ->
#if DEBUG
            printfn "%A" result
#endif
            assert (result = 288957uL))

Tests.``part 1 sample data`` ()
Tests.``part 2 sample data`` ()
