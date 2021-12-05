module Day04

open System.IO
open FSharp.Collections

type Part1 =
    { calledOrder: uint []
      boards: uint [,,]
      marked: bool [,,] }

let parse (a: string []) : Part1 =
    let calledOrder = a.[0].Split ',' |> Array.map uint
    let theRestOfTheFile = a |> Array.skip 2

    let boardCount =
        theRestOfTheFile
        |> Array.fold
            (fun boardCount curr ->
                if curr = "" then
                    boardCount + 1
                else
                    boardCount)
            1

    let (boards, _, _) =
        theRestOfTheFile
        |> Array.map (fun x ->
            x.Split(' ')
            |> Array.choose (fun x ->
                if x = "" then
                    None
                else
                    (Some << uint) x))
        |> fun x -> ((Array3D.zeroCreate 5 5 boardCount, 0, 0), x)
        ||> Array.fold (fun (acc, row, board) line ->
            let mutable newAcc = acc

            if line.Length = 0 then
                (acc, 0, board + 1)
            else
                (line
                 |> Array.iteri (fun col num -> newAcc.[col, row, board] <- uint num)

                 newAcc, row + 1, board))

    { calledOrder = calledOrder
      boards = boards
      marked = Array3D.init 5 5 boardCount (fun _ _ _ -> false) }

let possibleSequences () =
    seq {
        for x in [ 0 .. 4 ] do
            // vertical
            yield
                seq {
                    for y in [ 0 .. 4 ] do
                        yield x, y
                }
                |> Array.ofSeq

            // horizontal
            yield
                seq {
                    for y in [ 0 .. 4 ] do
                        yield y, x
                }
                |> Array.ofSeq
    }

let checkForBingo (marked: bool [,]) =
    possibleSequences ()
    |> Seq.exists (fun s -> s |> Seq.forall (fun (x, y) -> marked.[x, y]))

let input =
    File.ReadLines("puzzle_input/day_04")
    |> Array.ofSeq
    |> parse

let sumUpBoard (board: uint [,]) (marked: bool [,]) (lastNumber: uint) =
    board
    |> Array2D.mapi (fun x y value -> if marked.[x, y] then 0u else value)
    |> Seq.cast<uint>
    |> Seq.sum
    |> (fun total -> total * lastNumber)

let firstBingo (part1: Part1) =
    let mutable boards = part1.boards
    let mutable markings = part1.marked

    let (board, markings, lastNumberCalled) =
        seq {
            for num in part1.calledOrder do
                for i in [ 0 .. (boards.GetLength(2)) - 1 ] do
                    let maybeIndex =
                        markings.[*, *, i]
                        |> Array2D.mapi (fun x y value -> if value then None else Some(x, y))
                        |> Seq.cast<(int * int) option>
                        |> Seq.choose id
                        |> Seq.tryFind (fun (x, y) -> boards.[x, y, i] = num)

                    match maybeIndex with
                    | Some (x, y) ->
                        markings.[x, y, i] <- true

                        yield boards.[*, *, i], markings.[*, *, i], num
                    | None -> ()

        }
        |> Seq.find (fun (board, markings, num) -> checkForBingo markings)

    sumUpBoard board markings lastNumberCalled

let lastBingo (input: Part1) =
    let mutable boards = input.boards
    let mutable markings = input.marked
    let mutable noBingoYet: Set<int> = set [ 0 .. boards.GetLength(2) - 1 ]

    let (board, markings, lastNumberCalled) =
        seq {
            for num in input.calledOrder do
                for i in noBingoYet do
                    let maybeIndex =
                        markings.[*, *, i]
                        |> Array2D.mapi (fun x y value -> if value then None else Some(x, y))
                        |> Seq.cast<(int * int) option>
                        |> Seq.choose id
                        |> Seq.tryFind (fun (x, y) -> boards.[x, y, i] = num)

                    match maybeIndex with
                    | Some (x, y) ->
                        markings.[x, y, i] <- true

                        let isBingo = markings.[*, *, i] |> checkForBingo

                        if Set.count noBingoYet = 1 then
                            yield (boards.[*, *, i], markings.[*, *, i], num)

                        if isBingo then
                            noBingoYet <- Set.remove i noBingoYet
                        else
                            ()
                    | None -> ()
        }
        |> Seq.find (fun (board, markings, num) -> checkForBingo markings)

    sumUpBoard board markings lastNumberCalled

let part_1 () = firstBingo input |> printfn "%A"


let part_2 () = lastBingo input |> printfn "%A"

let parts = (4, part_1, part_2)

module Tests =
    let testInput =
        ("7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

        22 13 17 11  0
        8  2 23  4 24
        21  9 14 16  7
        6 10  3 18  5
        1 12 20 15 19

        3 15  0  2 22
        9 18 13 17  5
        19  8  7 25 23
        20 11 10 24  4
        14 21 16 12  6

        14 21 17 24  4
        10 16 15  9 19
        18  8 23 26 20
        22 11 13  6  5
        2  0 12  3  7")
            .Split '\n'
        |> parse

    let ``test checkForBingo`` () =
        let notABingo =
            array2D [ [ 1; 0; 0; 0; 1 ]
                      [ 0; 1; 0; 1; 0 ]
                      [ 0; 0; 1; 0; 0 ]
                      [ 0; 0; 0; 0; 0 ]
                      [ 0; 0; 0; 0; 0 ] ]
            |> Array2D.map (fun x -> x = 1)

        let aBingo =
            array2D [ [ 1; 0; 0; 0; 1 ]
                      [ 1; 1; 0; 1; 0 ]
                      [ 1; 0; 1; 0; 0 ]
                      [ 1; 0; 0; 0; 0 ]
                      [ 1; 0; 0; 0; 0 ] ]
            |> Array2D.map (fun x -> x = 1)

        assert (checkForBingo notABingo = false)
        assert (checkForBingo aBingo = true)

    let ``part 1 sample data`` () =
        let sum = firstBingo testInput
        assert (sum = 4512u)

    let ``part 2 sample data`` () =
        let sum = lastBingo testInput
        assert (sum = 1924u)

Tests.``part 1 sample data`` ()
Tests.``part 2 sample data`` ()
