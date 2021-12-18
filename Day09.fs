module Day09

open System.IO

let parse (input: string seq) =
    input
    |> Array.ofSeq
    |> Array.map (fun line ->
        line.ToCharArray()
        |> Array.map (fun c -> (uint c) - uint '0'))
    |> array2D

let findLocalMinima (input: uint [,]) =
    input
    |> Array2D.mapi (fun x y curr ->
        [ x - 1, y
          x + 1, y
          x, y - 1
          x, y + 1 ]
        |> List.forall (fun (x, y) ->
            try
                input.[x, y] > curr
            with
            | :? System.IndexOutOfRangeException -> true))


let part_1_ (input: uint [,]) =
    input
    |> findLocalMinima
    |> Array2D.mapi (fun x y on -> if on then 1u + input.[x, y] else 0u)
    |> Seq.cast<uint>
    |> Seq.sum

let surroundingPoints x y =
    set [ x - 1, y
          x + 1, y
          x, y - 1
          x, y + 1 ]

let part_2_ (input: uint [,]) =
    input
    |> findLocalMinima
    |> Array2D.mapi (fun x y on ->
        if on then
            (let mutable searched = set [ x, y ]
             let mutable remaining = surroundingPoints x y

             let mutable found = set [ x, y ]

             while remaining.Count <> 0 do
                 for item in remaining do
                     let (x, y) = item

                     if (Set.contains item searched) then
                         remaining <- Set.remove item remaining

                     if (try
                             input.[x, y] < 9u
                         with
                         | :? System.IndexOutOfRangeException -> false) then
                         found <- Set.add (x, y) found

                         remaining <-
                             Set.union remaining (Set.difference (surroundingPoints x y) (Set.union searched remaining))

                         remaining <- Set.remove item remaining

                     searched <- Set.add item searched

             found.Count)
        else
            0)
    |> Seq.cast<int>
    |> Seq.filter (fun x -> x > 0)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce ((*))

let input =
    File.ReadLines("puzzle_input/day_09") |> parse

let part_1 () = part_1_ input |> printfn "%A"

let part_2 () = part_2_ input |> printfn "%A"

let parts = (9, part_1, part_2)

module Tests =
    let testInput =
        ("2199943210
3987894921
9856789892
8767896789
9899965678")
            .Split '\n'
        |> parse

    let ``part 1 sample data`` () =
        testInput
        |> part_1_
        |> (fun result -> assert (result = 15u))

    let ``part 2 sample data`` () =
        testInput
        |> part_2_
        |> (fun result -> assert (result = 1134))

Tests.``part 1 sample data`` ()
Tests.``part 2 sample data`` ()
