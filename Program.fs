type Day = int * (unit -> unit) * (unit -> unit)

let Days: Day [] =
    [| Day01.parts
       Day02.parts
       Day03.parts
       Day04.parts
       Day05.parts
       Day06.parts
       Day07.parts
       Day08.parts
       Day09.parts
       Day10.parts
       Day11.parts
       Day12.parts
       Day13.parts |]

// https://stackoverflow.com/a/4646066/1924257
let duration f =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f ()
    printfn "Elapsed Time: %A" timer.Elapsed
    returnValue

// For more information see https://aka.ms/fsharp-console-apps
Days
|> Array.iter (fun (day, part_1, part_2) ->
    printfn "\nDay %i" day
    printfn "Part 1"
    duration part_1
    printfn "\nPart 2"
    duration part_2)
