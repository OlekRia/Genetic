module Genetic.Program

let evaluate  = List.sortByDescending List.sum

let selection (population: int list list): (int list * int list) list  =
    let rec loop acc = function
    | x::y::rest -> loop ((x, y)::acc) rest
    | _ -> List.rev acc

    loop [] population

let crossover random =
     List.map
        (fun (a, b) ->
            let splitAt = min a b |> List.length |> random |> List.splitAt
            let (x1,y1), (x2,y2) = splitAt a, splitAt b
            let res = (x1 @ y2), (y1 @ x2)
            res)
     >> List.collect (function (list1, list2) -> [list1; list2])



let rec algorithm population =
    let best =  List.maxBy List.sum population
    printfn $"Current Best: [{List.sum best}] {best}"
    
    if (List.sum best) > 31 then
        printf $"YO: {List.sum best}"
        best
    else
        population
        |> evaluate
        |> selection
        |> (crossover Genetic.API.Random.random)
        |> algorithm


let test = [[10; 11; 3]; [7; 8; 9]; [4; 5; 6]; [1; 2; 12]]
algorithm test



