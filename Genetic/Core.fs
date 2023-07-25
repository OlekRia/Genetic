module Genetic.Core

open Genetic.Types

let initialize opts (genotype: unit -> int list) =
    let size = opts.populationSize
    [ for _ in 1 .. (if size < 1 then 100 else size) do genotype() ]

let evaluate opts fitnessFunction =
    List.sortByDescending (fitnessFunction: int list -> int)

let select opts population =
    let rec loop acc = function
    | x::y::rest -> loop ((x, y)::acc) rest
    | _ -> List.rev acc
    loop [] population

let crossover opts =
    List.map
       (fun (a, b) ->
           let splitAt = min a b |> List.length |> opts.randomFn |> List.splitAt
           let (x1,y1), (x2,y2) = splitAt a, splitAt b
           let res = (x1 @ y2), (y1 @ x2)
           res)
    >> List.collect (function (list1, list2) -> [list1; list2])

let mutation opts =
    List.map (fun chromosome ->
                if opts.randomFn(100) < 5 then
                    Genetic.List.shuffle chromosome
                else
                    chromosome)

let evolve opts fitnessFunction maxFitness population =
    let mutable currentPopulation = evaluate opts fitnessFunction population
    let rec loop () =
        let best = List.head currentPopulation
        printfn $"Current Best: [{List.sum best}] {best}"
        if fitnessFunction(best) >= maxFitness then
            best
        else
            currentPopulation
                <- currentPopulation
                |> evaluate opts fitnessFunction
                |> select opts
                |> crossover opts
                |> mutation opts
            loop ()
    loop ()

let run (opts: Options)
        fitnessFunciton
        (genotype: (unit -> int list))
        maxFitness =
            let population = initialize opts genotype
            evolve opts fitnessFunciton maxFitness population
