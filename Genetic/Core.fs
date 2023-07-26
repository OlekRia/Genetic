module Genetic.Core

open Genetic.Types

let private initialize opts (genotype: unit -> Chromosome) =
    let size = opts.populationSize
    [ for _ in 1 .. (if size < 1 then 100 else size) do genotype() ]

let private evaluate opts fitnessFunction population =
    population
    |> List.map (fun gene -> {gene with
                               age = gene.age + 1
                               fitness = fitnessFunction gene})
    |> List.sortByDescending (fun x -> x.fitness)

let private select opts (population: Chromosome list): (Chromosome * Chromosome) list =
    let rec loop acc = function
    | x::y::rest -> loop ((x, y)::acc) rest
    | _ -> List.rev acc
    loop [] population

let private crossover opts =
    (List.map
        (fun (a, b) ->
            let splitAt = min a.genes b.genes |> List.length |> opts.randomFn |> List.splitAt
            let (x1,y1), (x2,y2) = splitAt a.genes, splitAt b.genes
            let res =  { a with genes = x1 @ y2}, { b with genes = y1 @ x2}
            res)
    >> List.collect (function (list1, list2) -> [list1; list2]))
    
let private mutation opts =
    List.map (fun (chromosome: Chromosome) ->
                if opts.randomFn(100) < 5 then
                    {chromosome with genes = List.shuffle chromosome.genes}
                else
                    chromosome)

let private evolve opts (problem: Problem) population =
    let mutable currentPopulation = population
    let rec loop () =
        currentPopulation <- (evaluate opts problem.fitnessFunction currentPopulation)
        let best = List.head currentPopulation
        printfn $"Current Best: [{best.fitness}]"
        if problem.isTerminate currentPopulation then
            best
        else
            currentPopulation
                <- currentPopulation
                |> select opts
                |> crossover opts
                |> mutation opts
            loop ()
    loop ()

let run (opts: Options) (problem: Problem) =
    initialize opts problem.genotype
    |> evolve opts problem 
