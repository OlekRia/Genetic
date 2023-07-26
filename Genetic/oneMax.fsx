#load "Types.fs"
#load "List.fs"
#load "Random.fs"
#load "Core.fs"

open Genetic.Types
    
let opts = {populationSize = 100;
            randomFn = Genetic.Random.random}

let problem =
    {genotype = fun () ->
                    let SIZE = 1000
                    {age = 0
                     size = SIZE
                     fitness = 0
                     genes = [ for _ in 1 .. SIZE do opts.randomFn(2) ]}
     fitnessFunction = fun chromosomes -> List.sum chromosomes.genes
     isTerminate = List.map (fun x -> x.fitness)
                    >> List.max
                    >> fun x -> x > 998}

let soln = Genetic.Core.run opts problem

printfn "==========================================="
printfn $"{soln}"