#load "src/Types.fs"
#load "src/List.fs"
#load "src/Random.fs"
#load "src/Core.fs"

open Genetic.Types

let opts =
    { populationSize = 100
      randomFn = Genetic.Random.random }

let problem =
    { genotype =
        fun () ->
            let SIZE = 1000

            { age = 0
              size = SIZE
              fitness = 0
              genes =
                [ for _ in 1..SIZE do
                      opts.randomFn (2) ] }
      fitnessFunction = fun chromosomes -> List.sum chromosomes.genes |> float
      mutationFunction = Genetic.List.shuffle
      isTerminate =
        List.map (fun x -> x.fitness)
        >> List.max
        >> fun x -> x > 999 }

let solution = Genetic.Core.run opts problem

printfn "==========================================="
printfn $"{solution}"
