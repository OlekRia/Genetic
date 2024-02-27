#load "src/Types.fs"
#load "src/List.fs"
#load "src/Fn.Strings.fs"
#load "src/Random.fs"
#load "src/Core.fs"

open Genetic.Types
open Genetic.Strings

let opts =
    { populationSize = 100
      randomFn = Genetic.Random.random }

let SIZE = 34

let problem =
    { genotype =
        fun () ->
            { age = 0
              size = SIZE
              fitness = 0.0
              genes = Seq.toList (generateRandomString SIZE) }
      fitnessFunction =
        fun chromosomes ->
            let target = "supercalifragilisticexpialidocious"
            let guess = chromosomes.genes |> List.toArray |> System.String
            levenshteinDistance target guess
      mutationFunction = fun x -> Seq.toList (generateRandomString SIZE)
      isTerminate =
        List.map (fun x -> x.fitness)
        >> List.max
        >> fun x -> x < 33 }

let soln = Genetic.Core.run opts problem

printfn "==========================================="
printfn $"{soln}"
