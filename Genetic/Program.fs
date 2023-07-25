module Genetic.Program

open Genetic.Types

let rnd = Genetic.API.Random.random

let opts = {populationSize = 100;
            randomFn = rnd}
let genotype () = [ for _ in 1 .. 1000 do rnd(2) ]
let fitnessFunction (xs: int list) = List.sum xs
let maxFitness = 1000

let soln = Core.run opts fitnessFunction genotype maxFitness

printfn "==========================================="
printfn $"{soln}"
