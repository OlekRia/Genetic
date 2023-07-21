module Genetic.Program

open Genetic.Business

let rnd = Genetic.API.Random.random
let genotype () = [ for _ in 1 .. 1000 do rnd(2) ]
let fitnessFunction (xs: int list) = List.sum xs
let maxFitness = 999
let opts = {populationSize = 100;
            randomFn = rnd}

let soln = Business.run opts fitnessFunction genotype maxFitness

printfn "==========================================="
printfn $"{soln}"
