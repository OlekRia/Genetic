module Genetic.Types

type Options = {populationSize: int
                randomFn: int -> int}

type Chromosome = {genes: int list
                   size: int
                   fitness: int
                   age: int }

type Problem = {genotype: unit -> Chromosome
                fitnessFunction: Chromosome -> int
                isTerminate: Chromosome list -> bool}