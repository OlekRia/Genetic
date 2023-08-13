module Genetic.Types

type Options =
    { populationSize: int
      randomFn: int -> int }

type Chromosome<'a> =
    { genes: 'a list
      size: int
      fitness: float
      age: int }

type Problem<'a> =
    { genotype: unit -> Chromosome<'a>
      fitnessFunction: Chromosome<'a> -> float
      mutationFunction: list<'a> -> list<'a>
      isTerminate: Chromosome<'a> list -> bool }
