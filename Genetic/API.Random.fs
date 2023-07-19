module Genetic.API.Random

let random toValue =
    let r = new System.Random()
    r.Next(0, toValue)

let randomFrom1 x = 1 + random (x - 1)
