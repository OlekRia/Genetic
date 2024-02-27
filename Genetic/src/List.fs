module Genetic.List

let rnd = System.Random()

let shuffle list =
    let n = List.length list
    List.sortBy (fun _ -> rnd.Next(n)) list
