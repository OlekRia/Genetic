module Genetic.Strings

open System.Collections.Generic
open Genetic
open Genetic

let private alphabet = "abcdefghijklmnopqrstuvwxyz"

let random = System.Random()

let generateRandomString length =
    let stringBuilder = System.Text.StringBuilder()

    for i in 1..length do
        let randomIndex = random.Next(0, alphabet.Length)

        stringBuilder.Append(alphabet[randomIndex])
        |> ignore

    stringBuilder.ToString()

let levenshteinDistance (s1: string) (s2: string) : int =
    let mutable d = Array2D.create (s1.Length + 1) (s2.Length + 1) 0

    for i in 0 .. s1.Length do
        d.[i, 0] <- i

    for j in 0 .. s2.Length do
        d.[0, j] <- j

    for i in 1 .. s1.Length do
        for j in 1 .. s2.Length do
            let cost = if s1.[i - 1] = s2.[j - 1] then 0 else 1
            d.[i, j] <- min (d.[i - 1, j] + 1) (min (d.[i, j - 1] + 1) (d.[i - 1, j - 1] + cost))

    d.[s1.Length, s2.Length]
