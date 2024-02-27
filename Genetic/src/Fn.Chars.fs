module Genetic.Chars

let intToBinaryArray (value: int) : int array =
    let mutable tempValue = value
    let mutable bits = []
    while tempValue > 0 do
        let bit = tempValue % 2
        bits <- bit :: bits
        tempValue <- tempValue / 2
    bits |> Array.ofList

let intToPaddedBinaryArray (value: int, length: int) : int array =
    let binaryArray = intToBinaryArray value
    let padding = max 0 (length - Array.length binaryArray)
    let paddedArray = Array.append (Array.create padding 0) binaryArray
    paddedArray

let valMap fn =
    "abcdefghijklmnopqrstuvwxyz"
    |> Seq.indexed
    |> Seq.map fn
    |> Map.ofSeq

let CHARMAP = valMap (fun (index, char) -> char, (intToPaddedBinaryArray (index, 5) ))
    
let ARRMAP = valMap (fun (index, char) -> (intToPaddedBinaryArray (index, 5), char ))

let charToBinaryIntArray (symbol: char) : int array =
    match (Map.tryFind symbol CHARMAP) with
    | Some x -> x
    | None -> [|0;0;0;0;0|]

let binaryArrayToChar (intArray: int array) : char =
    match (Map.tryFind intArray ARRMAP) with
    | Some x -> x
    | None -> 'a'
