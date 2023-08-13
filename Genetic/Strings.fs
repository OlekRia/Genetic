module Genetic.Strings

let private alphabet = "abcdefghijklmnopqrstuvwxyz"

let random = System.Random()

let generateRandomString length =
    let stringBuilder = System.Text.StringBuilder()
    for i in 1 .. length do
        let randomIndex = random.Next(0, alphabet.Length)
        stringBuilder.Append(alphabet[randomIndex]) |> ignore
    stringBuilder.ToString()
        
let jaroDistance (s1: string) (s2: string) : float =
    let s1Length = s1.Length
    let s2Length = s2.Length

    // Define the matching distance: max string length / 2 - 1
    let matchDistance = max (s1Length / 2) (s2Length / 2) - 1

    // Helper function to count the number of characters that match with a given match distance
    let rec countMatches (s1: string) (s2: string) (matchDistance: int) (index1: int) (index2: int) (count: int) : int =
        if index1 >= s1.Length then
            count
        else
            let startMatch = max 0 (index1 - matchDistance)
            let endMatch = min (s2.Length - 1) (index1 + matchDistance)
            let s2CharsUsed = Array.create s2.Length false

            let rec loop j count' =
                if j <= endMatch then
                    if not s2CharsUsed[j] && s1[index1] = s2[j] then
                        s2CharsUsed[j] <- true
                        countMatches s1 s2 matchDistance (index1 + 1) (j + 1) (count + 1)
                    else
                        loop (j + 1) count'
                else
                    count'

            let count' = loop startMatch count
            countMatches s1 s2 matchDistance (index1 + 1) index2 count'

    let numMatches1 = countMatches s1 s2 matchDistance 0 0 0
    let numMatches2 = countMatches s2 s1 matchDistance 0 0 0

    if numMatches1 = 0 || numMatches2 = 0 then
        0.0
    else
        let transpositions = countMatches s1 s2 (matchDistance / 2) 0 0 0
        let similarity =
            (float numMatches1 / float s1Length + float numMatches2 / float s2Length +
             float (numMatches1 - transpositions) / float numMatches1) / 3.0
        similarity

