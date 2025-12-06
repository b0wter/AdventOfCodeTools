module AoC.Shared.Library.Tests.CombinationsTests

open System
open Xunit
open FsUnit.Xunit
open AoC.Shared.Library

[<Theory>]
[<InlineData(2, 2,  4)>]
[<InlineData(2, 3,  8)>]
[<InlineData(2, 4, 16)>]
[<InlineData(3, 2,  9)>]
[<InlineData(3, 3, 27)>]
[<InlineData(3, 4, 81)>]
let ``Number of possible combinations are computed correctly`` (n, r, result) =
    let calculated = Combinations.count n r
    calculated |> should equal result


[<Theory>]
[<InlineData(3, 3)>]
[<InlineData(3, 4)>]
[<InlineData(4, 4)>]
[<InlineData(4, 5)>]
[<InlineData(5, 5)>]
[<InlineData(5, 6)>]
let ``Combination function returns all combinations`` (numberOfOptions, sampleSize) =
    let input = [1..numberOfOptions]
    
    let result = input |> Combinations.all sampleSize
    let expectedLength = Combinations.count numberOfOptions sampleSize
    
    result |> List.distinct |> should haveLength expectedLength
