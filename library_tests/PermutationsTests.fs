module AoC.Shared.Library.Tests.PermutationsTests

open System
open Xunit
open FsUnit.Xunit
open AoC.Shared.Library

    
[<Theory>]
[<InlineData(2, 2,  2)>]
[<InlineData(3, 3,  6)>]
[<InlineData(4, 4, 24)>]
[<InlineData(4, 3, 24)>]
[<InlineData(4, 2, 12)>]
let ``Number of possible permutations are calculated correctly`` (n, r, result) =
    let calculated = Permutations.count n r
    calculated |> should equal result


[<Fact>]
let ``Trying to calculate number of permutations with sample size larger than number of options throws`` () =
    (fun () -> Permutations.count 1 2 |> ignore) |> should throw typeof<Exception>


[<Fact>]
let ``Permutation function returns all permutations`` () =
    let input = [ 1; 2; 3 ]
    (* permutations:
        1; 2; 3
        2; 1; 3
        2; 3; 1
        1; 3; 2
        3; 1; 2
        3; 2; 1
    *)
    let result = Permutations.all input
    let expectedLength = Permutations.count input.Length input.Length
    
    result |> List.distinct |> should haveLength expectedLength

