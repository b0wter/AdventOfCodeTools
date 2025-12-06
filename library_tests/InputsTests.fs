module AoC.Shared.Library.Tests.InputsTests

open System
open Xunit
open FsUnit.Xunit
open AoC.Shared.Library

[<Fact>]
let ``readInput test`` () =
    let input = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""
    let parsed = Inputs.readInput
                     (fun () -> input.Split(Environment.NewLine))
                     Inputs.InputStyle.Rows
                     ' '
                     Int32.Parse
                     
    parsed |> Grid.at (1, 0) |> should equal 1
    
[<Fact>]
let ``readGrid test`` () =
    let input = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""
    let parsed = Inputs.readGrid
                     (fun () -> input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries))
                     (fun s -> s.ToCharArray() |> Array.map string |> Array.where (fun c -> c <> " "))
                     Int32.Parse
    parsed |> Grid.at (1, 0) |> should equal 1
