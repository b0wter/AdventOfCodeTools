module AoC.Shared.Library.Tests.PathsTests

open System
open Xunit
open FsUnit.Xunit
open AoC.Shared.Library
open AoC.Shared.Library.Paths
open AoC.Shared.Library.Shared

[<Fact>]
let ``Give a grid a* finds one of the shortest paths`` () =
    let grid =
        let init =
            [| [| 0; 0; 0; 0; 0 |]
               [| 0; 1; 1; 1; 0 |]
               [| 0; 0; 0; 1; 0 |]
               [| 0; 1; 0; 0; 0 |]
               [| 0; 0; 0; 0; 0 |] |]
        Array2D.init init.Length init[0].Length (fun y x -> init[y][x])

    let start = { X = 0; Y = 0 }
    let goal = { X = 4; Y = 4 }

    let addCost = fun (f: float) (g: float) -> f + g
    
    let path =
        aStar<Point2D<int>, float>
            addCost
            0.0
            (fun _ a b ->
                if (Math.Abs(a.X - b.X) + Math.Abs(a.Y - b.Y)) <> 1 then Double.MaxValue
                else (grid[b.Y,b.X] * 100) + 1 |> float)
            (fun _ point -> (Grid.tryGetNeighbors point.Y point.X grid).AsSinglePoints())
            (fun _ _ _ -> 1.0)
            start
            goal

    path |> should haveLength 8
    