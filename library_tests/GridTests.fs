module GridTests

open System
open Xunit
open FsUnit.Xunit
open AoC.Shared.Library

let private createCounterGrid height width =
    let counter =
        let mutable c = 0
        fun _ _ ->
            c <- c + 1
            c
    Grid.createWith height width counter

[<Fact>]
let ``Creating a grid with width 10 creates a grid of width 10`` () =
    let grid = Grid.create 2 10 1
    let width = grid |> Grid.width
    width |> should equal 10
    
[<Fact>]
let ``Creating a grid with height 10 creates a grid of height 10`` () =
    let grid = Grid.create 10 2 1
    let height = grid |> Grid.height
    height |> should equal 10
    
[<Fact>]
let ``Flattening an array returns a one-dimensional order in proper order`` () =
    let grid = createCounterGrid 2 2
    let flattened = grid |> Grid.flatten
    flattened |> should equal [|1; 2; 3; 4|]
    
[<Fact>]
let ``Mapping a grid changes every cell`` () =
    let grid = Grid.create 3 2 1
    let updated = grid |> Grid.map (fun _ _ _ -> 2)
    updated |> Grid.flatten |> should equal [|2; 2; 2; 2; 2; 2|]
    
[<Fact>]
let ``Converting a grid to string returns a pretty formatted string`` () =
    let grid = createCounterGrid 3 2
    let prettyPrinted = grid |> Grid.toString
    
    prettyPrinted |> should equal $"1 2{Environment.NewLine}3 4{Environment.NewLine}5 6"
    
[<Fact>]
let ``getNeighborsWith returns correct neighbors`` () =
    (*
     * 1 2 3
     * 4 5 6
     * 7 8 9
     *)
    let grid = createCounterGrid 3 3
    let neighbors = Grid.getNeighborsWith false 1 1 grid
    let expected =
        {
            Neighbors.Neighbors.Center = 5
            Neighbors.Neighbors.X = 1
            Neighbors.Neighbors.Y = 1
            Neighbors.Neighbors.Up = 2
            Neighbors.Neighbors.Right = 6
            Neighbors.Neighbors.Down = 8
            Neighbors.Neighbors.Left = 4
        }

    neighbors |> should equal expected
    
[<Fact>]
let ``getNeighborsWith with wrapping returns correct neighbors`` () =
    (*
     * 1 2 3
     * 4 5 6
     * 7 8 9
     *)
    let grid = createCounterGrid 3 3
    let neighbors = Grid.getNeighborsWith true 1 1 grid
    let expected =
        {
            Neighbors.Neighbors.Center = 5
            Neighbors.Neighbors.X = 1
            Neighbors.Neighbors.Y = 1
            Neighbors.Neighbors.Up = 2
            Neighbors.Neighbors.Right = 6
            Neighbors.Neighbors.Down = 8
            Neighbors.Neighbors.Left = 4
        }

    neighbors |> should equal expected
    
[<Fact>]
let ``getNeighborsWith on left edge wraps properly`` () =
    (*
     * 1 2 3
     * 4 5 6
     * 7 8 9
     *)
    let grid = createCounterGrid 3 3
    let neighbors = Grid.getNeighborsWith true 1 0 grid
    let expected =
        {
            Neighbors.Neighbors.Center = 4
            Neighbors.Neighbors.X = 0
            Neighbors.Neighbors.Y = 1
            Neighbors.Neighbors.Up = 1
            Neighbors.Neighbors.Right = 5
            Neighbors.Neighbors.Down = 7
            Neighbors.Neighbors.Left = 6
        }

    neighbors |> should equal expected
    
[<Fact>]
let ``getNeighborsWith on right edge wraps properly`` () =
    (*
     * 1 2 3
     * 4 5 6
     * 7 8 9
     *)
    let grid = createCounterGrid 3 3
    let neighbors = Grid.getNeighborsWith true 1 2 grid
    let expected =
        {
            Neighbors.Neighbors.Center = 6
            Neighbors.Neighbors.X = 2
            Neighbors.Neighbors.Y = 1
            Neighbors.Neighbors.Up = 3
            Neighbors.Neighbors.Right = 4
            Neighbors.Neighbors.Down = 9
            Neighbors.Neighbors.Left = 5
        }

    neighbors |> should equal expected
    
[<Fact>]
let ``getNeighborsWith on top edge wraps properly`` () =
    (*
     * 1 2 3
     * 4 5 6
     * 7 8 9
     *)
    let grid = createCounterGrid 3 3
    let neighbors = Grid.getNeighborsWith true 0 1 grid
    let expected =
        {
            Neighbors.Neighbors.Center = 2
            Neighbors.Neighbors.X = 1
            Neighbors.Neighbors.Y = 0
            Neighbors.Neighbors.Up = 8
            Neighbors.Neighbors.Right = 3
            Neighbors.Neighbors.Down = 5
            Neighbors.Neighbors.Left = 1
        }

    neighbors |> should equal expected
    
[<Fact>]
let ``getNeighborsWith on bottom edge wraps properly`` () =
    (*
     * 1 2 3
     * 4 5 6
     * 7 8 9
     *)
    let grid = createCounterGrid 3 3
    let neighbors = Grid.getNeighborsWith true 2 1 grid
    let expected =
        {
            Neighbors.Neighbors.Center = 8
            Neighbors.Neighbors.X = 1
            Neighbors.Neighbors.Y = 2
            Neighbors.Neighbors.Up = 5
            Neighbors.Neighbors.Right = 9
            Neighbors.Neighbors.Down = 2
            Neighbors.Neighbors.Left = 7
        }

    neighbors |> should equal expected

type StepSampleData = { Width: int; Height: int; FromX: int; FromY: int; Direction: Directions.Simple }
let stepSampleData : obj[] seq =
    seq {
        yield [|3, 3, 1, 1, Directions.Simple.Up|]
        //yield [|{ Width = 3; Height = 3; FromX = 1; FromY = 1; Direction = Shared.SimpleDirections.Up }|]
    }

type ClassDataBase(generator : obj [] seq) = 
    interface seq<obj []> with
        member this.GetEnumerator() = generator.GetEnumerator()
        member this.GetEnumerator() = 
            generator.GetEnumerator() :> System.Collections.IEnumerator

type OutOfBoundsStepTestData() =
    inherit ClassDataBase([
        [| 3; 3; 0; 0; Directions.Simple.Up;    1; { Shared.Point2D.X = 0; Shared.Point2D.Y = 0 }; { Shared.Point2D.X = 0; Shared.Point2D.Y = 2 } |]
        [| 3; 3; 1; 0; Directions.Simple.Up;    1; { Shared.Point2D.X = 1; Shared.Point2D.Y = 0 }; { Shared.Point2D.X = 1; Shared.Point2D.Y = 2 } |]
        [| 3; 3; 2; 0; Directions.Simple.Up;    1; { Shared.Point2D.X = 2; Shared.Point2D.Y = 0 }; { Shared.Point2D.X = 2; Shared.Point2D.Y = 2 } |]
        [| 3; 3; 0; 0; Directions.Simple.Up;    2; { Shared.Point2D.X = 0; Shared.Point2D.Y = 0 }; { Shared.Point2D.X = 0; Shared.Point2D.Y = 1 } |]
        [| 3; 3; 1; 0; Directions.Simple.Up;    2; { Shared.Point2D.X = 1; Shared.Point2D.Y = 0 }; { Shared.Point2D.X = 1; Shared.Point2D.Y = 1 } |]
        [| 3; 3; 2; 0; Directions.Simple.Up;    2; { Shared.Point2D.X = 2; Shared.Point2D.Y = 0 }; { Shared.Point2D.X = 2; Shared.Point2D.Y = 1 } |]
        
        [| 3; 3; 2; 0; Directions.Simple.Right; 1; { Shared.Point2D.X = 2; Shared.Point2D.Y = 0 }; { Shared.Point2D.X = 0; Shared.Point2D.Y = 0 } |]
        [| 3; 3; 2; 1; Directions.Simple.Right; 1; { Shared.Point2D.X = 2; Shared.Point2D.Y = 1 }; { Shared.Point2D.X = 0; Shared.Point2D.Y = 1 } |]
        [| 3; 3; 2; 2; Directions.Simple.Right; 1; { Shared.Point2D.X = 2; Shared.Point2D.Y = 2 }; { Shared.Point2D.X = 0; Shared.Point2D.Y = 2 } |]
        [| 3; 3; 2; 0; Directions.Simple.Right; 2; { Shared.Point2D.X = 2; Shared.Point2D.Y = 0 }; { Shared.Point2D.X = 1; Shared.Point2D.Y = 0 } |]
        [| 3; 3; 2; 1; Directions.Simple.Right; 2; { Shared.Point2D.X = 2; Shared.Point2D.Y = 1 }; { Shared.Point2D.X = 1; Shared.Point2D.Y = 1 } |]
        [| 3; 3; 2; 2; Directions.Simple.Right; 2; { Shared.Point2D.X = 2; Shared.Point2D.Y = 2 }; { Shared.Point2D.X = 1; Shared.Point2D.Y = 2 } |]
        
        [| 3; 3; 0; 2; Directions.Simple.Down;  1; { Shared.Point2D.X = 0; Shared.Point2D.Y = 2 }; { Shared.Point2D.X = 0; Shared.Point2D.Y = 0 } |]
        [| 3; 3; 1; 2; Directions.Simple.Down;  1; { Shared.Point2D.X = 1; Shared.Point2D.Y = 2 }; { Shared.Point2D.X = 1; Shared.Point2D.Y = 0 } |]
        [| 3; 3; 2; 2; Directions.Simple.Down;  1; { Shared.Point2D.X = 2; Shared.Point2D.Y = 2 }; { Shared.Point2D.X = 2; Shared.Point2D.Y = 0 } |]
        [| 3; 3; 0; 2; Directions.Simple.Down;  2; { Shared.Point2D.X = 0; Shared.Point2D.Y = 2 }; { Shared.Point2D.X = 0; Shared.Point2D.Y = 1 } |]
        [| 3; 3; 1; 2; Directions.Simple.Down;  2; { Shared.Point2D.X = 1; Shared.Point2D.Y = 2 }; { Shared.Point2D.X = 1; Shared.Point2D.Y = 1 } |]
        [| 3; 3; 2; 2; Directions.Simple.Down;  2; { Shared.Point2D.X = 2; Shared.Point2D.Y = 2 }; { Shared.Point2D.X = 2; Shared.Point2D.Y = 1 } |]
        
        [| 3; 3; 0; 0; Directions.Simple.Left;  1; { Shared.Point2D.X = 0; Shared.Point2D.Y = 0 }; { Shared.Point2D.X = 2; Shared.Point2D.Y = 0 } |]
        [| 3; 3; 0; 1; Directions.Simple.Left;  1; { Shared.Point2D.X = 0; Shared.Point2D.Y = 1 }; { Shared.Point2D.X = 2; Shared.Point2D.Y = 1 } |]
        [| 3; 3; 0; 2; Directions.Simple.Left;  1; { Shared.Point2D.X = 0; Shared.Point2D.Y = 2 }; { Shared.Point2D.X = 2; Shared.Point2D.Y = 2 } |]
        [| 3; 3; 0; 0; Directions.Simple.Left;  2; { Shared.Point2D.X = 0; Shared.Point2D.Y = 0 }; { Shared.Point2D.X = 1; Shared.Point2D.Y = 0 } |]
        [| 3; 3; 0; 1; Directions.Simple.Left;  2; { Shared.Point2D.X = 0; Shared.Point2D.Y = 1 }; { Shared.Point2D.X = 1; Shared.Point2D.Y = 1 } |]
        [| 3; 3; 0; 2; Directions.Simple.Left;  2; { Shared.Point2D.X = 0; Shared.Point2D.Y = 2 }; { Shared.Point2D.X = 1; Shared.Point2D.Y = 2 } |]
    ])
    
type OutOfBoundsDiagonalStepTestData() =
    inherit ClassDataBase([
        [| 3; 3; 0; 0; Directions.WithDiagonals.UpLeft;    1; { Shared.Point2D.X = 0; Shared.Point2D.Y = 0 }; { Shared.Point2D.X = 2; Shared.Point2D.Y = 2 } |]
        [| 3; 3; 0; 2; Directions.WithDiagonals.DownLeft;  1; { Shared.Point2D.X = 0; Shared.Point2D.Y = 2 }; { Shared.Point2D.X = 2; Shared.Point2D.Y = 0 } |]
        [| 3; 3; 2; 2; Directions.WithDiagonals.DownRight; 1; { Shared.Point2D.X = 2; Shared.Point2D.Y = 2 }; { Shared.Point2D.X = 0; Shared.Point2D.Y = 0 } |]
        [| 3; 3; 2; 0; Directions.WithDiagonals.UpRight;   1; { Shared.Point2D.X = 2; Shared.Point2D.Y = 0 }; { Shared.Point2D.X = 0; Shared.Point2D.Y = 2 } |]
        
        [| 3; 3; 0; 0; Directions.WithDiagonals.UpLeft;    2; { Shared.Point2D.X = 0; Shared.Point2D.Y = 0 }; { Shared.Point2D.X = 1; Shared.Point2D.Y = 1 } |]
        [| 3; 3; 0; 2; Directions.WithDiagonals.DownLeft;  2; { Shared.Point2D.X = 0; Shared.Point2D.Y = 2 }; { Shared.Point2D.X = 1; Shared.Point2D.Y = 1 } |]
        [| 3; 3; 2; 2; Directions.WithDiagonals.DownRight; 2; { Shared.Point2D.X = 2; Shared.Point2D.Y = 2 }; { Shared.Point2D.X = 1; Shared.Point2D.Y = 1 } |]
        [| 3; 3; 2; 0; Directions.WithDiagonals.UpRight;   2; { Shared.Point2D.X = 2; Shared.Point2D.Y = 0 }; { Shared.Point2D.X = 1; Shared.Point2D.Y = 1 } |]
    ])

[<Theory>]
[<ClassData(typedefof<OutOfBoundsStepTestData>)>]
let ``Stepping out of bounds with Fail-edge-handling throws an out of bounds exception`` (height: int) (width: int) (fromX: int) (fromY: int) (direction: Directions.Simple) (stepSize: int) _ _ : unit =
    let grid = createCounterGrid height width
    let action = fun () -> Grid.step Directions.simpleStepGenerator Grid.EdgeHandling.Fail { X = fromX; Y = fromY } stepSize direction grid |> ignore
    Assert.Throws<IndexOutOfRangeException>(action) |> ignore
    
[<Theory>]
[<ClassData(typedefof<OutOfBoundsStepTestData>)>]
let ``Stepping out of bounds with Stop-edge-handling does not move out of bounds`` (height: int) (width: int) (fromX: int) (fromY: int) (direction: Directions.Simple) (stepSize: int) (expected: Shared.Point2D<int>) _ : unit =
    let grid = createCounterGrid height width
    let newPosition, _ = Grid.step Directions.simpleStepGenerator Grid.EdgeHandling.Stop { X = fromX; Y = fromY } stepSize direction grid |> Option.get
    newPosition |> should equal expected
    
[<Theory>]
[<ClassData(typedefof<OutOfBoundsStepTestData>)>]
let ``Stepping out of bounds with Wrap-edge-handling does not move out of bounds`` (height: int) (width: int) (fromX: int) (fromY: int) (direction: Directions.Simple) (stepSize: int) _ (expected: Shared.Point2D<int>) : unit =
    let grid = createCounterGrid height width
    let newPosition, _ = Grid.step Directions.simpleStepGenerator Grid.EdgeHandling.Wrap { X = fromX; Y = fromY } stepSize direction grid |> Option.get
    newPosition |> should equal expected
    
[<Theory>]
[<ClassData(typedefof<OutOfBoundsDiagonalStepTestData>)>]
let ``Stepping out of bounds diagonally with Fail-edge-handling throws an out of bounds exception`` (height: int) (width: int) (fromX: int) (fromY: int) (direction: Directions.WithDiagonals) (stepSize: int) _ _: unit =
    let grid = createCounterGrid height width
    let action = fun () -> Grid.step Directions.withDiagonalsStepGenerator Grid.EdgeHandling.Fail { X = fromX; Y = fromY } stepSize direction grid |> ignore
    Assert.Throws<IndexOutOfRangeException>(action) |> ignore
    
[<Theory>]
[<ClassData(typedefof<OutOfBoundsDiagonalStepTestData>)>]
let ``Stepping out of bounds diagonally with Stop-edge-handling does not move out of bounds`` (height: int) (width: int) (fromX: int) (fromY: int) (direction: Directions.WithDiagonals) (stepSize: int) (expected: Shared.Point2D<int>) _ : unit =
    let grid = createCounterGrid height width
    let newPosition, _ = Grid.step Directions.withDiagonalsStepGenerator Grid.EdgeHandling.Stop { X = fromX; Y = fromY } stepSize direction grid |> Option.get
    newPosition |> should equal expected
    
[<Theory>]
[<ClassData(typedefof<OutOfBoundsDiagonalStepTestData>)>]
let ``Stepping out of bounds diagonally with Wrap-edge-handling does not move out of bounds`` (height: int) (width: int) (fromX: int) (fromY: int) (direction: Directions.WithDiagonals) (stepSize: int) _ (expected: Shared.Point2D<int>) : unit =
    let grid = createCounterGrid height width
    let newPosition, _ = Grid.step Directions.withDiagonalsStepGenerator Grid.EdgeHandling.Wrap { X = fromX; Y = fromY } stepSize direction grid |> Option.get
    newPosition |> should equal expected

[<Theory>]
[<InlineData(1,1,2,2,2)>]
[<InlineData(2,2,1,1,2)>]
[<InlineData(0,0,6,6,12)>]
[<InlineData(6,6,0,0,12)>]
let ``Calculating the Manhattan distance produces correct results`` (fromX: int, fromY: int, toX: int, toY: int, expected: int) =
    let result = Grid.manhattanDistance { X = fromX; Y = fromY } { X = toX; Y = toY }
    
    result |> should equal expected
    
    
[<Fact>]
let ``Filtering a grid with 'where' returns all matching elements`` () =
    let grid = createCounterGrid 3 3
    let predicate = fun i -> i % 2 = 0
    
    let result = grid |> Grid.where predicate |> List.ofSeq
    
    result |> should equal [
        { Shared.Point2D.X = 1; Shared.Point2D.Y = 0}, 2
        { Shared.Point2D.X = 0; Shared.Point2D.Y = 1}, 4
        { Shared.Point2D.X = 2; Shared.Point2D.Y = 1}, 6
        { Shared.Point2D.X = 1; Shared.Point2D.Y = 2}, 8
    ]
    
    
[<Fact>]
let ``Stepping with predicate returns the correct steps`` () =
    let grid = createCounterGrid 1 10
    // Grid:
    // index:   0   1   2   3   4   5   6   7   8   9
    // value:   1   2   3   4   5   6   7   8   9  10
    //          ^           ^
    //        Start     last true
    //              ^   ^   ^
    //          expect three values
    let predicate = fun i -> i < 5
    
    let result = grid |> Grid.stepWhile predicate Directions.simpleStepGenerator Grid.EdgeHandling.Fail { X = 0; Y = 0 } Directions.Right
    
    result |> should haveLength 3
    
    
[<Fact>]
let ``Transposing a grid works properly`` () =
    // create with height 2 and width 3
    let grid = createCounterGrid 3 2
    
    let transposed = grid |> Grid.transpose
    
    transposed |> Grid.width |> should equal 3
    transposed |> Grid.height |> should equal 2
    
    transposed |> Grid.at (0,0) |> should equal 1
    transposed |> Grid.at (0,1) |> should equal 3
    transposed |> Grid.at (0,2) |> should equal 5
    transposed |> Grid.at (1,0) |> should equal 2
    transposed |> Grid.at (1,1) |> should equal 4
    transposed |> Grid.at (1,2) |> should equal 6


[<Fact>]
let ``Taking a row from a grid should return the requested row`` () =
    let grid = createCounterGrid 3 2
    
    let first = grid |> Grid.row 0
    let second = grid |> Grid.row 1
    let third = grid |> Grid.row 2
    
    first |> should equal [| 1; 2 |]
    second |> should equal [| 3; 4 |]
    third |> should equal [| 5; 6 |]


[<Fact>]
let ``Taking a column from a grid should return the requested column`` () =
    let grid = createCounterGrid 2 3
    
    let first = grid |> Grid.col 0
    let second = grid |> Grid.col 1
    let third = grid |> Grid.col 2
    
    first |> should equal [| 1; 4 |]
    second |> should equal [| 2; 5 |]
    third |> should equal [| 3; 6 |]


[<Fact>]
let ``Creating a grid from a string returns a correctly parsed grid`` () =
    let input = $"abcd%s{Environment.NewLine}efgh"
    
    let grid = input |> Grid.fromString
    
    grid |> Grid.width |> should equal 4
    grid |> Grid.height |> should equal 2
    
    grid |> Grid.at (0,0) |> should equal 'a'
    grid |> Grid.at (0,1) |> should equal 'b'
    grid |> Grid.at (0,2) |> should equal 'c'
    grid |> Grid.at (0,3) |> should equal 'd'
    grid |> Grid.at (1,0) |> should equal 'e'
    grid |> Grid.at (1,1) |> should equal 'f'
    grid |> Grid.at (1,2) |> should equal 'g'
    grid |> Grid.at (1,3) |> should equal 'h'


[<Fact>]
let ``Mirroring a grid on the horizontal axis returns proper result`` () =
    let grid = createCounterGrid 2 2
    
    let mirrored = grid |> Grid.mirrorHorizontal
    
    mirrored |> Grid.at (0,0) |> should equal 3
    mirrored |> Grid.at (0,1) |> should equal 4
    mirrored |> Grid.at (1,0) |> should equal 1
    mirrored |> Grid.at (1,1) |> should equal 2


[<Fact>]
let ``Mirroring a grid on the vertical axis returns proper result`` () =
    let grid = createCounterGrid 2 2
    
    let mirrored = grid |> Grid.mirrorVertical
    
    mirrored |> Grid.at (0,0) |> should equal 2
    mirrored |> Grid.at (0,1) |> should equal 1
    mirrored |> Grid.at (1,0) |> should equal 4
    mirrored |> Grid.at (1,1) |> should equal 3
