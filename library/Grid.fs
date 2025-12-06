module AoC.Shared.Library.Grid

open System
open AoC.Shared.Library.Shared

type EdgeHandling =
    /// <summary>
    /// Does nothing to prevent an out of bounds read
    /// </summary>
    | Fail
    /// <summary>
    /// Leaving the bounds on one side causes the position to wrap around to the opposite edge
    /// </summary>
    | Wrap
    /// <summary>
    /// Stops at the edge. Accessing elements outside out the bounds will return the edge element
    /// </summary>
    | Stop
    /// <summary>
    /// Will return the result as an option
    /// </summary>
    | Skip

/// <summary>
/// Creates a grid by filling the cells with Unchecked.defaultOf&lt;'a&gt;
/// </summary>
let createWithDefaults<'a> (height: int) (width: int) : 'a[,] =
    Array2D.zeroCreate<'a> height width
    
/// <summary>
/// Creates a grid by filling the cells with the given initial value
/// </summary>
let create (height: int) (width: int) (t: 'a) : 'a[,] = 
    Array2D.create height width t
    
/// <summary>
/// Creates a grid by filling the cells using the given initializer
/// </summary>
let createWith (height: int) (width: int) (generator: int -> int -> 'a) : 'a[,] =
    Array2D.init height width generator

/// <summary>
/// Retrieves the height (length of the first rank) of the grid
/// </summary>
let height (grid: _[,]) : int =
    grid.GetLength(0)
    
/// <summary>
/// Retrieves the width (length of the second rank) of the grid
/// </summary>
/// <param name="grid"></param>
let width (grid: _[,]) : int =
    grid.GetLength(1)
    
/// <summary>
/// Retrieves the value of the cell at position (y, x)
/// </summary>
let at (y: int, x: int) (grid: 'a[,]) =
    grid[y,x]
    
/// <summary>
/// Retrieves the value of the cell at position (Point.Y, Point.X)
/// </summary>
let atPoint (point: Point2D<int>) (grid: 'a[,]) =
    grid[point.Y, point.X]
    

let where (predicate: 'a -> bool) (grid: 'a[,]) =
    let width = grid |> width
    let height = grid |> height
    
    seq {
        for i in 0..(width * height - 1) do
            let y = i / width
            let x = i % width
            if grid[y, x] |> predicate then ({ X = x; Y = y }, grid[y,x])
    }
    
    (*
    [0..(width * height - 1)]
    |> List.map (fun i -> grid[i / width, i % width])
    |> List.where predicate
    *)
            
    
/// <summary>
/// Calculates the new position after stepping <paramref name="stepSize"/> steps into <paramref name="direction"/>.
/// <paramref name="edgeHandling"/> defined the out-of=bounds behaviour
/// </summary>
/// <remarks>
/// This method takes configuration parameters to make it work with different types of directions (simple, diagonal, ...)
/// </remarks>
/// <param name="directionStepGenerator">Translates a direction into relative movement</param>
/// <param name="edgeHandling">Defines how out-of-bounds variables are handled</param>
/// <param name="from">Start point on the given <paramref name="grid"/></param>
/// <param name="stepSize">Number of steps taken (distance)</param>
/// <param name="direction">Direction where to go to. Is generic to make this function work with different types of directions</param>
/// –<param name="grid">The grid to operate on. Is required for the bounds and edge handling</param>
let step<'direction, 'cell> (directionStepGenerator: 'direction -> Point2D<int>) (edgeHandling: EdgeHandling) (from: Point2D<int>) (stepSize: int) (direction: 'direction) (grid: 'cell[,]) : (Point2D<int> * 'cell) option =
    let movement = (direction |> directionStepGenerator) * stepSize
    let height = grid |> height
    let width = grid |> width
    let newX = from.X + movement.X
    let newY = from.Y + movement.Y
    
    match edgeHandling with
    | Fail ->
        let target = from + movement
        Some (target, grid[target.Y, target.X])
    | Skip ->
        let temp = from + movement
        if (temp.X < 0) || (temp.X >= width) || (temp.Y < 0) || (temp.Y >= height) then None
        else Some ({ X = temp.X; Y = temp.Y }, grid[temp.Y, temp.X])
    | Wrap ->
        let temp = from + movement
        let newX =
            if (temp.X < 0) then width + temp.X
            else if (temp.X >= width) then newX % width
            else temp.X
        let newY =
            if (temp.Y < 0) then height + temp.Y
            else if (temp.Y >= height) then newY % height
            else temp.Y
        Some ({ X = newX; Y = newY }, grid[newY, newX])
    | Stop ->
        // HINT: Using 'stop' with diagonals should not move if ANY of the two directions is blocked!
        //       Otherwise, UpLeft may turn Left, DownRight into Down, ...
        let temp = from + movement
        let mutable xBlocked = false
        let mutable yBlocked = false
        let newX =
            if temp.X < 0 then
                xBlocked <- true
                0
            else if (temp.X >= width) then
                xBlocked <- true
                width - 1
            else temp.X
        let newY =
            if temp.Y < 0 then
                yBlocked <- true
                0
            else if (temp.Y >= height) then
                yBlocked <- true
                height - 1
            else temp.Y
        Some (
            if (xBlocked || yBlocked) then (from, grid[from.Y, from.X])
            else ({ X = newX; Y = newY }, grid[newY, newX]))
        
let stepSimple<'cell> =
    step<Directions.Simple, 'cell> Directions.simpleStepGenerator
let stepWithDiagonals<'cell> =
    step<Directions.WithDiagonals, 'cell> Directions.withDiagonalsStepGenerator


let stepMany<'direction, 'cell> directionStepGenerator edgeHandling from (numberOfSteps: int) direction grid : (Point2D<int> * 'cell) list =
    if numberOfSteps < 0 then failwithf "Cannot step with negative step count: %i" numberOfSteps
    else
        let stepper = fun stepSize -> step<'direction, 'cell> directionStepGenerator edgeHandling from stepSize direction grid
        [0..(numberOfSteps - 1)] |> List.map stepper |> List.choose id
            
let stepManySimple<'cell> =
    stepMany<Directions.Simple, 'cell> Directions.simpleStepGenerator
let stepManyDiagonals<'cell> =
    stepMany<Directions.WithDiagonals, 'cell> Directions.withDiagonalsStepGenerator
        
    
/// <summary>
/// Creates a copy of the array and applies the mapping operation to every cell
/// </summary>
/// <param name="grid">Grid to apply the operation to</param>
/// <param name="map">A mapping function that takes the row, column and previous value to create a new value</param>
let map (map: int -> int -> 'a -> 'b) (grid: 'a[,]) : 'b[,] =
    let gridHeight = grid |> height
    let gridWidth = grid |> width
    let newGrid = Array2D.zeroCreate gridHeight gridWidth
    for i = 0 to gridHeight - 1 do
        for j = 0 to gridWidth - 1 do
            newGrid[i,j] <- map i j grid[i,j]
    newGrid
    
/// <summary>
/// Flattens the grid into a one-dimensional array
/// </summary>
let flatten (grid: 'a[,]) : 'a[] =
    let enumerator = grid.GetEnumerator()
    let array = Array.zeroCreate ((grid |> height) * (grid |> width))
    let mutable index = 0
    while enumerator.MoveNext() do
        array[index] <- enumerator.Current :?> 'a
        index <- index + 1
    array
    
/// <summary>
/// Gets all neighboring cells. Does not include diagonals and will fail if any neighbors is out of the array bounds
/// and `wrapAround = false`.
/// </summary>
let getNeighborsWith (wrapAround: bool) (y: int) (x: int) (grid: 'a[,]) : Neighbors.Neighbors<'a> =
    let height = grid |> height
    let width = grid |> width
    let at = fun point ->
        let target =
            let sumX = point.X + x
            let sumY = point.Y + y
            let newX =
                if      (sumX >= width) && wrapAround then sumX % width
                else if (sumX < 0) && wrapAround then width + sumX
                else    sumX
            let newY =
                if      (sumY >= height) && wrapAround then sumY % height
                else if (sumY < 0) && wrapAround then height + sumY
                else    sumY
            {point with X = newX; Y = newY }
        atPoint target
    {
        Neighbors.Neighbors.Center = grid[y,x]
        Neighbors.Neighbors.X = x
        Neighbors.Neighbors.Y = y
        Neighbors.Neighbors.Up = at (Directions.SimpleAsSteps[Directions.Simple.Up]) grid 
        Neighbors.Neighbors.Right = at (Directions.SimpleAsSteps[Directions.Simple.Right]) grid 
        Neighbors.Neighbors.Down = at (Directions.SimpleAsSteps[Directions.Simple.Down]) grid 
        Neighbors.Neighbors.Left = at (Directions.SimpleAsSteps[Directions.Simple.Left]) grid 
    }
    
let rec getNeighbors y x grid = getNeighborsWith false y x grid
    
/// <summary>
/// Gets all neighboring cells. Include diagonals and will fail if any neighbors is out of the array bounds
/// </summary>
let getNeighborsWithDiagonals (wrapAround: bool) (y: int) (x: int) (grid: 'a[,]) : Neighbors.NeighborsWithDiagonals<'a> =
    let height = grid |> height
    let width = grid |> width
    let at = fun point ->
        let target =
            let sumX = point.X + x
            let sumY = point.Y + y
            let newX =
                if      (sumX >= width) && wrapAround then sumX % width
                else if (sumX < 0) && wrapAround then width + sumX
                else    sumX
            let newY =
                if      (sumY >= height) && wrapAround then sumY % height
                else if (sumY < 0) && wrapAround then height + sumY
                else    sumY
            {point with X = newX; Y = newY }
        atPoint target
    {
        Neighbors.NeighborsWithDiagonals.Center = grid[y,x]
        Neighbors.NeighborsWithDiagonals.X = x
        Neighbors.NeighborsWithDiagonals.Y = y
        Neighbors.NeighborsWithDiagonals.Up = at (Directions.WithDiagonalsAsSteps[Directions.WithDiagonals.Up]) grid 
        Neighbors.NeighborsWithDiagonals.Right = at (Directions.WithDiagonalsAsSteps[Directions.WithDiagonals.Right]) grid 
        Neighbors.NeighborsWithDiagonals.Down = at (Directions.WithDiagonalsAsSteps[Directions.WithDiagonals.Down]) grid 
        Neighbors.NeighborsWithDiagonals.Left = at (Directions.WithDiagonalsAsSteps[Directions.WithDiagonals.Left]) grid
        Neighbors.NeighborsWithDiagonals.DownLeft = at (Directions.WithDiagonalsAsSteps[Directions.WithDiagonals.DownLeft]) grid
        Neighbors.NeighborsWithDiagonals.UpLeft = at (Directions.WithDiagonalsAsSteps[Directions.WithDiagonals.UpLeft]) grid
        Neighbors.NeighborsWithDiagonals.DownRight = at (Directions.WithDiagonalsAsSteps[Directions.WithDiagonals.DownRight]) grid
        Neighbors.NeighborsWithDiagonals.UpRight = at (Directions.WithDiagonalsAsSteps[Directions.WithDiagonals.UpRight]) grid
    }     
    
/// <summary>
/// Gets all neighboring cells. Does not include diagonals.
/// Uses an option type to represent cells that might be out of bounds
/// </summary>
let tryGetNeighborsWith (predicate: 'a -> bool) (y: int) (x: int) (grid: 'a[,]) : Neighbors.MaybeNeighbors<'a> =
    let stepper direction =
        stepSimple EdgeHandling.Skip { X = x; Y = y } 1 direction grid
        |> Option.map snd
        |> Option.bind (fun x -> if x |> predicate then Some x else None)
        
    {
        Center = grid |> at (y, x)
        Y = y
        X = x
        Up = stepper Directions.Simple.Up
        Right = stepper Directions.Simple.Right
        Down = stepper Directions.Simple.Down
        Left = stepper Directions.Simple.Left
    }
    
    
let tryGetNeighbors y x grid = tryGetNeighborsWith (fun _ -> true) y x grid
    
    
/// <summary>
/// Gets all neighboring cells. Includes diagonals. Uses an option type to represent cells that might be out of bounds
/// </summary>
let tryGetNeighborsWithDiagonalsWith (predicate: 'a -> bool) (y: int) (x: int) (grid: 'a[,]) : Neighbors.MaybeNeighborsWithDiagonals<'a> =
    let stepper direction =
        stepWithDiagonals EdgeHandling.Skip { X = x; Y = y } 1 direction grid
        |> Option.map snd
        |> Option.bind (fun x -> if x |> predicate then Some x else None)
        
    {
        Center = grid |> at (y, x)
        Y = y
        X = x
        Up = stepper Directions.WithDiagonals.Up
        Right = stepper Directions.WithDiagonals.Right
        Down = stepper Directions.WithDiagonals.Down
        Left = stepper Directions.WithDiagonals.Left
        UpLeft = stepper Directions.WithDiagonals.UpLeft
        UpRight = stepper Directions.WithDiagonals.UpRight
        DownLeft = stepper Directions.WithDiagonals.DownLeft
        DownRight = stepper Directions.WithDiagonals.DownRight
    }
    

let tryGetNeighborsWithDiagonals y x grid : Neighbors.MaybeNeighborsWithDiagonals<'a> = tryGetNeighborsWithDiagonalsWith (fun _ -> true) y x grid
 

/// <summary>
/// Flatten the grid and find the first cell matching the argument.
/// Grid is flattened by concatenating the rows
/// </summary>
let find<'a> (predicate: 'a -> bool) (grid: 'a[,]) =
    let index = grid |> flatten |> Array.findIndex predicate
    let width = grid |> width
    let y = index / width
    let x = index % width
    { Point2D.X = x; Point2D.Y = y }
            
            
/// <summary>
/// Will step into the given direction until the <paramref name="predicate"/> no longer holds.
/// Returns the last point as the first list element
/// </summary>
/// <example> Index 0 is value 1 in this example!<code>
/// Grid.stepWhile (fun i -> i &lt; 4) ... [| 1; 2; 3; 4; 5; 6; 7 ; 8; 9; 10 |] =
///     [ { Y = 0; X = 3 }; { Y = 0; X = 2 }; { Y = 0; X = 1 } ]
/// </code></example>
/// <param name="predicate">Predicate to check the cell value</param>
/// <param name="stepGenerator">Step generator</param>
/// <param name="edgeHandling">Behaviour when leaving the grid</param>
/// <param name="from">Start point</param>
/// <param name="direction">Direction into which to move</param>
/// <param name="grid">Grid to operate on</param>
let stepWhile<'direction, 'cell>
            (predicate: 'cell -> bool)
            (stepGenerator: 'direction -> Point2D<int>)
            (edgeHandling: EdgeHandling)
            (from: Point2D<int>)
            (direction: 'direction)
            (grid: 'cell[,])
            : (Point2D<int> * 'cell) list =
    let stepper current =
        grid
        |> step<'direction, 'cell> stepGenerator edgeHandling current 1 direction
        |> Option.filter (snd >> predicate)
        
    let gridValue point =
        grid |> at (point.Y, point.X)
        
    let rec step (current: Point2D<int>) (acc: (Point2D<int> * 'cell) list) : (Point2D<int> * 'cell) list =
        let currentValue = current |> gridValue
        if currentValue |> predicate then
            let newAcc = (current, currentValue) :: acc
            match current |> stepper with
            | None -> newAcc
            | Some (p, _) -> step p newAcc
        else acc
            
        (*
        match current |> stepper with
        | Some (point, value) when value |> predicate ->
            if current = from then step point acc
            else step point (current :: acc)
        | Some _
        | None -> acc
        *)
        
    match from |> stepper with
    | Some (point, _) -> step point []
    | None -> []
        
    
/// <summary>
/// Converts the grid into a pretty string like:
/// <code>"1 2 3\n
/// 4 5 6"</code>
/// </summary>
let toString (grid: 'a[,]) =
    let cols = grid |> width
    
    let rowAsString (row: 'a array) : string =
        let strings = row |> Array.map string
        String.Join(" ", strings)
    
    grid
    |> flatten
    |> Array.chunkBySize cols
    |> Array.fold (
        fun (acc: string) (next: 'a array) ->
            if String.IsNullOrWhiteSpace(acc) then
                next |> rowAsString
            else
                acc + Environment.NewLine + (next |> rowAsString))
        String.Empty

/// <summary>
/// Computes the Manhattan distance between two points on a two-dimensional grid.
/// The Manhatten distance is the distance between two points when only using steps to left, right, top or bottom
/// </summary>
let manhattanDistance (from: Point2D<int>) (``to``: Point2D<int>) : int =
    let xDistance = Math.Abs(from.X - ``to``.Y)
    let yDistance = Math.Abs(from.Y - ``to``.Y)
    xDistance + yDistance
    
    
let private colors = [|
    ConsoleColor.Blue
    ConsoleColor.Cyan
    ConsoleColor.Gray
    ConsoleColor.Green
    ConsoleColor.Magenta
    ConsoleColor.Red
    ConsoleColor.White
    ConsoleColor.Yellow
    ConsoleColor.DarkBlue
    ConsoleColor.DarkCyan
    ConsoleColor.DarkGray
    ConsoleColor.DarkGreen
    ConsoleColor.DarkMagenta
    ConsoleColor.DarkRed
    ConsoleColor.DarkYellow
|]

type GridColorer<'a> = {| Value: 'a; Y:int; X:int |} -> ConsoleColor

let createDefaultColorer<'a when 'a: equality> () : GridColorer<'a> =
    let mutable colorIndex = 0
    let colorsDict = System.Collections.Generic.Dictionary<'a, ConsoleColor>()
    (fun s ->
        if colorsDict.ContainsKey(s.Value) = false then
            colorsDict.Add(s.Value, colors[colorIndex % colors.Length])
            colorIndex <- colorIndex + 1
        colorsDict[s.Value])


let print<'a> (colorer: GridColorer<'a>) (grid: 'a[,]) =
    let cols = grid |> width
    do printf "  "
    do [0..cols-1] |> List.iter (fun i ->
        Console.ForegroundColor <- colors[(i / 10 + 6) % colors.Length]
        printf "%i" (i%10))
    do printfn ""
    do Console.ResetColor()
    do printfn " ┏%s" (String('━', cols))
    grid |> flatten |> Array.iteri (fun index value ->
        let y = index / cols
        let x = index % cols
        
        if x = 0 then
            do Console.ForegroundColor <- colors[(y / 10 + 6) % colors.Length]
            do Console.Write(y % 10)
            do Console.ResetColor()
            do Console.Write('┃')
        
        do Console.ForegroundColor <- ({| Value = value; X = x; Y = y |} |> colorer)
        let isNewLine = index % cols = (cols-1) && index > 0
        if isNewLine then
            Console.WriteLine(value)
        else Console.Write(value)
        do Console.Out.Flush()
        )
    do Console.ResetColor()


let transpose (grid: 'a[,]) =
    // switch dimensions because we transpose
    Array2D.init (grid |> width) (grid |> height) (fun y x -> grid |> at (x,y))