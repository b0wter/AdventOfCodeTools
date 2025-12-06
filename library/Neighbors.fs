module AoC.Shared.Library.Neighbors

type Neighbor<'a, 'direction> = {
    Y: int
    X: int
    Value: 'a
    InDirection: 'direction
}

/// <summary>
/// Stores the contents of all neighbors of a cell. Includes diagonals
/// </summary>
type NeighborsWithDiagonals<'a> =
    {
    /// <summary>
    /// The target cell
    /// </summary>
    Center: 'a
    /// <summary>
    /// Y coordinate of the target cell
    /// </summary>
    Y: int
    /// <summary>
    /// X coordinate of the target cell
    /// </summary>
    X: int
    Up: 'a
    UpRight: 'a
    Right: 'a
    DownRight: 'a
    Down: 'a
    DownLeft: 'a
    Left: 'a
    UpLeft: 'a
    }
    member this.AsSingleNeighbors() : Neighbor<'a, Directions.WithDiagonals> list =
        [
            {
                X = this.X + 1
                Y = this.Y
                Value = this.Right
                InDirection = Directions.WithDiagonals.Right
            }
            {
                X = this.X - 1
                Y = this.Y
                Value = this.Left
                InDirection = Directions.WithDiagonals.Left
            }
            {
                X = this.X
                Y = this.Y - 1
                Value = this.Up
                InDirection = Directions.WithDiagonals.Up
            }
            {
                X = this.X + 1
                Y = this.Y - 1
                Value = this.UpRight
                InDirection = Directions.WithDiagonals.UpRight
            }
            {
                X = this.X + 1
                Y = this.Y + 1
                Value = this.DownRight
                InDirection = Directions.WithDiagonals.DownRight
            }
            {
                X = this.X
                Y = this.Y + 1
                Value = this.Down
                InDirection = Directions.WithDiagonals.Down
            }
            {
                X = this.X - 1
                Y = this.Y + 1
                Value = this.DownLeft
                InDirection = Directions.WithDiagonals.DownLeft
            }
            {
                X = this.X - 1
                Y = this.Y - 1
                Value = this.UpLeft
                InDirection = Directions.WithDiagonals.UpLeft
            }
        ]
        
let getNeighborsWithDiagonalsAsList (neighbors: NeighborsWithDiagonals<'a>) =
    neighbors.AsSingleNeighbors()

/// <summary>
/// Stores the contents of all neighbors of a cell (including in diagonal directions).
/// Uses an option type to represent neighbors that might not exist (e.g. at borders)
/// </summary>
type MaybeNeighborsWithDiagonals<'a> =
    {
    /// <summary>
    /// The target cell
    /// </summary>
    Center: 'a
    /// <summary>
    /// Y coordinate of the target cell
    /// </summary>
    Y: int
    /// <summary>
    /// X coordinate of the target cell
    /// </summary>
    X: int
    Up: 'a option
    UpRight: 'a option
    Right: 'a option
    DownRight: 'a option
    Down: 'a option
    DownLeft: 'a option
    Left: 'a option
    UpLeft: 'a option
    }
    member this.AsSingleNeighbors() : Neighbor<'a, Directions.WithDiagonals> list =
        [
            (this.Right |> Option.map (fun r ->
                {
                    X = this.X + 1
                    Y = this.Y
                    Value = r
                    InDirection = Directions.WithDiagonals.Right
                }))
            (this.Left |> Option.map (fun l ->
                {
                    X = this.X - 1
                    Y = this.Y
                    Value = l
                    InDirection = Directions.WithDiagonals.Left
                }))
            (this.Up |> Option.map (fun u ->
                {
                    X = this.X
                    Y = this.Y - 1
                    Value = u
                    InDirection = Directions.WithDiagonals.Up
                }))
            (this.UpRight |> Option.map (fun ur ->
                {
                    X = this.X + 1
                    Y = this.Y - 1
                    Value = ur
                    InDirection = Directions.WithDiagonals.UpRight
                }))
            (this.DownRight |> Option.map (fun dr ->
                {
                    X = this.X + 1
                    Y = this.Y + 1
                    Value = dr
                    InDirection = Directions.WithDiagonals.DownRight
                }))
            (this.Down |> Option.map (fun d ->
                {
                    X = this.X
                    Y = this.Y + 1
                    Value = d
                    InDirection = Directions.WithDiagonals.Down
                }))
            (this.DownLeft |> Option.map (fun dl ->
                {
                    X = this.X - 1
                    Y = this.Y + 1
                    Value = dl
                    InDirection = Directions.WithDiagonals.DownLeft
                }))
            (this.UpLeft |> Option.map (fun ul ->
                {
                    X = this.X - 1
                    Y = this.Y - 1
                    Value = ul
                    InDirection = Directions.WithDiagonals.UpLeft
                }))
        ] |> List.choose id
        
        
let getMaybeNeighborsWithDiagonalsAsList<'a> (n: MaybeNeighborsWithDiagonals<'a>) = n.AsSingleNeighbors()
        

/// <summary>
/// Stores the contents of all neighbors of a cell. Does not include diagonals
/// </summary>
type Neighbors<'a> =
    {
    /// <summary>
    /// The target cell
    /// </summary>
    Center: 'a
    /// <summary>
    /// Y coordinate of the target cell
    /// </summary>
    Y: int
    /// <summary>
    /// X coordinate of the target cell
    /// </summary>
    X: int
    Up: 'a
    Right: 'a
    Down: 'a
    Left: 'a
    }
    member this.AsSingleNeighbors() : Neighbor<'a, Directions.Simple> list =
        [
            {
                X = this.X + 1
                Y = this.Y
                Value = this.Right
                InDirection = Directions.Simple.Right
            }
            {
                X = this.X - 1
                Y = this.Y
                Value = this.Left
                InDirection = Directions.Simple.Left
            }
            {
                X = this.X
                Y = this.Y - 1
                Value = this.Up
                InDirection = Directions.Simple.Up
            }
            {
                X = this.X
                Y = this.Y + 1
                Value = this.Down
                InDirection = Directions.Simple.Down
            }
        ]
        
let getNeighborsAsList (neighbors: Neighbors<'a>) = neighbors.AsSingleNeighbors()

/// <summary>
/// Stores the contents of all neighbors of a cell. Does not include diagonals.
/// Uses an option type to represent neighbors that might not exist (e.g. at borders)
/// </summary>
type MaybeNeighbors<'a> =
    {
    /// <summary>
    /// The target cell
    /// </summary>
    Center: 'a
    /// <summary>
    /// Y coordinate of the target cell
    /// </summary>
    Y: int
    /// <summary>
    /// X coordinate of the target cell
    /// </summary>
    X: int
    Up: 'a option
    Right: 'a option
    Down: 'a option
    Left: 'a option
    }
    member this.AsSingleNeighbors() : Neighbor<'a, Directions.Simple> list =
        [
            match this.Up with
            | Some value -> 
                yield { X = this.X; Y = this.Y - 1; Value = value; InDirection = Directions.Simple.Up }
            | None -> ()
            
            match this.Right with
            | Some value -> 
                yield { X = this.X + 1; Y = this.Y; Value = value; InDirection = Directions.Simple.Right }
            | None -> ()

            match this.Down with
            | Some value -> 
                yield { X = this.X; Y = this.Y + 1; Value = value; InDirection = Directions.Simple.Down }
            | None -> ()

            match this.Left with
            | Some value -> 
                yield { X = this.X - 1; Y = this.Y; Value = value; InDirection = Directions.Simple.Left }
            | None -> ()
        ]
    member this.AsSinglePoints() : Shared.Point2D<int> list =
        [
            match this.Up with
            | Some _ -> 
                yield { X = this.X; Y = this.Y - 1 }
            | None -> ()
            
            match this.Right with
            | Some _ -> 
                yield { X = this.X + 1; Y = this.Y }
            | None -> ()

            match this.Down with
            | Some _ -> 
                yield { X = this.X; Y = this.Y + 1 }
            | None -> ()

            match this.Left with
            | Some _ -> 
                yield { X = this.X - 1; Y = this.Y }
            | None -> ()
        ]

let getMaybeNeighborsAsList (m: MaybeNeighbors<'a>) = m.AsSingleNeighbors()

let getMaybeNeighborsAsPointList (m: MaybeNeighbors<_>) = m.AsSinglePoints()
    
    