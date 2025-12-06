module AoC.Shared.Library.Paths

open System
open System.Collections.Generic
open AoC.Shared.Library.Shared

/// <summary>
/// The heuristic gives a measure of the distance of two points
/// </summary>
type Heuristic<'coordinate, 'cost> = 'coordinate -> 'coordinate -> 'cost

let distanceBasedHeuristic (a: Point2D<int>) (b: Point2D<int>) : float =
    (Math.Abs(a.X - b.X) + Math.Abs(a.Y - b.Y)) |> float
    

let reconstructPath<'coordinate when 'coordinate: equality> (cameFrom: Dictionary<'coordinate, 'coordinate option>) (from: 'coordinate) (target: 'coordinate) =
    if cameFrom.ContainsKey(target) = false then failwith "cameFrom does not contain the target coordinate"
    else
        let rec step (current: 'coordinate) (path: 'coordinate list) =
            if current = from then path
            else
                match cameFrom[current] with
                | None -> failwith "cannot reconstruct path, dictionary contains None for a point that is not the start"
                | Some p -> step p (current :: path)
            
        step target []


/// <summary>
/// Uses the A* algorithm to find a path through a graph
/// </summary>
/// <param name="addCosts">Operation to sum costs</param>
/// <param name="zeroCost">Value for zero costs</param>
/// <param name="capCosts">Function to cap the costs, useful to not make integers overflow</param>
/// <param name="costFunction">Gives the cost to move from one coordinate on the graph to another coordinate</param>
/// <param name="getNeighbors">Get all reachable neighbors for the given node</param>
/// <param name="heuristic">Heuristic to prefer certain coordinates. Will be given the current coordinate on the coordinate of the goal</param>
/// <param name="from">Coordinate to start from</param>
/// <param name="target">Coordinate that shall be reached</param>
/// <typeparam name="'coordinate">Type for the coordinates inside the graph</typeparam>
/// <typeparam name="'cost">Type to describe the costs to move between coordinates</typeparam>
let aStar<'coordinate, 'cost when 'coordinate: equality and 'cost: comparison>
        (addCosts: 'cost -> 'cost -> 'cost)
        (zeroCost: 'cost)
        (costFunction: int -> 'coordinate -> 'coordinate -> 'cost)
        (getNeighbors: int -> 'coordinate -> 'coordinate list)
        (heuristic: int -> 'coordinate -> 'coordinate -> 'cost)
        (from: 'coordinate)
        (target: 'coordinate) =
    
    let frontier = PriorityQueue<'coordinate, 'cost>()
    frontier.Enqueue(from, zeroCost)
    let cameFrom = Dictionary<'coordinate, 'coordinate option>()
    let costSoFar = Dictionary<'coordinate, 'cost>()
    let timeSoFar = Dictionary<'coordinate, int>()
    do cameFrom[from] <- None
    do costSoFar[from] <- zeroCost
    do timeSoFar[from] <- 0
    
    while frontier.Count <> 0 do
        let current = frontier.Dequeue()
        
        if current <> target then
            let timestamp = timeSoFar[current]
            let neighbors = current |> getNeighbors timestamp
            neighbors |> List.iter (fun next ->
                if current = next then ()
                else
                    let nextCosts = addCosts costSoFar[current] (costFunction timestamp current next)
                    if costSoFar.ContainsKey(next) = false || nextCosts < costSoFar[next] then
                        do timeSoFar[next] <- timestamp + 1
                        do costSoFar[next] <- nextCosts
                        let priority = addCosts nextCosts (heuristic (timestamp + 1) next target)
                        do frontier.Enqueue(next, priority)
                        cameFrom[next] <- Some current
                    )
            
    try
        reconstructPath cameFrom from target
    with
    | exn -> []
    