module AoC.Shared.Library.Shared

/// <summary>
/// Computes powers for integer values
/// </summary>
let powerTo exp ``base`` =
    let mutable result = 1
    for i in 1 .. exp do
        result <- result * ``base``
    result
    
    
/// <summary>
/// Recursively computes a factorial
/// </summary>
let factorial : int -> int =
    let cache = System.Collections.Generic.Dictionary<int, int>()
    fun n ->
        if n |> cache.ContainsKey = false then 
            let rec step n acc =
                match n with
                | 0 | 1 -> acc
                | _ -> step (n - 1) (n * acc)
            cache[n] <- step n 1
        cache[n]
    
    
/// <summary>
/// Recursively computes a factorial
/// </summary>
let factorialf : float -> float =
    let cache = System.Collections.Generic.Dictionary<float, float>()
    fun n ->
        if n |> cache.ContainsKey = false then 
            let rec step n acc =
                match n with
                | 0.0 | 1.0 -> acc
                | _ -> step (n - 1.0) (n * acc)
            cache[n] <- step n 1.0
        cache[n]

    
type Point2D<'a> =
    {
        Y: 'a
        X: 'a
    }
    static member (+) (p1: Point2D<int>, p2: Point2D<int>) =
        { Y = p1.Y + p2.Y; X = p1.X + p2.X }
    static member (-) (p1: Point2D<int>, p2: Point2D<int>) =
        { Y = p1.Y - p2.Y; X = p1.X - p2.X }
    static member (+) (p1: Point2D<float>, p2: Point2D<float>) =
        { Y = p1.Y + p2.Y; X = p1.X + p2.X }
    static member (-) (p1: Point2D<float>, p2: Point2D<float>) =
        { Y = p1.Y - p2.Y; X = p1.X - p2.X }
    static member (*) (p: Point2D<int>, factor: int) =
        { Y = p.Y * factor; X = p.X * factor }
    static member (*) (p: Point2D<float>, factor: int) =
        { Y = p.Y * (factor |> float); X = p.X * (factor |> float) }
    static member (*) (p: Point2D<float>, factor: float) =
        { Y = p.Y * factor; X = p.X * factor }
        
    /// <summary>
    /// Computes the (integer) distance between two points
    /// </summary>
    static member (%) (p1: Point2D<int>, p2: Point2D<int>) : int =
        System.Math.Abs(p1.X - p2.X) + System.Math.Abs(p1.Y - p2.Y)
