module AoC.Shared.Library.Directions
open AoC.Shared.Library.Shared


type Simple
    = Up
    | Right
    | Down
    | Left
    
    
let allSimpleDirections =
    [ Up; Right; Down; Left ]
    
    
type SimpleWithPayload<'a>
    = Up of 'a
    | Right of 'a
    | Down of 'a
    | Left of 'a


type WithDiagonals
    = Up
    | UpRight
    | Right
    | DownRight
    | Down
    | DownLeft
    | Left
    | UpLeft
    
    
let allDijgonals =
    [ Up; UpRight; Right; DownRight; Down; DownLeft; Left; UpLeft ]
    
    
type WithDiagonalsAndPayload<'a>
    = Up of 'a
    | UpRight of 'a
    | Right of 'a
    | DownRight of 'a
    | Down of 'a
    | DownLeft of 'a
    | Left of 'a
    | UpLeft of 'a
    
    
let SimpleAsSteps : Map<Simple, Point2D<int>> =
    Map [
        Simple.Up, {Y = -1; X = 0}
        Simple.Right, {Y = 0; X = 1}
        Simple.Down, {Y = 1; X = 0}
        Simple.Left, {Y = 0; X = -1}
    ]
let simpleStepGenerator direction =
    SimpleAsSteps[direction]


let WithDiagonalsAsSteps : Map<WithDiagonals, Point2D<int>> =
    Map [
        WithDiagonals.Up,         {Y = -1; X =  0}
        WithDiagonals.UpRight,    {Y = -1; X =  1}
        WithDiagonals.Right,      {Y =  0; X =  1}
        WithDiagonals.DownRight,  {Y =  1; X =  1}
        WithDiagonals.Down,       {Y =  1; X =  0}
        WithDiagonals.DownLeft,   {Y =  1; X = -1}
        WithDiagonals.Left,       {Y =  0; X = -1}
        WithDiagonals.UpLeft,     {Y = -1; X = -1}
    ]
let withDiagonalsStepGenerator direction =
    WithDiagonalsAsSteps[direction]
