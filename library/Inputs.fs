module AoC.Shared.Library.Inputs

open System
open System.IO

let asLineArray (filename: string) : string array =
    File.ReadAllLines(filename)
    
let asLineList (filename: string) : string list =
    filename |> asLineArray |> List.ofArray
    
type InputStyle
    = GridCells
    | Rows
    | Columns
    
let readGrid<'value, 'splitResult> (reader: unit -> string[]) (splitter: string -> 'splitResult[]) (map: 'splitResult -> 'value) : 'value[,] =
    let array2DFromArrayArray (input: 'a array array) : 'a[,] =
        Grid.createWith input.Length input[0].Length (fun y x -> input[y][x])
    reader () |> Array.map (splitter >> (Array.map map)) |> array2DFromArrayArray
    
/// <summary>
/// Reads a textfile and maps its contents to a grid
/// </summary>
/// <param name="filename">Name of the file to read</param>
/// <param name="splitter">Function to split a line of the text into cells</param>
/// <param name="map">Maps the cells from string to the required data type</param>
let readGridFromFile<'value, 'splitResult> filename splitter map =
    readGrid<'value, 'splitResult> (fun () -> filename |> File.ReadAllLines |> Array.where (not << String.IsNullOrWhiteSpace)) splitter map
    
let readInput<'a> (reader: unit -> string[]) (inputStyle: InputStyle) (separator: char) (map: string -> 'a) : 'a[,]=
    let array2DFromArrayArray (input: 'a array array) : 'a[,] =
        Grid.createWith input.Length input[0].Length (fun y x -> input[y][x])
        
    match inputStyle with
    | GridCells ->
        Array2D.zeroCreate 1 1
    | Rows ->
        reader ()
        |> Array.map _.Split(separator, StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (Array.map map)
        |> array2DFromArrayArray
    | Columns ->
        reader ()
        |> Array.filter (not << String.IsNullOrWhiteSpace)
        |> Array.map _.Split(separator, StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (Array.map map)
        |> Array.transpose
        |> array2DFromArrayArray
