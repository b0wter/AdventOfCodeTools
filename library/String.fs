module AoC.Shared.Library.String

open System

let splitByStringAndTrim (delimiter: string) (s: string) =
    s.Split(delimiter, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

let splitByCharAndTrim (delimiter: char) (s: string) =
    s.Split(delimiter, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    
