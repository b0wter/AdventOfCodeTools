module AoC.Shared.Library.Permutations

    
/// <summary>
/// Generates all permutations for the given list.
/// </summary>
/// <remarks>
/// Does not currently allow the generation of subsets.
/// </remarks>
let all (options: _ list) : _ list list =
    let maxDepth = options.Length
    let rec step (depth: int) (path: _ list) =
        if depth = maxDepth then [ path ]
        else
            options |> List.except path |> List.map (fun o -> o :: path) |> List.collect (step (depth + 1))
    step 0 []
    
    
let almostAll (sampleSize: int) (options: _ list) : _ list list =
    let rec step (depth: int) (path: _ list) =
        if depth = sampleSize then [ path ]
        else
            options |> List.except path |> List.map (fun o -> o :: path) |> List.collect (step (depth + 1))
    step 0 []
    
    
/// <summary>
/// Calculates the number of possible permutations for the given parameters.
/// In comparison to combinations permutations do not allow options to be used multiple times.
/// Therefor must the sample size not exceed the number of options!
/// </summary>
/// <param name="numberOfOptions">Number of possible states</param>
/// <param name="numberOfSamples">Number of samples to choose from the option pool</param>
let count numberOfOptions numberOfSamples =
    if numberOfSamples > numberOfOptions then failwith "Sample size must not exceed number of options when calculating permutations"
    else (Shared.factorial numberOfOptions) / (Shared.factorial (numberOfOptions - numberOfSamples))


            