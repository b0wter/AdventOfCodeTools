module AoC.Shared.Library.Combinations

/// <summary>
/// Calculates the number of possible combinations for the given parameters.
/// In comparison to permutations combinations allow options to be selected multiple times!

/// roll a die multiple times.
/// </summary>
/// <param name="numberOfOptions">Number of possible states, e.g. 6 for a regular die</param>
/// <param name="numberOfSamples">Number of samples to choose from the option pool</param>
let count numberOfOptions numberOfSamples =
    numberOfOptions |> Shared.powerTo numberOfSamples

/// <summary>
/// Computes all possible combinations of the given input.
/// In comparison to permutations combinations reuse options.
/// </summary>
/// <example>
/// Given `[ 1; 2; 3 ]` as input will return 27 distinct lists <code>[ [1;1;1]; [2;1;1]; [3;1;1]; ...]</code>
/// </example>
let all (sampleSize: int) (options: _ list) : _ list list =
    let rec step (depth: int) (path: _ list) =
        if depth = sampleSize then [ path ]
        else
            options |> List.map (fun o -> o :: path) |> List.collect (step (depth + 1))
    step 0 []
