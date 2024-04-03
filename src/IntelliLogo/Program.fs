namespace IntelliLogo

open System.IO
open IntelliLogo

// Uncomment to parse a sample file, and retarget to net6.0+
//
//[<EntryPoint>]
//let Main args =
//    let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "example.ilogo"))
//    match Parse.ProgramFromString input with
//    | Ok program ->
//        printfn "AST=%A" program
//        let env = Evaluator.Environment.Create()
//        let env, v = Evaluator.evalCommands env program
//        printfn "Environment=%A" env
//        printfn "Result=%A" v
//        0
//    | Result.Error (token, pos) ->
//        printfn "Parse error before %s at %A" token pos
//        1
