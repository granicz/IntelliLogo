namespace MyApp

open WebSharper
open WebSharper.UI
open WebSharper.UI.Templating
open WebSharper.JavaScript

[<JavaScript>]
module Communication =
    let fetch<'T> (isRaw: bool) (url: string) (onSuccess: 'T -> unit) (onError: string -> unit) =
        let request = new XMLHttpRequest()
        request.Open("GET", url, true)
        request.Onreadystatechange <- fun _ ->
            if request.ReadyState = XMLHttpRequest.DONE then
                if request.Status = 200 then
                    try
                        let data =
                            if isRaw then
                                box request.ResponseText :?> 'T
                            else
                                Json.Parse(request.ResponseText) :?> 'T
                        onSuccess data
                    with
                        | ex -> onError $"Parsing error: {ex.Message}"
                else
                    onError $"HTTP error: {request.StatusText}"
        request.Send()

[<JavaScript>]
module Client =
    open System
    open WebSharper.UI.Notation
    open WebSharper.UI.Client
    open WebSharper.UI.Html

    type MainTemplate = Template<"index.html", ClientLoad.FromDocument, ServerLoad.WhenChanged>

    module LocalStorageKeys =
        let Code = "intellilogo-code"
        let Theme = "intellilogo-theme"

    type Example =
        {
            [<Name "name">] Name: string
            [<Name "filename">] FileName: string
        }
    
    type Examples =
        {
            [<Name "examples">] Examples : Example array
        }
    
    let Examples : Var<Example array> = Var.Create [||]
    let CurrentExampleSelected : Var<string option> = Var.Create None

    let InitializeExampleSelector() =
        Communication.fetch<Examples> false "/examples/config.json"
            <| fun exs ->
                Examples := exs.Examples
            <| fun error ->
                printfn "Error: %A" error

    let editorCaretPosition = Var.Create (1, 1)
    let updateCaretPosition (editor: Dom.Element) =
        let editor = editor :?> HTMLTextAreaElement 
        let caretPos = editor.SelectionStart
        let textUpToCaret = editor.Value.Substring(0, caretPos)
        let row = textUpToCaret |> Seq.fold (fun acc c -> if c = '\n' then acc+1 else acc) 1
        let col = caretPos - textUpToCaret.LastIndexOf('\n')
        editorCaretPosition := (row, col)

    [<Inline "document.getElementById($id).style.display = $display">]
    let setDisplayOf(id, display) = ()

    let loadSelectedExample(editor:Var<string>, console:Var<string>, editorHasChanges: Var<bool>, fpath:string, dom: Dom.Element) =
        dom.ClassList.Add("animate-progressbar")
        Communication.fetch<string> true $"/examples/{fpath}"
            <| fun code ->
                editor := code
                console := ""
                editorHasChanges := false
                dom.ClassList.Remove("animate-progressbar")
            <| fun error ->
                Var.Concat(console, sprintf "Error: %A\n" error)
                dom.ClassList.Remove("animate-progressbar")
       
    [<SPAEntryPoint>]
    let Main =
        InitializeExampleSelector()
        let editorHasChanges = Var.Create false
        let editor = Var.Create ""
        let console = Var.Create "Welcome to IntelliLogo!\n"
        // Do we have a previous editor session in local storage?
        // If so, bring it into the editor
        let store = JS.Window.LocalStorage
        let codeInStore = store.GetItem LocalStorageKeys.Code
        if not <| String.IsNullOrEmpty codeInStore then
            Var.Concat(console, "Previous coding session restored.\n")
            editor := codeInStore
        // Retrieve the theme setting, if any
        let theme = store.GetItem LocalStorageKeys.Theme
        if not <| String.IsNullOrEmpty theme then
            ()
        MainTemplate()
            .Editor(editor)
            // We can put global initialization code here...
            .OAR_WithModel(fun e ->
                (e.Anchors.EditorNode :?> HTMLTextAreaElement).Focus()
            )
            .EditorOnClick(fun e -> updateCaretPosition e.Target)
            .EditorOnInput(fun e -> updateCaretPosition e.Target)
            .EditorOnKeyUp(fun e -> updateCaretPosition e.Target)
            .EditorOnScroll(fun e -> updateCaretPosition e.Target)
            .EditorOnKeyDown(fun e ->
                editorHasChanges := true
                if e.Event.Key = "Tab" then
                    let target = e.Target :?> HTMLTextAreaElement
                    e.Event.PreventDefault()
                    let start = target.SelectionStart
                    let end' = target.SelectionEnd
                    target.Value <- target.Value.Substring(0, start) + "  " + target.Value.Substring(end')
                    target.SelectionStart <- start + 2
                    target.SelectionEnd <- start + 2
            )
            .EditorCol(editorCaretPosition.View.Map(snd >> string))
            .EditorRow(editorCaretPosition.View.Map(fst >> string))
            .ExampleOptions(
                Examples.View.Doc(fun exs ->
                    exs |> Array.map (fun ex ->
                        Elt.option [attr.value ex.FileName] [text ex.Name] :> Doc
                    ) |> Doc.Concat
                )
            )
            .ExampleChanged(fun e ->
                if editorHasChanges.Value then
                    setDisplayOf("popupOverlay", "flex")
                else
                    loadSelectedExample(editor, console, editorHasChanges, e.Vars.Examples.Value, e.Anchors.ExamplesNode)
                    CurrentExampleSelected := Some e.Vars.Examples.Value
            )
            .AcceptAbandoningChanges(fun e ->
                setDisplayOf("popupOverlay", "none")
                loadSelectedExample(editor, console, editorHasChanges, e.Vars.Examples.Value, e.Anchors.ExamplesNode)
                CurrentExampleSelected := Some e.Vars.Examples.Value
            )
            .CancelAbandoningChanges(fun e ->
                setDisplayOf("popupOverlay", "none")
                if CurrentExampleSelected.Value.IsSome then
                    // Revert the selected example in the dropdown
                    e.Vars.Examples := CurrentExampleSelected.Value.Value
            )
            .Console(console)
            .ThemeToggle(fun e ->
                JS.Document.DocumentElement.ClassList.Toggle("dark") |> ignore
            )
            .OnRun(fun e ->
                ClientBuiltins.ResetState()
                // Set the drawing context
                let canvas = e.Anchors.Canvas
                let ctx = As<HTMLCanvasElement>(canvas).GetContext "2d"
                ClientBuiltins.State.Ctx <- ctx
                // Set the output console
                ClientBuiltins.State.Console <- Some console
                // Save the program before execution
                let input = editor.Value
                store.SetItem(LocalStorageKeys.Code, input)
                match IntelliLogo.Parse.ProgramFromString input with
                | Ok program ->
                    // Clear console+canvas
                    console := ""
                    ClientBuiltins.State.Ctx.ClearRect(0, 0, ClientBuiltins.State.CanvasX, ClientBuiltins.State.CanvasY);
                    let env = IntelliLogo.Evaluator.Environment.Create()
                    // Inject the specific client builtins
                    let env =
                        { env with
                            BuiltIns =
                                ClientBuiltins.Core
                                |> List.fold (fun builtins (fs, body) ->
                                    fs |> List.fold (fun builtins f ->
                                        builtins |> Map.add f body
                                    ) builtins
                                ) env.BuiltIns
                        }
                    try
                        let (_, v), isStopped = IntelliLogo.Evaluator.evalCommands env program
                        Var.Concat(console, sprintf "AST=%A\n" program)
                        if isStopped then
                            Var.Concat(console, sprintf "Program stopped.\n")
                        Var.Concat(console, sprintf "Result=%A\n" v)
                    with
                    | IntelliLogo.Evaluator.ArgumentTypeMismatch(f, v1, v2, pos) ->
                        Var.Concat(console, sprintf "Type mismatch in %s, got %A and %A at %A" f v1 v2 pos)
                    | IntelliLogo.Evaluator.UnexpectedArgument(f, args, pos) ->
                        Var.Concat(console, sprintf "Unexpected argument(s) to %s at %A: %A" f args pos)
                    | IntelliLogo.Evaluator.NumberExpectedButGot(v, pos) ->
                        Var.Concat(console, sprintf "Number expected at %A, but got %A" pos v)
                    | IntelliLogo.Evaluator.BooleanExpected(v, pos) ->
                        Var.Concat(console, sprintf "Boolean expected at %A, but got %A" pos v)
                    | IntelliLogo.Evaluator.FunctionArityMismatch(f, count, pos) ->
                        Var.Concat(console, sprintf "%s doesn't like %d inputs at %A" f count pos)
                    | IntelliLogo.Evaluator.UndefinedFunction(f, pos) ->
                        Var.Concat(console, sprintf "I don't know how to %s at %A" f pos)
                    | IntelliLogo.Evaluator.UndefinedVariable(v, pos) ->
                        Var.Concat(console, sprintf "Undefined variable %s at %A" v pos)
                    | IntelliLogo.Evaluator.UninitializedLocalVariable(v, pos) ->
                        Var.Concat(console, sprintf "Uninitialized local variable %s at %A" v pos)
                    | IntelliLogo.Evaluator.StringConversionErrorList(v, pos) ->
                        Var.Concat(console, sprintf "Unexpected value (list) in string converstion %A at %A" v pos)
                    | IntelliLogo.Evaluator.StringConversionErrorNothing(v, pos) ->
                        Var.Concat(console, sprintf "Unexpected value (uninitialized) in string converstion %A at %A" v pos)
                    // We let other exceptions bubble to the JS console
                    //| exn ->
                    //    Var.Concat(console, sprintf "%s - %A" exn.Message exn)
                | Result.Error (token, pos) ->
                    Var.Concat(console, sprintf "Parse error before %s at %A\n" token pos)
            )
            .Bind()
