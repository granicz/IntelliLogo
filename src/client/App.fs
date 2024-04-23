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
        request.OnReadyStateChange <- fun _ ->
            if request.ReadyState = XMLHttpRequest.DONE then
                if request.Status = 200 then
                    try
                        if isRaw then
                            box request.ResponseText :?> 'T
                        else
                            Json.Parse(request.ResponseText) :?> 'T
                        |> onSuccess
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

    let [<Literal>] ZOOM_INCREMENT = 0.1

    type LoseChangesReason =
        | ExampleChanged
        | NewFile

    module LocalStorageKeys =
        let Code = "intellilogo-code"
        let ThemeIsDark = "intellilogo-theme-isdark"

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

    let EditorCaretPosition = Var.Create (1, 1)
    let UpdateCaretPosition (editor: Dom.Element) =
        let editor = editor :?> HTMLTextAreaElement 
        let caretPos = editor.SelectionStart
        let textUpToCaret = editor.Value.Substring(0, caretPos)
        let row = textUpToCaret |> Seq.fold (fun acc c -> if c = '\n' then acc+1 else acc) 1
        let col = caretPos - textUpToCaret.LastIndexOf('\n')
        EditorCaretPosition := (row, col)

    let setDisplayOf(id, display) =
        As<HTMLElement>(JS.Document.GetElementById id)
            .Style.SetProperty("display", display)

    let LoadSelectedExample(editor:Var<string>, console:Var<string>, editorHasChanges: Var<bool>, fpath:string, dom: Dom.Element) =
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
        let canvasScale = Var.Create 1.0
        let loseChangesReason: Var<LoseChangesReason option> = Var.Create None
        let editorHasChanges = Var.Create false
        let editor = Var.Create ""
        let console = Var.Create "Welcome to IntelliLogo!\n"
        let store = JS.Window.LocalStorage
        let setTheme isDark =
            if isDark then
                if not <| JS.Document.DocumentElement.ClassList.Contains "dark" then
                    JS.Document.DocumentElement.ClassList.Add("dark")
            else
                if JS.Document.DocumentElement.ClassList.Contains "dark" then
                    JS.Document.DocumentElement.ClassList.Remove("dark")
            store.SetItem(LocalStorageKeys.ThemeIsDark, string isDark)
        // Do we have a previous editor session in local storage?
        // If so, bring it into the editor
        let codeInStore = store.GetItem LocalStorageKeys.Code
        if not <| String.IsNullOrEmpty codeInStore then
            Var.Concat(console, "Previous coding session restored.\n")
            editor := codeInStore
        // Retrieve the theme setting, if any
        let isDark = store.GetItem LocalStorageKeys.ThemeIsDark
        if not <| String.IsNullOrEmpty isDark then
            if isDark.ToLower() = "true" then
                setTheme true
            else
                setTheme false
        let loadSelectedExample(vars: MainTemplate.Vars, anchors: MainTemplate.Anchors) =
            LoadSelectedExample(editor, console, editorHasChanges, vars.Examples.Value, anchors.ExamplesNode)
            CurrentExampleSelected := Some vars.Examples.Value
        let newFile(vars: MainTemplate.Vars, anchors: MainTemplate.Anchors) =
            CurrentExampleSelected := None
            editorHasChanges := false
            vars.Examples := ""
            editor := ""
            console := ""
        let applyCanvasScale(anchors: MainTemplate.Anchors) =
            async {
                let! scale = canvasScale
                As<HTMLElement>(anchors.Canvas)
                    .Style.SetProperty("width", $"{1000. * scale}px")
                As<HTMLElement>(anchors.Canvas)
                    .Style.SetProperty("height", $"{1000. * scale}px")
                As<HTMLElement>(anchors.Canvas)
                    .Style.SetProperty("transform", $"scale({scale})")
                As<HTMLElement>(anchors.Canvas)
                    .Style.SetProperty("transformOrigin", "center")
            } |> Async.StartImmediate
        MainTemplate()
            .Editor(editor)
            // We can put global initialization code here...
            .OAR_WithModel(fun e ->
                (e.Anchors.EditorNode :?> HTMLTextAreaElement).Focus()
            )
            .EditorOnClick(fun e -> UpdateCaretPosition e.Target)
            .EditorOnInput(fun e -> UpdateCaretPosition e.Target)
            .EditorOnKeyUp(fun e -> UpdateCaretPosition e.Target)
            .EditorOnScroll(fun e -> UpdateCaretPosition e.Target)
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
            .EditorCol(EditorCaretPosition.View.Map(snd >> string))
            .EditorRow(EditorCaretPosition.View.Map(fst >> string))
            .ExampleOptions(
                Examples.View.Doc(fun exs ->
                    exs |> Array.map (fun ex ->
                        Elt.option [attr.value ex.FileName] [text ex.Name] :> Doc
                    ) |> Doc.Concat
                )
            )
            .ExampleChanged(fun e ->
                if editorHasChanges.Value then
                    loseChangesReason := Some LoseChangesReason.ExampleChanged
                    setDisplayOf("popupOverlay", "flex")
                else
                    loadSelectedExample(e.Vars, e.Anchors)
            )
            .AcceptAbandoningChanges(fun e ->
                async {
                    setDisplayOf("popupOverlay", "none")
                    match! loseChangesReason with
                    | Some LoseChangesReason.ExampleChanged ->
                        loadSelectedExample(e.Vars, e.Anchors)
                    | Some LoseChangesReason.NewFile ->
                        newFile(e.Vars, e.Anchors)
                    | None ->
                        ()
                } |> Async.StartImmediate
            )
            .CancelAbandoningChanges(fun e ->
                async {
                    setDisplayOf("popupOverlay", "none")
                    match! loseChangesReason with
                    | Some LoseChangesReason.ExampleChanged ->
                        if CurrentExampleSelected.Value.IsSome then
                            // Revert the selected example in the dropdown
                            e.Vars.Examples := CurrentExampleSelected.Value.Value
                    | Some LoseChangesReason.NewFile ->
                        ()
                    | None ->
                        ()
                } |> Async.StartImmediate
            )
            .NewFile(fun e ->
                if editorHasChanges.Value then
                    loseChangesReason := Some LoseChangesReason.NewFile
                    setDisplayOf("popupOverlay", "flex")
                else
                    newFile(e.Vars, e.Anchors)
            )
            .ZoomIn(fun e ->
                async {
                    let! scale = canvasScale
                    canvasScale := scale + ZOOM_INCREMENT
                    applyCanvasScale(e.Anchors)
                } |> Async.StartImmediate
            )
            .ZoomOut(fun e ->
                async {
                    let! scale = canvasScale
                    canvasScale := scale - ZOOM_INCREMENT
                    applyCanvasScale(e.Anchors)
                } |> Async.StartImmediate
            )
            .Console(console)
            .ThemeToggle(fun e ->
                async {
                    let isDT = JS.Document.DocumentElement.ClassList.Contains("dark")
                    setTheme (not isDT)
                } |> Async.StartImmediate
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
                    | exn ->
                        Var.Concat(console, sprintf "Uncaught error: %s - %A" exn.Message exn)
                | Result.Error (token, pos) ->
                    Var.Concat(console, sprintf "Parse error before %s at %A\n" token pos)
            )
            .Bind()
