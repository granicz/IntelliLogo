namespace MyApp

open WebSharper
open WebSharper.UI
open WebSharper.UI.Templating
open WebSharper.JavaScript

[<JavaScript>]
module Communication =
    let fetch<'T> (url: string) (onSuccess: 'T -> unit) (onError: string -> unit) =
        let request = new XMLHttpRequest()
        request.Open("GET", url, true)
        request.Onreadystatechange <- fun _ ->
            if request.ReadyState = XMLHttpRequest.DONE then
                if request.Status = 200 then
                    try
                        let person = Json.Parse(request.ResponseText) :?> 'T
                        onSuccess person
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

    let InitializeExampleSelector() =
        Examples :=
            [|
              {
                Name = "Rose petals"
                FileName = "rose-petals.ilogo"
              }
              {
                Name = "Rose petals"
                FileName = "rose-petals.ilogo"
              }
            |]

        //Communication.fetch<Examples> "https://"
        //    <| fun exs ->
        //        exs.Examples
        //        |> Array.map (fun (ex: Example) -> ex.FileName, ex.Name)
        //        |> fun _ -> ()
        //    <| fun error -> ()

    let editorCaretPosition = Var.Create (1, 1)
    let updateCaretPosition (editor: Dom.Element) =
        let editor = editor :?> HTMLTextAreaElement 
        let caretPos = editor.SelectionStart
        let textUpToCaret = editor.Value.Substring(0, caretPos)
        let row = textUpToCaret |> Seq.fold (fun acc c -> if c = '\n' then acc+1 else acc) 1
        let col = caretPos - textUpToCaret.LastIndexOf('\n')
        editorCaretPosition := (row, col)

    [<SPAEntryPoint>]
    let Main =
        InitializeExampleSelector()
        let editor = Var.Create ""
        let console = Var.Create "Welcome to IntelliLogo!\n"
        // Do we have a previous editor session in local storage?
        // If so, bring it into the editor
        let store = JS.Window.LocalStorage
        let codeInStore = store.GetItem LocalStorageKeys.Code
        if not <| String.IsNullOrEmpty codeInStore then
            Var.Concat(console, "Previous coding session restored.")
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
                        let _, v = IntelliLogo.Evaluator.evalCommands env program
                        Var.Concat(console, sprintf "AST=%A\n" program)
                        Var.Concat(console, sprintf "Result=%A\n" v)
                    with
                    | IntelliLogo.Evaluator.StopEvaluation pos ->
                        Var.Concat(console, sprintf "Program stopped at %A." pos)
                    | exn ->
                        Var.Concat(console, sprintf "%A" exn)
                | Result.Error (token, pos) ->
                    Var.Concat(console, sprintf "Parse error before %s at %A\n" token pos)
            )
            .Bind()
