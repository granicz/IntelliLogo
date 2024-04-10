namespace MyApp

[<AutoOpen>]
module Extensions =
    open WebSharper.UI
    open WebSharper.UI.Notation

    type Var with
        static member Concat(a: Var<string>, s: string) =
            a := a.Value + s

/// Built-in functions provided by the IDE
/// This includes all graphics, etc. commands.
module ClientBuiltins =
    open WebSharper.JavaScript
    open WebSharper.UI
    open IntelliLogo.Evaluator

    type LogoState =
        {
            mutable CanvasX : float
            mutable CanvasY : float
            mutable CurrentX : float
            mutable CurrentY : float
            mutable Direction : float
            mutable PenDown : bool
            mutable Ctx : CanvasRenderingContext2D
            mutable Console : Var<string> option
        }

    let State =
        {
            CanvasX = 1000.
            CanvasY = 1000.
            CurrentX = 500.
            CurrentY = 500.
            Direction = 90.
            PenDown = true
            Ctx = null
            Console = None
        }

    let ResetState() =
        State.CanvasX <- 1000.
        State.CanvasY <- 1000.
        State.CurrentX <- 500.
        State.CurrentY <- 500.
        State.Direction <- 90.
        State.PenDown <- true

    module TurtleOperations =
        open System

        let move(x, y) =
            State.CurrentX <- x
            State.CurrentY <- y

        let line(x: float, y: float) =
            State.Ctx.BeginPath()
            State.Ctx.MoveTo(State.CurrentX, State.CurrentY)
            State.Ctx.LineTo(x, y)
            State.Ctx.Stroke()
            move(x, y)

        let LT (angle: float) =
            State.Direction <- State.Direction + angle
        
        let RT (angle: float) =
            State.Direction <- State.Direction - angle

        let FD (dist: float) =
            let rad = State.Direction / 180. * Math.PI
            if State.PenDown then
                line(State.CurrentX+dist*Math.Cos(rad), State.CurrentY-dist*Math.Sin(rad))
            else
                move(State.CurrentX+dist*Math.Cos(rad), State.CurrentY-dist*Math.Sin(rad))

        let BK (dist: float) =
            let rad = State.Direction / 180. * Math.PI
            if State.PenDown then
                line(State.CurrentX-dist*Math.Cos(rad), State.CurrentY+dist*Math.Sin(rad))
            else
                move(State.CurrentX-dist*Math.Cos(rad), State.CurrentY+dist*Math.Sin(rad))

    // Remember to add functions with lowercase names
    let Core : (string list * (Environment -> IntelliLogo.pos -> Value list -> ((Environment * Value) * bool))) list = [
        // home -> pendown is unchanged
        ["home"], (fun env _ args ->
            State.CurrentX <- 500
            State.CurrentY <- 500
            (env, Value.Nothing), false
        )
        ["rt"; "right"], (fun env pos args ->
            match args with
            | [Value.Number n] ->
                TurtleOperations.RT n
            | _ ->
                raise (NumberExpectedButGot(args, pos))
            (env, Value.Nothing), false
        )
        ["lt"; "left"], (fun env pos args ->
            match args with
            | [Value.Number n] ->
                TurtleOperations.LT n
            | _ ->
                raise (NumberExpectedButGot(args, pos))
            (env, Value.Nothing), false
        )
        ["fd"; "forward"], (fun env pos args ->
            match args with
            | [Value.Number n] ->
                TurtleOperations.FD n
            | _ ->
                raise (NumberExpectedButGot(args, pos))
            (env, Value.Nothing), false
        )
        ["bk"; "back"], (fun env pos args ->
            match args with
            | [Value.Number n] ->
                TurtleOperations.BK n
            | _ ->
                raise (NumberExpectedButGot(args, pos))
            (env, Value.Nothing), false
        )
        ["pu"; "penup"], (fun env pos args ->
            match args with
            | [] ->
                State.PenDown <- false
            | _ ->
                raise (NumberExpectedButGot(args, pos))
            (env, Value.Nothing), false
        )
        ["pd"; "pendown"], (fun env pos args ->
            match args with
            | [] ->
                State.PenDown <- true
            | _ ->
                raise (NumberExpectedButGot(args, pos))
            (env, Value.Nothing), false
        )
        ["print"], (fun env pos args ->
            args |> List.iter (fun arg ->
                let s = ValueToString pos arg
                if State.Console.IsSome then
                    Var.Concat(State.Console.Value, s + "\n")
            )
            (env, Value.Nothing), false
        )
    ]

