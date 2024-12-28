namespace IntelliLogo

open FSharp.Text.Lexing
open IntelliLogo

module Parse =
    let ProgramFromString text = 
        let lexbuf = LexBuffer<char>.FromString text
        let initPos =
            {
                pos_fname = ""
                pos_orig_lnum = 1
                pos_cnum = 1
                pos_lnum = 1
                pos_bol = 0
            }
        lexbuf.StartPos <- initPos
        lexbuf.EndPos <- initPos
        try
            Ok <| Parser.program Lexer.main lexbuf
        with
        | ParserError (msg, pos) ->
            Error <| (msg, pos)
        | exn ->
            Error <| (sprintf "%s - %s\n" exn.Message (System.String(lexbuf.Lexeme)), pos.ofToken lexbuf)

module Evaluator =
    open AST

    type Value =
        | Nothing
        | Number of float
        | Boolean of bool
        | Text of string
        | List of Value list
        | Explicit of Value

    exception ArgumentTypeMismatch of f:string * v1:Value * v2:Value * pos
    exception ResultTypeMismatch of f:string * v:Value * pos
    exception UnexpectedArgument of f:string * args: Value list * pos
    exception NumberExpectedButGot of v:Value list * pos
    exception BooleanExpected of v:Value * pos
    exception FunctionArityMismatch of f:string * int * pos
    exception UndefinedFunction of f:string * pos
    exception UndefinedVariable of v:string * pos
    exception UninitializedLocalVariable of v:string * pos
    exception StringConversionErrorList of Value * pos
    exception StringConversionErrorNothing of Value * pos

    type Environment =
        {
            GlobalVariables: Map<string, Value>
            LocalVariables: Map<string, Value option>
            Functions: Map<string, (string list * Commands)>
            BuiltIns: Map<string, (Environment -> pos -> Value list -> ((Environment * Value) * bool))>
        }

    [<AutoOpen>]
    module Pervasives =
        type List<'a> with
            static member foldUntil f init list =
                let rec loop acc list =
                    match list with
                    | [] -> (acc, false)
                    | x::xs ->
                        let (newAcc, isStopped) = f acc x
                        if isStopped then (newAcc, true)
                        else loop newAcc xs
                loop init list

        type PrintKind =
            | Show
            | Print
            | Type

        let rec ValueToString kind isTop pos = function
            | Value.Boolean b ->
                string b
            | Value.Explicit exp ->
                ValueToString kind isTop pos exp
            | Value.List lst as v ->
                let inside =
                    lst
                    |> List.map (fun v -> ValueToString kind false pos v)
                    |> List.reduce (fun s1 s2 ->
                        match kind with
                        | Print
                        | Show ->
                            s1 + " " + s2
                        | Type ->
                            s1 + s2
                    )
                match kind with
                | Print
                | Type ->
                    if isTop then
                        inside
                    else
                        "[" + inside + "]"
                | Show ->
                    inside
                //raise (StringConversionErrorList(v, pos))
            | Value.Nothing as v ->
                raise (StringConversionErrorNothing(v, pos))
            | Value.Number n ->
                string n
            | Value.Text t ->
                t

        let rec NonListValueToString pos = function
            | Value.Boolean b ->
                string b
            | Value.Explicit exp ->
                NonListValueToString pos exp
            | Value.List lst as v ->
                raise (StringConversionErrorList(v, pos))
            | Value.Nothing as v ->
                raise (StringConversionErrorNothing(v, pos))
            | Value.Number n ->
                string n
            | Value.Text t ->
                t

    module Builtins =
        let rnd = System.Random()
        // Remember to add functions with lowercase names
        let Core : (string * (Environment -> pos -> Value list -> ((Environment * Value) * bool))) list = [
            "debug_environment", (fun env _ args ->
                printfn "DEBUG ENV = %A" env
                (env, Value.Nothing), false
            )
            "difference", (fun env pos args ->
                match args with
                | [Number n1; Number n2] ->
                    (env, Value.Number(n1-n2)), false
                | _ ->
                    raise (UnexpectedArgument ("difference", args, pos))
            )
            "form", (fun env pos args ->
                match args with
                | [Number n; Number width; Number dec] ->
                    let width, dec = int width, int dec
                    // Convert number to string with fixed decimal places
                    let multiplier = System.Math.Pow(10.0, float dec)
                    let rounded = System.Math.Round(n * multiplier) / multiplier
                    let numStr = rounded.ToString()
                    // Determine how many characters are taken by the decimal part and the decimal point
                    let decimalPartLength = if dec > 0 then dec + 1 else 0
                    // Calculate the total length of the number part
                    let totalNumLength = numStr.Length
                    // Calculate necessary padding
                    let paddingSize = width - totalNumLength
                    let padding = if paddingSize > 0 then String.replicate paddingSize " " else ""
                    // Construct the final string
                    (env, Value.Text (padding + numStr)), false
                    // The below is supposed to work, but it doesn't
                    //(env, Value.Text <| sprintf "%*.*f" (int width) (int dec) n), false
                | _ ->
                    raise (UnexpectedArgument ("form", args, pos))
            )
            "random", (fun env pos args ->
                match args with
                | [Number n] ->
                    (env, Value.Number(rnd.Next(int n))), false
                | _ ->
                    raise (UnexpectedArgument ("random", args, pos))
            )
            "sentence", (fun env pos args ->
                (env,
                    args
                    |> Seq.map (NonListValueToString pos)
                    |> String.concat " "
                    |> Value.Text),
                        false
            )
            "stop", (fun env pos args ->
                match args with
                | [] ->
                    // Stop the execution of the current user-defined function
                    (env, Value.Nothing), true
                | _ ->
                    raise (UnexpectedArgument ("stop", args, pos))
            )
            "word", (fun env pos args ->
                (env,
                    args
                    |> Seq.map (NonListValueToString pos)
                    |> String.concat ""
                    |> Value.Text),
                        false
            )
        ]

    type Environment with
        static member Create() =
            {
                GlobalVariables = Map.empty
                LocalVariables = Map.empty
                Functions = Map.empty
                BuiltIns = Builtins.Core |> Map.ofList
            }

        member this.AddFunction(f, pars, body) =
            { this with
                Functions = Map.add f (pars, body) this.Functions
            }

        member this.AddGlobalVariable(v, value) =
            { this with
                GlobalVariables = Map.add v value this.GlobalVariables
            }

        member this.AddLocalVariable(v, value) =
            { this with
                LocalVariables = Map.add v value this.LocalVariables
            }

        member this.PreserveGlobalStateFrom(env: Environment) =
            { this with
                GlobalVariables = env.GlobalVariables
            }

    let rec evalCommands (env: Environment) commands : ((Environment * Value) * bool)=
        commands
        |> List.foldUntil (fun (env, v) stmt ->
            evalStatement env stmt
        ) (env, Value.Nothing)

    // Evaluates a list of commands, assuming that they can only
    // return a value via OUTPUT.
    and evalStatementCommands env body =
        let (env, v), isStopped = evalCommands env body
        match v with
        | Value.Explicit v ->
            (env, v), isStopped
        | _ ->
            (env, Value.Nothing), isStopped

    // Functions and global variables have global scope, once
    // they appear, they are visible for the rest of the program.
    // Function parameters and local variables are local.
    and evalStatement (env: Environment) stmt : (Environment * Value) * bool =
        match stmt with
        | Statement.FunDef((f, fpos), pars, body, pos) ->
            (env.AddFunction(f.ToLower(), List.map fst pars, body),
                Value.Nothing), false
        // A repeat statement doesn't produce a return value.
        | Statement.Repeat(e, body, pos) ->
            let env, v = evalExpr env e
            match v with
            | Value.Number n ->
                [1 .. (int) n]
                |> List.foldUntil (fun (env: Environment, _) i ->
                    let env = env.AddLocalVariable("repcount", Some (Value.Number i))
                    evalCommands env body
                ) (env, Value.Nothing)
            | _ ->
                raise (NumberExpectedButGot([v], pos))
        | Statement.For((i, ipos), from, _to, step, body, pos) ->
            let env', from = evalExpr env from
            let env', _to = evalExpr env' _to
            match from with
            | Number from ->
                match _to with
                | Number _to ->
                    [int from .. int _to]
                    |> List.foldUntil (fun (env: Environment, _) i ->
                        let env = env.AddLocalVariable("i", Some (Value.Number i))
                        evalCommands env body
                    ) (env, Value.Nothing)
                | _ ->
                    raise (NumberExpectedButGot ([_to], pos))
            | _ ->
                raise (NumberExpectedButGot ([from], pos))
        // An IF or IFELSE can return a value via OUTPUT.
        | Statement.If(cond, yes, pos) ->
            let env, v = evalExpr env cond
            match v with
            | Value.Boolean b ->
                if b then
                    evalStatementCommands env yes
                else
                    (env, Value.Nothing), false
            | _ ->
                raise (BooleanExpected (v, pos))
        | Statement.IfElse(cond, yes, no, pos) ->
            let env, v = evalExpr env cond
            match v with
            | Value.Boolean b ->
                if b then
                    evalStatementCommands env yes
                else
                    evalStatementCommands env no
            | _ ->
                raise (BooleanExpected (v, pos))
        | Statement.Make((v, _), e, pos) ->
            let env, value = evalExpr env e
            if env.LocalVariables.ContainsKey(v.ToLower()) then
                (env.AddLocalVariable(v.ToLower(), Some value), Value.Nothing), false
            else
                (env.AddGlobalVariable(v.ToLower(), value), Value.Nothing), false
        | Statement.MakeLocal((v, _), e, pos) ->
            let env, value = evalExpr env e
            (env.AddLocalVariable(v.ToLower(), Some value), Value.Nothing), false
        | Statement.Local((v, _), pos) ->
            (env.AddLocalVariable(v.ToLower(), None), Value.Nothing), false
        | Statement.FunCall((f, fpos), args, pos) ->
            evalFunCall env ((f, fpos), args, pos)
        | Statement.Output(e, pos) ->
            let env, v = evalExpr env e
            (env, Value.Explicit v), false

    and evalFunCall env ((f, fpos), args, pos) =
        // Is this a user-defined function?
        if env.Functions.ContainsKey (f.ToLower()) then
            let pars, body = env.Functions.Item (f.ToLower())
            let argc = List.length args
            if argc <> List.length pars then
                raise (FunctionArityMismatch(f, argc, pos))
            let env' =
                args |> List.fold2 (fun (env: Environment) (par: string) arg ->
                    let env, v = evalExpr env arg
                    env.AddLocalVariable(par.ToLower(), Some v)
                ) env pars
            let (env', v), isStopped = evalCommands env' body
            // Return old environment + global updates and the result.
            match v with
            | Value.Explicit v
            | v ->
                (env.PreserveGlobalStateFrom(env'), v), false
        // Is it a built-in function?
        elif env.BuiltIns.ContainsKey (f.ToLower()) then
            let ff = env.BuiltIns.Item (f.ToLower())
            let env, args =
                args
                |> List.fold (fun (env, args) arg ->
                    let env, v = evalExpr env arg
                    env, v :: args
                ) (env, [])
                |> fun (env, args) ->
                    env, List.rev args
            let (env, v), isStopped = ff env fpos args
            // Return the result, flattening Explicits
            match v with
            | Value.Explicit v
            | v ->
                // We don't litter up the environment,
                // nor is there anything we want to carry
                // over from there.
                (env, v), isStopped
        else
            raise (UndefinedFunction(f, fpos))

    // Expressions can't contain STOP
    and evalExpr env = function
        | RepCount pos ->
            evalExpr env (Var ("repcount", pos))
        | Var (v, pos) ->
            match Map.tryFind (v.ToLower()) env.GlobalVariables with
            | Some value ->
                env, value
            | None ->
                match Map.tryFind (v.ToLower()) env.LocalVariables with
                | Some (Some value) ->
                    env, value
                | Some None ->
                    raise (UninitializedLocalVariable(v, pos))
                | None ->
                    raise (UndefinedVariable(v, pos))
        | Expr.Number (n, _) ->
            env, Value.Number n
        | Word (w, _) ->
            env, Value.Text w
        | Expr.Text (txt, _) ->
            env, Value.Text txt
        | FunCall ((f, fpos), args, pos) ->
            let (env, v), _ = evalFunCall env ((f, fpos), args, pos)
            env, v
        | Expr.List (es, pos) ->
            es
            |> List.fold (fun (env, lst) e ->
                let env, e = evalExpr env e
                env, e :: lst) (env, [])
            |> fun (env, lst) ->
                env, Value.List (List.rev lst)
        | Expr.Add (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Number (n1+n2)
            | _ ->
                raise (ArgumentTypeMismatch ("(+)", v1, v2, pos))
        | Expr.Sub (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Number (n1-n2)
            | _ ->
                raise (ArgumentTypeMismatch ("(-)", v1, v2, pos))
        | Expr.Mul (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Number (n1*n2)
            | _ ->
                raise (ArgumentTypeMismatch ("(*)", v1, v2, pos))
        | Expr.Div (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Number (n1/n2)
            | _ ->
                raise (ArgumentTypeMismatch ("(/)", v1, v2, pos))
        | Expr.Eq (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Boolean(n1=n2)
            | Value.Boolean b1, Value.Boolean b2 ->
                env, Value.Boolean(b1=b2)
            | Value.Text t1, Value.Text t2 ->
                env, Value.Boolean(t1=t2)
            | Value.List l1, Value.List l2 ->
                let rec leq lst1 lst2 =
                    match lst1, lst2 with
                    | [], [] ->
                        true
                    | a :: lst1, b :: lst2 when a=b ->
                        leq lst1 lst2
                    | _ ->
                        false
                env, Value.Boolean(leq l1 l2)
            | _ ->
                raise (ArgumentTypeMismatch ("(=|<>)", v1, v2, pos))
        | Expr.NotEq (e1, e2, pos) ->
            let env, v = evalExpr env (Expr.Eq(e1, e2, pos))
            match v with
            | Value.Boolean b ->
                env, Value.Boolean(not b)
            | v ->
                raise (ResultTypeMismatch ("(<>)", v, pos))
        | Expr.Gt (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Boolean (n1>n2)
            | _ ->
                raise (ArgumentTypeMismatch ("(>)", v1, v2, pos))
        | Expr.Ge (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Boolean (n1>=n2)
            | _ ->
                raise (ArgumentTypeMismatch ("(>=)", v1, v2, pos))
        | Expr.Lt (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Boolean (n1<n2)
            | _ ->
                raise (ArgumentTypeMismatch ("(<)", v1, v2, pos))
        | Expr.Le (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Boolean (n1<=n2)
            | _ ->
                raise (ArgumentTypeMismatch ("(<=)", v1, v2, pos))
