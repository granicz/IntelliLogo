namespace IntelliLogo

open FSharp.Text.Lexing
open IntelliLogo

module Parse =
    let ProgramFromString text = 
        let lexbuf = LexBuffer<char>.FromString text
        let initPos =
            {
                pos_fname = "..."
                pos_orig_lnum = 1
                pos_cnum = 1
                pos_lnum = 1
                pos_bol = 0
            }
        lexbuf.EndPos <- initPos
        lexbuf.StartPos <- initPos
        try
            Ok <| Parser.program Lexer.main lexbuf
        with
        | ParserError (msg, pos) ->
            Error <| (msg, pos)
        | exn ->
            Error <| (sprintf "%s - %s\n" exn.Message (System.String(lexbuf.Lexeme)), pos.ofToken lexbuf)

module Evaluator =
    open AST

    exception StopEvaluation of pos

    type Value =
        | Nothing
        | Number of float
        | Boolean of bool
        | Text of string
        | List of Value list
        | Explicit of Value

    type Environment =
        {
            Variables: Map<string, Value>
            Functions: Map<string, (string list * Commands)>
            BuiltIns: Map<string, (Environment -> pos -> Value list -> (Environment * Value))>
        }

    [<AutoOpen>]
    module Pervasives =
        let rec ValueToString = function
            | Value.Boolean b ->
                string b
            | Value.Explicit exp ->
                ValueToString exp
            | Value.List lst ->
                failwithf "Unexpected value: <List>"
            | Value.Nothing ->
                failwithf "Unexpected value: <Nothing>"
            | Value.Number n ->
                string n
            | Value.Text t ->
                t

    module Builtins =
        let rnd = System.Random()
        // Remember to add functions with lowercase names
        let Core : (string * (Environment -> pos -> Value list -> (Environment * Value))) list = [
            "debug_environment", (fun env _ args ->
                printfn "DEBUG ENV = %A" env
                env, Value.Nothing
            )
            "difference", (fun env pos args ->
                match args with
                | [Number n1; Number n2] ->
                    env, Value.Number(n1-n2)
                | _ ->
                    failwithf "Unexpected argument(s) to difference at %A: %A" pos args
            )
            "random", (fun env pos args ->
                match args with
                | [Number n] ->
                    env, Value.Number(rnd.Next(int(n)))
                | _ ->
                    failwithf "Unexpected argument(s) to random at %A: %A" pos args
            )
            "sentence", (fun env _ args ->
                env, args |> Seq.map ValueToString |> String.concat " " |> Value.Text
            )
            "stop", (fun _ pos args ->
                match args with
                | [] ->
                    raise (StopEvaluation pos)
                | _ ->
                    failwithf "Unexpected argument(s) to stop at %A: %A" pos args
            )
            "word", (fun env _ args ->
                env, args |> Seq.map ValueToString |> String.concat "" |> Value.Text
            )
        ]

    type Environment with
        static member Create() =
            {
                Variables = Map.empty
                Functions = Map.empty
                BuiltIns = Builtins.Core |> Map.ofList
            }

        member this.AddFunction(f, pars, body) =
            { this with
                Functions = Map.add f (pars, body) this.Functions
            }

        member this.AddVariable(v, value) =
            { this with
                Variables = Map.add v value this.Variables
            }

    let rec evalCommands (env: Environment) commands =
        commands
        |> List.fold (fun (env, _) stmt ->
            evalStatement env stmt
        ) (env, Value.Nothing)

    // Functions and variables have global scope, once
    // they appear, they are visible for the rest of the program.
    // Function parameters are local.
    and evalStatement (env: Environment) = function
        | Statement.FunDef((f, fpos), pars, body, pos) ->
            env.AddFunction(f.ToLower(), List.map fst pars, body), Value.Nothing
        // A repeat statement doesn't produce a return value.
        | Statement.Repeat(e, body, pos) ->
            let env, v = evalExpr env e
            match v with
            | Value.Number n ->
                let env, _ =
                    [1 .. (int) n]
                    |> List.fold (fun (env: Environment, _) i ->
                        let env = env.AddVariable("repcount", Value.Number i)
                        evalCommands env body
                    ) (env, Value.Nothing)
                env, Value.Nothing
            | _ ->
                failwithf "Number expected for loop count, got {%A}" v
        // An IF or IFELSE can return a value via OUTPUT.
        | Statement.If(cond, yes, pos) ->
            let env, v = evalExpr env cond
            match v with
            | Value.Boolean b ->
                if b then
                    let env, v = evalCommands env yes
                    match v with
                    | Value.Explicit v ->
                        env, v
                    | _ ->
                        env, Value.Nothing
                else
                    env, Value.Nothing
            | _ ->
                failwithf "Boolean expected for condition, got {%A}" v
        | Statement.IfElse(cond, yes, no, pos) ->
            let env, v = evalExpr env cond
            match v with
            | Value.Boolean b ->
                if b then
                    let env, v = evalCommands env yes
                    match v with
                    | Value.Explicit v ->
                        env, v
                    | _ ->
                        env, Value.Nothing
                else
                    let env, v = evalCommands env no
                    match v with
                    | Value.Explicit v ->
                        env, v
                    | _ ->
                        env, Value.Nothing
            | _ ->
                failwithf "Boolean expected for condition, got {%A}" v
        | Statement.Make((v, _), e, pos) ->
            let env, value = evalExpr env e
            env.AddVariable(v.ToLower(), value), Value.Nothing
        | Statement.FunCall((f, fpos), args, pos) ->
            evalFunCall env ((f, fpos), args, pos)
        | Statement.Output(e, pos) ->
            let env, v = evalExpr env e
            env, Value.Explicit v

    and evalFunCall env ((f, fpos), args, pos) =
        // Is this a user-defined function?
        if env.Functions.ContainsKey (f.ToLower()) then
            let pars, body = env.Functions.Item (f.ToLower())
            let argc = List.length args
            if argc <> List.length pars then
                failwithf "%s doesn't like %d inputs" f argc
            let env' =
                args |> List.fold2 (fun (env: Environment) (par: string) arg ->
                    let env, v = evalExpr env arg
                    env.AddVariable(par.ToLower(), v)
                ) env pars
            try
                let env', v = evalCommands env' body
                // Return old environment and the result
                match v with
                | Value.Explicit v ->
                    env, v
                | v ->
                    env, v
            with
            // If we encounter a "stop" command, we exit
            | StopEvaluation pos ->
                env, Value.Nothing
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
            let env, v = ff env fpos args
            // Return the result, flattening Explicits
            match v with
            | Value.Explicit v ->
                env, v
            | v ->
                env, v
        else
            failwithf "I don't know how to %s, at %A" f fpos

    and evalExpr env = function
        | Var (v, pos) ->
            match Map.tryFind (v.ToLower()) env.Variables with
            | Some value ->
                env, value
            | None ->
                failwithf "Undefined variable: %s at %A" v pos
        | Expr.Number (n, _) ->
            env, Value.Number n
        | Word (w, _) ->
            env, Value.Text w
        | Expr.Text (txt, _) ->
            env, Value.Text txt
        | FunCall ((f, fpos), args, pos) ->
            evalFunCall env ((f, fpos), args, pos)
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
                failwithf "Type mismatch in (+), got {%A} and {%A}" v1 v2
        | Expr.Sub (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Number (n1-n2)
            | _ ->
                failwithf "Type mismatch in (-), got {%A} and {%A}" v1 v2
        | Expr.Mul (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Number (n1*n2)
            | _ ->
                failwithf "Type mismatch in (*), got {%A} and {%A}" v1 v2
        | Expr.Div (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Number (n1/n2)
            | _ ->
                failwithf "Type mismatch in (/), got {%A} and {%A}" v1 v2
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
                failwithf "Type mismatch in (=|<>), got {%A} and {%A}" v1 v2
        | Expr.NotEq (e1, e2, pos) ->
            let env, v = evalExpr env (Expr.Eq(e1, e2, pos))
            match v with
            | Value.Boolean b ->
                env, Value.Boolean(not b)
            | v ->
                failwithf "Unexpected value in (<>), got {%A}" v
        | Expr.Gt (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Boolean (n1>n2)
            | _ ->
                failwithf "Type mismatch in (>), got {%A} and {%A}" v1 v2
        | Expr.Ge (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Boolean (n1>=n2)
            | _ ->
                failwithf "Type mismatch in (>=), got {%A} and {%A}" v1 v2
        | Expr.Lt (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Boolean (n1<n2)
            | _ ->
                failwithf "Type mismatch in (<), got {%A} and {%A}" v1 v2
        | Expr.Le (e1, e2, pos) ->
            let env, v1 = evalExpr env e1
            let env, v2 = evalExpr env e2
            match v1, v2 with
            | Value.Number n1, Value.Number n2 ->
                env, Value.Boolean (n1<=n2)
            | _ ->
                failwithf "Type mismatch in (<=), got {%A} and {%A}" v1 v2
