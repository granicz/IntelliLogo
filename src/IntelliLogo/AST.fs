namespace IntelliLogo

open FSharp.Text

module Var =
    let mutable counter = 0
    let Fresh s =
        counter <- counter+1
        sprintf "%s'%d" s counter

[<StructuredFormatDisplay("{ToString}")>]
type pos =
    {
        Filename: string
        StartLine: int
        StartColumn: int
        EndLine: int
        EndColumn: int
    }

    static member Dummy() =
        {
            Filename = "<dummy>"
            StartLine = 1
            StartColumn = 1
            EndLine = 1
            EndColumn = 1
        }

    member this.ToString =
        sprintf "\"%s\" %d:%d-%d:%d" this.Filename this.StartLine this.StartColumn this.EndLine this.EndColumn

    static member ofToken (lexbuf: Lexing.LexBuffer<char>) =
        let pos1 = lexbuf.StartPos in
        let pos2 = lexbuf.EndPos in
        {
            Filename = pos1.FileName
            StartLine = pos1.Line
            StartColumn = pos1.Column
            EndLine = pos2.Line
            EndColumn = pos2.Column
        }

    static member union pos1 pos2 =
        { pos1 with pos.EndLine = pos2.EndLine; EndColumn = pos2.EndColumn }

exception ParserError of string * pos

module AST =
    type id = string * pos

    type Program = Statement list

    and Commands = Statement list
    
    and Statement =
        | FunDef of id * id list * Commands * pos
        | Repeat of Expr * Commands * pos
        | If of Expr * Commands * pos
        | IfElse of Expr * Commands * Commands * pos
        // make "i 1
        | Make of id * Expr * pos
        // makelocal "i 1
        | MakeLocal of id * Expr * pos
        // local "i
        | Local of id * pos
        // Function calls are assumed to be commands
        | FunCall of id * Expr list * pos
        // Expressions can be returned with "output"
        | Output of Expr * pos

    and Expr =
        | Var of string * pos
        | Number of float * pos
        | Word of string * pos
        | Text of string * pos
        | FunCall of id * Expr list * pos
        | List of Expr list * pos
        | Add of Expr * Expr * pos
        | Sub of Expr * Expr * pos
        | Mul of Expr * Expr * pos
        | Div of Expr * Expr * pos
        | Gt of Expr * Expr * pos
        | Lt of Expr * Expr * pos
        | Ge of Expr * Expr * pos
        | Le of Expr * Expr * pos
        | Eq of Expr * Expr * pos
        | NotEq of Expr * Expr * pos
    with
        member this.Pos =
            match this with
            | Var (_, pos)
            | Number (_, pos)
            | Word (_, pos)
            | Text (_, pos)
            | FunCall (_, _, pos)
            | Add (_, _, pos)
            | Sub (_, _, pos)
            | Mul (_, _, pos)
            | Div (_, _, pos)
            | Gt (_, _, pos)
            | Lt (_, _, pos)
            | Ge (_, _, pos)
            | Le (_, _, pos)
            | Eq (_, _, pos)
            | NotEq (_, _, pos)
            | List (_, pos) ->
                pos

type Error =
    | SyntaxError of string * pos

    member this.ToFriendlyString() =
        match this with
        | SyntaxError (msg, pos) ->
            sprintf "Syntax error: %s at %A" msg pos
