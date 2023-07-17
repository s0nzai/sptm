# sptm
Structual Programming language for Turing Machine

## Usage

```Shell
sptm [-h|-p|-r] source initial_tape
```

## Grammar
EBNF is given below.
```
Val = ident | sym.
Expr = Val
     | "~" Expr
     | Expr "or" Expr
     | Expr "&" Expr
     | "(" Expr ")".
RepeatStat = "repeat" StatList "until" Expr.
WhileStat = "while" Expr "do" StatList "end".
IfStat = "if" Expr "then" StatList
         {"elsif" Expr "then StatList"}
         ["else" StatList] "end".
ActualArgs = ["(" Val {"," Val} ")"].
Call = ident ActualArgs.
Inst = "print" "(" Val ")"
     | "left"
     | "right"
     | "erase".
Stat = [Call | IfStat | WhileStat | RepeatStat].
StatList = Stat {";" Stat}.
ProcBody = Decls "begin" StatList "end".
FormalArgs = ["(" ident {"," ident} ")"].
ProcDecl = "proc" ident FormalArgs ";" ProcBody ident.
Decls = {ProcDecl ";"}.
SymbolDecl = "symbol" {symbol ";"}.
Prog = "program" ident ";" SymbolDecl Decls
       "begin" StatList "end" ident "." EOF.
```

## Examples
See [examples/](examples/)
