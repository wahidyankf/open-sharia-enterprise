---
title: "Part 4: Special Forms and Closures"
weight: 200004
date: 2026-04-20T00:00:00+07:00
draft: false
description: "Why special forms bypass normal evaluation, how closures capture their environment, and what lexical scope really means"
tags: ["compilers", "interpreters", "closures", "lexical-scope", "special-forms", "golang", "computer-science"]
---

Part 3's evaluator handles function application and symbol lookup. It cannot yet define variables, branch conditionally, or create functions. This part adds the four **special forms** that make the interpreter Turing-complete: `define`, `if`, `lambda`, and `begin`.

## CS Concept: Why Special Forms Are Special

In Part 3, general application follows one rule: evaluate every subexpression, then apply the operator. This rule breaks for some constructs.

**Normal application** — evaluates ALL arguments before calling:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    N1["(+ x y)"]
    N2["eval x"]
    N3["eval y"]
    N4["apply + to values"]
    N1 --> N2 --> N4
    N1 --> N3 --> N4

    classDef blue fill:#0173B2,color:#fff,stroke:#0173B2
    class N1,N2,N3,N4 blue
```

**`if` special form** — evaluates test first, then exactly one branch:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    I1["if test consequent alternate"] --> I2["eval test only"] --> I3["eval consequent\nOR alternate\nnever both"]

    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    class I1,I2,I3 teal
```

**`define` special form** — binds a name, never evaluates it as a lookup:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    D1["(define x 10)"] --> D2["x is a name to BIND\nnot a value to LOOK UP"]

    classDef orange fill:#DE8F05,color:#fff,stroke:#DE8F05
    class D1,D2 orange
```

These forms are **special** because they require control over _which_ subexpressions are evaluated and _when_. Every programming language has them, though they go by different names: keywords, reserved words, syntax forms.

## Extending the Evaluator

We extend the `eval` function's `List` branch to check for special form keywords before falling through to general application. In Go, this is a string switch inside the `Symbol` case check:

```go
case List:
    if len(e.Values) == 0 {
        return Nil{}, nil
    }
    head := e.Values[0]
    args := e.Values[1:]

    if sym, ok := head.(Symbol); ok {
        switch sym.Value {
        case "define":
            return evalDefine(args, env)
        case "if":
            return evalIf(args, env)
        case "lambda":
            return evalLambda(args, env)
        case "begin":
            return evalBegin(args, env)
        case "quote":
            if len(args) != 1 {
                return nil, fmt.Errorf("quote: expects 1 argument")
            }
            return args[0], nil
        }
    }

    // General application (unchanged from Part 3)
    proc, err := eval(head, env)
    ...
```

The string switch on `sym.Value` fires before the general application case, so `define` is never treated as a variable lookup.

## Implementing `define`

```go
func evalDefine(args []LispVal, env *Env) (LispVal, error) {
    if len(args) != 2 {
        return nil, fmt.Errorf("define: expects (define <name> <expr>)")
    }
    sym, ok := args[0].(Symbol)
    if !ok {
        return nil, fmt.Errorf("define: first argument must be a symbol")
    }
    value, err := eval(args[1], env)
    if err != nil {
        return nil, err
    }
    env.define(sym.Value, value)
    return Symbol{Value: sym.Value}, nil
}
```

`define` binds a name in the _current_ (innermost) frame. It does not look up or evaluate the name — it creates a new binding.

## Implementing `if`

```go
func evalIf(args []LispVal, env *Env) (LispVal, error) {
    if len(args) < 2 || len(args) > 3 {
        return nil, fmt.Errorf("if: expects (if <test> <consequent> [<alternate>])")
    }
    test, err := eval(args[0], env)
    if err != nil {
        return nil, err
    }
    isFalse := func(v LispVal) bool {
        b, ok := v.(Bool)
        return ok && !b.Value
    }
    if isFalse(test) {
        if len(args) == 3 {
            return eval(args[2], env)
        }
        return Nil{}, nil
    }
    return eval(args[1], env)
}
```

Scheme's truthiness rule: only `#f` is false. Every other value — including `0`, `""`, and `()` — is truthy.

## CS Concept: Closures

A **closure** is a function paired with the environment in which it was defined. When a `lambda` is evaluated, it captures a snapshot of the current `*Env` pointer.

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
sequenceDiagram
    participant C as Caller
    participant EV as eval
    participant ENV as Environment

    C->>EV: (define make-adder (lambda (n) (lambda (x) (+ n x))))
    EV->>ENV: bind make-adder in global frame

    C->>EV: (define add5 (make-adder 5))
    EV->>ENV: extend env with { n → 5 }
    note over EV,ENV: eval inner lambda in this extended env
    EV->>EV: Lambda captures *Env{n→5}
    EV->>ENV: bind add5 → Lambda{Params:[x], Env:*Env{n→5}}

    C->>EV: (add5 3)
    EV->>ENV: extend closure's *Env with { x → 3 }
    note over EV,ENV: env chain: {x→3} → {n→5} → global
    EV->>EV: eval (+ n x) → look up n=5, x=3 → 8
    EV-->>C: Number 8
```

## How a Closure Captures Its Environment

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    G["Global Frame\nmake-adder → Lambda\nadd5 → Closure"]
    C["Closure Frame\nn → 5\n(created when make-adder was called)"]
    I["Call Frame\nx → 3\n(created when add5 is called)"]
    LA["Lambda body\n(+ n x)"]

    I -->|"parent"| C
    C -->|"parent"| G

    LA -->|"evaluates in"| I
    LA -->|"n found in"| C
    LA -->|"+ found in"| G

    classDef blue fill:#0173B2,color:#fff,stroke:#0173B2
    classDef orange fill:#DE8F05,color:#fff,stroke:#DE8F05
    classDef teal fill:#029E73,color:#fff,stroke:#029E73

    class G blue
    class C orange
    class I,LA teal
```

**The critical point**: the captured `*Env` is the environment at _definition_ time, not at _call_ time. If `make-adder` has returned, the frame where `n = 5` lives is still alive — referenced by the closure — even though `make-adder`'s call has completed.

## Implementing `lambda`

```go
func evalLambda(args []LispVal, env *Env) (LispVal, error) {
    if len(args) < 2 {
        return nil, fmt.Errorf("lambda: expects (lambda (<params>) <body>)")
    }
    paramList, ok := args[0].(List)
    if !ok {
        return nil, fmt.Errorf("lambda: first argument must be a parameter list")
    }
    params := make([]string, len(paramList.Values))
    for i, p := range paramList.Values {
        sym, ok := p.(Symbol)
        if !ok {
            return nil, fmt.Errorf("lambda: parameters must be symbols")
        }
        params[i] = sym.Value
    }
    body := args[1]
    return Lambda{Params: params, Body: body, Env: env}, nil // capture env!
}
```

The key is `Env: env` — `env` here is the `*Env` pointer at the point where `lambda` is evaluated. When `apply` later invokes this `Lambda`, it calls `extendEnv(p.Params, args, p.Env)` — extending the closure's env, not the caller's.

## CS Concept: Lexical vs Dynamic Scope

**Lexical scope** (Scheme — correct) — f closes over n=1 at definition time:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    LS1["n=1, define f using n,\nredefine n=100, call f 5"] --> LS2["Result: 6\nf sees n=1\nfrom definition env"]

    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    class LS1,LS2 teal
```

**Dynamic scope** (not Scheme) — f would see the caller's n=100:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    DS1["n=1, define f using n,\nredefine n=100, call f 5"] --> DS2["Result: 105\nf would see n=100\nfrom caller's env"]

    classDef brown fill:#CA9161,color:#fff,stroke:#CA9161
    class DS1,DS2 brown
```

## CS Concept: Free Variables and Variable Capture

A **free variable** in a function body is one not in the parameter list — it must be looked up in an enclosing scope.

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart LR
    Body["(lambda (x) (+ x n))"]
    Bound["Bound variable\nx — in parameter list"]
    Free1["Free variable\nn — from enclosing scope"]
    Free2["Free variable\n+ — from global frame"]

    Body --> Bound
    Body --> Free1
    Body --> Free2

    classDef blue fill:#0173B2,color:#fff,stroke:#0173B2
    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    classDef orange fill:#DE8F05,color:#fff,stroke:#DE8F05

    class Body blue
    class Bound teal
    class Free1,Free2 orange
```

When a closure is created, all free variables become "captured" — accessible via the closure's `*Env` chain for as long as the closure lives.

## Implementing `begin`

```go
func evalBegin(args []LispVal, env *Env) (LispVal, error) {
    if len(args) == 0 {
        return Nil{}, nil
    }
    var result LispVal
    var err error
    for _, expr := range args {
        result, err = eval(expr, env)
        if err != nil {
            return nil, err
        }
    }
    return result, nil
}
```

`begin` sequences expressions and returns the value of the last one. Essential for function bodies that need side effects before returning.

## Testing Closures

```go
env := makeGlobalEnv()

eval(mustRead("(define square (lambda (x) (* x x)))"), env)
v, _ := eval(mustRead("(square 5)"), env)
// → Number{Value: 25}

eval(mustRead("(define make-adder (lambda (n) (lambda (x) (+ n x))))"), env)
eval(mustRead("(define add10 (make-adder 10))"), env)
v, _ = eval(mustRead("(add10 7)"), env)
// → Number{Value: 17}

eval(mustRead("(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))"), env)
v, _ = eval(mustRead("(fact 5)"), env)
// → Number{Value: 120}
```

## What the Evaluator Can Now Do

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart LR
    EV["eval"]

    A["Self-evaluating atoms\nNumber · Str · Bool"]
    B["Symbol lookup\nenv chain traversal"]
    C["quote\nreturn unevaluated"]
    D["define\nbind in current frame"]
    E["if\nshort-circuit branch"]
    F["lambda\ncreate closure"]
    G["begin\nsequence expressions"]
    H["General application\neval all → apply"]

    EV --> A
    EV --> B
    EV --> C
    EV --> D
    EV --> E
    EV --> F
    EV --> G
    EV --> H

    classDef blue fill:#0173B2,color:#fff,stroke:#0173B2
    classDef orange fill:#DE8F05,color:#fff,stroke:#DE8F05
    classDef teal fill:#029E73,color:#fff,stroke:#029E73

    class EV blue
    class A,B,C,D orange
    class E,F,G,H teal
```

This is a complete interpreter — it can express any computable function. What it lacks is convenience (`let`, `cond`) and stack safety (TCO). Parts 5 and 6 address these.

In [Part 5](/en/learn/software-engineering/compilers-and-interpreters/lisp-interpreter-in-golang/part-5-derived-forms-and-repl), we add `let` and `cond` as **derived forms** — showing how macro expansion reduces language surface area — and wire up the REPL.
