---
title: "Part 6: Tail-Call Optimization"
weight: 100006
date: 2026-04-20T00:00:00+07:00
draft: false
description: "Tail position, tail-call optimization, the loop-based evaluator transform, and the trampoline pattern — stack-safe recursion without cheating"
tags: ["compilers", "interpreters", "tail-call-optimization", "tco", "trampoline", "f-sharp", "computer-science"]
---

The interpreter from Part 5 is correct but fragile: deep recursion overflows the F# call stack. This is not an implementation detail — Scheme's R5RS standard mandates that tail calls must not consume stack space. This part explains why, and implements TCO by transforming the evaluator into a loop.

## CS Concept: The Call Stack

When a function calls another function, the runtime pushes a **stack frame** onto the call stack. The frame stores the caller's local variables and the return address — where execution should resume after the callee returns.

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart LR
    subgraph Stack["Call stack for (fact 4)"]
        direction TB
        F0["fact(0)\nn=0, return 1\n← top of stack"]
        F1["fact(1)\nn=1, waiting for fact(0)"]
        F2["fact(2)\nn=2, waiting for fact(1)"]
        F3["fact(3)\nn=3, waiting for fact(2)"]
        F4["fact(4)\nn=4, waiting for fact(3)"]
        Main["main\n← bottom of stack"]

        F0 --- F1 --- F2 --- F3 --- F4 --- Main
    end

    classDef blue fill:#0173B2,color:#fff,stroke:#0173B2
    classDef orange fill:#DE8F05,color:#fff,stroke:#DE8F05

    class F0 blue
    class F1,F2,F3,F4,Main orange
```

For `fact(5)` that is 6 frames. For `fact(1000000)`, it is one million frames — and a stack overflow.

## CS Concept: Tail Position

A **tail call** is a function call that is the _last thing a function does before returning_. Its result becomes the caller's result with no further computation.

**NOT a tail call** — result of recursive call is used in a further multiplication:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    NT1["fact uses\n(* n (fact (- n 1)))"] --> NT2["fact returns\nthen multiply by n\ncaller frame stays alive"]

    classDef brown fill:#CA9161,color:#fff,stroke:#CA9161
    class NT1,NT2 brown
```

**Tail call** — recursive call is the last thing; result returned directly:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    T1["fact-iter uses\n(fact-iter (- n 1) (* n acc))"] --> T2["result IS the return value\ncaller frame immediately useless"]

    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    class T1,T2 teal
```

**Tail-call optimization** replaces the recursive call with a jump back to the start of the function, reusing the existing frame. Stack depth stays constant regardless of iteration count.

**Without TCO** — each call pushes a new frame, O(n) stack:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    A1["fact-iter(5,1)"] --> A2["fact-iter(4,5)"] --> A3["fact-iter(3,20)"] --> A4["..."] --> A5["fact-iter(0,120)"]

    classDef brown fill:#CA9161,color:#fff,stroke:#CA9161
    class A1,A2,A3,A4,A5 brown
```

**With TCO** — same frame reused each iteration, O(1) stack:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    B1["frame: n=5 acc=1"] --> B2["frame: n=4 acc=5"] --> B3["frame: n=3 acc=20"] --> B4["...done"]

    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    class B1,B2,B3,B4 teal
```

## Why F# TCO Is Not Enough

F# natively supports tail recursion — the compiler emits a `tail.` IL instruction for tail-recursive `let rec` functions. So why does our Scheme interpreter still overflow?

**F# tail call** — handled automatically by the F# compiler:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart LR
    FS1["eval calls itself\nin tail position\nF# optimizes this automatically"]

    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    class FS1 teal
```

**Scheme tail call** — must be implemented explicitly in the interpreter:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart LR
    SC1["eval calls apply\napply calls eval"] --> SC2["each call = new F# frame\nnot a direct F# tail call"]

    classDef brown fill:#CA9161,color:#fff,stroke:#CA9161
    class SC1,SC2 brown
```

F# TCO operates on _F# functions_. Scheme's TCO guarantee must be implemented explicitly by the interpreter — it is a property of the _hosted_ language, not the _host_ language. Both F# and Scheme have TCO, but they are different guarantees at different levels of abstraction.

## Identifying Tail Positions in the Evaluator

Before transforming the evaluator, we must identify which `eval` calls are in tail position — those whose result is returned directly without further computation.

**NOT tail position** — `eval` result is used for further computation:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart LR
    EV["eval"] --> NT1["test in (if test ...)\nresult used to branch"]
    EV --> NT2["operator in (f a b)\nresult used as procedure"]
    EV --> NT3["each arg in (f a b)\nresult collected into list"]

    classDef blue fill:#0173B2,color:#fff,stroke:#0173B2
    classDef brown fill:#CA9161,color:#fff,stroke:#CA9161
    class EV blue
    class NT1,NT2,NT3 brown
```

**Tail position** — `eval` result is returned directly, no further computation:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart LR
    EV["eval"] --> TP1["consequent in (if #t ...)"]
    EV --> TP2["alternate in (if #f ...)"]
    EV --> TP3["last expr in (begin ...)"]
    EV --> TP4["body in lambda application"]
    EV --> TP5["expanded form in let / cond"]

    classDef blue fill:#0173B2,color:#fff,stroke:#0173B2
    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    class EV blue
    class TP1,TP2,TP3,TP4,TP5 teal
```

## The Loop Transform

Instead of calling `eval` recursively at tail positions, we update `currentExpr` and `currentEnv` and let the `while` loop restart. No new stack frame is created.

**Before** — recursive call creates a new stack frame:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    B1["eval consequent env"] --> B2["new F# stack frame"]

    classDef brown fill:#CA9161,color:#fff,stroke:#CA9161
    class B1,B2 brown
```

**After** — update variables and let the `while` loop restart instead:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    A1["currentExpr ← consequent"] --> A2["currentEnv ← env"] --> A3["continue while loop"]

    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    class A1,A2,A3 teal
```

```fsharp
let rec eval (expr: LispVal) (env: Env list) : LispVal =
    let mutable currentExpr = expr
    let mutable currentEnv  = env
    let mutable result      = None

    while result.IsNone do
        match currentExpr with
        | Number _ | Str _ | Bool _ ->
            result <- Some currentExpr

        | Symbol name ->
            result <- Some (envLookup name currentEnv)

        | List [] -> result <- Some Nil

        | List (Symbol "if" :: rest) ->
            match rest with
            | [test; consequent; alternate] ->
                match eval test currentEnv with   // test: NOT tail position
                | Bool false -> currentExpr <- alternate    // LOOP
                | _          -> currentExpr <- consequent   // LOOP
            | [test; consequent] ->
                match eval test currentEnv with
                | Bool false -> result <- Some Nil
                | _          -> currentExpr <- consequent   // LOOP
            | _ -> failwith "if: bad syntax"

        | List (Symbol "begin" :: exprs) ->
            match exprs with
            | [] -> result <- Some Nil
            | _  ->
                List.take (exprs.Length - 1) exprs
                |> List.iter (fun e -> eval e currentEnv |> ignore)
                currentExpr <- List.last exprs              // LOOP

        | List (Symbol "define" :: rest) ->
            evalDefine rest currentEnv |> ignore
            result <- Some Nil

        | List (Symbol "lambda" :: rest) ->
            result <- Some (evalLambda rest currentEnv)

        | List (Symbol "let"  :: rest)    -> currentExpr <- desugarLet rest    // LOOP
        | List (Symbol "cond" :: clauses) -> currentExpr <- desugarCond clauses // LOOP

        | List (Symbol "quote" :: [x]) -> result <- Some x

        | List (head :: args) ->
            let proc          = eval head currentEnv
            let evaluatedArgs = List.map (fun a -> eval a currentEnv) args
            match proc with
            | Builtin f -> result <- Some (f evaluatedArgs)
            | Lambda (parms, body, closureEnv) ->
                if parms.Length <> evaluatedArgs.Length then
                    failwith $"Arity mismatch: expected {parms.Length}, got {evaluatedArgs.Length}"
                currentEnv  <- envExtend (List.zip parms evaluatedArgs) closureEnv
                currentExpr <- body                          // LOOP — the key!
            | _ -> failwith $"Not a procedure: {proc}"

        | _ -> failwith $"Cannot evaluate: {currentExpr}"

    result.Value
```

## The Trampoline Pattern

The loop transform keeps `eval` iterative internally. An alternative that keeps `eval` recursive is the **trampoline**: a loop that repeatedly calls a function as long as it returns a deferred computation (a thunk) rather than a final value.

**The trampoline loop** — keeps calling until a `Done` value, not a `Bounce` thunk:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    T1["Call f()"] --> T2{"Result?"}
    T2 -->|"Done"| T3["Done: return value"]
    T2 -->|"Bounce"| T4["Bounce: call thunk()"]
    T4 -->|"loop"| T2

    classDef blue fill:#0173B2,color:#fff,stroke:#0173B2
    class T1,T2,T3,T4 blue
```

**Tail call in eval with trampoline** — return a thunk instead of recursing:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    TC1["Instead of:\neval body closureEnv"] --> TC2["Return:\nBounce (thunk to eval body)"]

    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    class TC1,TC2 teal
```

```fsharp
type EvalResult =
    | Done   of LispVal
    | Bounce of (unit -> EvalResult)

let trampoline (f: unit -> EvalResult) : LispVal =
    let mutable result = f ()
    while (match result with Bounce _ -> true | _ -> false) do
        result <- match result with Bounce thunk -> thunk () | r -> r
    match result with Done v -> v | _ -> failwith "impossible"
```

## Loop Transform vs Trampoline

**Loop transform:**

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    LT1["Stack depth: O(1)"] --> LT2["Style: while loop"] --> LT3["Allocation: none"] --> LT4["Explicit, easy to audit"]

    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    class LT1,LT2,LT3,LT4 teal
```

**Trampoline:**

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    TR1["Stack depth: O(1)"] --> TR2["Style: functional"] --> TR3["One thunk per bounce"] --> TR4["Elegant pattern"]

    classDef purple fill:#CC78BC,color:#fff,stroke:#CC78BC
    class TR1,TR2,TR3,TR4 purple
```

Both are correct. The loop transform is what Norvig uses in lispy2; the trampoline is more common in functional language implementations.

## Demonstrating Stack Safety

Without TCO:

```scheme
(define count-down
  (lambda (n)
    (if (= n 0) "done"
      (count-down (- n 1)))))

(count-down 1000000)  ; Stack overflow without TCO
```

With the loop transform, `count-down` runs in O(1) stack space:

**Without TCO** — each call creates a new frame, 1,000,000 frames total:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    W1["count-down(1000000)"] --> W2["count-down(999999)"] --> W3["count-down(999998)"] --> Wd["... 999,997 more ..."] --> We["Stack overflow"]

    classDef brown fill:#CA9161,color:#fff,stroke:#CA9161
    class W1,W2,W3,Wd,We brown
```

**With TCO** — while loop updates one frame 1,000,000 times:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    T1["expr=(count-down 999999)\nn=999999"] --> T2["expr=(count-down 999998)\nn=999998"] --> T3["1,000,000 iterations\nstill ONE frame"] --> T4["done"]

    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    class T1,T2,T3,T4 teal
```

```scheme
(count-down 1000000)
; → "done"  (no overflow, O(1) stack)
```

## CS Concept: Continuation-Passing Style

The trampoline is closely related to **continuation-passing style** (CPS) — a program transformation where every function takes an extra argument (the continuation) representing "what to do next". CPS makes all calls tail calls by construction.

**Direct style** — result flows backward through the call stack:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart LR
    D1["fact(n) = n * fact(n-1)"] --> D2["result flows back\nthrough call stack"]

    classDef blue fill:#0173B2,color:#fff,stroke:#0173B2
    class D1,D2 blue
```

**Continuation-passing style** — result passed forward to a continuation:

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    C1["fact_cps(n, k)\n= fact_cps(n-1, fun r → k(n*r))"] --> C2["result passed forward\nto continuation k"] --> C3["no stack growth\nall calls are tail calls"]

    classDef orange fill:#DE8F05,color:#fff,stroke:#DE8F05
    class C1,C2,C3 orange
```

**CPS enables:**

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    U1["call/cc\ncapture + resume"] --> U2["async/await\ndesugared CPS"] --> U3["Coroutines\ngenerators"] --> U4["Compiler IR\nCPS intermediate repr"]

    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    class U1,U2,U3,U4 teal
```

Our interpreter does not implement `call/cc`, but the trampoline pattern gives a taste of the underlying idea: instead of returning a value, you return a description of what to compute next.

## The Complete Interpreter: All Six Parts

```mermaid
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
flowchart TB
    subgraph P2["Part 2: Front End"]
        direction LR
        src["(fact 5)"] --> tok["tokenize"] --> par["parse"] --> ast["LispVal tree"]
    end

    subgraph P3["Part 3: Eval/Apply Core"]
        direction LR
        ev["eval"] <-->|"mutual recursion"| ap["apply"]
        en["Env chain"] --> ev
    end

    subgraph P4["Part 4: Special Forms"]
        direction LR
        sf["define · if · lambda · begin"] --> cl["Closures\ncapture env"]
    end

    subgraph P5["Part 5: Sugar + REPL"]
        direction LR
        ds["let · cond\n(desugar)"] --> rp["REPL loop\nread→eval→print"]
    end

    subgraph P6["Part 6: TCO"]
        direction LR
        lp["while loop\nreplace tail eval calls"] --> ss["O(1) stack\nfor tail calls"]
    end

    P2 --> P3 --> P4 --> P5 --> P6

    classDef blue fill:#0173B2,color:#fff,stroke:#0173B2
    classDef orange fill:#DE8F05,color:#fff,stroke:#DE8F05
    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    classDef purple fill:#CC78BC,color:#fff,stroke:#CC78BC
    classDef gray fill:#808080,color:#fff,stroke:#808080

    class P2 blue
    class P3 orange
    class P4 teal
    class P5 purple
    class P6 gray
```

## Summary

| Concept        | What it means                                                         | How we implemented it                                 |
| -------------- | --------------------------------------------------------------------- | ----------------------------------------------------- |
| Tail position  | A call whose result is returned directly, with no further computation | Identified in `if`, `begin`, `lambda` application     |
| TCO obligation | R5RS requires tail calls not grow the stack                           | Loop transform in `eval`                              |
| Host vs hosted | F#'s own TCO ≠ Scheme's TCO                                           | Explicit `while` loop; F# can't do this automatically |
| Loop transform | Replace tail-position `eval` calls with variable updates + loop       | `currentExpr <- body` instead of `eval body env`      |
| Trampoline     | Return thunks at tail positions; loop re-invokes them                 | Alternative functional approach; same O(1) depth      |

**Next steps** (not covered in this series):

- **Macros** — `define-macro` or `define-syntax`: user-defined syntactic transformations
- **Continuations** — `call/cc`: capture and resume the call stack as a first-class value
- **The full R5RS library** — strings, characters, vectors, ports, I/O procedures
- **Proper tail recursion in `map`** — the builtin `map` above is not itself tail-recursive
