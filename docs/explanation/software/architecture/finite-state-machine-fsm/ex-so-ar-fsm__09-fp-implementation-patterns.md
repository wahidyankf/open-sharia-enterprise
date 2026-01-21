---
title: FP Implementation Patterns
description: Finite state machine implementation patterns for functional programming languages
tags:
  - explanation
  - software
  - architecture
  - finite-state-machine
  - fsm
  - functional-programming
  - algebraic-data-types
  - pure-functions
  - islamic-finance
last_updated: 2026-01-21
---

# FP Implementation Patterns

Functional programming provides elegant patterns for implementing finite state machines through algebraic data types, pattern matching, and pure functions. This document explores FP-specific FSM implementation patterns with language-agnostic examples that translate to Haskell, Elm, Scala, OCaml, F#, and FP subsets of multi-paradigm languages.

## Core Principles

### Immutability

FSM state is immutable. Transitions produce new state values rather than mutating existing state.

```
// ❌ BAD (mutates state)
function transition(state) {
  state.current = "APPROVED";  // Mutation
  return state;
}

// ✅ GOOD (returns new state)
function transition(state) {
  return { ...state, current: "APPROVED" };  // New immutable value
}
```

### Pure Transition Functions

Transition functions are pure: deterministic, referentially transparent, with no side effects.

```
Signature: (State, Event) → (State, Effects)

Where:
  - State: Current FSM state (immutable)
  - Event: Trigger (immutable)
  - (State, Effects): Next state + side effects to execute
```

### Explicit Side Effects

Side effects (logging, network calls, database writes) are separated from transition logic and made explicit.

```
// Transition function (pure)
function transition(state, event) {
  return [newState, effects];
}

// Effect interpreter (impure)
function executeEffects(effects) {
  effects.forEach(effect => {
    if (effect.type === "LOG") {
      console.log(effect.message);
    } else if (effect.type === "HTTP") {
      http.post(effect.url, effect.body);
    }
  });
}
```

## Pattern 1: Algebraic Data Types (ADTs) for States and Events

ADTs model states and events as sum types (tagged unions), enabling exhaustive pattern matching.

### State ADT

```
type AssessmentState =
  | Draft of DraftData
  | Calculating of CalculatingData
  | Payable of PayableData
  | Exempt of ExemptData
  | Paid of PaidData
  | Cancelled of CancelledData

type DraftData = {
  id: string,
  created_at: timestamp
}

type CalculatingData = {
  id: string,
  wealth_data: WealthData,
  started_at: timestamp
}

type PayableData = {
  id: string,
  wealth_data: WealthData,
  zakat_amount: Money,
  calculated_at: timestamp
}

type PaidData = {
  id: string,
  wealth_data: WealthData,
  zakat_amount: Money,
  payment_id: string,
  paid_at: timestamp
}
```

### Event ADT

```
type AssessmentEvent =
  | StartCalculation of WealthData
  | CalculationComplete of CalculationResult
  | RecordPayment of Payment
  | Cancel of string

type CalculationResult = {
  zakat_amount: Money,
  nisab_threshold: Money,
  total_wealth: Money
}

type Payment = {
  payment_id: string,
  amount: Money,
  payment_method: string,
  timestamp: timestamp
}
```

### Pure Transition Function

```
function transition(state: AssessmentState, event: AssessmentEvent): (AssessmentState, Effect[]) {
  match (state, event) {
    // DRAFT + StartCalculation
    | (Draft(data), StartCalculation(wealthData)) =>
      if (isWealthDataComplete(wealthData)) {
        let newState = Calculating({
          id: data.id,
          wealth_data: wealthData,
          started_at: now()
        });
        let effects = [
          LogEffect(`Starting calculation for ${data.id}`)
        ];
        return (newState, effects);
      } else {
        return (state, [
          ErrorEffect("Wealth data incomplete")
        ]);
      }

    // CALCULATING + CalculationComplete
    | (Calculating(data), CalculationComplete(result)) =>
      if (result.zakat_amount > 0) {
        let newState = Payable({
          id: data.id,
          wealth_data: data.wealth_data,
          zakat_amount: result.zakat_amount,
          calculated_at: now()
        });
        let effects = [
          LogEffect(`Zakat calculated: ${result.zakat_amount}`),
          NotificationEffect({
            user_id: getUserId(data.id),
            message: `Zakat due: ${result.zakat_amount}`
          })
        ];
        return (newState, effects);
      } else {
        let newState = Exempt({
          id: data.id,
          wealth_data: data.wealth_data,
          reason: "Below nisab threshold"
        });
        let effects = [
          LogEffect("Zakat not due (below nisab)")
        ];
        return (newState, effects);
      }

    // PAYABLE + RecordPayment
    | (Payable(data), RecordPayment(payment)) =>
      if (payment.amount == data.zakat_amount) {
        let newState = Paid({
          id: data.id,
          wealth_data: data.wealth_data,
          zakat_amount: data.zakat_amount,
          payment_id: payment.payment_id,
          paid_at: payment.timestamp
        });
        let effects = [
          LogEffect(`Payment recorded: ${payment.payment_id}`),
          PersistEffect({
            type: "payment",
            data: payment
          }),
          NotificationEffect({
            user_id: getUserId(data.id),
            message: "Payment recorded successfully"
          })
        ];
        return (newState, effects);
      } else {
        return (state, [
          ErrorEffect(`Payment amount mismatch: expected ${data.zakat_amount}, got ${payment.amount}`)
        ]);
      }

    // PAYABLE + StartCalculation (recalculation)
    | (Payable(data), StartCalculation(wealthData)) =>
      let newState = Calculating({
        id: data.id,
        wealth_data: wealthData,
        started_at: now()
      });
      let effects = [
        LogEffect("Recalculating Zakat (wealth data changed)")
      ];
      return (newState, effects);

    // Invalid transitions
    | (state, event) =>
      return (state, [
        ErrorEffect(`Invalid transition: ${getStateName(state)} + ${getEventName(event)}`)
      ]);
  }
}
```

### Effect ADT

```
type Effect =
  | LogEffect of string
  | NotificationEffect of NotificationData
  | PersistEffect of PersistData
  | HttpEffect of HttpRequest
  | ErrorEffect of string

type NotificationData = {
  user_id: string,
  message: string
}

type PersistData = {
  type: string,
  data: any
}

type HttpRequest = {
  method: "GET" | "POST" | "PUT" | "DELETE",
  url: string,
  body: any
}
```

### Effect Interpreter

```
function executeEffect(effect: Effect): void {
  match effect {
    | LogEffect(message) =>
      logger.info(message);

    | NotificationEffect(data) =>
      notificationService.send(data.user_id, data.message);

    | PersistEffect(data) =>
      database.insert(data.type, data.data);

    | HttpEffect(request) =>
      http.request(request.method, request.url, request.body);

    | ErrorEffect(message) =>
      logger.error(message);
  }
}

function executeEffects(effects: Effect[]): void {
  effects.forEach(executeEffect);
}
```

## Pattern 2: State Machine as Fold

FSMs can be implemented as a fold (reduce) over event sequences, with transition function as the reducer.

### Structure

```
Signature: foldl transition initialState events

Where:
  - transition: (State, Event) → State
  - initialState: State
  - events: Event[]
  - Result: State (final state after all events processed)
```

### Implementation Example

```
function transition(state: AssessmentState, event: AssessmentEvent): AssessmentState {
  let (nextState, effects) = transitionWithEffects(state, event);
  executeEffects(effects);
  return nextState;
}

function processEvents(initialState: AssessmentState, events: AssessmentEvent[]): AssessmentState {
  return events.reduce(transition, initialState);
}
```

**Usage**:

```
let initialState = Draft({
  id: "zakat-2026-001",
  created_at: now()
});

let events = [
  StartCalculation({
    cash: 50000,
    gold_weight: 100,
    gold_price: 60
  }),
  CalculationComplete({
    zakat_amount: 2650,
    nisab_threshold: 5100,
    total_wealth: 106000
  }),
  RecordPayment({
    payment_id: "PAY-001",
    amount: 2650,
    payment_method: "BANK_TRANSFER",
    timestamp: now()
  })
];

let finalState = processEvents(initialState, events);
// finalState: Paid({...})
```

### Event Sourcing Compatibility

This pattern naturally supports event sourcing: state is reconstructed by folding over historical events.

```
function reconstructState(entity_id: string): AssessmentState {
  let events = eventStore.loadEvents(entity_id);
  let initialState = Draft({ id: entity_id, created_at: events[0].timestamp });
  return processEvents(initialState, events);
}
```

## Pattern 3: Monadic FSM Composition

Monads enable composition of FSM transition functions with error handling, logging, and other concerns.

### State Monad

The State monad threads state through a sequence of computations.

```
type State<S, A> = (S) → (A, S)

function return<S, A>(value: A): State<S, A> {
  return (state) => (value, state);
}

function bind<S, A, B>(
  ma: State<S, A>,
  f: (A) => State<S, B>
): State<S, B> {
  return (state) => {
    let (value, newState) = ma(state);
    return f(value)(newState);
  };
}
```

### FSM with State Monad

```
type FSMComputation<A> = State<AssessmentState, A>

function startCalculation(wealthData: WealthData): FSMComputation<void> {
  return (state) => {
    match state {
      | Draft(data) =>
        if (isWealthDataComplete(wealthData)) {
          let newState = Calculating({
            id: data.id,
            wealth_data: wealthData,
            started_at: now()
          });
          return ((), newState);
        } else {
          throw Error("Wealth data incomplete");
        }

      | _ =>
        throw Error("Invalid state for startCalculation");
    }
  };
}

function completeCalculation(result: CalculationResult): FSMComputation<void> {
  return (state) => {
    match state {
      | Calculating(data) =>
        if (result.zakat_amount > 0) {
          let newState = Payable({
            id: data.id,
            wealth_data: data.wealth_data,
            zakat_amount: result.zakat_amount,
            calculated_at: now()
          });
          return ((), newState);
        } else {
          let newState = Exempt({
            id: data.id,
            wealth_data: data.wealth_data,
            reason: "Below nisab threshold"
          });
          return ((), newState);
        }

      | _ =>
        throw Error("Invalid state for completeCalculation");
    }
  };
}

function recordPayment(payment: Payment): FSMComputation<string> {
  return (state) => {
    match state {
      | Payable(data) =>
        if (payment.amount == data.zakat_amount) {
          let newState = Paid({
            id: data.id,
            wealth_data: data.wealth_data,
            zakat_amount: data.zakat_amount,
            payment_id: payment.payment_id,
            paid_at: payment.timestamp
          });
          return (payment.payment_id, newState);
        } else {
          throw Error("Payment amount mismatch");
        }

      | _ =>
        throw Error("Invalid state for recordPayment");
    }
  };
}
```

**Composition**:

```
function processAssessment(
  wealthData: WealthData,
  result: CalculationResult,
  payment: Payment
): FSMComputation<string> {
  return bind(
    startCalculation(wealthData),
    () => bind(
      completeCalculation(result),
      () => recordPayment(payment)
    )
  );
}

// Usage
let initialState = Draft({ id: "zakat-2026-001", created_at: now() });
let computation = processAssessment(wealthData, calculationResult, payment);
let (paymentId, finalState) = computation(initialState);
```

## Pattern 4: Free Monad for Effect Management

Free monads separate FSM logic from effect interpretation, enabling testability and multiple interpreters.

### Free Monad Structure

```
type Free<F, A> =
  | Pure of A
  | Free of F<Free<F, A>>

function liftF<F, A>(fa: F<A>): Free<F, A> {
  return Free(map(fa, Pure));
}
```

### Effect Algebra

```
type FSMEffect<A> =
  | Log of string * A
  | Notify of NotificationData * A
  | Persist of PersistData * A
  | Http of HttpRequest * A

function log(message: string): Free<FSMEffect, void> {
  return liftF(Log(message, ()));
}

function notify(data: NotificationData): Free<FSMEffect, void> {
  return liftF(Notify(data, ()));
}

function persist(data: PersistData): Free<FSMEffect, void> {
  return liftF(Persist(data, ()));
}
```

### FSM Program

```
function zakatAssessmentProgram(
  wealthData: WealthData,
  result: CalculationResult,
  payment: Payment
): Free<FSMEffect, AssessmentState> {
  return bind(
    log("Starting Zakat calculation"),
    () => bind(
      log(`Calculation complete: zakat = ${result.zakat_amount}`),
      () => bind(
        notify({
          user_id: "USER-001",
          message: `Zakat due: ${result.zakat_amount}`
        }),
        () => bind(
          persist({
            type: "payment",
            data: payment
          }),
          () => bind(
            log(`Payment recorded: ${payment.payment_id}`),
            () => Pure(Paid({
              id: "zakat-2026-001",
              wealth_data: wealthData,
              zakat_amount: result.zakat_amount,
              payment_id: payment.payment_id,
              paid_at: payment.timestamp
            }))
          )
        )
      )
    )
  );
}
```

### Effect Interpreter

```
function interpret(program: Free<FSMEffect, A>): A {
  match program {
    | Pure(value) =>
      return value;

    | Free(Log(message, next)) =>
      console.log(message);
      return interpret(next);

    | Free(Notify(data, next)) =>
      notificationService.send(data.user_id, data.message);
      return interpret(next);

    | Free(Persist(data, next)) =>
      database.insert(data.type, data.data);
      return interpret(next);

    | Free(Http(request, next)) =>
      http.request(request.method, request.url, request.body);
      return interpret(next);
  }
}
```

**Test Interpreter** (for testing without side effects):

```
function testInterpret(program: Free<FSMEffect, A>): (A, Effect[]) {
  function go(p: Free<FSMEffect, A>, effects: Effect[]): (A, Effect[]) {
    match p {
      | Pure(value) =>
        return (value, effects);

      | Free(Log(message, next)) =>
        return go(next, effects.concat([LogEffect(message)]));

      | Free(Notify(data, next)) =>
        return go(next, effects.concat([NotificationEffect(data)]));

      | Free(Persist(data, next)) =>
        return go(next, effects.concat([PersistEffect(data)]));

      | Free(Http(request, next)) =>
        return go(next, effects.concat([HttpEffect(request)]));
    }
  }
  return go(program, []);
}
```

## Pattern 5: Elm Architecture (TEA)

The Elm Architecture provides a standard pattern for FSM implementation in Elm and Elm-like frameworks.

### Structure

```
Model: FSM state (immutable)
Msg: Events/actions
update: (Msg, Model) → (Model, Cmd Msg)
view: Model → Html Msg
```

### Implementation Example

```
-- Model (FSM State)
type Model =
  Draft DraftData
  | Calculating CalculatingData
  | Payable PayableData
  | Paid PaidData

type alias DraftData = {
  id : String,
  wealthData : Maybe WealthData
}

type alias PayableData = {
  id : String,
  wealthData : WealthData,
  zakatAmount : Float
}

-- Messages (Events)
type Msg =
  StartCalculation
  | CalculationComplete CalculationResult
  | RecordPayment Payment
  | WealthDataUpdated WealthData

-- Update (Transition Function)
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (model, msg) of
    (Draft data, StartCalculation) ->
      case data.wealthData of
        Just wealthData ->
          let
            newModel = Calculating {
              id = data.id,
              wealthData = wealthData,
              startedAt = currentTime
            }
            cmd = performCalculation wealthData
          in
          (newModel, cmd)

        Nothing ->
          (model, Cmd.none)

    (Calculating data, CalculationComplete result) ->
      if result.zakatAmount > 0 then
        let
          newModel = Payable {
            id = data.id,
            wealthData = data.wealthData,
            zakatAmount = result.zakatAmount
          }
          cmd = sendNotification ("Zakat due: " ++ String.fromFloat result.zakatAmount)
        in
        (newModel, cmd)
      else
        let
          newModel = Exempt {
            id = data.id,
            wealthData = data.wealthData,
            reason = "Below nisab threshold"
          }
        in
        (newModel, Cmd.none)

    (Payable data, RecordPayment payment) ->
      if payment.amount == data.zakatAmount then
        let
          newModel = Paid {
            id = data.id,
            wealthData = data.wealthData,
            zakatAmount = data.zakatAmount,
            paymentId = payment.id,
            paidAt = payment.timestamp
          }
          cmd = Cmd.batch [
            persistPayment payment,
            sendNotification "Payment recorded successfully"
          ]
        in
        (newModel, cmd)
      else
        (model, Cmd.none)

    _ ->
      (model, Cmd.none)

-- View
view : Model -> Html Msg
view model =
  case model of
    Draft data ->
      div [] [
        h1 [] [ text "Zakat Assessment - Draft" ],
        wealthDataForm WealthDataUpdated,
        button [ onClick StartCalculation ] [ text "Calculate Zakat" ]
      ]

    Calculating data ->
      div [] [
        h1 [] [ text "Calculating..." ],
        p [] [ text "Please wait while we calculate your Zakat" ]
      ]

    Payable data ->
      div [] [
        h1 [] [ text "Zakat Payable" ],
        p [] [ text ("Amount due: " ++ String.fromFloat data.zakatAmount) ],
        paymentForm RecordPayment
      ]

    Paid data ->
      div [] [
        h1 [] [ text "Payment Recorded" ],
        p [] [ text ("Payment ID: " ++ data.paymentId) ],
        p [] [ text "Thank you for your Zakat payment" ]
      ]
```

## Best Practices

### 1. Exhaustive Pattern Matching

Compiler-enforced exhaustive pattern matching prevents missing transition cases.

```
// ✅ GOOD: Exhaustive match
match (state, event) {
  | (Draft, StartCalculation) => ...
  | (Calculating, CalculationComplete) => ...
  | (Payable, RecordPayment) => ...
  | (Payable, StartCalculation) => ...  // Recalculation
  | (_, _) => InvalidTransition  // Catch-all for invalid transitions
}
```

### 2. Type-Driven Design

Use types to make illegal states unrepresentable.

```
// ✅ GOOD: Payable state always has zakat_amount
type PayableData = {
  id: string,
  wealth_data: WealthData,
  zakat_amount: Money  // Required field
}

// ❌ BAD: zakat_amount might be null/undefined
type PayableData = {
  id: string,
  wealth_data: WealthData,
  zakat_amount: Maybe<Money>  // Optional field
}
```

### 3. Smart Constructors

Use smart constructors to enforce invariants when creating state values.

```
function createPayableState(
  id: string,
  wealthData: WealthData,
  zakatAmount: Money
): Result<PayableData, Error> {
  if (zakatAmount <= 0) {
    return Error("Zakat amount must be positive");
  }
  return Ok({
    id: id,
    wealth_data: wealthData,
    zakat_amount: zakatAmount,
    calculated_at: now()
  });
}
```

## Cross-References

- [Core Concepts and Terminology](./ex-so-ar-fsm__02-core-concepts-and-terminology.md): FSM fundamentals
- [Events, Guards, and Actions](./ex-so-ar-fsm__06-events-guards-and-actions.md): Pure guard functions
- [OOP Implementation Patterns](./ex-so-ar-fsm__08-oop-implementation-patterns.md): OOP alternative
- [Event-Driven and Reactive FSM](./ex-so-ar-fsm__11-event-driven-and-reactive-fsm.md): Event sourcing with fold
- [Testing FSM Implementations](./ex-so-ar-fsm__12-testing-fsm-implementations.md): Property-based testing

## Related Principles

- [Immutability Over Mutability](../../../../governance/principles/software-engineering/immutability.md): Immutable state transitions
- [Pure Functions Over Side Effects](../../../../governance/principles/software-engineering/pure-functions.md): Pure transition functions
- [Explicit Over Implicit](../../../../governance/principles/software-engineering/explicit-over-implicit.md): Explicit effects and side effects

## Next Steps

1. **Compare with OOP**: [OOP Implementation Patterns](./ex-so-ar-fsm__08-oop-implementation-patterns.md)
2. **Test with properties**: [Testing FSM Implementations](./ex-so-ar-fsm__12-testing-fsm-implementations.md)
3. **Integrate with event sourcing**: [Event-Driven and Reactive FSM](./ex-so-ar-fsm__11-event-driven-and-reactive-fsm.md)
4. **Choose a framework**: [Framework Comparisons](./ex-so-ar-fsm__13-framework-spring-state-machine-xstate.md)
