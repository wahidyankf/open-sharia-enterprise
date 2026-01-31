---
title: "Beginner"
weight: 11000001
date: 2026-01-29T16:00:00+07:00
draft: false
description: "Master fundamental React with TypeScript concepts through 25 annotated examples covering components, props, state, hooks, events, forms, and composition"
tags: ["react", "typescript", "frontend", "tutorial", "by-example", "beginner", "hooks", "components"]
---

This beginner tutorial covers fundamental React + TypeScript concepts through 25 heavily annotated examples. Each example maintains 1-2.25 comment lines per code line to ensure deep understanding.

## Prerequisites

Before starting, ensure you understand:

- JavaScript ES6+ (arrow functions, destructuring, spread/rest operators)
- TypeScript basics (types, interfaces, generics)
- HTML/CSS fundamentals
- Basic programming concepts (variables, functions, loops)

## Group 1: React + TypeScript Fundamentals

### Example 1: First React Component with TypeScript

React components are TypeScript functions that return JSX. Function components use explicit type annotations for props.

```typescript
// App.tsx
// => React component: TypeScript function returning JSX
// => Must start with capital letter (React convention)
// => Component names like App, Button, UserProfile (PascalCase)
function App() {
  // => No props, no state - simplest possible component
  // => Function body executes on every render
  // => Returns JSX element describing UI structure
  return (
    // => JSX expression starts here
    // => Must return single root element
    <div>
      {/* => JSX: JavaScript XML - looks like HTML but isn't */}
      {/* => React transforms this to React.createElement() calls */}
      {/* => Compiles to: React.createElement('div', null, ...) */}
      <h1>Welcome to React</h1>
      {/* => h1 rendered as actual DOM <h1> element */}
      {/* => Text content rendered directly - no escaping needed */}
      <p>This is your first component.</p>
      {/* => p rendered as actual DOM <p> element */}
      {/* => JSX prevents XSS attacks automatically */}
    </div>
    // => Closing </div> marks end of returned JSX
  );
  // => div is root element - components must return single element
  // => Alternative: use React.Fragment or <> </> for no wrapper
}

// => Export makes component importable in other files
// => import App from './App' in another file
export default App;
```

**Key Takeaway**: React components are TypeScript functions that return JSX. They must start with capital letters and return a single root element.

**Expected Output**: Page displays heading "Welcome to React" and paragraph "This is your first component."

**Common Pitfalls**: Forgetting to capitalize component name (React won't recognize it), or returning multiple root elements without wrapper.

### Example 2: JSX and TSX Syntax

JSX combines HTML-like syntax with TypeScript expressions. Use curly braces for dynamic values.

```typescript
// => TSX file extension required for JSX syntax in TypeScript
// => .tsx files support both TypeScript and JSX
// => Use .ts for TypeScript without JSX, .tsx with JSX
function JsxDemo() {
  // => Variables defined in component scope
  // => Accessible throughout component body
  // => Re-created on every render (new values each time)
  const name = "Fatima";                    // => name is "Fatima" (type: string)
                                             // => TypeScript infers string type
  const age = 25;                            // => age is 25 (type: number)
                                             // => TypeScript infers number type
  const isStudent = true;                    // => isStudent is true (type: boolean)
                                             // => TypeScript infers boolean type

  // => Expressions evaluated at render time
  // => Template literal combines text with variables
  const greeting = `Salam, ${name}!`;        // => greeting is "Salam, Fatima!"
                                             // => ${name} interpolates name variable

  return (
    // => JSX expression starts
    <div>
      {/* => Curly braces {} embed TypeScript expressions in JSX */}
      {/* => Any valid TS expression works: variables, calls, operators */}
      <h1>{greeting}</h1>
      {/* => {greeting} evaluates to "Salam, Fatima!" */}
      {/* => Output: <h1>Salam, Fatima!</h1> */}

      <p>Age: {age}</p>
      {/* => {age} evaluates to 25 */}
      {/* => Numbers converted to strings automatically */}
      {/* => Output: <p>Age: 25</p> */}

      <p>Status: {isStudent ? "Student" : "Not Student"}</p>
      {/* => Ternary operator: condition ? trueValue : falseValue */}
      {/* => isStudent is true, so evaluates to "Student" */}
      {/* => Output: <p>Status: Student</p> */}

      <p>Age in 5 years: {age + 5}</p>
      {/* => Arithmetic operations work in JSX expressions */}
      {/* => age + 5 evaluates to 25 + 5 = 30 */}
      {/* => Output: <p>Age in 5 years: 30</p> */}
    </div>
    // => Closing </div>
  );
}

export default JsxDemo;
```

**Key Takeaway**: Use curly braces `{}` in JSX to embed TypeScript expressions. Any valid TypeScript expression works: variables, functions, operators, ternaries.

**Expected Output**: Page displays personalized greeting, age information, student status, and calculated future age.

**Common Pitfalls**: Using single curly brace for CSS-like styling (JSX uses double braces: `style={{ color: 'red' }}`), or forgetting curly braces around expressions.

### Example 3: Component Props with Interfaces

Props pass data from parent to child components. Use TypeScript interfaces for type safety.

```typescript
// => Interface defines prop types - TypeScript validates at compile time
// => Contract between parent (provider) and child (consumer)
interface GreetingProps {
  name: string;                              // => Required prop (no ? suffix)
                                             // => Parent MUST provide string value
  age: number;                               // => Number type enforced by TypeScript
                                             // => Passing string gives compile error
  isStudent: boolean;                        // => Boolean type enforced
                                             // => Only true or false allowed
}

// => Props parameter destructured with type annotation
// => TypeScript ensures all required props provided
// => Missing props cause compile-time error
function Greeting({ name, age, isStudent }: GreetingProps) {
  // => Props are read-only - never modify props directly
  // => Trying to reassign causes TypeScript error
  // => Destructuring extracts: name, age, isStudent from props object
  // => Alternative: function Greeting(props: GreetingProps) { props.name }

  return (
    // => JSX using prop values
    <div>
      <h2>Profile: {name}</h2>
      {/* => name comes from parent component via props */}
      {/* => One-way data flow: parent → child */}

      <p>Age: {age}</p>
      {/* => age comes from parent component */}
      {/* => Numbers automatically converted to strings in JSX */}

      <p>Status: {isStudent ? "Student" : "Professional"}</p>
      {/* => Conditional logic using prop value */}
      {/* => isStudent is true → "Student", false → "Professional" */}
    </div>
  );
}

// => Parent component using Greeting
// => Parent controls child's props
function App() {
  return (
    <div>
      {/* => Pass props as JSX attributes */}
      {/* => TypeScript validates types match interface */}
      {/* => String literals use quotes, numbers/booleans use curly braces */}
      <Greeting name="Aisha" age={28} isStudent={false} />
      {/* => Creates Greeting with props object: */}
      {/* => { name: "Aisha", age: 28, isStudent: false } */}
      {/* => Component receives destructured values */}

      <Greeting name="Omar" age={22} isStudent={true} />
      {/* => Same component, different props (reusability) */}
      {/* => Props: { name: "Omar", age: 22, isStudent: true } */}
    </div>
  );
}

export default App;
```

**Key Takeaway**: Define prop types with TypeScript interfaces. Destructure props in component parameter. Props are read-only and flow one-way from parent to child.

**Expected Output**: Two profile cards displaying names, ages, and status ("Professional" for Aisha, "Student" for Omar).

**Common Pitfalls**: Mutating props directly (props are immutable), forgetting to pass required props (TypeScript error), or using wrong types for props.

### Example 4: Children Props Pattern

The `children` prop passes nested JSX content to components. Use `ReactNode` type for flexibility.

```typescript
import { ReactNode } from 'react';          // => Import ReactNode type from React
                                             // => ReactNode is built-in React type
                                             // => Covers all renderable content

// => Interface with children prop
// => ReactNode accepts any valid JSX: elements, strings, numbers, arrays, fragments
// => Most flexible type for component composition
interface CardProps {
  title: string;                             // => Card header text (required)
                                             // => Displayed at top of card
  children: ReactNode;                       // => Content nested inside <Card>...</Card>
                                             // => Special prop - receives JSX between tags
                                             // => Type ReactNode = string | number | Element | Fragment | ...
}

// => Layout component wrapping children
// => Provides consistent styling to arbitrary content
function Card({ title, children }: CardProps) {
  // => children contains all JSX between <Card> and </Card>
  // => React automatically passes this prop
  // => No need to explicitly pass children={...}
  return (
    // => Container with inline styles
    <div style={{ border: '1px solid #ccc', padding: '16px', margin: '8px' }}>
      {/* => Wrapper styling applied to all cards */}
      {/* => Inline styles: object with camelCase properties */}
      {/* => border, padding, margin define card appearance */}

      <h3>{title}</h3>
      {/* => title prop renders as header */}
      {/* => Same title styling for all cards */}

      <div>
        {children}
        {/* => children renders whatever parent passed */}
        {/* => Could be text, single element, or complex JSX tree */}
        {/* => Complete flexibility - Card doesn't care about structure */}
      </div>
      {/* => Nested div provides content wrapper */}
    </div>
  );
}

// => Parent using Card with different children
// => Demonstrates composition pattern
function App() {
  return (
    <div>
      <Card title="Zakat Information">
        {/* => Everything between <Card> and </Card> is children prop */}
        {/* => Card component receives this as children parameter */}
        <p>Annual wealth threshold: 85g gold</p>
        {/* => First child: paragraph element */}
        <p>Rate: 2.5% of qualifying wealth</p>
        {/* => Second child: paragraph element */}
        {/* => Two <p> elements passed as array to children */}
      </Card>
      {/* => Card wraps content with border and title */}

      <Card title="Prayer Times">
        {/* => Different content, same Card wrapper */}
        {/* => Same styling, different children */}
        <ul>
          <li>Fajr: 5:30 AM</li>
          <li>Dhuhr: 12:45 PM</li>
        </ul>
        {/* => <ul> element with list items passed as children */}
        {/* => Card component handles any valid JSX */}
      </Card>
    </div>
  );
}

export default App;
```

**Key Takeaway**: Use `children` prop for component composition. Type it as `ReactNode` to accept any JSX content. This pattern enables flexible, reusable layout components.

**Expected Output**: Two styled cards - one showing Zakat information with two paragraphs, one showing prayer times in a list.

**Common Pitfalls**: Forgetting to render `{children}` in component (content won't appear), or using wrong type for children (use `ReactNode` not `JSX.Element`).

### Example 5: Default Props with TypeScript

Use optional props with default values for flexibility. TypeScript ensures type safety.

```typescript
// => Interface with optional props (? suffix)
interface ButtonProps {
  text: string;                              // => Required: button label
  variant?: 'primary' | 'secondary';         // => Optional: union type with literal values
  disabled?: boolean;                        // => Optional: boolean with undefined fallback
}

// => Default parameter values for optional props
// => ES6 destructuring with defaults
function Button({
  text,
  variant = 'primary',                       // => Default: 'primary' if not provided
  disabled = false                           // => Default: false if not provided
}: ButtonProps) {
  // => variant is 'primary' or 'secondary' (or default 'primary')
  // => disabled is true, false, or default false

  // => Compute styles based on variant
  const backgroundColor = variant === 'primary' ? '#0173B2' : '#DE8F05';
  // => primary: blue (#0173B2), secondary: orange (#DE8F05)

  const color = variant === 'primary' ? '#fff' : '#000';
  // => primary: white text, secondary: black text

  return (
    <button
      disabled={disabled}
      {/* => Disabled attribute controls button state */}
      style={{ backgroundColor, color, padding: '8px 16px', border: 'none' }}
      {/* => Inline styles using computed values */}
    >
      {text}
      {/* => Button label from required prop */}
    </button>
  );
}

// => Parent using Button with different prop combinations
function App() {
  return (
    <div>
      <Button text="Submit" />
      {/* => Uses defaults: variant='primary', disabled=false */}
      {/* => Blue button with white text */}

      <Button text="Cancel" variant="secondary" />
      {/* => Overrides variant, disabled still false */}
      {/* => Orange button with black text */}

      <Button text="Disabled" disabled={true} />
      {/* => Overrides disabled, variant still 'primary' */}
      {/* => Blue button, grayed out and unclickable */}
    </div>
  );
}

export default App;
```

**Key Takeaway**: Use optional props (`?`) with default parameter values for flexibility. TypeScript enforces type safety even with defaults. Destructure with `=` to provide fallback values.

**Expected Output**: Three buttons - blue "Submit", orange "Cancel", and grayed-out blue "Disabled" button.

**Common Pitfalls**: Providing defaults in interface instead of component parameter (doesn't work), or using nullable types (`| null`) when optional (`?`) is more appropriate.

## Group 2: State Management Basics

### Example 6: useState Hook with TypeScript

State stores component data that changes over time. `useState` returns current value and setter function.

```typescript
import { useState } from 'react';           // => Import useState hook from React
                                             // => Hook = function that uses React features
                                             // => Must call in component body (not conditionals)

function Counter() {
  // => useState<T>(initialValue) declares state with type T
  // => Returns tuple: [currentValue, setterFunction]
  // => Array destructuring: [value, setter] from hook
  const [count, setCount] = useState<number>(0);
  // => count is 0 initially (type: number)
  // => First render: count = 0
  // => setCount is function: (newValue: number) => void
  // => Calling setCount triggers re-render with new value
  // => TypeScript infers type from initial value (could omit <number>)

  // => Event handler function
  // => Defined inside component - recreated each render
  const handleIncrement = () => {
    setCount(count + 1);                     // => Increments count by 1
                                             // => count is current value (e.g., 0)
                                             // => count + 1 computes new value (e.g., 1)
    // => setCount schedules re-render with new value
    // => State update is asynchronous (doesn't happen immediately)
    // => Component re-executes, count has new value
    // => console.log(count) here would show OLD value
  };

  return (
    // => Render UI based on current state
    <div>
      <p>Count: {count}</p>
      {/* => Displays current count value from state */}
      {/* => Initially: "Count: 0" */}
      {/* => After first click: "Count: 1" */}
      {/* => React automatically updates DOM when state changes */}

      <button onClick={handleIncrement}>Increment</button>
      {/* => onClick expects function reference (not call) */}
      {/* => onClick calls handleIncrement when user clicks */}
      {/* => After click: setCount(1), component re-renders, count = 1 */}
    </div>
  );
}

export default Counter;
```

**Key Takeaway**: Use `useState` to store values that change over time. Calling the setter function triggers re-render with new value. State updates are asynchronous.

**Expected Output**: Page displays "Count: 0" initially. Clicking "Increment" updates display to "Count: 1", "Count: 2", etc.

**Common Pitfalls**: Calling state setter directly in render (infinite loop), expecting state to update immediately (updates are asynchronous), or mutating state directly instead of using setter.

### Example 7: Multiple State Variables

Each piece of independent state should be its own `useState` call. Group related state.

```typescript
import { useState } from 'react';

function DonationForm() {
  // => Separate state for independent values
  // => Each useState manages one piece of state
  const [amount, setAmount] = useState<number>(0);
  // => amount is 0 (type: number)

  const [donorName, setDonorName] = useState<string>('');
  // => donorName is '' (type: string)

  const [isAnonymous, setIsAnonymous] = useState<boolean>(false);
  // => isAnonymous is false (type: boolean)

  // => Event handlers for each input
  const handleAmountChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setAmount(Number(e.target.value));       // => Converts string to number
    // => e.target.value is string from input
    // => Number() converts to number type
  };

  const handleSubmit = () => {
    // => All state values accessible here
    console.log({ amount, donorName, isAnonymous });
    // => Logs: { amount: 100, donorName: "Fatima", isAnonymous: false }
  };

  return (
    <div>
      <input
        type="number"
        value={amount}
        {/* => Controlled input: value comes from state */}
        onChange={handleAmountChange}
        {/* => onChange updates state on every keystroke */}
        placeholder="Amount"
      />

      <input
        type="text"
        value={donorName}
        onChange={(e) => setDonorName(e.target.value)}
        {/* => Inline handler with arrow function */}
        placeholder="Donor Name"
      />

      <label>
        <input
          type="checkbox"
          checked={isAnonymous}
          {/* => Controlled checkbox: checked comes from state */}
          onChange={(e) => setIsAnonymous(e.target.checked)}
          {/* => e.target.checked is boolean */}
        />
        Anonymous Donation
      </label>

      <button onClick={handleSubmit}>Submit Donation</button>
    </div>
  );
}

export default DonationForm;
```

**Key Takeaway**: Use multiple `useState` calls for independent state variables. Each state has its own setter. Group related state into single object if they always change together.

**Expected Output**: Form with three inputs (amount, donor name, anonymous checkbox) and submit button. Each input updates its own state independently.

**Common Pitfalls**: Using one giant state object when values are independent (harder to update), or forgetting to convert input values to correct types (inputs return strings).

### Example 8: State with Objects

When state is an object, create new object when updating. Never mutate state directly.

```typescript
import { useState } from 'react';

// => Type for user object
// => Define structure for state
interface User {
  name: string;                              // => User's full name
  email: string;                             // => Email address
  age: number;                               // => Age in years
}

function UserProfile() {
  // => State as object with explicit type
  // => Object state groups related values
  const [user, setUser] = useState<User>({
    name: 'Aisha',                           // => Initial name value
    email: 'aisha@example.com',              // => Initial email value
    age: 28                                  // => Initial age value
  });
  // => user is { name: 'Aisha', email: 'aisha@example.com', age: 28 }
  // => Single state variable for entire user object
  // => Alternative: 3 separate useState calls (less cohesive)

  // => Update single property immutably
  // => CRITICAL: Never mutate state directly (user.name = 'x' - WRONG)
  const updateName = (newName: string) => {
    setUser({ ...user, name: newName });     // => Spread operator creates new object
                                             // => {...user} copies all properties (shallow copy)
    // => ...user expands to: name: 'Aisha', email: 'aisha@example.com', age: 28
    // => name: newName overwrites name property with new value
    // => Result: { name: newName, email: 'aisha@example.com', age: 28 }
    // => New object triggers re-render, old object discarded
  };

  const updateEmail = (newEmail: string) => {
    setUser({ ...user, email: newEmail });   // => New object with updated email
                                             // => Spread preserves name and age
    // => Other properties (name, age) preserved from ...user
    // => Only email changes
  };

  const incrementAge = () => {
    setUser({ ...user, age: user.age + 1 }); // => New object with incremented age
                                             // => user.age reads current value (28)
                                             // => user.age + 1 computes new value (29)
    // => Computes new age, spreads other properties unchanged
    // => Creates entirely new object (immutable update pattern)
  };

  return (
    <div>
      <p>Name: {user.name}</p>
      {/* => Access object property with dot notation */}
      {/* => Displays: "Name: Aisha" initially */}

      <p>Email: {user.email}</p>
      {/* => user.email reads email property */}

      <p>Age: {user.age}</p>
      {/* => user.age reads age property (number) */}

      <button onClick={() => updateName('Fatima')}>Change Name</button>
      {/* => Inline arrow function wraps call with parameter */}
      {/* => onClick expects function, not function call */}
      {/* => () => updateName('Fatima') creates function that calls updateName */}

      <button onClick={() => updateEmail('fatima@example.com')}>Change Email</button>
      {/* => Similar pattern: arrow function with parameter */}

      <button onClick={incrementAge}>Increment Age</button>
      {/* => Direct function reference (incrementAge takes no parameters) */}
      {/* => No arrow wrapper needed */}
    </div>
  );
}

export default UserProfile;
```

**Key Takeaway**: When updating object state, create new object with spread operator (`...`). Never mutate state directly. Spread existing properties and override specific ones.

**Expected Output**: Profile displaying name, email, age. Buttons update respective fields. Clicking "Change Name" updates only name, preserving email and age.

**Common Pitfalls**: Mutating state directly (`user.name = 'New'` - won't trigger re-render), forgetting spread operator (loses other properties), or using nested object updates (requires nested spreads).

### Example 9: State with Arrays

Arrays in state require creating new arrays for updates. Use array methods that return new arrays.

```typescript
import { useState } from 'react';

// => Type for todo item
// => Define structure for each array element
interface Todo {
  id: number;                                // => Unique identifier
  text: string;                              // => Todo description
  completed: boolean;                        // => Completion status
}

function TodoList() {
  // => State as array of Todo objects
  // => Array state holds collection of items
  const [todos, setTodos] = useState<Todo[]>([
    { id: 1, text: 'Pray Fajr', completed: true },
    { id: 2, text: 'Read Quran', completed: false }
  ]);
  // => todos is array with 2 items
  // => todos[0] = { id: 1, text: 'Pray Fajr', completed: true }
  // => todos[1] = { id: 2, text: 'Read Quran', completed: false }

  // => Add new todo to end of array
  // => CRITICAL: Use immutable array methods (spread, concat)
  // => NEVER use mutating methods (push, splice) - won't trigger re-render
  const addTodo = (text: string) => {
    const newTodo: Todo = {
      id: Date.now(),                        // => Unique ID from timestamp (milliseconds since epoch)
                                             // => Simple but sufficient for demo (use UUID in production)
      text,                                  // => Shorthand for text: text (ES6 property shorthand)
                                             // => Value comes from function parameter
      completed: false                       // => New todos start uncompleted
    };
    // => newTodo is { id: 1234567890, text: "Study Fiqh", completed: false }

    setTodos([...todos, newTodo]);           // => Spread existing, add new to end
                                             // => [...todos] creates shallow copy of array
                                             // => Appends newTodo to end
    // => Creates new array: [todo1, todo2, newTodo]
    // => Original todos array unchanged (immutability)
    // => New array reference triggers re-render
  };

  // => Toggle completed status
  // => map creates new array by transforming each element
  const toggleTodo = (id: number) => {
    setTodos(todos.map(todo =>              // => map returns new array (immutable)
                                             // => Iterates through each todo
      todo.id === id                         // => Check if this is target todo to toggle
                                             // => Comparison returns true or false
        ? { ...todo, completed: !todo.completed }  // => If match: toggle completed
                                                    // => {...todo} spreads id, text, completed
                                                    // => completed: !todo.completed flips boolean
        // => Creates new todo object: { id: 1, text: 'Pray Fajr', completed: false }
        : todo                               // => If no match: keep todo unchanged
                                             // => Returns same object reference
    ));
    // => Returns new array with one todo object replaced
    // => Other todos remain same objects (optimization)
  };

  // => Remove todo by ID
  // => filter creates new array with matching elements removed
  const deleteTodo = (id: number) => {
    setTodos(todos.filter(todo => todo.id !== id));
    // => filter returns new array excluding elements where callback returns false
    // => todo.id !== id: keep todos where id doesn't match
    // => Keeps all todos where id !== provided id
    // => Excludes todo where id matches
  };

  return (
    <div>
      <ul>
        {/* => Map array to JSX elements */}
        {/* => Each array item becomes list item */}
        {todos.map(todo => (
          // => map iterates: todo = todos[0], then todos[1], etc.
          <li key={todo.id}>
            {/* => key prop REQUIRED for list items (React optimization) */}
            {/* => Must be unique and stable (id is perfect, don't use index) */}
            {/* => React uses keys to track which items changed/added/removed */}

            <input
              type="checkbox"
              checked={todo.completed}
              {/* => checked from state (controlled component) */}
              {/* => todo.completed is true or false */}
              onChange={() => toggleTodo(todo.id)}
              {/* => Arrow function wraps toggleTodo call with parameter */}
              {/* => Clicking checkbox calls toggleTodo(todo.id) */}
            />

            <span style={{ textDecoration: todo.completed ? 'line-through' : 'none' }}>
              {/* => Inline style object with conditional value */}
              {/* => todo.completed true → 'line-through', false → 'none' */}
              {todo.text}
              {/* => Display todo text */}
              {/* => Conditional styling based on completed status */}
            </span>

            <button onClick={() => deleteTodo(todo.id)}>Delete</button>
            {/* => Clicking calls deleteTodo with current todo's id */}
          </li>
        ))}
      </ul>

      <button onClick={() => addTodo('Study Fiqh')}>Add Todo</button>
      {/* => Clicking adds new todo with text "Study Fiqh" */}
    </div>
  );
}

export default TodoList;
```

**Key Takeaway**: Use array methods that return new arrays (`map`, `filter`, `concat`, spread). Never mutate arrays directly (`push`, `splice`). Always provide `key` prop for list items.

**Expected Output**: Todo list with two items. Checkboxes toggle completion (strikethrough). "Add Todo" adds new item. "Delete" removes items.

**Common Pitfalls**: Using mutating methods (`push`, `splice` - won't trigger re-render), forgetting `key` prop (React warning), or using array index as key (causes bugs with reordering).

### Example 10: Functional State Updates

When new state depends on previous state, use functional updates for correct behavior.

```typescript
import { useState } from 'react';

function ZakatCalculator() {
  // => State for wealth amount
  const [wealth, setWealth] = useState<number>(0);
  // => wealth is 0 (type: number)

  // => WRONG: Closure problem
  // => Multiple rapid clicks may use stale wealth value
  const addWealthWrong = (amount: number) => {
    setWealth(wealth + amount);              // => Uses wealth from current render
    // => If clicked 3 times quickly, all 3 updates use same initial wealth
    // => Result: only increments once instead of three times
  };

  // => CORRECT: Functional update
  // => Guarantees correct value even with rapid updates
  const addWealthCorrect = (amount: number) => {
    setWealth(prevWealth => prevWealth + amount);
    // => prevWealth is latest state value from React
    // => Arrow function: (previous) => new
    // => React queues updates, applies in order
    // => Result: all 3 clicks accumulate correctly
  };

  // => Calculate Zakat (2.5% of wealth)
  const calculateZakat = () => {
    setWealth(prevWealth => prevWealth * 0.025);
    // => Functional update: multiply previous by 2.5%
    // => Ensures calculation uses latest wealth value
  };

  // => Reset to zero
  const reset = () => {
    setWealth(0);                            // => Direct value OK when not based on previous
    // => No need for functional update since not using previous state
  };

  return (
    <div>
      <p>Wealth: ${wealth.toFixed(2)}</p>
      {/* => toFixed(2) formats to 2 decimal places */}

      <button onClick={() => addWealthCorrect(1000)}>Add \$1000</button>
      {/* => Functional update handles rapid clicks correctly */}

      <button onClick={calculateZakat}>Calculate Zakat (2.5%)</button>
      {/* => Applies Zakat rate to current wealth */}

      <button onClick={reset}>Reset</button>
    </div>
  );
}

export default ZakatCalculator;
```

**Key Takeaway**: Use functional updates (`setState(prev => newValue)`) when new state depends on previous state. Prevents stale closure bugs. Use direct values when state is independent.

**Expected Output**: Counter starting at \$0.00. "Add \$1000" increases by \$1000 each click. "Calculate Zakat" applies 2.5% rate. "Reset" returns to \$0.00.

**Common Pitfalls**: Not using functional updates for derived state (stale closures), overusing functional updates when unnecessary (adds complexity), or misunderstanding when state updates happen (asynchronous).

## Group 3: Side Effects and Lifecycle

### Example 11: useEffect Basics

`useEffect` runs side effects after render. Use for DOM manipulation, subscriptions, or data fetching.

```typescript
import { useState, useEffect } from 'react';
// => Import both hooks: useState for state, useEffect for side effects

function DocumentTitleUpdater() {
  const [count, setCount] = useState<number>(0);
  // => count is 0 initially
  // => State triggers re-render when updated

  // => useEffect runs side effects after component renders to DOM
  // => Lifecycle: 1) Render JSX, 2) Update DOM, 3) Run effects
  // => First parameter: effect function (what to do)
  // => Second parameter: dependency array (when to run - omitted here)
  useEffect(() => {
    // => This code runs AFTER render completes
    // => Render phase: pure, no side effects
    // => Effect phase: side effects allowed (DOM, network, timers)
    // => Safe to access DOM here - component already painted to screen
    document.title = `Count: ${count}`;      // => Updates browser tab title (DOM side effect)
                                             // => document.title is browser API
    // => Browser tab shows "Count: 0" initially (first render)
    // => Updates to "Count: 1" after first click, etc.

    console.log('Effect ran with count:', count);
    // => Logs every time effect runs (debugging)
    // => Output on mount: "Effect ran with count: 0"
    // => Output after first click: "Effect ran with count: 1"
    // => Shows effect execution timing
  });
  // => No dependency array: effect runs after EVERY render
  // => Initial render: effect runs
  // => State update: re-render → effect runs again
  // => Usually not what you want (performance issue - unnecessary re-runs)
  // => Next example shows dependency array for optimization

  return (
    <div>
      <p>Count: {count}</p>
      {/* => Display current count from state */}

      <button onClick={() => setCount(count + 1)}>Increment</button>
      {/* => onClick increments count */}
      {/* => Clicking triggers: setCount → re-render → effect runs */}
      {/* => Execution flow: click → state update → re-render → DOM update → effect */}
    </div>
  );
}

export default DocumentTitleUpdater;
```

**Key Takeaway**: `useEffect` runs side effects after component renders to DOM. Without dependency array, runs after every render. Use for DOM manipulation, subscriptions, or external interactions.

**Expected Output**: Counter displays "Count: 0". Browser tab title shows "Count: 0". Clicking button updates both counter and tab title. Console logs effect execution.

**Common Pitfalls**: Running effects every render (performance issue), performing effects in render body (use `useEffect`), or accessing state before render completes (use `useEffect`).

### Example 12: useEffect with Dependencies

Dependency array controls when effect runs. Empty array runs once. Array with values runs when those values change.

```typescript
import { useState, useEffect } from 'react';

function PrayerTimeAlert() {
  const [prayerTime, setPrayerTime] = useState<string>('Fajr');
  // => prayerTime is 'Fajr' initially

  const [alertMessage, setAlertMessage] = useState<string>('');
  // => alertMessage is '' initially

  // => Effect with dependency array
  // => Runs on mount AND when prayerTime changes
  useEffect(() => {
    console.log('Effect running because prayerTime changed to:', prayerTime);
    // => Logs on mount: "Effect running because prayerTime changed to: Fajr"
    // => Logs when changed: "Effect running because prayerTime changed to: Dhuhr"

    setAlertMessage(`Time for ${prayerTime} prayer!`);
    // => Updates alertMessage based on current prayerTime
    // => alertMessage becomes "Time for Fajr prayer!" on mount
  }, [prayerTime]);
  // => Dependency array: [prayerTime]
  // => Effect runs when prayerTime value changes
  // => Changing alertMessage does NOT re-run effect (not in dependencies)

  // => Effect with empty dependency array
  // => Runs ONLY on mount (component first appears)
  useEffect(() => {
    console.log('Component mounted - runs once');
    // => Logs exactly once when component first renders
    // => Never runs again, even after state updates
  }, []);
  // => Empty array: no dependencies to watch
  // => Equivalent to componentDidMount in class components

  return (
    <div>
      <p>{alertMessage}</p>
      {/* => Displays current alert message */}

      <button onClick={() => setPrayerTime('Dhuhr')}>Set Dhuhr</button>
      {/* => Changes prayerTime, triggers first effect */}

      <button onClick={() => setPrayerTime('Asr')}>Set Asr</button>
      {/* => Changes prayerTime, triggers first effect */}
    </div>
  );
}

export default PrayerTimeAlert;
```

**Key Takeaway**: Dependency array controls effect execution. Empty array `[]` runs once on mount. Array with values runs when those values change. Omitting array runs every render.

**Expected Output**: Initially shows "Time for Fajr prayer!". Console logs mount message once. Clicking prayer buttons updates message and logs dependency change.

**Common Pitfalls**: Missing dependencies (linter warns), including unnecessary dependencies (effect runs too often), or mutating objects/arrays in dependencies (reference doesn't change, effect won't run).

### Example 13: useEffect Cleanup

Return cleanup function from effect to prevent memory leaks. Cleanup runs before next effect and on unmount.

```typescript
import { useState, useEffect } from 'react';

function TimerComponent() {
  const [seconds, setSeconds] = useState<number>(0);
  // => seconds is 0 initially
  // => Will increment every second via timer

  useEffect(() => {
    console.log('Setting up timer');
    // => Logs when effect executes (component mount)
    // => Setup phase: create timer

    // => setInterval creates recurring timer (browser API)
    // => Calls callback function repeatedly at interval
    const intervalId = setInterval(() => {
      // => Callback runs every 1000ms (1 second)
      setSeconds(prev => prev + 1);          // => Functional update increments seconds
                                             // => prev is LATEST seconds value from React
                                             // => Functional update prevents stale closure
      // => Runs every 1000ms (1 second)
      // => First run: prev = 0, sets to 1
      // => Second run: prev = 1, sets to 2, etc.
    }, 1000);
    // => Second parameter: interval in milliseconds
    // => intervalId is number (unique timer reference ID)
    // => Timer runs in background independent of React
    // => Need to clear it manually to prevent memory leak

    // => Cleanup function
    // => React calls this in two situations:
    // => 1) Before next effect runs (if dependencies change)
    // => 2) When component unmounts (removed from DOM)
    return () => {
      console.log('Cleaning up timer');
      // => Logs when cleanup happens
      // => Cleanup phase: destroy timer

      clearInterval(intervalId);             // => Stops the timer (browser API)
                                             // => Uses intervalId to identify which timer
      // => Prevents timer from running after component unmounts
      // => Without cleanup: memory leak (timer runs forever in background)
      // => Without cleanup: setState on unmounted component (React warning)
    };
  }, []);
  // => Empty dependency array: [] means no dependencies
  // => Effect runs ONCE on mount, cleanup runs ONCE on unmount
  // => Timer runs continuously until component removed
  // => Like componentDidMount + componentWillUnmount in class components

  return (
    <div>
      <p>Timer: {seconds} seconds</p>
      {/* => Displays current seconds from state */}
      {/* => Updates every second when timer calls setSeconds */}
      {/* => React re-renders component with new seconds value */}
    </div>
  );
}

// => Parent component to demonstrate unmounting
// => Controls timer component lifecycle
function App() {
  const [showTimer, setShowTimer] = useState<boolean>(true);
  // => Controls whether timer component is mounted in DOM
  // => true: component exists, false: component removed

  return (
    <div>
      <button onClick={() => setShowTimer(!showTimer)}>
        {/* => Toggles showTimer between true and false */}
        {/* => !showTimer flips boolean value */}
        {showTimer ? 'Hide' : 'Show'} Timer
        {/* => Conditional text: "Hide Timer" when true, "Show Timer" when false */}
      </button>

      {showTimer && <TimerComponent />}
      {/* => Conditional rendering with logical AND (&&) */}
      {/* => showTimer true: renders <TimerComponent /> */}
      {/* => showTimer false: renders nothing (component unmounts) */}
      {/* => When showTimer becomes false: cleanup runs, timer cleared */}
    </div>
  );
}

export default App;
```

**Key Takeaway**: Return cleanup function from `useEffect` to prevent memory leaks. Cleanup runs before next effect and when component unmounts. Essential for timers, subscriptions, and event listeners.

**Expected Output**: Timer counts seconds. "Hide Timer" button removes component and cleans up timer. Console logs show setup and cleanup. No timer runs after hiding.

**Common Pitfalls**: Forgetting cleanup for timers/listeners (memory leaks), not returning cleanup function (can't cancel side effects), or accessing stale state in cleanup (use refs or functional updates).

### Example 14: Fetching Data with useEffect

Fetch external data in `useEffect` with proper loading and error states.

```typescript
import { useState, useEffect } from 'react';

// => Type for fetched user data
interface User {
  id: number;
  name: string;
  email: string;
}

function UserFetcher() {
  // => State for fetched data
  const [user, setUser] = useState<User | null>(null);
  // => user is null initially (data not loaded yet)
  // => Union type: User object or null

  // => State for loading indicator
  const [loading, setLoading] = useState<boolean>(true);
  // => loading is true initially (fetch in progress)

  // => State for error handling
  const [error, setError] = useState<string | null>(null);
  // => error is null initially (no error yet)

  useEffect(() => {
    // => Fetch data on mount
    console.log('Fetching user data...');

    fetch('https://jsonplaceholder.typicode.com/users/1')
    // => fetch returns Promise<Response>
    // => GET request to public API
      .then(response => response.json())     // => Parse JSON body
      // => response.json() returns Promise<any>
      .then((data: User) => {
        console.log('Fetch successful:', data);
        // => Logs fetched user object

        setUser(data);                       // => Store user in state
        // => Triggers re-render with user data
        setLoading(false);                   // => Hide loading indicator
        // => Triggers re-render showing user
      })
      .catch(err => {
        console.error('Fetch failed:', err);
        // => Logs error to console

        setError('Failed to fetch user data');
        // => Store error message in state
        setLoading(false);                   // => Hide loading indicator
        // => Show error message instead
      });
  }, []);
  // => Empty dependency array: fetch once on mount
  // => No cleanup needed (fetch can't be cancelled easily)

  // => Loading state
  if (loading) {
    return <div>Loading user data...</div>;
    // => Shows while fetch in progress
    // => Early return prevents rendering user data
  }

  // => Error state
  if (error) {
    return <div>Error: {error}</div>;
    // => Shows if fetch failed
    // => Displays error message to user
  }

  // => Success state
  return (
    <div>
      <h2>User Profile</h2>
      <p>Name: {user?.name}</p>
      {/* => Optional chaining (?) handles null case */}
      {/* => If user is null, expression returns undefined */}
      <p>Email: {user?.email}</p>
    </div>
  );
}

export default UserFetcher;
```

**Key Takeaway**: Fetch data in `useEffect` with empty dependency array. Track loading and error states separately. Use conditional rendering for loading/error/success states.

**Expected Output**: Initially shows "Loading user data...". After fetch succeeds, displays user name and email. If fetch fails, shows error message.

**Common Pitfalls**: Not handling loading state (flashes old content), not handling errors (app crashes), fetching every render (infinite loop), or race conditions with multiple requests.

### Example 15: useEffect with Async/Await

Use async/await for cleaner data fetching code. Define async function inside effect.

```typescript
import { useState, useEffect } from 'react';

// => Type for Zakat calculation result
interface ZakatCalculation {
  wealth: number;
  zakatAmount: number;
  date: string;
}

function ZakatHistory() {
  const [calculations, setCalculations] = useState<ZakatCalculation[]>([]);
  // => calculations is empty array initially

  const [loading, setLoading] = useState<boolean>(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    // => Define async function inside effect
    // => Can't make useEffect callback itself async
    const fetchCalculations = async () => {
      try {
        console.log('Fetching Zakat calculations...');

        // => await pauses until fetch completes
        const response = await fetch('https://jsonplaceholder.typicode.com/posts?_limit=3');
        // => Response object contains status, headers, body

        if (!response.ok) {
          // => Check HTTP status code
          // => response.ok is true for 200-299
          throw new Error(`HTTP error! status: ${response.status}`);
          // => Throws error for 4xx, 5xx responses
        }

        // => await pauses until JSON parsing completes
        const data = await response.json();
        // => data is parsed JavaScript object/array

        console.log('Fetch successful, received:', data.length, 'items');

        // => Transform API data to our type
        const mapped: ZakatCalculation[] = data.map((item: any) => ({
          wealth: item.id * 10000,           // => Mock wealth data
          zakatAmount: item.id * 250,        // => Mock zakat data (2.5%)
          date: new Date().toISOString()     // => Current timestamp
        }));
        // => mapped is array of ZakatCalculation objects

        setCalculations(mapped);             // => Store in state
        setLoading(false);                   // => Hide loading indicator

      } catch (err) {
        // => Catch any errors from fetch or JSON parsing
        console.error('Error fetching calculations:', err);

        setError(err instanceof Error ? err.message : 'Unknown error');
        // => Type guard: extract message if Error type
        setLoading(false);
      }
    };
    // => fetchCalculations is async function definition

    fetchCalculations();                     // => Call async function
    // => Returns Promise but we don't await it
    // => useEffect callback can't be async directly
  }, []);
  // => Empty dependency array: runs once on mount

  if (loading) return <div>Loading calculations...</div>;
  if (error) return <div>Error: {error}</div>;

  return (
    <div>
      <h2>Zakat Calculation History</h2>
      <ul>
        {calculations.map((calc, index) => (
          <li key={index}>
            {/* => Using index as key (OK here - list never reorders) */}
            Wealth: ${calc.wealth} → Zakat: ${calc.zakatAmount}
          </li>
        ))}
      </ul>
    </div>
  );
}

export default ZakatHistory;
```

**Key Takeaway**: Define async function inside `useEffect`, then call it immediately. Can't make effect callback itself async. Use try/catch for error handling with async/await.

**Expected Output**: Initially shows "Loading calculations...". After successful fetch, displays list of 3 Zakat calculations with wealth and Zakat amounts. Shows error message if fetch fails.

**Common Pitfalls**: Making effect callback async (TypeScript error), not handling errors with try/catch, forgetting to call async function after defining it, or not handling cleanup for cancelled requests.

## Group 4: Event Handling and Forms

### Example 16: Click Event Handlers

React events are synthetic events wrapping browser events. Use camelCase event names.

```typescript
import { useState } from 'react';

function DonationButton() {
  const [donations, setDonations] = useState<number>(0);
  // => donations is 0 initially

  // => Event handler with no parameters
  const handleDonation = () => {
    console.log('Donation button clicked');
    // => Logs to console on every click

    setDonations(prev => prev + 1);          // => Functional update
    // => Increments donation count
  };

  // => Event handler with event parameter
  // => React.MouseEvent<HTMLButtonElement> is synthetic event type
  const handleDonationWithEvent = (e: React.MouseEvent<HTMLButtonElement>) => {
    console.log('Button clicked at:', e.clientX, e.clientY);
    // => Logs mouse position relative to viewport
    // => e.clientX is horizontal position
    // => e.clientY is vertical position

    console.log('Button text:', e.currentTarget.textContent);
    // => e.currentTarget is button element that handler attached to
    // => textContent is button's text

    setDonations(prev => prev + 1);
  };

  // => Event handler with custom parameter
  const handleDonationAmount = (amount: number) => {
    console.log('Donating:', amount);
    // => Logs donation amount

    setDonations(prev => prev + amount);     // => Add specific amount
  };

  return (
    <div>
      <p>Total Donations: {donations}</p>

      <button onClick={handleDonation}>
        {/* => onClick expects function, not function call */}
        {/* => Correct: onClick={handleDonation} */}
        {/* => Wrong: onClick={handleDonation()} - calls immediately */}
        Donate \$1
      </button>

      <button onClick={handleDonationWithEvent}>
        Donate \$1 (with event)
      </button>

      <button onClick={() => handleDonationAmount(5)}>
        {/* => Inline arrow function to pass parameter */}
        {/* => Arrow function delays execution until click */}
        Donate \$5
      </button>

      <button onClick={() => handleDonationAmount(10)}>
        Donate \$10
      </button>
    </div>
  );
}

export default DonationButton;
```

**Key Takeaway**: Pass function reference to event handlers, not function call. Use arrow functions for handlers with parameters. React synthetic events wrap browser events with consistent API.

**Expected Output**: Counter starts at 0. Each button increments by its amount (\$1, \$5, \$10). Console logs click events with position and details.

**Common Pitfalls**: Calling function immediately in JSX (`onClick={handleClick()}` - runs on render), forgetting arrow function wrapper for parameters, or misunderstanding event object properties.

### Example 17: Form Input Events

Handle input changes with `onChange` event. Synthetic event type depends on input element.

```typescript
import { useState } from 'react';

function RegistrationForm() {
  const [name, setName] = useState<string>('');
  const [email, setEmail] = useState<string>('');
  const [age, setAge] = useState<number>(0);

  // => Text input handler
  // => React.ChangeEvent<HTMLInputElement> for text inputs
  const handleNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    console.log('Name changed to:', e.target.value);
    // => e.target is input element
    // => e.target.value is string (current input value)

    setName(e.target.value);                 // => Update state with input value
  };

  // => Inline handler for email
  const handleEmailChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setEmail(e.target.value);                // => Direct state update
    // => No need for separate function if just updating state
  };

  // => Number input handler
  const handleAgeChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const value = Number(e.target.value);    // => Convert string to number
    // => Input values are always strings
    // => Number() converts to number type

    console.log('Age changed to:', value, 'Type:', typeof value);
    // => Logs: "Age changed to: 25 Type: number"

    setAge(value);                           // => Store as number
  };

  // => Textarea handler
  // => React.ChangeEvent<HTMLTextAreaElement> for textareas
  const [bio, setBio] = useState<string>('');
  const handleBioChange = (e: React.ChangeEvent<HTMLTextAreaElement>) => {
    setBio(e.target.value);                  // => Same as input, different element type
    // => Synthetic event API consistent across element types
  };

  return (
    <form>
      <div>
        <label>Name:</label>
        <input
          type="text"
          value={name}
          {/* => Controlled input: value from state */}
          onChange={handleNameChange}
          {/* => onChange fires on every keystroke */}
        />
        {/* => Current value: {name} */}
      </div>

      <div>
        <label>Email:</label>
        <input
          type="email"
          value={email}
          onChange={handleEmailChange}
          {/* => Inline handler (no separate function) */}
        />
      </div>

      <div>
        <label>Age:</label>
        <input
          type="number"
          value={age}
          onChange={handleAgeChange}
          {/* => Number input still returns string value */}
        />
      </div>

      <div>
        <label>Bio:</label>
        <textarea
          value={bio}
          onChange={handleBioChange}
          {/* => textarea uses value prop (not children) */}
          rows={4}
        />
      </div>

      <p>Form Data: {JSON.stringify({ name, email, age, bio }, null, 2)}</p>
      {/* => Display current form state */}
    </form>
  );
}

export default RegistrationForm;
```

**Key Takeaway**: Use `onChange` for input events. Event type varies by element (HTMLInputElement, HTMLTextAreaElement, etc.). Input values are always strings - convert to numbers when needed.

**Expected Output**: Form with four fields. Typing updates state immediately. Bottom displays current form data as JSON. Number input requires manual conversion to number type.

**Common Pitfalls**: Forgetting value conversion for number inputs (state becomes string), using wrong event type (TypeScript error), or not using controlled components (state not synced).

### Example 18: Controlled Components

Controlled components derive value from state. React state is single source of truth.

```typescript
import { useState } from 'react';

function DonationAmountForm() {
  // => All input values stored in state
  const [amount, setAmount] = useState<number>(0);
  const [currency, setCurrency] = useState<string>('USD');
  const [recurring, setRecurring] = useState<boolean>(false);

  // => Number input handler with validation
  const handleAmountChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const value = Number(e.target.value);

    if (value < 0) {
      console.log('Negative amount rejected');
      return;                                // => Reject negative values
      // => State doesn't update, input shows previous value
    }

    setAmount(value);                        // => Accept non-negative values
  };

  // => Select dropdown handler
  // => React.ChangeEvent<HTMLSelectElement> for select elements
  const handleCurrencyChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    console.log('Currency changed to:', e.target.value);
    // => e.target.value is selected option value

    setCurrency(e.target.value);             // => Update currency state
  };

  // => Checkbox handler
  const handleRecurringChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    console.log('Recurring changed to:', e.target.checked);
    // => e.target.checked is boolean (not value)
    // => true when checked, false when unchecked

    setRecurring(e.target.checked);          // => Update boolean state
  };

  return (
    <form>
      <div>
        <label>Amount:</label>
        <input
          type="number"
          value={amount}
          {/* => value controlled by state */}
          {/* => React prevents typing that would violate state */}
          onChange={handleAmountChange}
          min="0"
          {/* => HTML validation attribute (additional check) */}
        />
      </div>

      <div>
        <label>Currency:</label>
        <select value={currency} onChange={handleCurrencyChange}>
          {/* => value prop on select (not option) */}
          {/* => Controls which option selected */}
          <option value="USD">USD</option>
          <option value="EUR">EUR</option>
          <option value="IDR">IDR</option>
          {/* => option values match state values */}
        </select>
      </div>

      <div>
        <label>
          <input
            type="checkbox"
            checked={recurring}
            {/* => checked prop (not value) for checkboxes */}
            {/* => Controls checkbox state */}
            onChange={handleRecurringChange}
          />
          Recurring Monthly Donation
        </label>
      </div>

      <p>
        You are donating: {amount} {currency}
        {recurring ? ' monthly' : ' one-time'}
      </p>
    </form>
  );
}

export default DonationAmountForm;
```

**Key Takeaway**: Controlled components use `value` (or `checked`) prop from state. State is single source of truth. React controls input value - allows validation before updating state.

**Expected Output**: Form with amount input, currency dropdown, and recurring checkbox. Amount rejects negative values. Display shows formatted donation details based on current state.

**Common Pitfalls**: Forgetting value prop (uncontrolled component), using value with checkbox (use checked), or not handling all input changes (some inputs won't work).

### Example 19: Form Submission

Handle form submission with `onSubmit` event. Prevent default browser behavior.

```typescript
import { useState } from 'react';

// => Type for form data
// => Defines structure for all form fields
interface DonationFormData {
  donorName: string;                         // => Donor's name
  amount: number;                            // => Donation amount
  message: string;                           // => Optional message
}

function DonationSubmitForm() {
  // => Store all form fields in single state object
  // => Alternative: 3 separate useState (less cohesive)
  const [formData, setFormData] = useState<DonationFormData>({
    donorName: '',                           // => Initial: empty string
    amount: 0,                               // => Initial: zero
    message: ''                              // => Initial: empty string
  });
  // => formData is { donorName: '', amount: 0, message: '' }
  // => Single state object groups related form fields

  const [submitted, setSubmitted] = useState<boolean>(false);
  // => Track whether form has been submitted
  const [submittedData, setSubmittedData] = useState<DonationFormData | null>(null);
  // => Store submitted values for display (null until submitted)

  // => Generic handler for all text/number inputs
  // => Single handler reduces code duplication
  const handleChange = (e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
    // => Event type: union of input and textarea
    // => Works for both element types
    const { name, value } = e.target;        // => Destructure name and value from event target
                                             // => name is input's name attribute ("donorName", "amount", etc.)
                                             // => value is current input value (always string)

    setFormData(prev => ({
      // => Functional update with previous state
      ...prev,                               // => Spread existing form data (preserves unchanged fields)
                                             // => Copies donorName, amount, message
      [name]: name === 'amount' ? Number(value) : value
      // => Computed property name: [name] uses variable as key
      // => If name="donorName", updates donorName property
      // => If name="amount", updates amount property
      // => Convert amount to number (input values are strings)
      // => Keep others as string
    }));
    // => Updates only changed field, preserves others
    // => Example: changing amount doesn't affect donorName or message
  };

  // => Form submit handler
  // => React.FormEvent<HTMLFormElement> for form events
  const handleSubmit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();                      // => CRITICAL: Prevents browser default behavior
                                             // => Default behavior: send GET/POST, refresh page
    // => Without this, browser would refresh page
    // => All React state would be lost on refresh
    // => Must prevent default to handle submission in JavaScript

    console.log('Form submitted:', formData);
    // => Log submitted data for debugging
    // => Shows: { donorName: "Fatima", amount: 100, message: "..." }

    // => Validate before processing
    // => Client-side validation for UX (also validate on server)
    if (formData.amount <= 0) {
      alert('Amount must be greater than 0');
      return;                                // => Stop submission (early return)
                                             // => Don't process invalid data
    }

    if (formData.donorName.trim() === '') {
      // => .trim() removes whitespace from both ends
      // => Prevents submitting just spaces
      alert('Name is required');
      return;
    }

    // => Process valid submission
    // => All validation passed, safe to proceed
    setSubmittedData(formData);              // => Store submitted data for thank you message
                                             // => Preserves data before form reset
    setSubmitted(true);                      // => Show success message (conditional render)

    // => Reset form after submission
    // => Clear form for next donation
    setFormData({
      donorName: '',                         // => Reset to empty
      amount: 0,                             // => Reset to zero
      message: ''                            // => Reset to empty
    });
    // => Clears all inputs (controlled components sync with state)
  };

  // => Conditional render: success message
  if (submitted && submittedData) {
    // => Early return pattern for different UI states
    return (
      <div>
        <h2>Thank you for your donation!</h2>
        <p>Donor: {submittedData.donorName}</p>
        {/* => Display preserved submitted data */}
        <p>Amount: ${submittedData.amount}</p>
        <p>Message: {submittedData.message}</p>
        <button onClick={() => setSubmitted(false)}>Make Another Donation</button>
        {/* => Reset submitted flag to show form again */}
      </div>
    );
  }

  // => Render form (default state)
  return (
    <form onSubmit={handleSubmit}>
      {/* => onSubmit on form element (not button!) */}
      {/* => Triggered by: 1) submit button click, 2) Enter key in input */}

      <div>
        <label>Donor Name:</label>
        <input
          type="text"
          name="donorName"
          {/* => name attribute matches state property (CRITICAL) */}
          {/* => handleChange uses this to update correct field */}
          value={formData.donorName}
          {/* => Controlled input: value from state */}
          onChange={handleChange}
          {/* => Updates state on every keystroke */}
          required
          {/* => HTML5 validation (browser enforces) */}
        />
      </div>

      <div>
        <label>Amount ($):</label>
        <input
          type="number"
          name="amount"
          {/* => name="amount" matches formData.amount */}
          value={formData.amount}
          onChange={handleChange}
          {/* => Same handler for all inputs (generic) */}
          required
        />
      </div>

      <div>
        <label>Message (optional):</label>
        <textarea
          name="message"
          {/* => name="message" matches formData.message */}
          value={formData.message}
          {/* => textarea uses value prop (not children) */}
          onChange={handleChange}
          {/* => handleChange handles both input and textarea */}
          rows={3}
        />
      </div>

      <button type="submit">Submit Donation</button>
      {/* => type="submit" triggers form onSubmit event */}
      {/* => Alternative: type="button" + onClick (doesn't trigger onSubmit) */}
    </form>
  );
}

export default DonationSubmitForm;
```

**Key Takeaway**: Handle form submission with `onSubmit` on form element. Always call `e.preventDefault()` to prevent browser refresh. Use `name` attribute matching state properties for generic handlers.

**Expected Output**: Form with three fields. Clicking "Submit Donation" validates inputs and shows thank you message. Form resets after successful submission.

**Common Pitfalls**: Forgetting `e.preventDefault()` (page refreshes), putting `onSubmit` on button instead of form, or not validating inputs before processing.

### Example 20: Form Validation Basics

Implement real-time validation with error messages. Validate on blur and on submit.

```typescript
import { useState } from 'react';

// => Type for validation errors
interface FormErrors {
  email?: string;                            // => Optional: only exists if error
  age?: string;
}

function ValidatedRegistrationForm() {
  const [email, setEmail] = useState<string>('');
  const [age, setAge] = useState<number>(0);
  const [errors, setErrors] = useState<FormErrors>({});
  // => errors is empty object initially (no errors)

  // => Validation function for email
  const validateEmail = (value: string): string | undefined => {
    if (value.trim() === '') {
      return 'Email is required';
    }

    // => Simple email regex pattern
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(value)) {
      return 'Invalid email format';
    }

    return undefined;                        // => No error
  };

  // => Validation function for age
  const validateAge = (value: number): string | undefined => {
    if (value < 13) {
      return 'Must be at least 13 years old';
    }

    if (value > 120) {
      return 'Invalid age';
    }

    return undefined;
  };

  // => Email change handler with validation
  const handleEmailChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const value = e.target.value;
    setEmail(value);                         // => Update state immediately

    // => Clear error on change (give user chance to fix)
    setErrors(prev => ({ ...prev, email: undefined }));
  };

  // => Email blur handler (validation on blur)
  const handleEmailBlur = () => {
    const error = validateEmail(email);      // => Validate current value
    setErrors(prev => ({ ...prev, email: error }));
    // => Update only email error, preserve other errors
  };

  // => Age change handler
  const handleAgeChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const value = Number(e.target.value);
    setAge(value);
    setErrors(prev => ({ ...prev, age: undefined }));
  };

  // => Age blur handler
  const handleAgeBlur = () => {
    const error = validateAge(age);
    setErrors(prev => ({ ...prev, age: error }));
  };

  // => Form submit handler with complete validation
  const handleSubmit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();

    // => Validate all fields
    const emailError = validateEmail(email);
    const ageError = validateAge(age);

    // => Update errors object
    setErrors({
      email: emailError,
      age: ageError
    });

    // => Check if any errors exist
    if (emailError || ageError) {
      console.log('Form has validation errors');
      return;                                // => Stop submission
    }

    // => All validation passed
    console.log('Form submitted successfully:', { email, age });
    alert('Registration successful!');
  };

  return (
    <form onSubmit={handleSubmit}>
      <div>
        <label>Email:</label>
        <input
          type="email"
          value={email}
          onChange={handleEmailChange}
          onBlur={handleEmailBlur}
          {/* => onBlur fires when input loses focus */}
          style={{ borderColor: errors.email ? 'red' : undefined }}
          {/* => Red border if error exists */}
        />
        {errors.email && (
          <p style={{ color: 'red', fontSize: '0.875rem' }}>
            {errors.email}
            {/* => Conditional rendering: only show if error exists */}
          </p>
        )}
      </div>

      <div>
        <label>Age:</label>
        <input
          type="number"
          value={age}
          onChange={handleAgeChange}
          onBlur={handleAgeBlur}
          style={{ borderColor: errors.age ? 'red' : undefined }}
        />
        {errors.age && (
          <p style={{ color: 'red', fontSize: '0.875rem' }}>
            {errors.age}
          </p>
        )}
      </div>

      <button type="submit">Register</button>
    </form>
  );
}

export default ValidatedRegistrationForm;
```

**Key Takeaway**: Validate on blur (when input loses focus) and on submit. Store errors in state. Show error messages conditionally. Visual feedback (red border) improves UX.

**Expected Output**: Form with email and age inputs. Typing clears errors. Losing focus validates field and shows errors if invalid. Submit button validates all fields.

**Common Pitfalls**: Validating on every keystroke (annoying UX), not clearing errors when user starts typing again, or forgetting to validate on submit (only validating on blur).

## Group 5: Component Patterns

### Example 21: Conditional Rendering

Render different UI based on state. Use ternary operators, `&&` operator, or early returns.

```typescript
import { useState } from 'react';

// => Type for user authentication status
interface User {
  name: string;
  role: 'admin' | 'user';
}

function ConditionalRenderingDemo() {
  const [isLoggedIn, setIsLoggedIn] = useState<boolean>(false);
  const [user, setUser] = useState<User | null>(null);
  // => user is null when not logged in

  const handleLogin = () => {
    setIsLoggedIn(true);
    setUser({ name: 'Fatima', role: 'admin' });
    // => Set user data on login
  };

  const handleLogout = () => {
    setIsLoggedIn(false);
    setUser(null);                           // => Clear user data on logout
  };

  // => Pattern 1: Early return
  // => Return different JSX based on condition
  if (!isLoggedIn) {
    return (
      <div>
        <h2>Please Log In</h2>
        <button onClick={handleLogin}>Log In</button>
      </div>
    );
    // => Early return prevents rest of component from rendering
  }

  // => Pattern 2: Ternary operator for inline conditions
  // => condition ? trueValue : falseValue
  return (
    <div>
      <h2>Welcome, {user?.name}!</h2>

      {/* => Pattern 3: Logical AND (&&) for conditional rendering */}
      {/* => condition && elementToRender */}
      {user?.role === 'admin' && (
        <div style={{ backgroundColor: '#029E73', padding: '8px', color: '#fff' }}>
          Admin Controls
          {/* => Only renders if user.role is 'admin' */}
          {/* => If false, nothing renders (not even null) */}
        </div>
      )}

      {/* => Ternary for either/or rendering */}
      <p>
        Status: {user?.role === 'admin' ? 'Administrator' : 'Regular User'}
        {/* => Shows one of two strings */}
      </p>

      {/* => Multiple conditions with nested ternaries */}
      <p>
        Access Level: {
          user?.role === 'admin'
            ? 'Full Access'
            : user?.role === 'user'
              ? 'Limited Access'
              : 'No Access'
        }
        {/* => Chain ternaries for multiple conditions */}
        {/* => Can get complex - consider extracting to function */}
      </p>

      <button onClick={handleLogout}>Log Out</button>
    </div>
  );
}

export default ConditionalRenderingDemo;
```

**Key Takeaway**: Use early returns for entire component branches. Use `&&` for conditional elements. Use ternary for either/or rendering. Extract complex conditions to variables or functions.

**Expected Output**: Initially shows "Please Log In" with login button. After login, shows personalized welcome, admin controls (if admin), access level, and logout button.

**Common Pitfalls**: Using `&&` with falsy numbers (0 renders as "0"), nesting too many ternaries (hard to read), or using `if` statements in JSX (doesn't work - use ternary or `&&`).

### Example 22: Lists and Keys

Render arrays of data with `map()`. Provide unique `key` prop for performance.

```typescript
import { useState } from 'react';

// => Type for prayer time
interface PrayerTime {
  id: string;
  name: string;
  time: string;
  completed: boolean;
}

function PrayerTimesList() {
  const [prayers, setPrayers] = useState<PrayerTime[]>([
    { id: 'fajr', name: 'Fajr', time: '5:30 AM', completed: true },
    { id: 'dhuhr', name: 'Dhuhr', time: '12:45 PM', completed: false },
    { id: 'asr', name: 'Asr', time: '4:15 PM', completed: false },
    { id: 'maghrib', name: 'Maghrib', time: '6:30 PM', completed: false },
    { id: 'isha', name: 'Isha', time: '8:00 PM', completed: false }
  ]);
  // => Array of 5 prayer objects

  // => Toggle prayer completion
  const togglePrayer = (id: string) => {
    setPrayers(prev => prev.map(prayer =>
      prayer.id === id                       // => Find matching prayer
        ? { ...prayer, completed: !prayer.completed }  // => Toggle completed
        : prayer                             // => Keep others unchanged
    ));
    // => map returns new array, triggers re-render
  };

  // => Derived state: filter completed prayers
  const completedPrayers = prayers.filter(p => p.completed);
  // => New array containing only completed prayers
  // => Recomputed on every render (OK for small lists)

  const incompletePrayers = prayers.filter(p => !p.completed);

  return (
    <div>
      <h2>Today's Prayers</h2>

      {/* => Render all prayers */}
      <ul>
        {prayers.map(prayer => (
          <li key={prayer.id}>
            {/* => key prop REQUIRED for list items */}
            {/* => Must be unique and stable (don't use index if order changes) */}
            {/* => React uses keys for efficient updates */}

            <input
              type="checkbox"
              checked={prayer.completed}
              onChange={() => togglePrayer(prayer.id)}
            />

            <span style={{
              textDecoration: prayer.completed ? 'line-through' : 'none',
              marginLeft: '8px'
            }}>
              {prayer.name} - {prayer.time}
            </span>
          </li>
        ))}
      </ul>

      {/* => Render filtered subset */}
      <h3>Completed ({completedPrayers.length})</h3>
      {completedPrayers.length === 0 ? (
        <p>No prayers completed yet</p>
        {/* => Fallback when list empty */}
      ) : (
        <ul>
          {completedPrayers.map(prayer => (
            <li key={prayer.id}>{prayer.name}</li>
            {/* => Same key prop required even in filtered list */}
          ))}
        </ul>
      )}

      <h3>Remaining ({incompletePrayers.length})</h3>
      <ul>
        {incompletePrayers.map(prayer => (
          <li key={prayer.id}>{prayer.name} - {prayer.time}</li>
        ))}
      </ul>
    </div>
  );
}

export default PrayerTimesList;
```

**Key Takeaway**: Use `map()` to transform arrays into JSX. Always provide unique, stable `key` prop. Keys help React identify which items changed. Avoid using array index as key when list can reorder.

**Expected Output**: List of 5 prayers with checkboxes. Initially Fajr is checked. Checking prayer adds strikethrough. Completed and Remaining sections update automatically.

**Common Pitfalls**: Missing key prop (React warning), using index as key when order changes (causes bugs), or using non-unique keys (unpredictable behavior).

### Example 23: Lifting State Up

Share state between components by lifting it to common parent.

```typescript
import { useState } from 'react';

// => Child component: Temperature input
// => Controlled component - doesn't own its state
interface TemperatureInputProps {
  scale: 'celsius' | 'fahrenheit';           // => Temperature scale for display
                                             // => Literal union type (only these 2 values)
  temperature: number;                       // => Current temperature value (from parent)
  onTemperatureChange: (temp: number) => void;
  // => Callback prop: parent provides handler function
  // => Child calls this to notify parent of changes
  // => Pattern: data flows down (props), events flow up (callbacks)
}

function TemperatureInput({
  scale,
  temperature,
  onTemperatureChange
}: TemperatureInputProps) {
  // => Child doesn't own temperature state
  // => Receives value and change handler from parent
  // => "Lifted state" pattern - state owned by parent
  // => Child is "controlled" by parent

  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    onTemperatureChange(Number(e.target.value));
    // => Call parent's handler with new value
    // => e.target.value is string, convert to number
    // => Parent updates state, triggers re-render of both inputs
    // => Data flows: child input → parent state → both children
  };

  return (
    <div>
      <label>
        Temperature in {scale === 'celsius' ? 'Celsius' : 'Fahrenheit'}:
        {/* => Conditional label text based on scale prop */}
        <input
          type="number"
          value={temperature}
          {/* => Value comes from parent via props (controlled input) */}
          {/* => Input displays parent's state value */}
          onChange={handleChange}
          {/* => On change, notify parent via callback */}
        />
      </label>
    </div>
  );
}

// => Parent component: Manages shared state
// => Owns temperature state shared between two child inputs
function TemperatureConverter() {
  const [temperature, setTemperature] = useState<number>(0);
  // => Single source of truth for temperature
  // => Stored in ONE scale (celsius or fahrenheit)
  // => Initial: 0 degrees

  const [scale, setScale] = useState<'celsius' | 'fahrenheit'>('celsius');
  // => Track which scale user is currently editing
  // => Determines how to interpret temperature value
  // => Initial: celsius scale

  // => Conversion functions
  // => Pure functions: same input → same output
  const toCelsius = (fahrenheit: number) => ((fahrenheit - 32) * 5) / 9;
  // => Formula: (°F - 32) × 5/9 = °C
  // => Example: toCelsius(32) = 0, toCelsius(212) = 100

  const toFahrenheit = (celsius: number) => (celsius * 9) / 5 + 32;
  // => Formula: (°C × 9/5) + 32 = °F
  // => Example: toFahrenheit(0) = 32, toFahrenheit(100) = 212

  // => Handler for celsius input
  // => Called when celsius input changes
  const handleCelsiusChange = (temp: number) => {
    setScale('celsius');                     // => Remember last edited scale
                                             // => Now temperature is in celsius
    setTemperature(temp);                    // => Store celsius value
                                             // => State update triggers re-render
  };

  // => Handler for fahrenheit input
  // => Called when fahrenheit input changes
  const handleFahrenheitChange = (temp: number) => {
    setScale('fahrenheit');                  // => Remember last edited scale
                                             // => Now temperature is in fahrenheit
    setTemperature(temp);                    // => Store fahrenheit value
                                             // => State update triggers re-render
  };

  // => Calculate display values based on current scale
  // => Computed values (derived state) - recalculated on each render
  const celsius = scale === 'fahrenheit' ? toCelsius(temperature) : temperature;
  // => If temperature stored as fahrenheit, convert to celsius
  // => Otherwise, use temperature as-is (already celsius)
  // => Example: scale='fahrenheit', temperature=212 → celsius=100

  const fahrenheit = scale === 'celsius' ? toFahrenheit(temperature) : temperature;
  // => If temperature stored as celsius, convert to fahrenheit
  // => Otherwise, use temperature as-is (already fahrenheit)
  // => Example: scale='celsius', temperature=0 → fahrenheit=32

  return (
    <div>
      <h2>Temperature Converter</h2>

      <TemperatureInput
        scale="celsius"
        temperature={celsius}
        {/* => Pass computed celsius value to child */}
        {/* => Child displays this value in input */}
        onTemperatureChange={handleCelsiusChange}
        {/* => Pass celsius change handler to child */}
        {/* => Child calls this when user types */}
      />

      <TemperatureInput
        scale="fahrenheit"
        temperature={fahrenheit}
        {/* => Pass computed fahrenheit value to child */}
        {/* => Both inputs stay in sync via parent state */}
        onTemperatureChange={handleFahrenheitChange}
        {/* => Pass fahrenheit change handler to child */}
      />

      <p>
        Water boils at 100°C (212°F).
        {/* => Conditional message based on temperature */}
        {temperature >= (scale === 'celsius' ? 100 : 212)
          // => Check if temperature above boiling point
          // => Compare against 100 if celsius, 212 if fahrenheit
          ? ' Water would boil at this temperature.'
          : ' Water would not boil at this temperature.'}
      </p>
      {/* => Data flow: parent state → derived values → child props */}
      {/* => Event flow: child input → callback → parent state → re-render */}
    </div>
  );
}

export default TemperatureConverter;
```

**Key Takeaway**: Lift state to closest common parent when multiple components need same data. Parent owns state and passes handlers down. Children are controlled by parent. Single source of truth prevents inconsistency.

**Expected Output**: Two temperature inputs (Celsius and Fahrenheit). Editing either updates both automatically via conversion. Message indicates if water boils at current temperature.

**Common Pitfalls**: Duplicating state in children (gets out of sync), forgetting to pass handlers (children can't update), or lifting state too high (unnecessary prop drilling).

### Example 24: Component Composition

Build complex UI from smaller components. Composition is React's primary code reuse pattern.

```typescript
import { ReactNode } from 'react';

// => Layout components

interface PanelProps {
  title: string;
  children: ReactNode;
  color?: string;
}

function Panel({ title, children, color = '#0173B2' }: PanelProps) {
  // => Generic panel wrapper with title and content
  return (
    <div style={{
      border: `2px solid ${color}`,
      borderRadius: '8px',
      padding: '16px',
      margin: '8px 0'
    }}>
      <h3 style={{ color, marginTop: 0 }}>{title}</h3>
      {children}
      {/* => Renders whatever content passed between <Panel> tags */}
    </div>
  );
}

// => Content components

interface PrayerInfoProps {
  name: string;
  time: string;
  description: string;
}

function PrayerInfo({ name, time, description }: PrayerInfoProps) {
  // => Specialized component for prayer information
  return (
    <div>
      <h4>{name}</h4>
      <p><strong>Time:</strong> {time}</p>
      <p>{description}</p>
    </div>
  );
}

interface ZakatInfoProps {
  wealth: number;
  rate: number;
}

function ZakatInfo({ wealth, rate }: ZakatInfoProps) {
  // => Specialized component for Zakat calculation
  const zakatAmount = wealth * (rate / 100);

  return (
    <div>
      <p><strong>Wealth:</strong> ${wealth.toFixed(2)}</p>
      <p><strong>Zakat Rate:</strong> {rate}%</p>
      <p><strong>Zakat Amount:</strong> ${zakatAmount.toFixed(2)}</p>
    </div>
  );
}

// => Main app using composition
function IslamicDashboard() {
  return (
    <div>
      <h1>Islamic Dashboard</h1>

      {/* => Compose Panel with PrayerInfo */}
      <Panel title="Next Prayer: Dhuhr" color="#0173B2">
        <PrayerInfo
          name="Dhuhr (Noon Prayer)"
          time="12:45 PM"
          description="The second daily prayer, performed after the sun passes its zenith."
        />
      </Panel>

      {/* => Compose Panel with ZakatInfo */}
      <Panel title="Zakat Calculation" color="#DE8F05">
        <ZakatInfo wealth={50000} rate={2.5} />
      </Panel>

      {/* => Compose Panel with custom content */}
      <Panel title="Daily Reminder" color="#029E73">
        <p>Remember to recite morning and evening adhkar.</p>
        <ul>
          <li>Ayat al-Kursi after Fajr</li>
          <li>Last two verses of Surah Al-Baqarah before sleep</li>
        </ul>
      </Panel>

      {/* => Nested composition: Panel containing Panels */}
      <Panel title="Weekly Overview" color="#CC78BC">
        <p>Track your spiritual progress:</p>

        <Panel title="Quran Reading" color="#0173B2">
          <p>Pages read this week: 35</p>
          <p>Goal: 50 pages</p>
        </Panel>

        <Panel title="Charity Given" color="#DE8F05">
          <p>Total donations: \$150</p>
          <p>Monthly goal: \$200</p>
        </Panel>
      </Panel>
    </div>
  );
}

export default IslamicDashboard;
```

**Key Takeaway**: Compose complex UIs from smaller, focused components. Use `children` prop for flexible composition. Nest components arbitrarily. Each component has single responsibility.

**Expected Output**: Dashboard with 4 panels. First shows prayer info, second shows Zakat calculation, third shows reminder, fourth contains nested panels for weekly overview.

**Common Pitfalls**: Creating monolithic components (hard to maintain), prop drilling through many levels (consider Context), or over-abstracting (premature optimization).

### Example 25: Simple Zakat Calculator (Financial Domain Example)

Combine concepts from all previous examples into practical application.

```typescript
import { useState } from 'react';

// => Type for wealth categories
interface WealthCategory {
  id: string;
  name: string;
  amount: number;
}

function ZakatCalculator() {
  // => State: Array of wealth categories
  const [categories, setCategories] = useState<WealthCategory[]>([
    { id: '1', name: 'Cash & Bank Accounts', amount: 0 },
    { id: '2', name: 'Gold & Silver', amount: 0 },
    { id: '3', name: 'Business Inventory', amount: 0 },
    { id: '4', name: 'Investment Accounts', amount: 0 }
  ]);

  // => State: Debts to deduct
  const [debts, setDebts] = useState<number>(0);

  // => Update specific category amount
  const updateCategory = (id: string, amount: number) => {
    setCategories(prev => prev.map(cat =>
      cat.id === id ? { ...cat, amount } : cat
      // => Update matching category, keep others unchanged
    ));
  };

  // => Derived state: Calculate total wealth
  const totalWealth = categories.reduce((sum, cat) => sum + cat.amount, 0);
  // => Sums all category amounts
  // => Recomputed on every render

  // => Derived state: Calculate zakatable wealth (after debts)
  const zakatableWealth = Math.max(0, totalWealth - debts);
  // => Total wealth minus debts, minimum 0

  // => Nisab threshold (85g gold at ~\$60/g = \$5,100)
  const nisabThreshold = 5100;

  // => Derived state: Check if Zakat is due
  const isZakatDue = zakatableWealth >= nisabThreshold;

  // => Derived state: Calculate Zakat amount (2.5%)
  const zakatAmount = isZakatDue ? zakatableWealth * 0.025 : 0;

  return (
    <div style={{ maxWidth: '600px', margin: '0 auto', padding: '20px' }}>
      <h1>Zakat Calculator</h1>
      <p>Calculate your annual Zakat obligation (2.5% of zakatable wealth)</p>

      {/* => Render wealth categories */}
      <div>
        <h2>Your Wealth Categories</h2>
        {categories.map(category => (
          <div key={category.id} style={{ marginBottom: '12px' }}>
            <label style={{ display: 'block', marginBottom: '4px' }}>
              {category.name}:
            </label>
            <input
              type="number"
              value={category.amount}
              onChange={(e) => updateCategory(category.id, Number(e.target.value))}
              min="0"
              style={{ width: '100%', padding: '8px' }}
            />
          </div>
        ))}
      </div>

      {/* => Debts input */}
      <div style={{ marginTop: '24px' }}>
        <h2>Debts & Liabilities</h2>
        <label style={{ display: 'block', marginBottom: '4px' }}>
          Total Debts:
        </label>
        <input
          type="number"
          value={debts}
          onChange={(e) => setDebts(Number(e.target.value))}
          min="0"
          style={{ width: '100%', padding: '8px' }}
        />
      </div>

      {/* => Calculation results */}
      <div style={{
        marginTop: '24px',
        padding: '16px',
        backgroundColor: '#f5f5f5',
        borderRadius: '8px'
      }}>
        <h2>Calculation Summary</h2>

        <div style={{ marginBottom: '8px' }}>
          <strong>Total Wealth:</strong> ${totalWealth.toFixed(2)}
        </div>

        <div style={{ marginBottom: '8px' }}>
          <strong>Debts:</strong> -${debts.toFixed(2)}
        </div>

        <div style={{
          marginBottom: '16px',
          paddingTop: '8px',
          borderTop: '1px solid #ccc'
        }}>
          <strong>Zakatable Wealth:</strong> ${zakatableWealth.toFixed(2)}
        </div>

        <div style={{ marginBottom: '8px' }}>
          <strong>Nisab Threshold:</strong> ${nisabThreshold.toFixed(2)}
        </div>

        {/* => Conditional rendering based on Zakat due status */}
        {isZakatDue ? (
          <div style={{
            marginTop: '16px',
            padding: '12px',
            backgroundColor: '#029E73',
            color: '#fff',
            borderRadius: '4px'
          }}>
            <h3 style={{ margin: '0 0 8px 0' }}>Zakat is Due</h3>
            <p style={{ margin: 0, fontSize: '1.5rem', fontWeight: 'bold' }}>
              ${zakatAmount.toFixed(2)}
            </p>
            <p style={{ margin: '8px 0 0 0', fontSize: '0.875rem' }}>
              (2.5% of ${zakatableWealth.toFixed(2)})
            </p>
          </div>
        ) : (
          <div style={{
            marginTop: '16px',
            padding: '12px',
            backgroundColor: '#CC78BC',
            color: '#000',
            borderRadius: '4px'
          }}>
            <p style={{ margin: 0 }}>
              Your wealth is below the nisab threshold. Zakat is not obligatory this year.
            </p>
            <p style={{ margin: '8px 0 0 0', fontSize: '0.875rem' }}>
              Nisab threshold: ${nisabThreshold.toFixed(2)} (85g gold equivalent)
            </p>
          </div>
        )}
      </div>

      {/* => Educational note */}
      <div style={{ marginTop: '16px', fontSize: '0.875rem', color: '#666' }}>
        <p><strong>Note:</strong> This calculator provides an estimate. Consult with a qualified Islamic scholar for specific questions about your Zakat obligation.</p>
      </div>
    </div>
  );
}

export default ZakatCalculator;
```

**Key Takeaway**: Combine state management, event handling, forms, lists, conditional rendering, and derived state to build complete feature. Separate concerns with focused functions. Use descriptive variable names for clarity.

**Expected Output**: Full Zakat calculator with 4 wealth category inputs, debt input, and live calculation summary. Shows total wealth, debts, zakatable wealth, and Zakat amount (or message if below nisab).

**Common Pitfalls**: Computing derived state incorrectly (recalculate dependencies), storing derived state (duplicates data), or mixing display logic with calculation logic (separate concerns).

## Next Steps

You've completed the beginner section covering fundamental React + TypeScript concepts through 25 annotated examples. Continue learning:

- **Intermediate** - Production patterns (custom hooks, Context API, forms, data fetching, routing)
- **Advanced** - Performance optimization, testing strategies, accessibility, deployment patterns

Practice building small applications combining these concepts. Experimentation builds intuition faster than reading.
