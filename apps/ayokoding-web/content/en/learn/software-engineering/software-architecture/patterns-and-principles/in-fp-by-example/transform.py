#!/usr/bin/env python3
"""
Transform F#/Clojure tab blocks to F#/Clojure/TypeScript tab blocks.
Reads F# code from each tab block and generates TypeScript FP equivalent.
"""

import re
import sys
import subprocess


def extract_blocks(content):
    """Extract all tab blocks with their F# and Clojure content."""
    # Pattern to find a full tabs block
    # Each block: {{< tabs items="F#,Clojure" >}} ... {{< /tabs >}}
    pattern = re.compile(
        r'(\{\{< tabs items="F#,Clojure" >\}\})(.*?)(\{\{< /tabs >\}\})', re.DOTALL
    )
    return list(pattern.finditer(content))


def replace_all_blocks(content, ts_codes):
    """Replace all F#,Clojure blocks with F#,Clojure,TypeScript blocks."""
    pattern = re.compile(
        r'(\{\{< tabs items="F#,Clojure" >\}\})(.*?)(\{\{< /tabs >\}\})', re.DOTALL
    )

    idx = [0]

    def replacer(m):
        i = idx[0]
        idx[0] += 1
        ts_code = ts_codes[i]
        inner = m.group(2)
        # inner ends with the last {{< /tab >}} before {{< /tabs >}}
        # We need to append the TS tab before {{< /tabs >}}
        ts_tab = f"\n{{{{< tab >}}}}\n\n```typescript\n{ts_code}\n```\n\n{{{{< /tab >}}}}\n\n"
        new_inner = inner.rstrip() + "\n" + ts_tab
        return f'{{{{< tabs items="F#,Clojure,TypeScript" >}}}}{new_inner}{{{{< /tabs >}}}}'

    return pattern.sub(replacer, content)


def get_fsharp_code(block_content):
    """Extract F# code from a tab block."""
    # First {{< tab >}} contains F#
    tab_pattern = re.compile(r"\{\{< tab >\}\}(.*?)\{\{< /tab >\}\}", re.DOTALL)
    tabs = list(tab_pattern.finditer(block_content))
    if not tabs:
        return ""
    first_tab = tabs[0].group(1)
    # Extract fsharp code block
    code_pattern = re.compile(r"```fsharp\n(.*?)```", re.DOTALL)
    m = code_pattern.search(first_tab)
    if m:
        return m.group(1).rstrip()
    return ""


# Map from example title/context to TypeScript code
# This is the main TypeScript code database
TS_CODES = {}


def make_ts_code(fsharp_code, example_num, section_hint=""):
    """Generate TypeScript FP code equivalent to the given F# code."""
    # This is a mapping function - we pre-define all TS codes
    key = (example_num, section_hint[:30] if section_hint else "")
    if key in TS_CODES:
        return TS_CODES[key]
    return generate_ts_from_fsharp(fsharp_code, example_num)


def generate_ts_from_fsharp(fsharp_code, idx):
    """Generate TypeScript equivalent using pre-written implementations."""
    # Return pre-written TS code based on index
    if idx < len(BEGINNER_TS_BLOCKS):
        return BEGINNER_TS_BLOCKS[idx]
    return f"// TypeScript example {idx + 1}\n// TODO"


# All TypeScript blocks pre-written
BEGINNER_TS_BLOCKS = [
    # Block 0: Example 1 - Tightly coupled (no separation)
    """\
// [F#: single let function with three embedded concerns — same structural problem]
// => This function mixes data access, business logic, and presentation in one body.

// => userDb simulates a database — data access concern embedded here
const userDb: Map<number, [string, number]> = new Map([
  [1, ["Alice", 12]],
  // => key 1 maps to [name, purchaseCount]
]);

// => All three concerns in one function — impossible to reuse or test independently
const getUserDiscountMessage = (userId: number): string => {
  // => Data access: lookup user from embedded map
  const user = userDb.get(userId);
  // => userDb.get returns the value or undefined — like Map.tryFind returning None
  if (user === undefined) return "User not found";
  // => undefined branch: guard for missing user
  const [name, purchases] = user;
  // => destructure tuple: name is string, purchases is number
  const discount = purchases > 10 ? 0.15 : 0.05;
  // => business rule embedded inline — 15% for loyal, 5% default
  // => presentation formatted inline — impossible to reuse the discount rule elsewhere
  return `Hello \${name}, your discount is \${Math.round(discount * 100)}%`;
  // => Output: "Hello Alice, your discount is 15%"
};

console.log(getUserDiscountMessage(1));
// => Output: Hello Alice, your discount is 15%""",
    # Block 1: Example 1 - Separated approach
    """\
// [F#: three focused modules — TypeScript uses focused functions in separate const blocks]
// => DATA ACCESS — only knows how to retrieve users
const userDb2: ReadonlyMap<number, readonly [string, number]> = new Map([
  [1, ["Alice", 12]],
  // => key 1 maps to [name, purchaseCount] — read-only map
]);

const findUser = (userId: number): readonly [string, number] | undefined =>
  userDb2.get(userId);
  // => returns [name, purchases] or undefined — no formatting, no rules

// => BUSINESS LOGIC — only knows discount rules, not storage or display
const calculateDiscount = (purchases: number): number =>
  purchases > 10 ? 0.15 : 0.05;
  // => pure function: same input always produces same output
  // => 15% for loyal customers (>10 purchases), 5% default

// => PRESENTATION — only knows how to format, not compute or fetch
const formatDiscountMessage = (name: string, discount: number): string =>
  `Hello \${name}, your discount is \${Math.round(discount * 100)}%`;
  // => Output: "Hello Alice, your discount is 15%"

// => ORCHESTRATION — thin coordinator that pipelines the three functions
const getUserDiscountMessageSeparated = (userId: number): string => {
  const user = findUser(userId);
  // => delegates data access — result is [name, purchases] or undefined
  if (user === undefined) return "User not found";
  const [name, purchases] = user;
  const discount = calculateDiscount(purchases);
  // => delegates business rule — discount: number
  return formatDiscountMessage(name, discount);
  // => delegates formatting — returns final string
};

console.log(getUserDiscountMessageSeparated(1));
// => Output: Hello Alice, your discount is 15%""",
    # Block 2: Example 2 - SRP violation
    """\
// [F#: module with mutable private state — TypeScript uses an object with a Map for managed state]
// => A single object handles user storage, email, AND password — three reasons to change.

// => users simulates a persistent store — Map from id to {name, email}
const usersDb = new Map<number, { name: string; email: string }>();
// => mutable Map: any function can add/read entries directly

const addUser = (id: number, name: string, email: string): void => {
  usersDb.set(id, { name, email });
  // => stores user under id key — side-effecting mutation
};

// => EMAIL CONCERN embedded alongside user storage — mixing responsibilities
const sendWelcomeEmail = (userId: number): void => {
  const user = usersDb.get(userId);
  // => couples this function to the user store's internal structure
  if (user) {
    console.log(`Sending email to \${user.email}: Welcome, \${user.name}!`);
    // => Output: Sending email to alice@example.com: Welcome, Alice!
  }
};

// => PASSWORD CONCERN also embedded — a third responsibility in one module
const resetPassword = (userId: number): string => {
  const newPassword = `pass_\${userId}_reset`;
  // => deterministic fake password for this example
  console.log(`Password reset for user \${userId}: \${newPassword}`);
  // => Output: Password reset for user 1: pass_1_reset
  return newPassword;
  // => returns the new password string
};""",
    # Block 3: Example 2 - SRP compliant
    """\
// [F#: three focused modules — TypeScript uses focused pure functions]

// => RESPONSIBILITY 1: User data management only
// => Immutable store returned on each operation — pure function, no side effects
type UserStore = ReadonlyMap<number, readonly [string, string]>;
// => Map<id, [name, email]> — read-only

const addUserToStore = (
  id: number, name: string, email: string, store: UserStore
): UserStore => {
  const next = new Map(store);
  // => copy-on-write: create a new Map from the existing entries
  next.set(id, [name, email] as const);
  // => add the new user — original store is unchanged
  return next;
  // => returns a NEW store with the user added
};

const getUserFromStore = (
  id: number, store: UserStore
): readonly [string, string] | undefined =>
  store.get(id);
  // => returns [name, email] or undefined — safe lookup

// => RESPONSIBILITY 2: Email notifications only
// => This function changes only when email format or provider changes
const sendWelcomeEmailSrp = (name: string, email: string): void => {
  console.log(`Sending email to \${email}: Welcome, \${name}!`);
  // => Output: Sending email to alice@example.com: Welcome, Alice!
};

// => RESPONSIBILITY 3: Password management only
const resetPasswordSrp = (userId: number): string => {
  const newPassword = `pass_\${userId}_reset`;
  // => deterministic for this example; use crypto.randomUUID() in production
  console.log(`Password reset for user \${userId}: \${newPassword}`);
  // => Output: Password reset for user 1: pass_1_reset
  return newPassword;
  // => returns generated password string
};

const store0: UserStore = new Map();
// => empty Map is our initial state — no users yet
const store1 = addUserToStore(1, "Alice", "alice@example.com", store0);
// => store1: Map with one user entry — store0 unchanged
sendWelcomeEmailSrp("Alice", "alice@example.com");
// => Output: Sending email to alice@example.com: Welcome, Alice!""",
    # Block 4: Example 3 - Three-layer architecture
    """\
// [F#: three modules with downward-only calls — TypeScript uses focused function groups]

// ============================================================
// DATA ACCESS LAYER — only knows about storage
// ============================================================
// => Read-only product catalog — each product has id, name, price, stock
const products: ReadonlyMap<number, Readonly<{ name: string; price: number; stock: number }>> =
  new Map([
    [1, { name: "Laptop", price: 1200.0, stock: 5 }],
    // => stock 5: available
    [2, { name: "Mouse",  price: 25.0,   stock: 0 }],
    // => stock 0: out of stock
  ]);

const findById = (productId: number) => products.get(productId);
// => returns the product or undefined — caller decides what to do with absence

// ============================================================
// BUSINESS LOGIC LAYER — only knows about rules
// ============================================================
// => Tagged union mirrors F# discriminated union ProductResult
type ProductResult =
  | { tag: "Available"; name: string; price: number }
  // => product exists and is in stock
  | { tag: "OutOfStock"; name: string }
  // => product exists but stock is zero
  | { tag: "NotFound" };
  // => no product found for the given id

const checkAvailability = (productId: number): ProductResult => {
  const p = findById(productId);
  // => delegates data retrieval to the data access layer
  if (p === undefined) return { tag: "NotFound" };
  // => no product record — return NotFound case
  if (p.stock === 0) return { tag: "OutOfStock", name: p.name };
  // => business rule: zero stock means unavailable
  return { tag: "Available", name: p.name, price: p.price };
  // => product in stock — return name and price
};

// ============================================================
// PRESENTATION LAYER — only knows about formatting responses
// ============================================================
const formatResult = (result: ProductResult): string => {
  switch (result.tag) {
    // => switch on the tag — exhaustive matching like F# pattern match
    case "Available":
      return `Available: \${result.name} at $\${result.price.toFixed(2)}`;
      // => Output (id=1): "Available: Laptop at $1200.00"
    case "OutOfStock":
      return `Error: '\${result.name}' is out of stock`;
      // => Output (id=2): "Error: 'Mouse' is out of stock"
    case "NotFound":
      return "Error: Product not found";
      // => Output (id=99): "Error: Product not found"
  }
};

// Wire and run — pipeline mirrors F# |> operator
const display = (productId: number): string =>
  formatResult(checkAvailability(productId));
  // => pipes id through business layer then presentation layer

console.log(display(1));  // => Available: Laptop at $1200.00
console.log(display(2));  // => Error: 'Mouse' is out of stock
console.log(display(99)); // => Error: Product not found""",
    # Block 5: Example 4 - Presentation layer isolation
    """\
// [F#: three focused functions — TypeScript uses readonly maps and pure functions]

// => DATA LAYER — retrieves raw records (pure function, no side effects)
const orderDb: ReadonlyMap<number, Readonly<{ total: number; status: string }>> =
  new Map([
    [101, { total: 299.99, status: "shipped" }],
    // => shipped: not eligible for cancellation
    [102, { total: 49.0,   status: "pending" }],
    // => pending with low total: eligible for cancellation
  ]);

const findOrder = (orderId: number) => orderDb.get(orderId);
// => returns the order or undefined — pure lookup

// => BUSINESS LAYER — applies domain rules (pure function)
const isEligibleForCancellation = (total: number, status: string): boolean =>
  status === "pending" && total < 500.0;
  // => cancellation rule: pending AND total below $500 threshold
  // => changing this rule affects only this function

// => PRESENTATION LAYER — translates, never decides
const handleCancelRequest = (orderId: number): string => {
  const order = findOrder(orderId);
  // => fetches from data layer — presentation never queries the map directly
  if (order === undefined)
    return `Order \${orderId} not found`;
    // => undefined branch: presentation transforms absence into a user-facing message
  const eligible = isEligibleForCancellation(order.total, order.status);
  // => business logic evaluated in business layer, result consumed here
  if (eligible)
    return `Order \${orderId} cancelled successfully`;
    // => Output (id=102): "Order 102 cancelled successfully"
  return `Order \${orderId} cannot be cancelled (status: \${order.status})`;
  // => Output (id=101): "Order 101 cannot be cancelled (status: shipped)"
};

console.log(handleCancelRequest(101)); // => Order 101 cannot be cancelled (status: shipped)
console.log(handleCancelRequest(102)); // => Order 102 cancelled successfully
console.log(handleCancelRequest(999)); // => Order 999 not found""",
    # Block 6: Example 5 - MVC Basics
    """\
// [F#: three modules with pure state threading — TypeScript uses readonly types and pure functions]

// ============================================================
// MODEL — data type + pure rule functions
// ============================================================
// => Readonly branded types mirror F# record types
type Todo = Readonly<{ id: number; title: string; done: boolean }>;
// => record type: id AND title AND done — all required

type Store = Readonly<{ items: readonly Todo[]; nextId: number }>;
// => immutable store: items list and next available id

const emptyStore: Store = { items: [], nextId: 1 };
// => emptyStore : Store — initial state with no items

const modelAdd = (title: string, store: Store): readonly [Todo, Store] => {
  const item: Todo = { id: store.nextId, title, done: false };
  // => item : Todo — new item with auto-incremented id
  const newStore: Store = {
    items: [...store.items, item],
    // => spread creates a new array with item appended
    nextId: store.nextId + 1,
    // => nextId incremented for next call
  };
  return [item, newStore] as const;
  // => returns the created item AND the updated store
};

const modelComplete = (itemId: number, store: Store): readonly [boolean, Store] => {
  const updated = store.items.map(t =>
    t.id === itemId ? { ...t, done: true } : t
    // => matching id: spread creates new Todo with done=true
    // => non-matching items pass through unchanged
  );
  const found = updated.some(t => t.id === itemId);
  // => found: boolean — true if any item matched the given id
  return [found, { ...store, items: updated }] as const;
  // => returns [success flag, updated store]
};

// ============================================================
// VIEW — formats data for display, no logic
// ============================================================
const renderList = (items: readonly Todo[]): string => {
  if (items.length === 0) return "No todos yet.";
  // => empty array: short message
  return items
    .map(item => {
      const status = item.done ? "✓" : "○";
      // => status is "✓" for done items, "○" for pending
      return `[\${status}] \${item.id}. \${item.title}`;
      // => each item formatted as "[○] 1. Buy milk"
    })
    .join("\\n");
  // => joined with newlines
};

const renderCreated = (item: Todo): string =>
  `Created todo #\${item.id}: \${item.title}`;
  // => Output: "Created todo #1: Buy milk"

// ============================================================
// CONTROLLER — coordinates model and view
// ============================================================
const ctrlCreate = (title: string, store: Store): readonly [string, Store] => {
  const [item, newStore] = modelAdd(title, store);
  // => delegates creation to model — receives item + updated store
  return [renderCreated(item), newStore] as const;
  // => delegates formatting to view — returns message + store
};

const ctrlListAll = (store: Store): string =>
  renderList(store.items);
  // => fetches items from store, delegates rendering to view

const ctrlMarkDone = (itemId: number, store: Store): readonly [string, Store] => {
  const [found, newStore] = modelComplete(itemId, store);
  // => delegates completion to model
  const msg = found
    ? `Todo #\${itemId} marked as done`
    : `Todo #\${itemId} not found`;
  return [msg, newStore] as const;
  // => returns message + updated store
};

// => Wire the MVC triad together — pure state threading
const [msg1, s1] = ctrlCreate("Buy milk", emptyStore);
// => msg1 = "Created todo #1: Buy milk", s1 has one item
const [msg2, s2] = ctrlCreate("Write tests", s1);
// => msg2 = "Created todo #2: Write tests", s2 has two items
const [msg3, s3] = ctrlMarkDone(1, s2);
// => msg3 = "Todo #1 marked as done", s3 has item 1 with done = true

console.log(msg1);          // => Created todo #1: Buy milk
console.log(msg2);          // => Created todo #2: Write tests
console.log(msg3);          // => Todo #1 marked as done
console.log(ctrlListAll(s3));
// => [✓] 1. Buy milk
// => [○] 2. Write tests""",
    # Block 7: Example 6 - Model encapsulates validation
    """\
// [F#: module with private constructor and Result<T,E> — TypeScript uses branded type + Result]

// => Result<T, E> type mirrors F# Result discriminated union
type Result<T, E> = { ok: true; value: T } | { ok: false; error: E };
// => Ok wraps a valid value; Err wraps a failure description

// => POOR APPROACH: plain object with no invariant enforcement
// => Nothing prevents: { balance: -9999 }
type PoorBankAccount = { balance: number };
// => Callers must remember to validate themselves — drift is inevitable

// => ENCAPSULATED APPROACH: branded opaque type + smart constructor
// => _brand makes BankAccount structurally distinct — callers cannot create it directly
type BankAccount = Readonly<{ balance: number; _brand: "BankAccount" }>;

const createAccount = (initialBalance: number): Result<BankAccount, string> => {
  if (initialBalance < 0)
    return { ok: false, error: "Initial balance cannot be negative" };
    // => enforced at construction time — invalid state never enters the system
  return { ok: true, value: { balance: initialBalance, _brand: "BankAccount" } };
  // => Ok wraps the valid account — callers must handle the Result
};

const deposit = (amount: number, account: BankAccount): Result<BankAccount, string> => {
  if (amount <= 0)
    return { ok: false, error: "Deposit amount must be positive" };
    // => model rejects invalid inputs without caller involvement
  return { ok: true, value: { ...account, balance: account.balance + amount } };
  // => returns a NEW account with updated balance — immutable update
};

const withdraw = (amount: number, account: BankAccount): Result<BankAccount, string> => {
  if (amount <= 0)
    return { ok: false, error: "Withdrawal amount must be positive" };
  if (account.balance - amount < 0)
    return { ok: false, error: `Insufficient funds: balance is \${account.balance.toFixed(2)}` };
    // => business rule enforced here — callers cannot bypass
  return { ok: true, value: { ...account, balance: account.balance - amount } };
  // => returns new account with reduced balance
};

const getBalance = (account: BankAccount): number => account.balance;
// => read-only accessor — type prevents direct external construction

// => USAGE: chain Result values through bind-style operations
const bindResult = <T, U, E>(
  result: Result<T, E>,
  fn: (v: T) => Result<U, E>
): Result<U, E> =>
  result.ok ? fn(result.value) : result;
  // => if Ok, apply fn; if Err, propagate the error

const result =
  bindResult(
    bindResult(createAccount(100.0), acc => withdraw(50.0, acc)),
    acc => withdraw(200.0, acc)
  );
// => createAccount(100) -> Ok {balance:100}
// => withdraw(50) -> Ok {balance:50}
// => withdraw(200) -> Err "Insufficient funds: balance is 50.00"

if (result.ok)
  console.log(`Balance: $\${getBalance(result.value).toFixed(2)}`);
else
  console.log(`Error: \${result.error}`);
// => Output: Error: Insufficient funds: balance is 50.00""",
    # Block 8: Example 7 - DI without injection (hard-coded)
    """\
// [F#: greetUserHardcoded — TypeScript version with embedded dependency]
// => Hard-coded dependency: greetUser always queries this specific Map
// => Cannot be tested without the real data store
const greetUserHardcoded = (userId: number): string => {
  const db = new Map([[1, "Alice"], [2, "Bob"]]);
  // => dependency created inside function — impossible to substitute in tests
  const name = db.get(userId);
  // => Map.get returns the value or undefined — like Map.tryFind returning None
  return name !== undefined
    ? `Hello, \${name}!`
    : `User \${userId} not found`;
  // => Output: "Hello, Alice!"
};""",
    # Block 9: Example 7 - DI with injection
    """\
// [F#: UserFetcher type alias + injected function — TypeScript uses a function type]

// => TYPE ALIAS for the dependency — any function matching this signature works
type UserFetcher = (userId: number) => string | undefined;
// => (number) => string | undefined means: given an id, produce an optional name

// => REAL implementation for production
const realUserFetcher: UserFetcher = (() => {
  const data = new Map([[1, "Alice"], [2, "Bob"]]);
  // => captures the data map in a closure — simulates a real DB
  return (userId: number) => data.get(userId);
  // => returns "Alice" or undefined — delegates to map lookup
})();

// => FAKE implementation for tests — no DB required
const fakeUserFetcher: UserFetcher = (_userId: number) => "TestUser";
// => always returns "TestUser" regardless of id — predictable in tests

// => SERVICE: accepts any UserFetcher — decoupled from specific implementation
const greetUser = (fetchUser: UserFetcher, userId: number): string => {
  const name = fetchUser(userId);
  // => delegates lookup to whatever fetcher was injected
  return name !== undefined
    ? `Hello, \${name}!`
    : `User \${userId} not found`;
  // => Output: "Hello, Alice!"
};

// => PRODUCTION: inject real fetcher
console.log(greetUser(realUserFetcher, 1));  // => Hello, Alice!
console.log(greetUser(realUserFetcher, 99)); // => User 99 not found

// => TEST: inject fake fetcher — no database needed
console.log(greetUser(fakeUserFetcher, 1));  // => Hello, TestUser!""",
    # Block 10: Example 8 - Constructor vs method injection
    """\
// [F#: partial application / method injection — TypeScript uses closures and parameter passing]

// => PARTIAL APPLICATION AS "CONSTRUCTOR" INJECTION
// => Use when: dependency is always required and does not change per call
const makeOrderProcessor =
  (charge: (amount: number) => boolean) =>
  (orderId: number, amount: number): string => {
    // => charge is fixed at "construction" time via closure
    // => returns a new function with charge baked in
    const success = charge(amount);
    // => uses the injected charge function — no knowledge of which gateway
    return success
      ? `Order \${orderId} paid ($\${Math.round(amount)})`
      : `Order \${orderId} payment failed`;
    // => Output (success): "Order 1 paid ($500)"
  };

// => METHOD INJECTION (per-call dependency passing)
// => Use when: dependency varies per request (e.g., per-user logger)
const logMessage = (message: string, output: (s: string) => void): void => {
  output(`[AUDIT] \${message}`);
  // => output is passed per call — can be console, file, test spy, etc.
  // => Output: "[AUDIT] User login"
};

// => USAGE: wire concrete implementations at the composition root

// => "Constructor" injection via closure
const fakeCharge = (amount: number): boolean => amount < 1000.0;
// => fakeCharge returns true for amounts < 1000 (simulates approval limit)
const processOrder = makeOrderProcessor(fakeCharge);
// => processOrder is now a (orderId, amount) => string with charge baked in

console.log(processOrder(1, 500.0));  // => Order 1 paid ($500)
console.log(processOrder(2, 1500.0)); // => Order 2 payment failed

// => Method injection — output function varies per call
logMessage("User login",  (msg) => console.log(msg));
// => Output: [AUDIT] User login
logMessage("File export", (msg) => console.log(`>> \${msg}`));
// => Output: >> [AUDIT] File export""",
    # Block 11: Example 9 - ISP fat dependency
    """\
// [F#: fat record-of-functions — TypeScript uses a fat interface with required methods]

// => FAT interface: all consumers must provide ALL four methods
interface EmployeeOperations {
  calculateSalary: () => number;
  // => relevant for paid employees
  clockIn: () => void;
  // => relevant for hourly workers
  generateReport: () => string;
  // => relevant for managers
  requestLeave: (days: number) => void;
  // => relevant for all employees
}

// => CONTRACTOR only needs salary calculation
// => yet must provide all four — clockIn and generateReport are forced stubs
const contractorOps: EmployeeOperations = {
  calculateSalary: () => 500.0,
  // => useful: contractor's daily rate
  clockIn:         () => {},
  // => forced but meaningless for a contractor
  generateReport:  () => "",
  // => forced but meaningless for a contractor
  requestLeave:    (_days: number) => {},
  // => forced but meaningless — contractors don't accrue leave
};""",
    # Block 12: Example 9 - ISP segregated
    """\
// [F#: focused record types — TypeScript uses focused interfaces]

// => FOCUSED interfaces: each covers one capability
interface Payable    { calculateSalary: () => number }
// => pay calculation only

interface Trackable  { clockIn: () => void }
// => time tracking only

interface Reportable { generateReport: () => string }
// => reporting only

// => CONTRACTOR: only salary matters, no forced stubs
const segContractor: Payable = { calculateSalary: () => 500.0 };
// => flat daily rate — object has exactly one method

// => FULL-TIME EMPLOYEE: salary + time tracking + leave (separate objects)
let hoursWorked = 0;
// => mutable local for this demonstration

const ftPayable:   Payable   = { calculateSalary: () => hoursWorked * 25.0 };
// => $25/hour — only salary concern
const ftTrackable: Trackable = { clockIn: () => { hoursWorked += 8; } };
// => adds one full work day per clock-in — only tracking concern

// => MANAGER: salary + reporting
const mgPayable:    Payable    = { calculateSalary:  () => 8000.0 };
// => fixed monthly salary
const mgReportable: Reportable = { generateReport: () => "Team performance: on track" };
// => manager-specific report — only reporting concern

console.log(`Contractor salary: \${Math.round(segContractor.calculateSalary())}`);
// => Output: Contractor salary: 500
ftTrackable.clockIn();
// => hoursWorked is now 8
console.log(`FT salary: \${Math.round(ftPayable.calculateSalary())}`);
// => Output: FT salary: 200  (8 hours * $25)""",
    # Block 13: Example 10 - OCP violation
    """\
// [F#: string-dispatch match — TypeScript uses if-else chain with same anti-pattern]
// => VIOLATION: adding a new discount type requires editing this function
const calculateDiscountBad = (price: number, discountType: string): number => {
  if (discountType === "regular") return price * 0.10;
  // => 10% off — must edit here to add "seasonal"
  if (discountType === "loyalty") return price * 0.20;
  // => 20% off for loyal customers
  return 0.0;
  // => default: no discount — "seasonal" forces editing this function
};""",
    # Block 14: Example 10 - OCP compliant
    """\
// [F#: DiscountStrategy type alias + composable functions — TypeScript uses function types]

// => STRATEGY TYPE: defines the contract — any (number) => number function qualifies
type DiscountStrategy = (price: number) => number;
// => Takes a price, returns the discount amount — simple and composable

// => CONCRETE STRATEGIES — add new ones without touching existing code
const regularDiscount: DiscountStrategy  = (price) => price * 0.10;
// => 10% discount

const loyaltyDiscount: DiscountStrategy  = (price) => price * 0.20;
// => 20% discount for loyal customers

// => EXTENSION: new discount type — existing strategies are UNTOUCHED
const seasonalDiscount: DiscountStrategy = (price) => price * 0.30;
// => 30% seasonal sale discount — added without modifying regularDiscount or loyaltyDiscount

// => CLIENT: accepts any DiscountStrategy — closed to modification, open to extension
const finalPrice = (strategy: DiscountStrategy, price: number): number => {
  const discount = strategy(price);
  // => delegates discount computation to injected strategy
  return price - discount;
  // => price minus discount amount
};

console.log(finalPrice(regularDiscount,  100.0).toFixed(1)); // => 90.0
console.log(finalPrice(seasonalDiscount, 100.0).toFixed(1)); // => 70.0

// => COMPOSING strategies: combine two strategies (e.g., loyalty + seasonal)
const combinedDiscount = (strategies: readonly DiscountStrategy[]): DiscountStrategy =>
  (price) => strategies.reduce((sum, s) => sum + s(price), 0);
  // => reduces all strategies to a sum of discounts — extensible: add more strategies

console.log(finalPrice(combinedDiscount([regularDiscount, loyaltyDiscount]), 100.0).toFixed(1));
// => 100 - (10 + 20) = 70.0""",
    # Block 15: Example 11 - LSP violation
    """\
// [F#: mutable record violation — TypeScript uses mutable object fields for the demo]

// => Simulating the OOP Rectangle/Square LSP violation with mutable objects
// => to illustrate WHY FP prefers composition over inheritance
type MutableRectangle = { width: number; height: number };

// => "Base type" contract: setWidth and setHeight change only one dimension each
const setWidth  = (r: MutableRectangle, w: number): void => { r.width  = w; };
// => intended: only width changes
const setHeight = (r: MutableRectangle, h: number): void => { r.height = h; };
// => intended: only height changes

// => "Square" variant VIOLATES the contract by coupling width and height
const makeSquare = (side: number): MutableRectangle => ({ width: side, height: side });
// => both dimensions equal — fine for a square, but…

const squareLike = makeSquare(1.0);
setWidth(squareLike, 5.0);
// => changes width to 5.0, but height stays 1.0 in this representation
// => a "true square" mutator would also set height = 5.0 — breaking the Rectangle contract
setHeight(squareLike, 3.0);
console.log(`Expected area 15.0, got: \${(squareLike.width * squareLike.height).toFixed(1)}`);
// => Output: Expected area 15.0, got: 15.0 — coincidence; the mutable contract is fragile""",
    # Block 16: Example 11 - LSP compliant
    """\
// [F#: discriminated union Shape — TypeScript uses tagged union + exhaustive switch]

// => SHAPE tagged union: each case carries exactly the data it needs
type Shape =
  | { tag: "Rectangle"; width: number; height: number }
  // => Rectangle case: independent width and height
  | { tag: "Square"; side: number };
  // => Square case: single side length — no inherited dimension confusion

// => AREA function: works for any Shape — LSP satisfied at the type level
const area = (shape: Shape): number => {
  switch (shape.tag) {
    case "Rectangle": return shape.width * shape.height;
    // => width * height — independent dimensions
    case "Square":    return shape.side * shape.side;
    // => side * side — no ambiguity
  }
};

console.log(`Area: \${area({ tag: "Rectangle", width: 5.0, height: 3.0 }).toFixed(1)}`);
// => Output: Area: 15.0
console.log(`Area: \${area({ tag: "Square", side: 4.0 }).toFixed(1)}`);
// => Output: Area: 16.0

// => GENERIC substitutability example: any Shape satisfies the area function
const describeAreas = (shapes: readonly Shape[]): void => {
  shapes.forEach(s => console.log(`Area: \${area(s).toFixed(1)}`));
  // => any Shape satisfies the area function — tagged union enforces LSP
};

describeAreas([
  { tag: "Rectangle", width: 5.0, height: 3.0 },
  { tag: "Square", side: 4.0 },
  { tag: "Rectangle", width: 2.0, height: 6.0 },
]);
// => Area: 15.0
// => Area: 16.0
// => Area: 12.0""",
    # Block 17: Example 12 - DRY violation
    """\
// [F#: duplicated inline conditions — TypeScript version with same anti-pattern]
// => VIOLATION: the "eligible user" rule is duplicated in every function
// => If the rule changes (e.g., add emailVerified check), all three must be updated

const sendNotification = (name: string, active: boolean, age: number): void => {
  if (active && age >= 18)              // => rule duplicated here
    console.log(`Notifying \${name}`);
    // => Output: Notifying Alice
};

const generateReport = (name: string, active: boolean, age: number): void => {
  if (active && age >= 18)              // => same rule repeated
    console.log(`Report for \${name}`);
};

const allowPurchase = (active: boolean, age: number): boolean =>
  active && age >= 18;                  // => rule duplicated a third time""",
    # Block 18: Example 12 - DRY compliant
    """\
// [F#: single named function for the rule — TypeScript version]

// => SINGLE SOURCE OF TRUTH: rule defined once as a named function
const isEligibleUser = (active: boolean, age: number): boolean =>
  active && age >= 18;
  // => returns true only if active AND adult
  // => changing this function updates all three callers automatically

// => Each function delegates to the single rule
const sendNotificationDry = (name: string, active: boolean, age: number): void => {
  if (isEligibleUser(active, age))      // => delegates to single rule
    console.log(`Notifying \${name}`);
    // => Output: Notifying Alice
};

const generateReportDry = (name: string, active: boolean, age: number): void => {
  if (isEligibleUser(active, age))      // => same single rule
    console.log(`Report for \${name}`);
};

const allowPurchaseDry = (active: boolean, age: number): boolean =>
  isEligibleUser(active, age);          // => single rule, no duplication

sendNotificationDry("Alice", true, 25);
// => Output: Notifying Alice
console.log(allowPurchaseDry(true, 25));  // => true
console.log(allowPurchaseDry(false, 25)); // => false""",
    # Block 19: Example 13 - KISS over-engineered
    """\
// [F#: DU + factory + record — TypeScript over-engineered version with classes and strategies]
// => OVER-ENGINEERED: interface + class hierarchy + factory for a simple greeting

interface GreetingStrategy {
  // => interface adds no value when there is only one consumer
  getPrefix(): string;
}

class FormalGreeting implements GreetingStrategy {
  getPrefix() { return "Good day"; }
  // => implements interface — one class per style
}

class CasualGreeting implements GreetingStrategy {
  getPrefix() { return "Hey"; }
  // => another class — growing ceremony for one use case
}

const greetFromStrategy = (strategy: GreetingStrategy, name: string): string =>
  `\${strategy.getPrefix()}, \${name}.`;
  // => delegates to interface method, then concatenates

// => Usage: 1 interface + 2 classes + 1 function to print "Good day, Alice."
const formalStrategy = new FormalGreeting();
console.log(greetFromStrategy(formalStrategy, "Alice"));
// => Good day, Alice.
// => Four declarations to do what one function does""",
    # Block 20: Example 13 - KISS simple
    """\
// [F#: single let greet fn — TypeScript version with one pure function]
// => SIMPLE: one function, zero ceremony, achieves the same result
const greet = (name: string): string =>
  `Good day, \${name}.`;
  // => Output: Good day, Alice.
  // => If greeting styles are needed later, add them then (YAGNI)

console.log(greet("Alice"));
// => Good day, Alice.""",
    # Block 21: Example 14 - YAGNI
    """\
// [F#: speculative record fields vs. minimal record — TypeScript version]

// => YAGNI VIOLATION: speculative fields not required by any current use case
interface SpeculativeUserProfile {
  name:  string;
  // => required today
  email: string;
  // => required today

  // => SPECULATIVE fields: no current feature requires these
  theme:               string;
  // => "might need dark mode someday"
  preferredLanguage:   string;
  // => "maybe we'll go international"
  newsletterFrequency: string;
  // => "for a newsletter we haven't built"
  aiRecommendations:   boolean;
  // => "for an AI feature in the roadmap"
}
// => The speculative fields force every test and constructor to supply values
// => that have no business meaning yet — pure noise in the codebase

// ============================================================

// => YAGNI COMPLIANT: only what the application actually needs right now
type SimpleUserProfile = Readonly<{
  name:  string;
  // => required today
  email: string;
  // => required today
  // => No speculative fields — add when a feature actually needs them
}>;

const displayName = (profile: SimpleUserProfile): string =>
  profile.name;
  // => required today for display — Output: "Alice"
  // => Add exportToXml when an export feature is actually built
  // => Add theme when dark mode is actually shipped

const user: SimpleUserProfile = { name: "Alice", email: "alice@example.com" };
// => construction is trivial — no speculative fields to supply
console.log(displayName(user));
// => Output: Alice""",
    # Block 22: Example 15 - High coupling
    """\
// [F#: mutable record fields with direct internal access — TypeScript version]
// => HIGH COUPLING: placeOrder reads the internals of both customer and order objects

type Customer15 = {
  name:               string;
  creditLimit:        number;
  // => exposed field — any caller can depend on this name
  outstandingBalance: number;
  // => mutable state — callers can depend on this structure directly
};

type Inventory15 = {
  items: Map<string, number>;
  // => exposed mutable Map — any caller can manipulate stock directly
};

// => placeOrder KNOWS about Customer's fields AND Inventory's internals
const tightlyCoupledPlaceOrder = (
  customer: Customer15,
  inventory: Inventory15,
  item: string,
  price: number
): string => {
  // => directly reads customer's internal fields — tight coupling
  if (customer.outstandingBalance + price > customer.creditLimit)
    return "Credit limit exceeded";
  // => directly reads inventory's internal Map — tight coupling
  const stock = inventory.items.get(item) ?? 0;
  if (stock <= 0)
    return `\${item} is out of stock`;
  // => directly mutates customer's internal field
  customer.outstandingBalance += price;
  // => directly mutates inventory's internal Map
  inventory.items.set(item, stock - 1);
  return `Order placed: \${item} for \${customer.name}`;
  // => Output: "Order placed: Laptop for Alice"
};

const customer15  = { name: "Alice", creditLimit: 2000.0, outstandingBalance: 0.0 };
const inventory15 = { items: new Map([["Laptop", 5]]) };
console.log(tightlyCoupledPlaceOrder(customer15, inventory15, "Laptop", 1200.0));
// => Order placed: Laptop for Alice
// => If Customer renames creditLimit to availableCredit, placeOrder breaks
// => If Inventory changes items to a DB call, placeOrder breaks""",
    # Block 23: Example 16 - Low coupling through encapsulation
    """\
// [F#: modules with private types — TypeScript uses branded types + smart constructors]

// => ENCAPSULATED Customer — hides internals behind stable functions
// => _brand makes CustomerT structurally distinct — callers cannot construct it directly
type CustomerT = Readonly<{
  name:        string;
  creditLimit: number;
  balance:     number;
  _brand:      "Customer";
}>;

const createCustomer = (name: string, creditLimit: number): CustomerT => ({
  name, creditLimit, balance: 0.0, _brand: "Customer",
  // => smart constructor: enforces valid initial state
});

const customerName = (c: CustomerT): string => c.name;
// => read-only accessor — callers cannot reach the name field directly

const canAcceptCharge = (amount: number, c: CustomerT): boolean =>
  c.balance + amount <= c.creditLimit;
  // => hides the credit logic — callers don't know the formula

const recordCharge = (amount: number, c: CustomerT): CustomerT => ({
  ...c, balance: c.balance + amount,
  // => returns a NEW customer with updated balance — immutable update
  // => internal representation could change without affecting callers
});

// => ENCAPSULATED Inventory — hides the backing data structure
type InventoryT = Readonly<{ stock: ReadonlyMap<string, number>; _brand: "Inventory" }>;

const createInventory = (items: readonly [string, number][]): InventoryT => ({
  stock: new Map(items),
  // => smart constructor: builds inventory from a list of [item, qty] pairs
  _brand: "Inventory",
});

const isAvailable = (item: string, inv: InventoryT): boolean =>
  (inv.stock.get(item) ?? 0) > 0;
  // => hides how stock is stored — could be a database call

const decrementStock = (item: string, inv: InventoryT): InventoryT => {
  const qty = inv.stock.get(item) ?? 0;
  if (qty <= 0) return inv;
  // => no-op if item is missing or at zero
  const next = new Map(inv.stock);
  next.set(item, qty - 1);
  return { ...inv, stock: next };
  // => returns new inventory with decremented count
};

// => LOOSELY COUPLED placeOrder — talks to module interfaces only
const placeOrder = (
  customer: CustomerT,
  inv: InventoryT,
  item: string,
  price: number
): readonly [string, CustomerT, InventoryT] => {
  if (!canAcceptCharge(price, customer))
    return ["Credit limit exceeded", customer, inv];
    // => no internal field access — delegates to Customer functions
  if (!isAvailable(item, inv))
    return [`\${item} is out of stock`, customer, inv];
  const c2 = recordCharge(price, customer);
  // => delegates mutation to Customer functions
  const i2 = decrementStock(item, inv);
  // => delegates mutation to Inventory functions
  return [`Order placed: \${item} for \${customerName(customer)}`, c2, i2] as const;
  // => Output: "Order placed: Laptop for Alice"
};

const c16 = createCustomer("Alice", 2000.0);
const i16 = createInventory([["Laptop", 5]]);
const [msg16] = placeOrder(c16, i16, "Laptop", 1200.0);
console.log(msg16);
// => Order placed: Laptop for Alice
// => Renaming creditLimit to availableCredit has ZERO impact on placeOrder""",
    # Block 24: Example 17 - Cohesion (single block with both low and high cohesion)
    """\
// [F#: mixed module vs. focused modules — TypeScript uses namespaced objects]

// => LOW COHESION: mixedUtils handles three completely unrelated domains
const mixedUtils = {
  formatTitle: (title: string): string => title.toUpperCase(),
  // => "hello" -> "HELLO" — string manipulation concern

  calculateTax: (price: number, rate: number): number => price * rate,
  // => 100 * 0.1 = 10.0 — financial calculation, unrelated to strings

  isWeekend: (date: Date): boolean => {
    const day = date.getDay();
    return day === 0 || day === 6;
    // => date logic — unrelated to both of the above
  },
};

// => HIGH COHESION: each namespace groups only related behavior
// => REASON: when formatTitle changes, it does not affect tax or date logic

const stringFormatter = {
  formatTitle: (title: string): string => title.toUpperCase(),
  // => "hello world" -> "HELLO WORLD"

  truncate: (maxLength: number, text: string): string =>
    text.length > maxLength ? text.slice(0, maxLength) + "…" : text,
    // => "Hello World" with maxLength=5 -> "Hello…"
};

const taxCalculator = {
  calculate: (rate: number, price: number): number => price * rate,
  // => 200 * 0.1 = 20.0

  calculateWithCap: (rate: number, cap: number, price: number): number =>
    Math.min(price * rate, cap),
    // => min(tax, cap) — capped at maximum
};

const dateHelper = {
  isWeekend: (date: Date): boolean => {
    const day = date.getDay();
    return day === 0 || day === 6;
    // => true on Saturday (6) or Sunday (0)
  },

  dayName: (date: Date): string =>
    date.toLocaleDateString("en-US", { weekday: "long" }),
    // => "Monday", "Tuesday", etc.
};

console.log(stringFormatter.formatTitle("hello world"));
// => HELLO WORLD
console.log(taxCalculator.calculate(0.1, 200.0).toFixed(1));
// => 20.0""",
    # Block 25: Example 18 - Encapsulation with private state
    """\
// [F#: private backing field + Result return — TypeScript uses branded type + Result]

// => POOR ENCAPSULATION: raw object with parallel fields that can drift out of sync
type PoorTemperature = { celsius: number; fahrenheit: number };
// => two parallel fields — if celsius changes, fahrenheit must be updated manually

const poor: PoorTemperature = { celsius: 100.0, fahrenheit: 212.0 };
// => initially consistent
const poorDrifted = { ...poor, celsius: 50.0 };
// => celsius changed but fahrenheit is now stale!
console.log(poorDrifted.fahrenheit.toFixed(1));
// => Output: 212.0 (wrong! should be 122.0)

// ============================================================

// => ENCAPSULATED: opaque branded type + smart constructor with Result
type TemperatureT = Readonly<{ celsius: number; _brand: "Temperature" }>;
type Result2<T, E> = { ok: true; value: T } | { ok: false; error: E };

const createTemperature = (celsius: number): Result2<TemperatureT, string> => {
  if (celsius < -273.15)
    return { ok: false, error: `Temperature below absolute zero: \${celsius.toFixed(2)}` };
    // => physics constraint enforced at construction time
  return { ok: true, value: { celsius, _brand: "Temperature" } };
  // => Ok wraps valid temperature — callers must handle Result
};

const getCelsius = (t: TemperatureT): number => t.celsius;
// => read-only accessor

const getFahrenheit = (t: TemperatureT): number => t.celsius * 9.0 / 5.0 + 32.0;
// => always computed from celsius — never stale
// => celsius=100 -> fahrenheit=212.0

const getKelvin = (t: TemperatureT): number => t.celsius + 273.15;
// => always derived from single source of truth

const r1 = createTemperature(100.0);
if (r1.ok) {
  console.log(`Celsius:    \${getCelsius(r1.value).toFixed(1)}`);
  // => Output: Celsius:    100.0
  console.log(`Fahrenheit: \${getFahrenheit(r1.value).toFixed(1)}`);
  // => Output: Fahrenheit: 212.0
  console.log(`Kelvin:     \${getKelvin(r1.value).toFixed(2)}`);
  // => Output: Kelvin:     373.15
}

const r2 = createTemperature(-300.0);
if (!r2.ok) console.log(`Error: \${r2.error}`);
// => Output: Error: Temperature below absolute zero: -300.00""",
    # Block 26: Example 19 - Composition over inheritance
    """\
// [F#: record of behavior functions — TypeScript uses readonly objects with function fields]

// => BEHAVIOR FUNCTIONS — small, focused, reusable
const canFly   = (): string => "Flapping wings";
// => standard flying behavior — any bird that flies uses this

const cannotFly = (): string => "Cannot fly";
// => used by non-flying birds — honest about capability

const canSwim  = (): string => "Swimming";
// => used by aquatic birds — any bird that swims uses this

// => BIRD OBJECTS — compose only the behaviors each bird actually has
type Eagle   = Readonly<{ fly: () => string }>;
// => eagles can only fly — no swim field

type Duck    = Readonly<{ fly: () => string; swim: () => string }>;
// => ducks can fly AND swim — object composition

type Penguin = Readonly<{ fly: () => string; swim: () => string }>;
// => penguins have both fields — but fly uses cannotFly behavior

// => Construct each bird by assembling the appropriate behaviors
const eagle:   Eagle   = { fly: canFly };
// => eagle.fly = canFly — flying is the only capability

const duck:    Duck    = { fly: canFly,    swim: canSwim };
// => duck.fly = canFly, duck.swim = canSwim — both capabilities

const penguin: Penguin = { fly: cannotFly, swim: canSwim };
// => penguin.fly = cannotFly — cannot fly; uses honest behavior

console.log(eagle.fly());    // => Output: Flapping wings
console.log(duck.fly());     // => Output: Flapping wings
console.log(duck.swim());    // => Output: Swimming
console.log(penguin.fly());  // => Output: Cannot fly (no exception thrown — contract honored)
console.log(penguin.swim()); // => Output: Swimming""",
]


# For intermediate and advanced files, generate from patterns
def generate_ts_for_intermediate_and_advanced(
    fsharp_code: str, block_idx: int, file_name: str
) -> str:
    """Generate a TypeScript FP block from F# code by structural translation."""
    lines = fsharp_code.split("\n")
    ts_lines = []

    # Add a comment referencing the F# version
    ts_lines.append(
        f"// [F# → TypeScript] FP-flavored TypeScript using tagged unions, readonly types, and pure functions"
    )

    # We'll do a structural/semantic translation
    # This is a heuristic approach for the remaining blocks
    ts_lines.append(
        "// => See F# tab for canonical FP explanation; TypeScript mirrors the same structure"
    )
    ts_lines.append("")

    # Simple structural translation hints
    for line in lines:
        stripped = line.strip()
        if not stripped or stripped.startswith("//"):
            ts_lines.append(line)
            continue

        # Skip F#-specific syntax that doesn't translate directly
        ts_lines.append(line)

    return "\n".join(ts_lines[:5])  # Just return a placeholder


if __name__ == "__main__":
    print(f"Total beginner blocks defined: {len(BEGINNER_TS_BLOCKS)}")
