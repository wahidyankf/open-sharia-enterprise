---
title: WebAssembly Development
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 618
description: Practical guide to compiling Rust to WebAssembly for browser applications
tags:
  ["rust", "how-to", "webassembly", "wasm", "web", "javascript", "wasm-bindgen"]
---

**Need to run Rust in the browser?** This guide covers compiling Rust to WebAssembly, JavaScript interop with wasm-bindgen, and deploying WASM applications.

## Problem: Setting Up WASM Development

### Scenario

You want to compile Rust to WebAssembly.

### Solution: Install wasm-pack

```bash
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
```

**Create WASM project**:

```bash
cargo new --lib my_wasm_project
cd my_wasm_project
```

**Cargo.toml**:

```toml
[lib]
crate-type = ["cdylib"]

[dependencies]
wasm-bindgen = "0.2"
```

---

## Problem: Exposing Rust Functions to JavaScript

### Scenario

You want to call Rust functions from JavaScript.

### Solution: Use wasm-bindgen

**src/lib.rs**:

```rust
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

#[wasm_bindgen]
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

**Build**:

```bash
wasm-pack build --target web
```

**index.html**:

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>Rust WASM</title>
  </head>
  <body>
    <script type="module">
      import init, { greet, add } from "./pkg/my_wasm_project.js";

      async function run() {
        await init();

        console.log(greet("World")); // "Hello, World!"
        console.log(add(5, 3)); // 8
      }

      run();
    </script>
  </body>
</html>
```

---

## Problem: Working with JavaScript Types

### Scenario

You need to pass complex types between Rust and JavaScript.

### Solution: Use JsValue and Type Conversions

```rust
use wasm_bindgen::prelude::*;
use serde::{Serialize, Deserialize};
use wasm_bindgen::JsValue;

#[derive(Serialize, Deserialize)]
pub struct User {
    name: String,
    age: u32,
}

#[wasm_bindgen]
pub fn create_user(name: String, age: u32) -> JsValue {
    let user = User { name, age };
    serde_wasm_bindgen::to_value(&user).unwrap()
}

#[wasm_bindgen]
pub fn process_user(js_user: JsValue) -> String {
    let user: User = serde_wasm_bindgen::from_value(js_user).unwrap();
    format!("{} is {} years old", user.name, user.age)
}
```

**Add serde dependency**:

```toml
[dependencies]
wasm-bindgen = "0.2"
serde = { version = "1.0", features = ["derive"] }
serde-wasm-bindgen = "0.6"
```

**JavaScript**:

```javascript
const user = create_user("Alice", 30);
console.log(user); // { name: "Alice", age: 30 }

const message = process_user(user);
console.log(message); // "Alice is 30 years old"
```

---

## Problem: Calling JavaScript from Rust

### Scenario

Your WASM code needs to call JavaScript APIs.

### Solution: Import JavaScript Functions

```rust
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);

    #[wasm_bindgen(js_namespace = Math)]
    fn random() -> f64;

    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn log_message() {
    log("Hello from Rust!");
}

#[wasm_bindgen]
pub fn random_number() -> f64 {
    random()
}

#[wasm_bindgen]
pub fn show_alert() {
    alert("Alert from Rust!");
}
```

---

## Problem: DOM Manipulation

### Scenario

You need to interact with the DOM.

### Solution: Use web-sys

```toml
[dependencies]
wasm-bindgen = "0.2"
web-sys = { version = "0.3", features = ["Document", "Element", "HtmlElement", "Window"] }
```

```rust
use wasm_bindgen::prelude::*;
use web_sys::{Document, Element, Window};

#[wasm_bindgen]
pub fn create_element() {
    let window = web_sys::window().unwrap();
    let document = window.document().unwrap();

    let element = document.create_element("div").unwrap();
    element.set_inner_html("Hello from Rust!");
    element.set_class_name("rust-element");

    let body = document.body().unwrap();
    body.append_child(&element).unwrap();
}
```

---

## Problem: Event Handling

### Scenario

You need to handle DOM events.

### Solution: Use Closures with wasm-bindgen

```rust
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{Document, HtmlElement};

#[wasm_bindgen]
pub fn setup_button() {
    let window = web_sys::window().unwrap();
    let document = window.document().unwrap();

    let button = document
        .create_element("button")
        .unwrap()
        .dyn_into::<HtmlElement>()
        .unwrap();

    button.set_inner_html("Click me!");

    let closure = Closure::wrap(Box::new(move || {
        web_sys::console::log_1(&"Button clicked!".into());
    }) as Box<dyn FnMut()>);

    button.set_onclick(Some(closure.as_ref().unchecked_ref()));

    closure.forget();  // Keep closure alive

    document.body().unwrap().append_child(&button).unwrap();
}
```

---

## Problem: Canvas Graphics

### Scenario

You want to draw on HTML5 canvas.

### Solution: Use web-sys Canvas API

```toml
web-sys = { version = "0.3", features = [
    "Document",
    "Element",
    "HtmlCanvasElement",
    "CanvasRenderingContext2d",
    "Window"
] }
```

```rust
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{CanvasRenderingContext2d, HtmlCanvasElement};

#[wasm_bindgen]
pub fn draw_circle() {
    let document = web_sys::window().unwrap().document().unwrap();

    let canvas = document
        .get_element_by_id("canvas")
        .unwrap()
        .dyn_into::<HtmlCanvasElement>()
        .unwrap();

    let context = canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into::<CanvasRenderingContext2d>()
        .unwrap();

    context.begin_path();
    context.arc(75.0, 75.0, 50.0, 0.0, std::f64::consts::PI * 2.0).unwrap();
    context.set_fill_style(&"blue".into());
    context.fill();
}
```

**HTML**:

```html
<canvas id="canvas" width="150" height="150"></canvas>
```

---

## Problem: Async Operations

### Scenario

You need to make async HTTP requests from WASM.

### Solution: Use wasm-bindgen-futures

```toml
[dependencies]
wasm-bindgen = "0.2"
wasm-bindgen-futures = "0.4"
web-sys = { version = "0.3", features = ["Request", "Response", "Window"] }
js-sys = "0.3"
```

```rust
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;
use web_sys::{Request, RequestInit, Response};

#[wasm_bindgen]
pub async fn fetch_data(url: String) -> Result<String, JsValue> {
    let mut opts = RequestInit::new();
    opts.method("GET");

    let request = Request::new_with_str_and_init(&url, &opts)?;

    let window = web_sys::window().unwrap();
    let resp_value = JsFuture::from(window.fetch_with_request(&request)).await?;
    let resp: Response = resp_value.dyn_into()?;

    let text = JsFuture::from(resp.text()?).await?;
    Ok(text.as_string().unwrap())
}
```

---

## Problem: Debugging WASM

### Scenario

You need to debug WASM code.

### Solution: Enable Debug Info and Use Console

**Cargo.toml**:

```toml
[profile.release]
debug = true  # Include debug info
```

**Use console_error_panic_hook**:

```toml
[dependencies]
console_error_panic_hook = "0.1"
```

```rust
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn main() {
    console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub fn might_panic() {
    panic!("This will show in console!");
}
```

**Browser DevTools**: Use source maps to debug Rust code in browser.

---

## Problem: Optimizing WASM Size

### Scenario

Your WASM binary is too large.

### Solution: Enable wasm-opt and LTO

**Cargo.toml**:

```toml
[profile.release]
opt-level = "z"  # Optimize for size
lto = true       # Link-time optimization
codegen-units = 1
strip = true     # Remove debug symbols
```

**Build with optimization**:

```bash
wasm-pack build --target web --release
wasm-opt -Oz -o output.wasm input.wasm  # Further optimize
```

---

## Problem: Using npm Packages

### Scenario

You want to use existing JavaScript libraries.

### Solution: Import via wasm-bindgen

```rust
use wasm_bindgen::prelude::*;

#[wasm_bindgen(module = "/node_modules/lodash/lodash.js")]
extern "C" {
    #[wasm_bindgen(js_name = upperCase)]
    fn upper_case(s: &str) -> String;
}

#[wasm_bindgen]
pub fn to_upper(s: &str) -> String {
    upper_case(s)
}
```

---

## Problem: Testing WASM Code

### Scenario

You need to test WASM functionality.

### Solution: Use wasm-bindgen-test

```toml
[dev-dependencies]
wasm-bindgen-test = "0.3"
```

**tests/wasm.rs**:

```rust
use wasm_bindgen_test::*;

wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn test_add() {
    assert_eq!(add(2, 3), 5);
}
```

**Run tests**:

```bash
wasm-pack test --headless --firefox
```

---

## Common Pitfalls

### Pitfall 1: Forgetting to Initialize

**Problem**: Calling WASM before initialization.

```javascript
// Bad
import { greet } from "./pkg/my_wasm.js";
greet("World"); // Error!
```

**Solution**: Wait for init().

```javascript
// Good
import init, { greet } from "./pkg/my_wasm.js";
await init();
greet("World");
```

### Pitfall 2: Memory Leaks with Closures

**Problem**: Not managing closure lifetime.

**Solution**: Use forget() carefully or drop explicitly.

### Pitfall 3: Blocking Operations

**Problem**: Long synchronous operations freeze browser.

**Solution**: Use async/await or Web Workers.

---

## Related Resources

- [Tutorials: Advanced](/en/learn/swe/prog-lang/rust/tutorials/advanced) - WebAssembly basics
- [Cookbook](/en/learn/swe/prog-lang/rust/how-to/cookbook) - WASM recipes
- [Resources](/en/learn/swe/prog-lang/rust/reference/resources) - WASM tools and books
- [Async/Await](/en/learn/swe/prog-lang/rust/how-to/async-await-patterns) - Async in WASM

---

**Build high-performance web applications with Rust and WebAssembly!**
