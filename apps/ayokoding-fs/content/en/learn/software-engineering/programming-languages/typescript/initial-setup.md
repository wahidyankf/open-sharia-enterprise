---
title: "Initial Setup"
date: 2026-02-07T00:00:00+07:00
draft: false
weight: 100001
description: "Get TypeScript installed and running on your system - Node.js installation, TypeScript compiler setup, and your first working program"
tags: ["typescript", "installation", "setup", "beginner", "nodejs"]
---

**Want to start programming in TypeScript?** This initial setup guide gets TypeScript installed and working on your system in minutes. By the end, you'll have the TypeScript compiler running and will execute your first type-safe program.

This tutorial provides 0-5% coverage - just enough to get TypeScript working on your machine. For deeper learning, continue to [Quick Start](/en/learn/software-engineering/programming-languages/typescript/quick-start) (5-30% coverage).

## Prerequisites

Before installing TypeScript, you need:

- A computer running Windows, macOS, or Linux
- Administrator/sudo access for installation
- A terminal/command prompt
- A text editor (VS Code recommended, or any editor)
- Basic command-line navigation skills

No prior TypeScript or JavaScript experience required - this guide starts from zero.

## Learning Objectives

By the end of this tutorial, you will be able to:

1. **Install** Node.js and npm on your operating system
2. **Install** the TypeScript compiler globally
3. **Create** a basic tsconfig.json configuration file
4. **Write** your first TypeScript program with type annotations
5. **Compile** TypeScript source code to JavaScript
6. **Execute** the compiled JavaScript using Node.js

## Understanding TypeScript: Language, Compiler, Runtime

Before installation, understand key TypeScript components:

- **TypeScript Language**: Superset of JavaScript with static typing - adds types to JavaScript syntax
- **TypeScript Compiler (tsc)**: Converts TypeScript (.ts) to JavaScript (.js) - performs type checking
- **Node.js Runtime**: Executes JavaScript (compiled TypeScript) - browsers also run compiled output

**For development, you need**: Node.js + npm (package manager) + TypeScript compiler

## Platform-Specific Installation

Choose your operating system and follow the installation steps.

### Windows Installation

**Step 1: Install Node.js and npm**

1. Visit [https://nodejs.org/](https://nodejs.org/)
2. Download **LTS version** (v20+ recommended - Long Term Support)
3. Run the `.msi` installer
4. Follow the installation wizard:
   - Accept license agreement
   - Keep default installation directory (`C:\Program Files\nodejs\`)
   - Keep all default features checked (npm, online documentation)
   - Check **Automatically install necessary tools** (installs build tools)
   - Click **Install** (may require administrator privileges)
5. Click **Finish** when complete

**Step 2: Verify Node.js and npm**

Open Command Prompt or PowerShell (restart if previously open):

```cmd
node --version
```

Expected output:

```
v20.X.X
```

Check npm (Node Package Manager):

```cmd
npm --version
```

Expected output:

```
10.X.X
```

**Step 3: Install TypeScript Compiler**

Install TypeScript globally using npm:

```cmd
npm install -g typescript
```

This installs the `tsc` (TypeScript Compiler) command globally.

**Step 4: Verify TypeScript Installation**

```cmd
tsc --version
```

Expected output:

```
Version 5.X.X
```

**Troubleshooting Windows**:

- If `tsc` command not found, restart terminal or computer to reload PATH
- Verify npm global bin directory is in PATH: `npm config get prefix` should be in system PATH
- Manually add to PATH: `C:\Users\<YourUsername>\AppData\Roaming\npm`

### macOS Installation

**Step 1: Install Node.js and npm**

**Option A: Using Official Installer**

1. Visit [https://nodejs.org/](https://nodejs.org/)
2. Download **LTS version** for macOS
3. Run the `.pkg` installer
4. Follow installation wizard (requires password for system-wide install)
5. Click **Close** when complete

**Option B: Using Homebrew (Recommended)**

```bash
brew install node
```

This installs both Node.js and npm.

**Step 2: Verify Installation**

Open Terminal:

```bash
node --version
npm --version
```

Expected output: Node v20+ and npm 10+

**Step 3: Install TypeScript Compiler**

```bash
npm install -g typescript
```

**Step 4: Verify TypeScript**

```bash
tsc --version
```

Expected output: Version 5.X.X

**Troubleshooting macOS**:

- Permission errors: Use `sudo npm install -g typescript` (not recommended) or configure npm prefix:

  ```bash
  mkdir ~/.npm-global
  npm config set prefix '~/.npm-global'
  echo 'export PATH=~/.npm-global/bin:$PATH' >> ~/.zshrc
  source ~/.zshrc
  npm install -g typescript
  ```

- Multiple Node versions: Use `nvm` (Node Version Manager) for version management

### Linux Installation

**Step 1: Install Node.js and npm**

**Ubuntu/Debian**:

```bash
curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
sudo apt-get install -y nodejs
```

**Fedora/RHEL/CentOS**:

```bash
curl -fsSL https://rpm.nodesource.com/setup_20.x | sudo bash -
sudo dnf install -y nodejs
```

**Arch Linux**:

```bash
sudo pacman -S nodejs npm
```

**Step 2: Verify Installation**

```bash
node --version
npm --version
```

**Step 3: Install TypeScript Compiler**

```bash
npm install -g typescript
```

If permission denied, use one of these approaches:

**Option A: Use sudo (simple but not ideal)**:

```bash
sudo npm install -g typescript
```

**Option B: Configure npm prefix (recommended)**:

```bash
mkdir ~/.npm-global
npm config set prefix '~/.npm-global'
echo 'export PATH=~/.npm-global/bin:$PATH' >> ~/.bashrc
source ~/.bashrc
npm install -g typescript
```

**Step 4: Verify TypeScript**

```bash
tsc --version
```

**Troubleshooting Linux**:

- Use `nvm` (Node Version Manager) for better Node.js version control
- Check `$PATH` if `tsc` not found: `echo $PATH | grep npm`

## Your First TypeScript Program

Let's write and compile your first TypeScript program with type annotations.

### Create a Project Directory

```bash
mkdir -p ~/typescript-projects/hello
cd ~/typescript-projects/hello
```

### Initialize TypeScript Configuration

Create `tsconfig.json` (TypeScript compiler configuration):

```bash
tsc --init
```

This generates `tsconfig.json` with sensible defaults. You can also create it manually:

```json
{
  "compilerOptions": {
    "target": "ES2020",
    "module": "commonjs",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "outDir": "./dist"
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules"]
}
```

**Key options explained**:

- `target`: JavaScript version to compile to (ES2020 is modern and widely supported)
- `module`: Module system (commonjs for Node.js, ESNext for modern environments)
- `strict`: Enable all strict type checking options (recommended)
- `outDir`: Output directory for compiled JavaScript files

### Write the Program

Create a `src` directory and your first TypeScript file:

```bash
mkdir src
```

Create `src/hello.ts`:

```typescript
// hello.ts - First TypeScript program with type annotations
function greet(name: string): string {
  return `Hello, ${name}!`;
}

const userName: string = "TypeScript";
const message: string = greet(userName);

console.log(message);
```

**Type annotations explained**:

- `name: string` - parameter type annotation (name must be string)
- `: string` after function - return type annotation (function returns string)
- `const userName: string` - variable type annotation (userName is string)

**CRITICAL**: Notice how types make intent explicit and catch errors at compile time.

### Compile the Program

TypeScript must be compiled to JavaScript before execution:

```bash
tsc
```

This reads `tsconfig.json` and compiles all TypeScript files in `src/` to `dist/`.

**Directory structure after compilation**:

```
~/typescript-projects/hello/
├── tsconfig.json
├── src/
│   └── hello.ts       # TypeScript source
└── dist/
    └── hello.js       # Compiled JavaScript
```

**Compiled output** (`dist/hello.js`):

```javascript
"use strict";
function greet(name) {
  return `Hello, ${name}!`;
}
const userName = "TypeScript";
const message = greet(userName);
console.log(message);
```

Notice: Type annotations are removed, clean JavaScript remains.

### Run the Program

Execute the compiled JavaScript with Node.js:

```bash
node dist/hello.js
```

**Output**:

```
Hello, TypeScript!
```

**What happened**:

1. `tsc` compiled `src/hello.ts` → `dist/hello.js` (with type checking)
2. TypeScript verified all types are correct
3. `node` executed the JavaScript output
4. Program printed the greeting

## Compile and Run in One Step

For convenience, use `ts-node` to run TypeScript directly without manual compilation:

**Install ts-node**:

```bash
npm install -g ts-node
```

**Run TypeScript directly**:

```bash
ts-node src/hello.ts
```

This compiles and executes in memory (no `dist/` files created).

**Alternative: Use npm scripts** in `package.json`:

```bash
npm init -y  # Create package.json
```

Add scripts to `package.json`:

```json
{
  "scripts": {
    "build": "tsc",
    "start": "node dist/hello.js",
    "dev": "ts-node src/hello.ts"
  }
}
```

Now run:

```bash
npm run build  # Compile TypeScript
npm start      # Run compiled JavaScript
npm run dev    # Run TypeScript directly
```

## More Detailed Example - Type Safety in Action

Let's see how TypeScript catches errors at compile time. Create `src/calculator.ts`:

```typescript
// calculator.ts - Demonstrates TypeScript type safety
interface CalculatorResult {
  value: number;
  operation: string;
}

function add(a: number, b: number): CalculatorResult {
  return {
    value: a + b,
    operation: "addition",
  };
}

function divide(a: number, b: number): CalculatorResult {
  if (b === 0) {
    throw new Error("Cannot divide by zero");
  }
  return {
    value: a / b,
    operation: "division",
  };
}

// Type-safe usage
const result1: CalculatorResult = add(10, 5);
console.log(`${result1.operation}: ${result1.value}`); // addition: 15

const result2: CalculatorResult = divide(10, 2);
console.log(`${result2.operation}: ${result2.value}`); // division: 5

// TypeScript catches errors at compile time
// Uncomment these lines to see type errors:

// const badResult = add("10", 5);  // Error: Argument of type 'string' is not assignable to parameter of type 'number'
// const badResult2 = add(10);      // Error: Expected 2 arguments, but got 1
// result1.value = "string";        // Error: Type 'string' is not assignable to type 'number'
```

Compile and run:

```bash
tsc
node dist/calculator.js
```

**Output**:

```
addition: 15
division: 5
```

**Try uncommenting the error lines and running `tsc`** - the compiler will catch these errors before runtime!

**Type safety benefits**:

- Catches type mismatches at compile time
- Prevents calling functions with wrong number of arguments
- Ensures object properties have correct types
- Provides autocomplete and inline documentation in editors

## Understanding TypeScript Compilation Flow

TypeScript uses a compile-time type checking model:

1. **Write**: Create `.ts` files with type annotations
2. **Compile**: `tsc` checks types and generates `.js` files
3. **Execute**: Node.js or browser runs JavaScript (no TypeScript runtime)

**Why this matters**:

- **Type Safety Without Runtime Cost**: Type checking happens at compile time, no runtime overhead
- **JavaScript Compatibility**: Compiled output is clean JavaScript (runs anywhere)
- **Gradual Adoption**: Mix TypeScript and JavaScript files in same project
- **Tooling**: IDEs leverage types for autocomplete, refactoring, error detection

**Contrast with JavaScript** (no compilation step):

- JavaScript: `.js` → interpreter executes directly (no type checking)
- TypeScript: `.ts` → compiler checks types → `.js` → runtime executes

## IDE Setup - VS Code (Recommended)

**Visual Studio Code** provides the best TypeScript development experience (built with TypeScript):

**Step 1: Install VS Code**

Download from [https://code.visualstudio.com/](https://code.visualstudio.com/)

**Step 2: Open Your Project**

```bash
code ~/typescript-projects/hello
```

**Step 3: TypeScript Support is Built-in**

VS Code includes TypeScript language support automatically. You get:

- Instant type checking (red squiggles for errors)
- Autocomplete for all available methods and properties
- Go to definition (Cmd/Ctrl + Click)
- Inline documentation on hover
- Safe refactoring (rename symbol across files)

**Step 4: Install Useful Extensions**

- **ESLint** - Linting for TypeScript
- **Prettier** - Code formatting
- **Error Lens** - Inline error messages

**Step 5: Configure Auto-Compile on Save**

Create `.vscode/tasks.json`:

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "type": "typescript",
      "tsconfig": "tsconfig.json",
      "problemMatcher": ["$tsc"],
      "group": {
        "kind": "build",
        "isDefault": true
      }
    }
  ]
}
```

Now TypeScript compiles automatically on save.

## Summary

**What you've accomplished**:

- Installed Node.js and npm on your operating system
- Installed TypeScript compiler globally
- Created tsconfig.json configuration
- Wrote and compiled your first TypeScript programs
- Executed compiled JavaScript using Node.js
- Understood TypeScript compilation flow
- Set up IDE with excellent TypeScript support

**Key commands learned**:

- `node --version` - Check Node.js version
- `npm --version` - Check npm version
- `npm install -g typescript` - Install TypeScript globally
- `tsc --version` - Check TypeScript version
- `tsc --init` - Initialize TypeScript configuration
- `tsc` - Compile TypeScript to JavaScript
- `node <file>.js` - Execute JavaScript
- `ts-node <file>.ts` - Compile and run TypeScript directly

**Skills gained**:

- TypeScript compiler installation and verification
- Basic tsconfig.json configuration
- Writing type-annotated TypeScript code
- Compiling TypeScript to JavaScript
- Understanding type safety benefits

## Next Steps

**Ready to learn TypeScript fundamentals?**

- [Quick Start](/en/learn/software-engineering/programming-languages/typescript/quick-start) (5-30% coverage) - Touch all core TypeScript concepts in a fast-paced tour

**Want comprehensive fundamentals?**

- [By-Example Tutorial](/en/learn/software-engineering/programming-languages/typescript/by-example) - Learn through heavily annotated examples

**Want to understand TypeScript's design philosophy?**

- [Overview](/en/learn/software-engineering/programming-languages/typescript/overview) - Why TypeScript exists and when to use it

## Troubleshooting Common Issues

### "tsc: command not found" or "tsc is not recognized"

**Problem**: Terminal doesn't recognize TypeScript compiler.

**Solution**:

- Verify installation: `npm list -g typescript`
- Check npm global bin directory: `npm config get prefix`
- Add npm global bin to PATH:
  - **Windows**: `C:\Users\<YourUsername>\AppData\Roaming\npm`
  - **macOS/Linux**: `~/.npm-global/bin` or `/usr/local/bin`
- Restart terminal after PATH changes

### Permission denied when installing TypeScript

**Problem**: npm install fails with EACCES error.

**Solution**:

- **Don't use sudo** (security risk and causes permission issues)
- Configure npm to use user directory:

  ```bash
  mkdir ~/.npm-global
  npm config set prefix '~/.npm-global'
  echo 'export PATH=~/.npm-global/bin:$PATH' >> ~/.bashrc
  source ~/.bashrc
  npm install -g typescript
  ```

### TypeScript compiler error: "Cannot find name 'console'"

**Problem**: Missing type definitions for Node.js.

**Solution**:

```bash
npm install --save-dev @types/node
```

Add to `tsconfig.json`:

```json
{
  "compilerOptions": {
    "types": ["node"]
  }
}
```

### Compiled JavaScript not running

**Problem**: `node dist/hello.js` fails or produces errors.

**Solution**:

- Verify compilation succeeded: Check for `dist/hello.js` file
- Check Node.js version: Ensure it supports target in tsconfig.json
- Verify no TypeScript compilation errors: Run `tsc` and check output
- Check module system: If using ES modules, add `"type": "module"` to package.json

### IDE not showing TypeScript errors

**Problem**: VS Code or other IDE doesn't show type errors.

**Solution**:

- Reload VS Code: Cmd/Ctrl + Shift + P → "Reload Window"
- Check TypeScript version in IDE: Bottom right corner in VS Code
- Verify `tsconfig.json` in project root
- Install TypeScript in project: `npm install --save-dev typescript`

## Further Resources

**Official TypeScript Documentation**:

- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html) - Complete TypeScript guide
- [TypeScript Playground](https://www.typescriptlang.org/play) - Online TypeScript editor and compiler
- [TypeScript Release Notes](https://www.typescriptlang.org/docs/handbook/release-notes/overview.html) - What's new in each version

**Development Tools**:

- [VS Code](https://code.visualstudio.com/) - Best TypeScript IDE (built with TypeScript)
- [ts-node](https://typestrong.org/ts-node/) - Execute TypeScript directly
- [tsc-watch](https://www.npmjs.com/package/tsc-watch) - Auto-compile on file changes

**Learning Resources**:

- [DefinitelyTyped](https://github.com/DefinitelyTyped/DefinitelyTyped) - Type definitions for JavaScript libraries
- [TypeScript Deep Dive](https://basarat.gitbook.io/typescript/) - Free online book
- [Type Challenges](https://github.com/type-challenges/type-challenges) - Practice type-level programming

**Community**:

- [TypeScript Discord](https://discord.gg/typescript) - Real-time chat community
- [Stack Overflow - TypeScript](https://stackoverflow.com/questions/tagged/typescript) - Q&A community
- [/r/typescript](https://www.reddit.com/r/typescript/) - Reddit community
