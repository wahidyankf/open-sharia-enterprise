---
title: "Initial Setup"
date: 2025-01-29T00:00:00+07:00
draft: false
weight: 10000000
description: "Get React with TypeScript installed and running - installation, project setup, and your first working component"
tags: ["react", "typescript", "installation", "setup", "beginner", "vite"]
---

**Want to start building with React?** This initial setup guide gets React with TypeScript installed and working on your system in minutes. By the end, you'll have a React development environment configured and will create your first interactive component.

This tutorial provides 0-5% coverage - just enough to get React working on your machine. For deeper learning, continue to [Quick Start](/en/learn/software-engineering/platform-web/tools/fe-react/quick-start) (5-30% coverage).

## Prerequisites

Before installing React, you need:

- A computer running Windows, macOS, or Linux
- Administrator/sudo access for installation
- A terminal/command prompt
- A code editor (VS Code recommended, but IntelliJ IDEA, WebStorm, Sublime Text work too)
- Basic command-line navigation skills
- Node.js 18+ installed (we'll verify this)
- npm or pnpm package manager
- Basic understanding of HTML, JavaScript, and CSS

No prior React or frontend framework experience required - this guide starts from zero.

## Learning Objectives

By the end of this tutorial, you will be able to:

1. **Verify** Node.js and npm installation
2. **Choose** the right React setup method for your needs
3. **Create** a new React application with TypeScript
4. **Understand** the generated project structure
5. **Write** your first React component
6. **Run** the development server
7. **Build** production-ready React applications

## Verify Node.js Installation

React requires Node.js 18 or later.

```bash
node --version
```

Expected output:

```
v18.17.0
```

**Required**: Node.js 18.0.0 or later

Verify npm (Node Package Manager):

```bash
npm --version
```

Expected output:

```
9.6.7
```

**If Node.js is not installed**, follow the platform-specific instructions below.

## Platform-Specific Node.js Installation

### Windows Node.js Installation

**Method 1: Official Installer (Recommended)**

**Step 1: Download Node.js Installer**

1. Visit [https://nodejs.org/](https://nodejs.org/)
2. Download "LTS" version (Long Term Support)
3. Select Windows Installer (.msi) for your architecture (64-bit recommended)

**Step 2: Run Installer**

1. Double-click downloaded `.msi` file
2. Follow installation wizard:
   - Click Next through license agreement
   - Keep default installation directory
   - Select "Automatically install necessary tools" checkbox
   - Click Install
   - Click Finish

**Step 3: Verify Installation**

Open Command Prompt or PowerShell:

```powershell
node --version
npm --version
```

**Method 2: Package Manager (Chocolatey)**

```powershell
choco install nodejs-lts
```

**Troubleshooting Windows**:

- If `node` not found after installation, restart Command Prompt/PowerShell
- Add `C:\Program Files\nodejs` to PATH if necessary
- Run PowerShell as Administrator if permission errors occur

### macOS Node.js Installation

**Method 1: Homebrew (Recommended)**

```bash
brew install node@18
```

Link the installed version:

```bash
brew link node@18
```

**Method 2: Official Installer**

1. Visit [https://nodejs.org/](https://nodejs.org/)
2. Download macOS Installer (.pkg) LTS version
3. Run installer and follow prompts

**Method 3: Node Version Manager (nvm)**

```bash
# Install nvm
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.5/install.sh | bash

# Restart terminal or source profile
source ~/.zshrc  # or ~/.bash_profile

# Install Node.js 18
nvm install 18
nvm use 18
nvm alias default 18
```

**Verify Installation**:

```bash
node --version
npm --version
```

**Troubleshooting macOS**:

- If `node` not found, ensure Homebrew's bin directory in PATH
- For Apple Silicon (M1/M2), use Rosetta if compatibility issues arise
- Restart terminal after installation

### Linux Node.js Installation

**Ubuntu/Debian**

```bash
# Add NodeSource repository for Node.js 18
curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -

# Install Node.js
sudo apt update
sudo apt install nodejs
```

**Fedora/RHEL/CentOS**

```bash
# Add NodeSource repository
curl -fsSL https://rpm.nodesource.com/setup_18.x | sudo bash -

# Install Node.js
sudo dnf install nodejs
```

**Arch Linux**

```bash
sudo pacman -S nodejs npm
```

**Using Node Version Manager (nvm) - All Distributions**

```bash
# Install nvm
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.5/install.sh | bash

# Reload shell configuration
source ~/.bashrc  # or ~/.zshrc

# Install Node.js 18
nvm install 18
nvm use 18
nvm alias default 18
```

**Verify Installation**:

```bash
node --version
npm --version
```

**Troubleshooting Linux**:

- If permission errors occur, avoid using `sudo` with npm - use nvm instead
- Ensure `~/.bashrc` or `~/.zshrc` sources nvm configuration
- For build-essential dependencies: `sudo apt install build-essential` (Ubuntu/Debian)

## React Installation Methods

React offers multiple setup approaches. Choose based on your needs:

### Option 1: Vite (Recommended for Learning)

**Best for**: Modern development, fast builds, learning React

**Advantages**:

- Lightning-fast Hot Module Replacement (HMR)
- Minimal configuration needed
- Native ESM support
- TypeScript out-of-the-box
- Small bundle sizes

**Use when**: Starting a new React project, building SPAs (Single Page Applications)

### Option 2: Next.js (Production-Ready Framework)

**Best for**: Production applications, SEO-critical sites, full-stack apps

**Advantages**:

- Server-Side Rendering (SSR)
- Static Site Generation (SSG)
- API routes built-in
- File-based routing
- Optimized for production

**Use when**: Building production apps, needing SEO, requiring server-side features

### Option 3: Create React App (Deprecated)

**Status**: No longer recommended by React team

**Why avoid**:

- Slower build times than Vite
- Heavyweight configuration
- Less actively maintained
- Superseded by Vite and Next.js

**Do NOT use for new projects**

## Create Your First React App with Vite

We'll use Vite with TypeScript template - the recommended modern approach.

### Generate New Project

```bash
npm create vite@latest
```

You'll see interactive prompts:

```
√ Project name: ... my-react-app
√ Select a framework: » React
√ Select a variant: » TypeScript
```

**Configuration choices**:

- **Project name**: `my-react-app` (use lowercase with hyphens)
- **Framework**: Select `React`
- **Variant**: Select `TypeScript` (NOT `TypeScript + SWC` for beginners)

**Output**:

```
Scaffolding project in /path/to/my-react-app...

Done. Now run:

  cd my-react-app
  npm install
  npm run dev
```

### Install Dependencies

```bash
cd my-react-app
npm install
```

Vite downloads React dependencies:

- `react` - Core React library
- `react-dom` - React rendering for web browsers
- `typescript` - TypeScript compiler
- `vite` - Build tool and dev server
- `@vitejs/plugin-react` - Vite React plugin

This takes 30 seconds to 2 minutes depending on internet speed.

**Output summary**:

```
added 42 packages, and audited 43 packages in 15s

7 packages are looking for funding
  run `npm fund` for details

found 0 vulnerabilities
```

## Project Structure Walkthrough

Vite creates this structure:

```
my-react-app/
├── node_modules/         # Installed dependencies (don't edit)
├── public/               # Static assets (served as-is)
│   └── vite.svg         # Example static file
├── src/                  # Source code (your work goes here)
│   ├── assets/          # Images, fonts, etc.
│   │   └── react.svg
│   ├── App.css          # App component styles
│   ├── App.tsx          # Main App component
│   ├── index.css        # Global styles
│   ├── main.tsx         # Application entry point
│   └── vite-env.d.ts    # TypeScript Vite types
├── .gitignore           # Git ignore rules
├── index.html           # HTML entry point
├── package.json         # Project metadata and scripts
├── package-lock.json    # Dependency lock file
├── tsconfig.json        # TypeScript configuration
├── tsconfig.node.json   # TypeScript config for Node.js
└── vite.config.ts       # Vite configuration
```

### Key Files Explained

**index.html** - HTML entry point (unique to Vite):

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <link rel="icon" type="image/svg+xml" href="/vite.svg" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Vite + React + TS</title>
  </head>
  <body>
    <div id="root"></div>
    <script type="module" src="/src/main.tsx"></script>
  </body>
</html>
```

**src/main.tsx** - Application entry point:

```typescript
import React from 'react'
import ReactDOM from 'react-dom/client'
import App from './App.tsx'
import './index.css'

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
)
```

**src/App.tsx** - Main application component:

```typescript
import { useState } from 'react'
import reactLogo from './assets/react.svg'
import viteLogo from '/vite.svg'
import './App.css'

function App() {
  const [count, setCount] = useState(0)

  return (
    <>
      <div>
        <a href="https://vite.dev" target="_blank">
          <img src={viteLogo} className="logo" alt="Vite logo" />
        </a>
        <a href="https://react.dev" target="_blank">
          <img src={reactLogo} className="logo react" alt="React logo" />
        </a>
      </div>
      <h1>Vite + React</h1>
      <div className="card">
        <button onClick={() => setCount((count) => count + 1)}>
          count is {count}
        </button>
        <p>
          Edit <code>src/App.tsx</code> and save to test HMR
        </p>
      </div>
      <p className="read-the-docs">
        Click on the Vite and React logos to learn more
      </p>
    </>
  )
}

export default App
```

**package.json** - Project configuration:

```json
{
  "name": "my-react-app",
  "private": true,
  "version": "0.0.0",
  "type": "module",
  "scripts": {
    "dev": "vite",
    "build": "tsc && vite build",
    "preview": "vite preview"
  },
  "dependencies": {
    "react": "^18.2.0",
    "react-dom": "^18.2.0"
  },
  "devDependencies": {
    "@types/react": "^18.2.43",
    "@types/react-dom": "^18.2.17",
    "@vitejs/plugin-react": "^4.2.1",
    "typescript": "^5.2.2",
    "vite": "^5.0.8"
  }
}
```

## Your First Component - Hello World

Let's modify `App.tsx` to create a simple Hello World component.

### Replace App.tsx Content

Open `src/App.tsx` in your editor and replace everything with:

```typescript
import { useState } from 'react'
import './App.css'

function App() {
  const [name, setName] = useState('World')

  return (
    <div className="App">
      <h1>Hello, {name}!</h1>
      <input
        type="text"
        value={name}
        onChange={(e) => setName(e.target.value)}
        placeholder="Enter your name"
      />
      <p>Type in the input to see the greeting change!</p>
    </div>
  )
}

export default App
```

**What this component does**:

- **State management**: Uses `useState` hook to track `name` value
- **Reactive rendering**: Displays "Hello, {name}!" that updates automatically
- **User input**: Text input controlled by React state
- **Event handling**: `onChange` updates state when user types

### Understanding the Code

```typescript
import { useState } from 'react'  // Import React hook for state
import './App.css'                // Import component styles

function App() {                  // Functional component definition
  const [name, setName] = useState('World')
  // name: current state value (starts as 'World')
  // setName: function to update state

  return (                        // JSX return statement
    <div className="App">         {/* Root element */}
      <h1>Hello, {name}!</h1>     {/* Display dynamic value */}
      <input
        type="text"
        value={name}              {/* Controlled input */}
        onChange={(e) => setName(e.target.value)}  {/* Update on change */}
        placeholder="Enter your name"
      />
      <p>Type in the input to see the greeting change!</p>
    </div>
  )
}

export default App                // Export for use in main.tsx
```

**Key React Concepts**:

- **Components**: JavaScript functions that return JSX
- **JSX**: HTML-like syntax in JavaScript
- **State**: Data that changes over time
- **Hooks**: Functions that let you use React features (`useState`, etc.)
- **Props**: Data passed from parent to child components (not shown here)

## Run Development Server

### Start the Server

```bash
npm run dev
```

Output:

```
  VITE v5.0.8  ready in 523 ms

  ➜  Local:   http://localhost:5173/
  ➜  Network: use --host to expose
  ➜  press h to show help
```

**Server is ready at**: `http://localhost:5173/`

**Note**: Port may differ if 5173 is occupied (Vite automatically finds an available port)

### Access Your Application

Open browser and visit:

```
http://localhost:5173/
```

You should see:

- Heading: "Hello, World!"
- Text input with placeholder "Enter your name"
- Description text below

**Try it out**:

1. Type your name in the input field
2. Watch the heading update in real-time
3. Notice no page reload - this is React's reactive rendering!

**Congratulations!** Your first React component is working.

### Hot Module Replacement (HMR)

Vite provides instant updates when you edit code.

**Test HMR**:

1. Keep browser and terminal open
2. Edit `src/App.tsx` - change "Hello" to "Welcome"
3. Save file
4. Browser updates **instantly** without full reload

This is HMR - updating only changed modules without losing state.

### Stop the Server

Press `Ctrl+C` in terminal to stop development server.

## Understanding the Build Process

React apps go through these stages:

### Development Mode

```bash
npm run dev
```

**What happens**:

1. Vite starts development server
2. Serves source files with fast HMR
3. TypeScript type-checking runs separately
4. Source maps enabled for debugging
5. No minification (human-readable code)

**Use for**: Active development, debugging

### Production Build

```bash
npm run build
```

**What happens**:

1. TypeScript compiles and checks types (`tsc`)
2. Vite bundles all code
3. Minifies JavaScript and CSS
4. Optimizes assets (images, fonts)
5. Generates static files in `dist/` folder
6. Creates optimized chunks for code splitting

**Output**:

```
vite v5.0.8 building for production...
✓ 34 modules transformed.
dist/index.html                   0.46 kB │ gzip:  0.30 kB
dist/assets/react-35ef61ed.svg    4.13 kB │ gzip:  2.14 kB
dist/assets/index-d526a0c5.css    1.42 kB │ gzip:  0.74 kB
dist/assets/index-b9e74e72.js   143.42 kB │ gzip: 46.09 kB
✓ built in 857ms
```

### Preview Production Build

```bash
npm run preview
```

Serves production build locally for testing:

```
  ➜  Local:   http://localhost:4173/
  ➜  Network: use --host to expose
```

**Use for**: Testing production build before deployment

## TypeScript Configuration Basics

Vite generates TypeScript configuration automatically.

### tsconfig.json Overview

```json
{
  "compilerOptions": {
    "target": "ES2020", // JavaScript version to compile to
    "useDefineForClassFields": true,
    "lib": ["ES2020", "DOM", "DOM.Iterable"], // Available APIs
    "module": "ESNext", // Module system
    "skipLibCheck": true, // Speed up compilation

    /* Bundler mode */
    "moduleResolution": "bundler",
    "allowImportingTsExtensions": true,
    "resolveJsonModule": true,
    "isolatedModules": true,
    "noEmit": true, // Don't emit JS (Vite handles this)
    "jsx": "react-jsx", // JSX transformation

    /* Linting */
    "strict": true, // Enable all strict checks
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "noFallthroughCasesInSwitch": true
  },
  "include": ["src"],
  "references": [{ "path": "./tsconfig.node.json" }]
}
```

**Key settings**:

- **strict**: Enables strict type checking (recommended)
- **jsx**: Configures JSX transformation (react-jsx uses new transform)
- **noEmit**: Vite handles compilation, TypeScript only checks types
- **lib**: Includes DOM types for browser APIs

**Don't edit unless you know what you're doing** - defaults work well.

### Type Checking

TypeScript checks types during build:

```bash
npm run build
```

If type errors exist, build fails with detailed messages.

**Manual type check** (without building):

```bash
npx tsc --noEmit
```

Shows type errors without emitting files.

## Add Your Second Component

Let's create a reusable component to understand component composition.

### Create Greeting Component

Create new file `src/Greeting.tsx`:

```typescript
interface GreetingProps {
  name: string
  isExcited?: boolean
}

function Greeting({ name, isExcited = false }: GreetingProps) {
  const punctuation = isExcited ? '!' : '.'

  return (
    <div>
      <h2>Welcome, {name}{punctuation}</h2>
      <p>This is a reusable Greeting component.</p>
    </div>
  )
}

export default Greeting
```

**TypeScript features**:

- **Interface**: Defines prop types (`GreetingProps`)
- **Required prop**: `name: string` must be provided
- **Optional prop**: `isExcited?: boolean` has default value
- **Type safety**: TypeScript ensures correct usage

### Use Greeting Component

Update `src/App.tsx` to use the new component:

```typescript
import { useState } from 'react'
import Greeting from './Greeting'
import './App.css'

function App() {
  const [name, setName] = useState('World')

  return (
    <div className="App">
      <h1>React TypeScript Demo</h1>

      <Greeting name={name} isExcited={true} />

      <input
        type="text"
        value={name}
        onChange={(e) => setName(e.target.value)}
        placeholder="Enter your name"
      />
    </div>
  )
}

export default App
```

**Save and observe**:

- Browser updates instantly via HMR
- Greeting component displays with exclamation mark
- Typing in input updates greeting
- TypeScript ensures you pass correct props

**Try this**: Remove `name={name}` from Greeting - TypeScript shows error immediately.

## Verify Your Setup Works

Let's confirm everything is functioning correctly.

### Test 1: Node.js and npm Installed

```bash
node --version
npm --version
```

Should show Node.js 18+ and npm 9+.

### Test 2: Create Project

```bash
npm create vite@latest test-react-app -- --template react-ts
cd test-react-app
npm install
```

Should create project and install dependencies without errors.

### Test 3: Start Development Server

```bash
npm run dev
```

Should start server and show local URL.

### Test 4: Access Application

Open browser to displayed URL - should see React + Vite welcome page.

### Test 5: Production Build

```bash
npm run build
```

Should compile TypeScript and create `dist/` folder with optimized files.

### Test 6: Preview Production

```bash
npm run preview
```

Should serve production build and show preview URL.

**All tests passed?** Your React TypeScript setup is complete!

## Summary

**What you've accomplished**:

- Verified Node.js 18+ and npm installation
- Understood React setup method options (Vite recommended)
- Created React application with TypeScript using Vite
- Explored project structure and key files
- Wrote your first React component with state management
- Started development server with Hot Module Replacement
- Created reusable component with TypeScript props
- Built production-ready application bundle
- Previewed production build locally

**Key commands learned**:

- `npm create vite@latest` - Create new Vite project
- `npm install` - Install dependencies
- `npm run dev` - Start development server
- `npm run build` - Build for production
- `npm run preview` - Preview production build
- `npx tsc --noEmit` - Type-check without building

**Skills gained**:

- React project setup with modern tooling
- Component creation with TypeScript
- State management with useState hook
- JSX syntax and reactive rendering
- Development workflow with HMR
- Production build optimization
- TypeScript configuration basics

## Next Steps

**Ready to learn React fundamentals?**

- [Quick Start](/en/learn/software-engineering/platform-web/tools/fe-react/quick-start) (5-30% coverage) - Touch all core React concepts in a fast-paced tour

**Want comprehensive React mastery?**

- [Beginner Tutorial](/en/learn/software-engineering/platform-web/tools/fe-react/by-example/beginner) (0-60% coverage) - Deep dive into React with extensive practice

**Prefer code-first learning?**

- [By-Example Tutorial](/en/learn/software-engineering/platform-web/tools/fe-react/by-example) - Learn through heavily annotated examples

**Want to understand React's design philosophy?**

- [Overview](/en/learn/software-engineering/platform-web/tools/fe-react/overview) - Why React exists and when to use it

## Troubleshooting Common Issues

### "npm: command not found"

**Problem**: Node.js/npm not installed or not in PATH.

**Solution**:

- Install Node.js following platform-specific instructions above
- Ensure Node.js bin directory in PATH
- Restart terminal after installation
- Verify: `node --version` and `npm --version`

### "EACCES: permission denied" on Linux/macOS

**Problem**: npm trying to install globally without permissions.

**Solution**:

- **DO NOT use sudo with npm** - creates permission issues
- Use nvm (Node Version Manager) instead:

```bash
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.5/install.sh | bash
source ~/.bashrc  # or ~/.zshrc
nvm install 18
nvm use 18
```

- Or configure npm to use user directory:

```bash
mkdir ~/.npm-global
npm config set prefix '~/.npm-global'
echo 'export PATH=~/.npm-global/bin:$PATH' >> ~/.bashrc
source ~/.bashrc
```

### "Port 5173 is already in use"

**Problem**: Another process using port 5173.

**Solution**:

- Vite automatically finds next available port
- Or manually specify port in `vite.config.ts`:

```typescript
export default defineConfig({
  plugins: [react()],
  server: {
    port: 3000,
  },
});
```

### TypeScript errors after project creation

**Problem**: Type declaration issues or outdated types.

**Solution**:

- Update TypeScript and type packages:

```bash
npm install -D typescript@latest @types/react@latest @types/react-dom@latest
```

- Delete `node_modules` and reinstall:

```bash
rm -rf node_modules package-lock.json
npm install
```

### "Cannot find module" errors

**Problem**: Import path or dependency issues.

**Solution**:

- Check import paths match file locations
- Ensure file extensions correct (`.tsx` for components, `.ts` for utilities)
- Verify dependencies installed: `npm install`
- Check `package.json` has required packages

### Development server starts but shows blank page

**Problem**: JavaScript errors preventing rendering.

**Solution**:

- Open browser DevTools (F12) and check Console tab
- Look for error messages with stack traces
- Common issues:
  - Syntax errors in JSX
  - Missing imports
  - Incorrect prop types
  - State initialization errors

### HMR not working

**Problem**: Changes not reflecting in browser.

**Solution**:

- Hard refresh browser: `Ctrl+Shift+R` (Windows/Linux) or `Cmd+Shift+R` (macOS)
- Restart development server: `Ctrl+C` then `npm run dev`
- Clear browser cache
- Check browser console for HMR connection errors

### Build fails with "Out of memory"

**Problem**: Large project or insufficient memory.

**Solution**:

- Increase Node.js memory limit:

```bash
export NODE_OPTIONS="--max-old-space-size=4096"
npm run build
```

- Or in `package.json`:

```json
{
  "scripts": {
    "build": "NODE_OPTIONS='--max-old-space-size=4096' tsc && vite build"
  }
}
```

### "Cannot access 'X' before initialization"

**Problem**: Circular dependencies or hoisting issues.

**Solution**:

- Reorganize imports to avoid circular dependencies
- Move component definitions before usage
- Use dynamic imports for code splitting if needed

## Further Resources

**Official React Documentation**:

- [React Docs](https://react.dev/) - Official React documentation (new)
- [React Tutorial](https://react.dev/learn) - Interactive learning path
- [React API Reference](https://react.dev/reference/react) - Comprehensive API docs

**TypeScript and React**:

- [TypeScript Handbook](https://www.typescriptlang.org/docs/) - TypeScript fundamentals
- [React TypeScript Cheatsheet](https://react-typescript-cheatsheet.netlify.app/) - Common patterns

**Vite**:

- [Vite Documentation](https://vitejs.dev/) - Build tool documentation
- [Vite Guide](https://vitejs.dev/guide/) - Getting started and concepts

**Interactive Learning**:

- [React DevTools](https://react.dev/learn/react-developer-tools) - Browser extension for debugging
- [CodeSandbox](https://codesandbox.io/s/react-new) - Online React playground
- [StackBlitz](https://stackblitz.com/fork/vitejs-vite-react-ts) - Online Vite + React environment

**Community**:

- [Reactiflux Discord](https://www.reactiflux.com/) - Active React community chat
- [React Forum](https://github.com/facebook/react/discussions) - Official GitHub discussions
- [Stack Overflow React Tag](https://stackoverflow.com/questions/tagged/reactjs) - Q&A for specific issues

**Advanced Topics**:

- [React Hooks](https://react.dev/reference/react) - useState, useEffect, custom hooks
- [React Router](https://reactrouter.com/) - Client-side routing
- [State Management](https://react.dev/learn/managing-state) - Context, Redux, Zustand
- [Testing](https://react.dev/learn/testing) - Jest, React Testing Library, Vitest
