---
title: "Initial Setup"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100001
description: "Install Clojure and run your first program"
tags:
  - clojure
  - installation
  - setup
---

Get Clojure installed and running your first program. This guide walks you through installation on any operating system and gets you writing code immediately.

## 🎯 What You'll Accomplish

By the end of this tutorial, you'll have:

- ✅ Clojure and Leiningen installed and verified
- ✅ Your first Clojure program running
- ✅ A working REPL for interactive development

## 📋 Prerequisites

- Java Development Kit (JDK) 8 or later
- Basic familiarity with your computer's terminal/command line
- No prior Clojure experience required

## 💾 Step 1: Install Java

Clojure runs on the Java Virtual Machine (JVM), so you need Java installed first.

### Verify Java Installation

```bash
java -version
```

If Java is installed, you'll see version information. If not, install it:

### Install Java

**Windows/macOS/Linux**: Download from [Adoptium](https://adoptium.net/) (formerly AdoptOpenJDK):

1. Visit [adoptium.net](https://adoptium.net/)
2. Download Temurin JDK 17 or later (LTS version recommended)
3. Run the installer
4. Verify: `java -version`

**macOS (Homebrew)**:

```bash
brew install openjdk@17
```

**Linux (Ubuntu/Debian)**:

```bash
sudo apt update
sudo apt install openjdk-17-jdk
```

## 💾 Step 2: Install Clojure CLI Tools

### Windows

Using [Scoop](https://scoop.sh/):

```bash
scoop install clojure
```

Or download the installer from [clojure.org](https://clojure.org/guides/install_clojure#_windows).

### macOS

Using Homebrew:

```bash
brew install clojure/tools/clojure
```

### Linux

```bash
curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh
chmod +x linux-install.sh
sudo ./linux-install.sh
```

### Verify Installation

```bash
clojure --version
```

Expected output:

```
Clojure CLI version 1.11.x.x
```

## 💾 Step 3: Install Leiningen (Build Tool)

Leiningen is the most popular Clojure build tool for managing dependencies and projects.

### Windows

Download [lein.bat](https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein.bat) and place it in a directory on your PATH.

### macOS/Linux

```bash
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > ~/bin/lein
chmod +x ~/bin/lein
lein
```

The first run will download dependencies.

### Verify Installation

```bash
lein version
```

Expected output:

```
Leiningen 2.x.x on Java x.x.x
```

## 🚀 Step 4: Create Your First Clojure Program

### Using REPL (Interactive Shell)

```bash
clojure
```

You'll see the Clojure REPL prompt:

```clojure
user=>
```

Try some commands:

```clojure
user=> (println "Hello, Clojure!")
Hello, Clojure!
nil

user=> (+ 1 2 3)
6

user=> (def name "Alice")
#'user/name

user=> (str "Hello, " name "!")
"Hello, Alice!"

user=> (exit)
```

Type `(exit)` or press `Ctrl+D` to exit the REPL.

### Create a Project with Leiningen

```bash
lein new app hello-clojure
cd hello-clojure
```

This creates a new project structure:

```
hello-clojure/
├── project.clj       # Project configuration
├── src/
│   └── hello_clojure/
│       └── core.clj  # Main source file
└── test/
    └── hello_clojure/
        └── core_test.clj
```

### Edit the Main File

Open `src/hello_clojure/core.clj`:

```clojure
(ns hello-clojure.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, Clojure World!"))
```

### Run Your Program

```bash
lein run
```

Expected output:

```
Hello, Clojure World!
```

## ✅ Verification Checklist

Before moving forward, verify:

- [ ] `java -version` shows Java 8 or later
- [ ] `clojure --version` shows Clojure CLI installed
- [ ] `lein version` shows Leiningen installed
- [ ] REPL starts with `clojure` command
- [ ] `lein run` executes your hello-clojure project

## 🎉 You're Done

You've successfully installed Clojure and run your first program. You're ready for the next step.

## 📚 What's Next?

**Quick learner**: [Clojure Quick Start](/en/learn/software-engineering/programming-languages/clojure/tutorials/quick-start)

- Learn core syntax and basic patterns in one session
- Understand enough to explore Clojure independently

**Code-first learner**: [Clojure By Example](/en/learn/software-engineering/programming-languages/clojure/tutorials/by-example)

- 80 annotated, runnable examples covering 95% of production Clojure
- Best for experienced developers who prefer learning through code

- Progressive learning path from beginner to advanced
- Deep understanding of Clojure's philosophy and patterns

## 🆘 Troubleshooting

### Java Issues

**Problem**: "java: command not found"

**Solution**: Verify Java is installed and in your PATH. Restart terminal after installation.

### Clojure CLI Issues

**Problem**: "clojure: command not found"

**Solution**: Verify installation completed successfully. On macOS, ensure `/usr/local/bin` is in your PATH.

### Leiningen Issues

**Problem**: "lein: command not found"

**Solution**: Verify `lein` script is in a directory on your PATH. On macOS/Linux, ensure `~/bin` is in PATH.

**Problem**: "Could not find or load main class clojure.main"

**Solution**: Run `lein` by itself to download dependencies first.
