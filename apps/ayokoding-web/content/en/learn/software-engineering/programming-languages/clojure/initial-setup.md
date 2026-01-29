---
title: "Initial Setup"
date: 2025-01-29T00:00:00+07:00
draft: false
weight: 100001
description: "Get Clojure installed and running on your system - JDK installation, Leiningen setup, REPL access, and your first program"
tags: ["clojure", "installation", "setup", "jvm", "leiningen", "beginner"]
---

**Want to start programming in Clojure?** This initial setup guide gets Clojure installed and working on your system in minutes. By the end, you'll have Clojure running and will execute your first program interactively.

This tutorial provides 0-5% coverage - just enough to get Clojure working on your machine. For deeper learning, continue to [Quick Start](/en/learn/software-engineering/programming-languages/clojure/quick-start) (5-30% coverage).

## Prerequisites

Before installing Clojure, you need:

- A computer running Windows, macOS, or Linux
- Administrator/sudo access for installation
- A terminal/command prompt
- A text editor (VS Code, IntelliJ IDEA, Emacs, Vim, or any editor)
- Basic command-line navigation skills
- Java Development Kit (JDK) - we'll install this first

No prior Lisp or functional programming experience required - this guide starts from zero.

## Learning Objectives

By the end of this tutorial, you will be able to:

1. **Install** the Java Development Kit (JDK 11 or later)
2. **Install** Leiningen (Clojure build tool and project manager)
3. **Access** the Clojure REPL (interactive programming environment)
4. **Execute** Clojure code interactively in the REPL
5. **Create** and run your first Clojure project

## Java Development Kit Installation

Clojure runs on the JVM, so we need Java first.

### Verify Existing Java Installation

Check if Java is already installed:

```bash
java -version
```

If you see output like this, Java is installed:

```
openjdk version "17.0.9" 2023-10-17
```

**Required**: Java 11 or later (Java 17 or 21 recommended)

If Java is not installed or version is below 11, continue to installation steps.

### Windows Java Installation

**Step 1: Download JDK**

Visit [Adoptium](https://adoptium.net/) (formerly AdoptOpenJDK):

1. Go to [https://adoptium.net/temurin/releases/](https://adoptium.net/temurin/releases/)
2. Select **Java Version: 17** (LTS)
3. Select **Operating System: Windows**
4. Select **Architecture: x64** (or ARM if you have ARM Windows)
5. Download `.msi` installer

**Step 2: Install JDK**

1. Double-click downloaded `.msi` file
2. Follow installer wizard:
   - Click **Next** through introduction
   - Keep default installation path (`C:\Program Files\Eclipse Adoptium\...`)
   - **IMPORTANT**: Select "Set JAVA_HOME variable" option
   - **IMPORTANT**: Select "Add to PATH" option
   - Click **Install**
3. Click **Finish** when complete

**Step 3: Verify Installation**

Open new Command Prompt or PowerShell:

```cmd
java -version
```

Expected output:

```
openjdk version "17.0.9" 2023-10-17
```

Also verify JAVA_HOME:

```cmd
echo %JAVA_HOME%
```

Should output something like: `C:\Program Files\Eclipse Adoptium\jdk-17.0.9+9`

**Troubleshooting Windows**:

- If `java -version` fails, restart Command Prompt to load new PATH
- Verify `C:\Program Files\Eclipse Adoptium\jdk-...\bin` is in PATH
- If JAVA_HOME is missing, set it manually in System Environment Variables

### macOS Java Installation

**Step 1: Install via Homebrew (Recommended)**

If you have Homebrew installed:

```bash
brew install openjdk@17
```

Follow post-installation instructions to create symlink:

```bash
sudo ln -sfn /usr/local/opt/openjdk@17/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-17.jdk
```

**Alternative: Download from Adoptium**

1. Visit [https://adoptium.net/temurin/releases/](https://adoptium.net/temurin/releases/)
2. Select Java Version 17, macOS, architecture (x64 or AArch64 for M1/M2/M3)
3. Download `.pkg` file
4. Double-click to install
5. Follow installer prompts

**Step 2: Verify Installation**

```bash
java -version
```

Expected output:

```
openjdk version "17.0.9" 2023-10-17
```

**Step 3: Set JAVA_HOME (if needed)**

Add to `~/.zshrc` or `~/.bash_profile`:

```bash
export JAVA_HOME=$(/usr/libexec/java_home -v 17)
```

Reload shell:

```bash
source ~/.zshrc  # or source ~/.bash_profile
```

Verify:

```bash
echo $JAVA_HOME
```

**Troubleshooting macOS**:

- If multiple Java versions exist, use `/usr/libexec/java_home -V` to list them
- Select specific version: `export JAVA_HOME=$(/usr/libexec/java_home -v 17)`

### Linux Java Installation

**Step 1: Install via Package Manager**

**Ubuntu/Debian**:

```bash
sudo apt update
sudo apt install openjdk-17-jdk
```

**Fedora/RHEL/CentOS**:

```bash
sudo dnf install java-17-openjdk-devel
```

**Arch Linux**:

```bash
sudo pacman -S jdk-openjdk
```

**Step 2: Verify Installation**

```bash
java -version
```

Expected output:

```
openjdk version "17.0.9" 2023-10-17
```

**Step 3: Set JAVA_HOME**

Add to `~/.bashrc` or `~/.zshrc`:

```bash
export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64  # Ubuntu/Debian path
export JAVA_HOME=/usr/lib/jvm/java-17-openjdk  # Fedora/Arch path
```

Reload shell:

```bash
source ~/.bashrc  # or source ~/.zshrc
```

Verify:

```bash
echo $JAVA_HOME
```

**Troubleshooting Linux**:

- Find Java installation: `sudo update-alternatives --config java` (Ubuntu/Debian)
- Multiple versions: Use `update-alternatives` to select default
- JAVA_HOME path varies by distribution - check actual installation location

## Leiningen Installation

Leiningen is the standard Clojure build tool and project manager.

### Windows Leiningen Installation

**Step 1: Download Leiningen Script**

1. Create directory: `C:\leiningen`
2. Download `lein.bat` from [https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein.bat](https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein.bat)
3. Save to `C:\leiningen\lein.bat`

**Step 2: Add to PATH**

1. Open System Environment Variables:
   - Press Windows key, type "environment variables"
   - Click "Edit the system environment variables"
   - Click "Environment Variables" button
2. Under "System variables", find "Path", click "Edit"
3. Click "New", add `C:\leiningen`
4. Click OK on all dialogs

**Step 3: Install Leiningen**

Open new Command Prompt:

```cmd
lein version
```

First run downloads and installs Leiningen. This takes a few minutes. Expected output:

```
Leiningen 2.11.2 on Java 17.0.9 OpenJDK 64-Bit Server VM
```

**Troubleshooting Windows**:

- If `lein` not found, restart Command Prompt after adding PATH
- If download fails, check firewall/antivirus settings
- Ensure JAVA_HOME is set correctly (Leiningen needs it)

### macOS Leiningen Installation

**Step 1: Install via Homebrew**

```bash
brew install leiningen
```

Homebrew downloads and installs Leiningen automatically.

**Alternative: Manual Installation**

```bash
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -o ~/bin/lein

chmod +x ~/bin/lein

export PATH=$HOME/bin:$PATH

lein version
```

**Step 2: Verify Installation**

```bash
lein version
```

Expected output:

```
Leiningen 2.11.2 on Java 17.0.9 OpenJDK 64-Bit Server VM
```

**Troubleshooting macOS**:

- If `lein` not found, ensure `~/bin` or `/usr/local/bin` is in PATH
- Check executable permissions: `ls -l $(which lein)`
- First run may take time to download dependencies

### Linux Leiningen Installation

**Step 1: Download Leiningen Script**

```bash
mkdir -p ~/bin

curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -o ~/bin/lein

chmod +x ~/bin/lein
```

**Step 2: Add to PATH**

Add to `~/.bashrc` or `~/.zshrc`:

```bash
export PATH=$HOME/bin:$PATH
```

Reload shell:

```bash
source ~/.bashrc  # or source ~/.zshrc
```

**Step 3: Install Leiningen**

```bash
lein version
```

First run downloads Leiningen JAR and dependencies. Expected output:

```
Leiningen 2.11.2 on Java 17.0.9 OpenJDK 64-Bit Server VM
```

**Alternative: Package Manager**

Some distributions package Leiningen:

```bash
sudo apt install leiningen

yay -S leiningen
```

**Troubleshooting Linux**:

- If `lein` not found, restart terminal after PATH modification
- Check permissions: `ls -l ~/bin/lein` should show executable bit
- If download fails, check network connection and try manual download

## Your First Clojure REPL Session

The REPL (Read-Eval-Print Loop) is Clojure's interactive programming environment.

### Launch the REPL

Open terminal and run:

```bash
lein repl
```

You'll see Leiningen starting and the Clojure REPL prompt:

```
nREPL server started on port 54321 on host 127.0.0.1 - nrepl://127.0.0.1:54321
REPL-y 0.5.1, nREPL 1.0.0
Clojure 1.11.1
OpenJDK 64-Bit Server VM 17.0.9+9
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=>
```

The `user=>` prompt means Clojure is ready for input.

### Execute Your First Clojure Code

**Print "Hello, World!"**:

```clojure
user=> (println "Hello, World!")
```

Output:

```
Hello, World!
nil
```

Explanation:

- `println` - Function to print with newline
- `"Hello, World!"` - String argument
- `nil` - Return value (println returns nil)

**Basic Arithmetic**:

```clojure
user=> (+ 2 3)
5

user=> (* 4 5)
20

user=> (/ 10 2)
5
```

Clojure uses **prefix notation** (operator first): `(+ 2 3)` instead of `2 + 3`.

**Define a Variable**:

```clojure
user=> (def x 42)
#'user/x

user=> x
42

user=> (def message "Hello, Clojure!")
#'user/message

user=> message
"Hello, Clojure!"
```

**Call Functions**:

```clojure
user=> (str "Hello, " "World!")
"Hello, World!"

user=> (count "Clojure")
7

user=> (reverse "Clojure")
(\e \r \u \j \o \l \C)
```

**Define a Function**:

```clojure
user=> (defn greet [name]
         (str "Hello, " name "!"))
#'user/greet

user=> (greet "Alice")
"Hello, Alice!"
```

### Exit the REPL

Press `Ctrl+D` or type:

```clojure
user=> (exit)
```

## Create Your First Clojure Project

Leiningen manages Clojure projects with dependencies, build configuration, and directory structure.

### Create a New Project

```bash
lein new app hello-clojure
```

Output:

```
Generating a project called hello-clojure based on the 'app' template.
```

This creates `hello-clojure/` directory with project structure:

```
hello-clojure/
├── project.clj           # Project configuration
├── README.md             # Project documentation
├── src/
│   └── hello_clojure/
│       └── core.clj      # Main source file
├── test/
│   └── hello_clojure/
│       └── core_test.clj # Test file
└── resources/            # Resources (config files, assets)
```

### Explore Project Structure

Navigate into project:

```bash
cd hello-clojure
```

**View project.clj**:

```bash
cat project.clj
```

Contents:

```clojure
(defproject hello-clojure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main ^:skip-aot hello-clojure.core
  :target-path "target/%s"
  :profiles {:uproject {:aot :all
                        :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
```

**View src/hello_clojure/core.clj**:

```bash
cat src/hello_clojure/core.clj
```

Contents:

```clojure
(ns hello-clojure.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
```

This is your main source file with a `-main` function (entry point).

### Run the Project

Execute the project:

```bash
lein run
```

Output:

```
Hello, World!
```

Leiningen compiles and runs `src/hello_clojure/core.clj`, executing the `-main` function.

### Modify the Program

Edit `src/hello_clojure/core.clj`:

```clojure
(ns hello-clojure.core
  (:gen-class))

(defn greet
  "Greet someone by name"
  [name]
  (str "Hello, " name "! Welcome to Clojure."))

(defn -main
  "Entry point"
  [& args]
  (println (greet "World"))
  (println (greet "Alice"))
  (println "2 + 2 =" (+ 2 2)))
```

Run again:

```bash
lein run
```

Output:

```
Hello, World! Welcome to Clojure.
Hello, Alice! Welcome to Clojure.
2 + 2 = 4
```

### Start Project REPL

Launch REPL with project context:

```bash
lein repl
```

Now you can test functions interactively:

```clojure
user=> (require '[hello-clojure.core :as core])
nil

user=> (core/greet "Bob")
"Hello, Bob! Welcome to Clojure."
```

**Why project REPL?** It loads your project's code and dependencies, enabling interactive development.

### Build an Executable JAR

Create standalone JAR file:

```bash
lein uberjar
```

Output shows compilation and packaging:

```
Compiling hello-clojure.core
Created /path/to/hello-clojure/target/uberjar/hello-clojure-0.1.0-SNAPSHOT.jar
Created /path/to/hello-clojure/target/uberjar/hello-clojure-0.1.0-SNAPSHOT-standalone.jar
```

Run the JAR:

```bash
java -jar target/uberjar/hello-clojure-0.1.0-SNAPSHOT-standalone.jar
```

Output:

```
Hello, World! Welcome to Clojure.
Hello, Alice! Welcome to Clojure.
2 + 2 = 4
```

This JAR can run on any system with Java 11+ installed.

## Understanding Clojure's Interactive Development

Clojure emphasizes REPL-driven development - writing and testing code interactively.

### REPL Workflow

Typical Clojure development:

1. Start REPL with `lein repl`
2. Write function in source file
3. Reload code in REPL: `(require '[namespace :as alias] :reload)`
4. Test function interactively
5. Refine based on results
6. Repeat

This tight feedback loop enables rapid experimentation.

### Useful REPL Commands

**Documentation**:

```clojure
user=> (doc println)
```

Shows function documentation.

**Source Code**:

```clojure
user=> (source map)
```

Displays function implementation.

**Find Functions**:

```clojure
user=> (find-doc "string")
```

Searches documentation for "string".

**View Namespace**:

```clojure
user=> (ns-publics 'clojure.string)
```

Lists public functions in namespace.

**Previous Results**:

```clojure
user=> (+ 2 3)
5

user=> *1
5

user=> (* *1 2)
10
```

`*1` holds last result, `*2` second-to-last, `*3` third-to-last.

## Development Environment Setup

While any text editor works, some provide better Clojure support.

### VS Code with Calva

**Step 1: Install VS Code**

Download from [https://code.visualstudio.com/](https://code.visualstudio.com/)

**Step 2: Install Calva Extension**

1. Open VS Code
2. Go to Extensions (Ctrl+Shift+X or Cmd+Shift+X)
3. Search for "Calva"
4. Click Install

**Step 3: Open Clojure Project**

1. File → Open Folder → Select `hello-clojure` directory
2. Calva detects Leiningen project
3. Start REPL: Ctrl+Alt+C Ctrl+Alt+J (or Cmd+Option+C Cmd+Option+J on macOS)
4. Select "Leiningen"

Now you can evaluate code inline with Ctrl+Enter.

### IntelliJ IDEA with Cursive

**Step 1: Install IntelliJ IDEA**

Download Community Edition from [https://www.jetbrains.com/idea/download/](https://www.jetbrains.com/idea/download/)

**Step 2: Install Cursive Plugin**

1. Open IntelliJ IDEA
2. File → Settings → Plugins
3. Search for "Cursive"
4. Install and restart IDE

**Step 3: Import Clojure Project**

1. File → Open → Select `hello-clojure` directory
2. Choose "Import project from external model" → Leiningen
3. Configure SDK (select installed JDK)
4. Run REPL: Tools → REPL → Start REPL

Cursive provides excellent refactoring, debugging, and structural editing (paredit).

### Emacs with CIDER (Advanced)

Emacs with CIDER is popular among experienced Clojure developers:

1. Install Emacs
2. Install CIDER package: `M-x package-install RET cider RET`
3. Open Clojure file: `C-x C-f src/hello_clojure/core.clj`
4. Start REPL: `M-x cider-jack-in`

Emacs offers powerful structural editing with paredit/parinfer.

## Verify Your Setup Works

Let's confirm everything is functioning correctly.

### Test 1: Java Installed

```bash
java -version
```

Should show Java 11 or later.

### Test 2: Leiningen Installed

```bash
lein version
```

Should show Leiningen version and Clojure version.

### Test 3: REPL Access

```bash
lein repl
```

Should open Clojure REPL. Try `(+ 1 2)`, should return `3`. Exit with `Ctrl+D`.

### Test 4: Project Creation

```bash
lein new app test-project
cd test-project
lein run
```

Should output "Hello, World!"

### Test 5: Build JAR

```bash
lein uberjar
java -jar target/uberjar/test-project-0.1.0-SNAPSHOT-standalone.jar
```

Should run successfully and print output.

**All tests passed?** Your Clojure setup is complete!

## Summary

**What you've accomplished**:

- Installed Java Development Kit (JDK 11 or later)
- Installed Leiningen build tool and project manager
- Accessed and used the Clojure REPL interactively
- Executed Clojure code and explored basic syntax
- Created and ran your first Clojure project
- Built a standalone executable JAR file
- Understood REPL-driven development workflow

**Key commands learned**:

- `java -version` - Check Java installation
- `lein version` - Check Leiningen version
- `lein repl` - Start interactive REPL
- `lein new app <name>` - Create new project
- `lein run` - Run project
- `lein uberjar` - Build standalone JAR
- `(println ...)` - Print output
- `(def ...)` - Define variable
- `(defn ...)` - Define function

**Skills gained**:

- JVM environment setup for Clojure
- Leiningen project management basics
- Interactive REPL usage and workflow
- Basic Clojure syntax understanding
- Project structure navigation

## Next Steps

**Ready to learn Clojure syntax and concepts?**

- [Quick Start](/en/learn/software-engineering/programming-languages/clojure/quick-start) (5-30% coverage) - Touch all core Clojure concepts in a fast-paced tour

**Want comprehensive fundamentals?**

- [Beginner Tutorial](/en/learn/software-engineering/programming-languages/clojure/by-concept/beginner) (0-60% coverage) - Deep dive into Clojure basics with extensive practice

**Prefer code-first learning?**

- [By-Example Tutorial](/en/learn/software-engineering/programming-languages/clojure/by-example) - Learn through 80 heavily annotated examples

**Want to understand Clojure's design philosophy?**

- [Overview](/en/learn/software-engineering/programming-languages/clojure/overview) - Why Clojure exists and when to use it

## Troubleshooting Common Issues

### "java: command not found"

**Problem**: Java not installed or not in PATH.

**Solution**:

- Install JDK following platform-specific instructions above
- Verify PATH includes Java bin directory
- Restart terminal after installation

### "lein: command not found"

**Problem**: Leiningen not in PATH.

**Solution**:

- Verify lein script downloaded to correct location
- Check PATH includes directory containing lein
- Make lein executable: `chmod +x ~/bin/lein` (macOS/Linux)
- Restart terminal after PATH modification

### REPL fails to start

**Problem**: Leiningen can't start Clojure REPL.

**Solution**:

- Check Java is installed: `java -version`
- Verify JAVA_HOME set correctly: `echo $JAVA_HOME` (Linux/macOS) or `echo %JAVA_HOME%` (Windows)
- Delete `~/.m2/repository` to clear corrupted Maven cache
- Retry `lein repl`

### "Could not find or load main class"

**Problem**: JAR execution fails.

**Solution**:

- Ensure you're running standalone JAR: `*-standalone.jar`
- Rebuild JAR: `lein clean && lein uberjar`
- Check `:main` is set in `project.clj`

### Slow project creation or first REPL start

**Problem**: Leiningen downloads dependencies on first use.

**Solution**:

- This is normal behavior - first run downloads JARs from Maven Central
- Subsequent runs use cached dependencies and start quickly
- Be patient during first-time setup (can take 2-5 minutes)

### Windows: "Access is denied" when running lein

**Problem**: Antivirus blocking script execution.

**Solution**:

- Temporarily disable antivirus during installation
- Add `C:\leiningen` to antivirus exclusions
- Use alternative security software that doesn't block JVM tools

## Further Resources

**Official Documentation**:

- [Clojure.org](https://clojure.org/) - Official Clojure website
- [Leiningen Documentation](https://leiningen.org/) - Leiningen user guide
- [ClojureDocs](https://clojuredocs.org/) - Community-powered documentation with examples
- [Clojure API Reference](https://clojure.github.io/clojure/) - Complete API documentation

**Interactive Learning**:

- [Clojure Koans](https://github.com/functional-koans/clojure-koans) - Learn through failing tests
- [4Clojure](https://4clojure.oxal.org/) - Interactive Clojure problems
- [Exercism Clojure Track](https://exercism.org/tracks/clojure) - Practice problems with mentorship

**Books**:

- [Clojure for the Brave and True](https://www.braveclojure.com/) - Free online book, beginner-friendly
- [Programming Clojure](https://pragprog.com/titles/shcloj3/programming-clojure-third-edition/) - Comprehensive introduction
- [The Joy of Clojure](https://www.manning.com/books/the-joy-of-clojure-second-edition) - Intermediate to advanced

**Community**:

- [Clojurians Slack](https://clojurians.net/) - Active community chat
- [/r/Clojure](https://www.reddit.com/r/Clojure/) - Reddit community
- [ClojureVerse](https://clojureverse.org/) - Discussion forum
- [Clojure Mailing List](https://groups.google.com/g/clojure) - Official mailing list
