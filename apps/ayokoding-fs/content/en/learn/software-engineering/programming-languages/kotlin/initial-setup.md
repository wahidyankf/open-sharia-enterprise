---
title: "Initial Setup"
date: 2025-01-29T00:00:00+07:00
draft: false
weight: 100001
description: "Get Kotlin installed and running on your system - installation, verification, and your first working program"
tags: ["kotlin", "installation", "setup", "beginner", "jvm"]
---

**Want to start programming in Kotlin?** This initial setup guide gets Kotlin installed and working on your system in minutes. By the end, you'll have Kotlin running and will execute your first program.

This tutorial provides 0-5% coverage - just enough to get Kotlin working on your machine. For deeper learning, continue to [Quick Start](/en/learn/software-engineering/programming-languages/kotlin/quick-start) (5-30% coverage).

## Prerequisites

Before installing Kotlin, you need:

- A computer running Windows, macOS, or Linux
- Java Development Kit (JDK) 8 or higher installed
- Administrator/sudo access for installation
- A terminal/command prompt
- A text editor (IntelliJ IDEA recommended, or any editor)
- Basic command-line navigation skills

**IMPORTANT**: Kotlin runs on the JVM, so Java must be installed first. If you haven't installed Java yet, see [Java Initial Setup](/en/learn/software-engineering/programming-languages/java/initial-setup).

## Learning Objectives

By the end of this tutorial, you will be able to:

1. **Install** the Kotlin compiler on your operating system
2. **Verify** that Kotlin is installed correctly and check the version
3. **Write** your first Kotlin program (Hello, World!)
4. **Compile** Kotlin source code to JVM bytecode
5. **Execute** Kotlin programs on the JVM

## Installation Methods

Kotlin can be installed via:

1. **Kotlin command-line compiler** - Standalone compiler for terminal use
2. **SDKMAN!** - Version manager for JVM languages (Linux/macOS)
3. **Homebrew** - Package manager (macOS)
4. **IntelliJ IDEA** - IDE with built-in Kotlin support (all platforms, recommended for serious development)

We'll cover method 1 (command-line compiler) for all platforms, as it's the most universal.

## Platform-Specific Installation

Choose your operating system and follow the installation steps.

### Windows Installation

**Step 1: Install Scoop (Package Manager)**

Scoop simplifies Kotlin installation on Windows.

Open PowerShell and run:

```powershell
Set-ExecutionPolicy RemoteSigned -Scope CurrentUser
irm get.scoop.sh | iex
```

**Step 2: Install Kotlin via Scoop**

```powershell
scoop install kotlin
```

**Step 3: Verify Installation**

```cmd
kotlin -version
```

Expected output:

```
Kotlin version 1.9.X-release-XXX (JRE 21.X.X)
```

**Alternative: Manual Installation**

1. Download Kotlin compiler from [https://github.com/JetBrains/kotlin/releases/latest](https://github.com/JetBrains/kotlin/releases/latest)
2. Extract `kotlin-compiler-1.9.X.zip` to `C:\Program Files\kotlinc\`
3. Add `C:\Program Files\kotlinc\bin` to PATH environment variable
4. Restart terminal and verify: `kotlinc -version`

**Troubleshooting Windows**:

- If `kotlinc` not found, check PATH includes Kotlin bin directory
- Ensure JDK is installed: `java -version` should work
- Restart terminal after PATH changes

### macOS Installation

**Step 1: Install Homebrew (if not already installed)**

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

**Step 2: Install Kotlin**

```bash
brew install kotlin
```

**Step 3: Verify Installation**

```bash
kotlin -version
kotlinc -version
```

Expected output:

```
Kotlin version 1.9.X-release-XXX (JRE 21.X.X)
```

**Alternative: Install via SDKMAN!**

SDKMAN! manages multiple JVM tool versions.

Install SDKMAN!:

```bash
curl -s "https://get.sdkman.io" | bash
source "$HOME/.sdkman/bin/sdkman-init.sh"
```

Install Kotlin:

```bash
sdk install kotlin
```

Verify:

```bash
kotlin -version
```

**Troubleshooting macOS**:

- If `kotlin` not found, restart terminal to load Homebrew changes
- Check Homebrew installation: `brew doctor`
- Ensure JDK installed: `java -version`

### Linux Installation

**Step 1: Install SDKMAN!**

SDKMAN! is the recommended way to install Kotlin on Linux.

```bash
curl -s "https://get.sdkman.io" | bash
source "$HOME/.sdkman/bin/sdkman-init.sh"
```

**Step 2: Install Kotlin**

```bash
sdk install kotlin
```

**Step 3: Verify Installation**

```bash
kotlin -version
kotlinc -version
```

Expected output:

```
Kotlin version 1.9.X-release-XXX (JRE 21.X.X)
```

**Alternative: Manual Installation**

```bash
wget https://github.com/JetBrains/kotlin/releases/download/v1.9.X/kotlin-compiler-1.9.X.zip

unzip kotlin-compiler-1.9.X.zip -d ~/kotlin

echo 'export PATH=$PATH:~/kotlin/kotlinc/bin' >> ~/.bashrc
source ~/.bashrc

kotlinc -version
```

**Alternative: Install via Snap (Ubuntu)**

```bash
sudo snap install --classic kotlin
```

**Troubleshooting Linux**:

- If `kotlinc` not found, ensure PATH includes Kotlin bin directory
- Check SDKMAN installation: `sdk version`
- Verify JDK: `java -version` should show Java 8+

## Your First Kotlin Program

Let's write and run your first Kotlin program - the classic "Hello, World!".

### Create a Project Directory

```bash
mkdir -p ~/kotlin-projects/hello
cd ~/kotlin-projects/hello
```

### Write the Program

Create a file named `HelloWorld.kt`:

```kotlin
fun main() {
    println("Hello, World!")
}
```

**Code breakdown**:

- `fun main()`: Entry point - Kotlin starts execution here (no `args` parameter needed if not used)
- `println(...)`: Prints text to console with newline (similar to Java's `System.out.println()`)

**Save the file** as `HelloWorld.kt` in your project directory.

### Compile the Program

Kotlin code compiles to JVM bytecode:

```bash
kotlinc HelloWorld.kt -include-runtime -d HelloWorld.jar
```

**Flags explained**:

- `-include-runtime`: Includes Kotlin runtime library in JAR (makes it standalone)
- `-d HelloWorld.jar`: Output JAR file name

This creates `HelloWorld.jar` containing bytecode and runtime.

### Run the Program

Execute the JAR with Java:

```bash
java -jar HelloWorld.jar
```

**Output**:

```
Hello, World!
```

**What happened**:

1. `kotlinc` compiled `HelloWorld.kt` → `HelloWorld.jar` (bytecode + runtime)
2. `java` launched JVM to execute JAR
3. JVM executed `main()` function
4. `println()` printed output

## Using Kotlin REPL (Interactive Mode)

Kotlin has a Read-Eval-Print Loop (REPL) for testing code interactively.

Start the REPL:

```bash
kotlinc
```

You'll see the Kotlin prompt:

```
Welcome to Kotlin version 1.9.X
Type :help for help, :quit for quit
>>>
```

Try some commands:

```kotlin
>>> println("Hello, World!")
Hello, World!
>>> val x = 42
>>> x * 2
84
>>> fun greet(name: String) = "Hello, $name!"
>>> greet("Kotlin")
Hello, Kotlin!
>>> :quit
```

**Exit REPL**: Type `:quit` or press `Ctrl+D`.

## Script Mode (No Compilation)

Kotlin supports script mode for quick execution without explicit compilation:

Create `hello.kts` (note `.kts` extension for script):

```kotlin
println("Hello from Kotlin script!")

val numbers = listOf(1, 2, 3, 4, 5)
val doubled = numbers.map { it * 2 }
println("Doubled: $doubled")
```

Run directly:

```bash
kotlinc -script hello.kts
```

**Output**:

```
Hello from Kotlin script!
Doubled: [2, 4, 6, 8, 10]
```

**Difference**:

- `.kt` files: Full programs, need compilation to JAR
- `.kts` files: Scripts, run directly (slower but convenient for quick tasks)

## More Detailed Example

Let's write a program with user input. Create `Greeting.kt`:

```kotlin
fun main() {
    // Get user's name
    print("What's your name? ")
    val name = readln()

    // Get user's age
    print("What's your age? ")
    val age = readln().toInt()

    // Display greeting
    println("Hello, $name! Welcome to Kotlin.")
    println("In 10 years, you'll be ${age + 10} years old!")
}
```

Compile and run:

```bash
kotlinc Greeting.kt -include-runtime -d Greeting.jar
java -jar Greeting.jar
```

**Interaction**:

```
What's your name? Alice
What's your age? 25
Hello, Alice! Welcome to Kotlin.
In 10 years, you'll be 35 years old!
```

**Code breakdown**:

- `readln()`: Reads user input (returns String)
- `toInt()`: Converts String to Int
- `$name`: String interpolation - embeds variable in string
- `${age + 10}`: Expression interpolation - evaluates expression in string

**Kotlin vs Java differences**:

- **No semicolons required** (optional in Kotlin)
- **Type inference**: `val name = readln()` (compiler infers `String` type)
- **String templates**: `"Hello, $name"` instead of Java's `"Hello, " + name`

## Understanding Kotlin Compilation

Kotlin compiles to JVM bytecode (same as Java):

```
HelloWorld.kt → kotlinc → HelloWorld.jar (JVM bytecode) → java → Output
```

**Why Kotlin uses JVM**:

- **Java interoperability**: Call Java libraries from Kotlin and vice versa
- **Mature platform**: Leverage JVM's performance optimizations and ecosystem
- **Cross-platform**: JVM bytecode runs on any OS with Java installed

**Kotlin also compiles to**:

- **Kotlin/JS**: JavaScript for browser/Node.js
- **Kotlin/Native**: Native binaries (no JVM needed)

This tutorial focuses on Kotlin/JVM.

## Summary

**What you've accomplished**:

- Installed Kotlin compiler on your operating system
- Verified Kotlin installation with version checks
- Wrote and compiled your first Kotlin programs
- Executed Kotlin bytecode on the JVM
- Used Kotlin REPL for interactive experimentation
- Ran Kotlin scripts without explicit compilation

**Key commands learned**:

- `kotlinc -version` - Check Kotlin compiler version
- `kotlin -version` - Check Kotlin runtime version
- `kotlinc <file>.kt -include-runtime -d <output>.jar` - Compile to JAR
- `java -jar <output>.jar` - Run compiled Kotlin program
- `kotlinc` - Start Kotlin REPL
- `kotlinc -script <file>.kts` - Run Kotlin script

**Skills gained**:

- Platform-specific Kotlin installation
- Compiling and running Kotlin programs
- Using Kotlin REPL and script mode
- Understanding Kotlin/JVM architecture

## Next Steps

**Ready to learn Kotlin syntax and concepts?**

- [Quick Start](/en/learn/software-engineering/programming-languages/kotlin/quick-start) (5-30% coverage) - Touch all core Kotlin concepts in a fast-paced tour

**Want comprehensive fundamentals?**

**Prefer code-first learning?**

- [By-Example Tutorial](/en/learn/software-engineering/programming-languages/kotlin/by-example) - Learn through heavily annotated examples

**Want to understand Kotlin's design philosophy?**

- [Overview](/en/learn/software-engineering/programming-languages/kotlin/overview) - Why Kotlin exists and when to use it

## Troubleshooting Common Issues

### "kotlinc: command not found"

**Problem**: Terminal doesn't recognize Kotlin commands.

**Solution**:

- Verify Kotlin is installed: Check installation directory
- Add Kotlin to PATH:
  - **Windows**: Add `C:\Program Files\kotlinc\bin` to PATH
  - **macOS/Linux**: Add `export PATH=$PATH:~/kotlin/kotlinc/bin` to `~/.bashrc` or `~/.zshrc`
- Restart terminal after PATH changes
- For SDKMAN: Run `source "$HOME/.sdkman/bin/sdkman-init.sh"`

### "Error: Could not find or load main class"

**Problem**: JVM can't find main class in JAR.

**Solution**:

- Ensure `-include-runtime` flag used during compilation
- Verify JAR file exists: `ls HelloWorld.jar`
- Check `main()` function is defined correctly (must be at top level or in object)

### Java not found

**Problem**: Kotlin requires Java, but it's not installed.

**Solution**:

- Install JDK 8 or higher first: See [Java Initial Setup](/en/learn/software-engineering/programming-languages/java/initial-setup)
- Verify Java installation: `java -version`
- Set JAVA_HOME environment variable if needed

### REPL won't start

**Problem**: `kotlinc` command hangs or fails.

**Solution**:

- Verify Kotlin installation: `kotlinc -version`
- Check JDK is accessible: `java -version`
- Try updating Kotlin: `sdk upgrade kotlin` (if using SDKMAN)

### Script execution fails

**Problem**: `kotlinc -script` gives errors.

**Solution**:

- Verify file has `.kts` extension (not `.kt`)
- Check script syntax: Start with simple `println("test")`
- Ensure Kotlin compiler supports scripts (should be default)

## Further Resources

**Official Kotlin Documentation**:

- [Kotlin Language Site](https://kotlinlang.org/) - Official website
- [Kotlin Docs](https://kotlinlang.org/docs/home.html) - Comprehensive documentation
- [Kotlin Playground](https://play.kotlinlang.org/) - Try Kotlin in browser (no installation)

**Development Tools**:

- [IntelliJ IDEA](https://www.jetbrains.com/idea/) - Best IDE for Kotlin (Community Edition free)
- [Android Studio](https://developer.android.com/studio) - For Android development with Kotlin
- [VS Code](https://code.visualstudio.com/) with [Kotlin extension](https://marketplace.visualstudio.com/items?itemName=fwcd.kotlin)

**Community**:

- [Kotlin Slack](https://surveys.jetbrains.com/s3/kotlin-slack-sign-up) - Official Slack community
- [/r/Kotlin](https://www.reddit.com/r/Kotlin/) - Reddit community
- [Kotlin Forum](https://discuss.kotlinlang.org/) - Official discussion forum
- [Stack Overflow - Kotlin](https://stackoverflow.com/questions/tagged/kotlin) - Q&A
