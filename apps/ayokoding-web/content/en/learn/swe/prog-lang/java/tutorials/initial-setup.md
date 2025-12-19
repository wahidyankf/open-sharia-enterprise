---
title: "Initial Setup"
date: 2025-12-09T00:00:00+07:00
draft: false
description: Get Java running quickly - install JDK, verify installation, and run your first program
weight: 502
tags:
  - java
  - initial-setup
  - installation
  - hello-world
  - jdk
  - learn
---

**Get Java running quickly.** This tutorial provides the quickest path to running your first Java program. You'll install the Java Development Kit (JDK), verify it works, and run a simple "Hello World" application.

## What You'll Achieve

By the end of this tutorial, you'll have:

- ✅ Java Development Kit (JDK 21 or later) installed on your system
- ✅ Verified Java installation works correctly
- ✅ Created and compiled your first Java program
- ✅ Run your first Java application successfully

## Prerequisites

- **Basic command line familiarity** - You should know how to open a terminal/command prompt and navigate directories
- **No programming experience required** - This tutorial assumes you're completely new to Java

## Why Learn Java?

Java is one of the world's most popular programming languages, powering:

- **Enterprise applications** - Banking systems, e-commerce platforms, large-scale business applications
- **Android mobile apps** - The primary language for Android app development
- **Cloud services** - Microservices, serverless functions, distributed systems
- **Big data systems** - Hadoop, Apache Spark, Kafka, and other data processing frameworks

Java's "write once, run anywhere" philosophy means code runs on any platform with a Java Virtual Machine (JVM).

## Step 1: Install the Java Development Kit (JDK)

The JDK contains everything you need to develop Java applications: compiler, runtime, and standard libraries.

### Choose Your Installation Method

We recommend **OpenJDK** - the free, open-source implementation of Java.

#### Option A: Download from Adoptium (Recommended)

**Adoptium** provides high-quality, free OpenJDK binaries for all platforms.

1. **Visit** [https://adoptium.net/](https://adoptium.net/)
2. **Select** "JDK 21 (LTS)" from the version dropdown
3. **Download** the installer for your operating system
4. **Run** the installer and follow the prompts

#### Option B: Use a Package Manager

**macOS (Homebrew)**:

```bash
brew install openjdk@21
```

After installation, link it to your system:

```bash
sudo ln -sfn /opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-21.jdk
```

**Linux (Ubuntu/Debian)**:

```bash
sudo apt update
sudo apt install openjdk-21-jdk
```

**Linux (Fedora/RHEL)**:

```bash
sudo dnf install java-21-openjdk-devel
```

**Windows (Chocolatey)**:

```powershell
choco install openjdk21
```

### Platform-Specific Installation Notes

#### Windows

- The installer will typically install to `C:\Program Files\Eclipse Adoptium\jdk-21.x.x\`
- The installer should automatically set your `PATH` environment variable
- If not, you'll need to manually add the `bin` directory to your PATH

#### macOS

- JDK is installed to `/Library/Java/JavaVirtualMachines/`
- The `java` command should be available immediately after installation
- You may need to restart your terminal

#### Linux

- Package managers handle PATH configuration automatically
- JDK is typically installed to `/usr/lib/jvm/`
- No additional configuration needed

## Step 2: Verify Installation

Open a terminal (or command prompt on Windows) and run:

```bash
java -version
```

**Expected output** (version numbers may vary):

```
openjdk version "21.0.1" 2023-10-17 LTS
OpenJDK Runtime Environment Temurin-21.0.1+12 (build 21.0.1+12-LTS)
OpenJDK 64-Bit Server VM Temurin-21.0.1+12 (build 21.0.1+12-LTS, mixed mode, sharing)
```

Also verify the compiler is available:

```bash
javac -version
```

**Expected output**:

```
javac 21.0.1
```

### Troubleshooting Installation

**"Command not found" error?**

- **Windows**: Add JDK `bin` directory to PATH (e.g., `C:\Program Files\Eclipse Adoptium\jdk-21.x.x\bin`)
- **macOS/Linux**: Your package manager should have configured PATH automatically. Try restarting your terminal

**Wrong version displayed?**

- You may have multiple Java versions installed
- Use `which java` (macOS/Linux) or `where java` (Windows) to see which version is active
- Uninstall older versions or update your PATH to prioritize JDK 21

**Permission denied?**

- Use `sudo` on Linux/macOS for installation commands
- Run PowerShell/Command Prompt as Administrator on Windows

## Step 3: Create Your First Java Program

Let's write a simple "Hello World" program to verify everything works.

### Create a Project Directory

```bash
mkdir hello-java
cd hello-java
```

### Write the Code

Create a file named `Hello.java` with the following content:

```java
public class Hello {
    public static void main(String[] args) {
        System.out.println("Hello, Java!");
    }
}
```

**Important naming rule**: The filename `Hello.java` MUST exactly match the class name `Hello` (case-sensitive). This is a Java requirement.

### Understanding the Code (Brief)

- `public class Hello` - Defines a class named "Hello"
- `public static void main(String[] args)` - The entry point where Java starts execution
- `System.out.println()` - Prints text to the console
- Every Java program must have at least one class and a `main` method to run

**Note**: You'll learn these concepts in detail in the Quick Start and Beginner tutorials.

## Step 4: Compile the Program

Compile your Java code into bytecode that the JVM can run:

```bash
javac Hello.java
```

If successful, this command creates a file named `Hello.class` (the compiled bytecode). No output means success.

**Check that the file was created**:

```bash
ls  # macOS/Linux
dir # Windows
```

You should see both `Hello.java` (source) and `Hello.class` (compiled bytecode).

### Troubleshooting Compilation

**"error: class Hello is public, should be declared in a file named Hello.java"**

- Your filename doesn't match the class name
- Make sure the file is named exactly `Hello.java` (case-sensitive)

**"javac: command not found"**

- JDK is not installed or not in your PATH
- Go back to Step 1 and verify installation

**Syntax errors?**

- Make sure you copied the code exactly as shown
- Check for missing semicolons, braces, or parentheses

## Step 5: Run the Program

Execute your compiled program:

```bash
java Hello
```

**Expected output**:

```
Hello, Java!
```

**Note**: Run with `java Hello` (no `.class` extension), not `java Hello.class`.

### How This Works

1. **`javac Hello.java`** - The Java compiler translates source code into bytecode
2. **`Hello.class`** - The bytecode file created by the compiler
3. **`java Hello`** - The JVM loads and executes the bytecode

This two-step process (compile → run) is fundamental to Java development.

## Verification Checklist

Confirm you can do all of the following:

- ✅ Run `java -version` and see Java 21 or later
- ✅ Run `javac -version` and see the compiler version
- ✅ Create a `.java` file with a simple program
- ✅ Compile it with `javac` to produce a `.class` file
- ✅ Run it with `java` and see the output

If all checkboxes are checked, you're ready to start learning Java!

## 🎯 Quick Challenge

Now that you have Java working, try this quick challenge to solidify your understanding:

### Challenge: Modify the Hello World Program

1. **Edit** `Hello.java` and change the message to print your name instead of "Hello, Java!"
2. **Compile** it again with `javac Hello.java`
3. **Run** it with `java Hello` and verify your name appears

**Success criteria**:

- Program compiles without errors
- Program runs without errors
- Your custom message displays

### Challenge: Create a Second Program

Try creating another Java program from scratch:

1. **Create** a file named `Calculator.java`
2. **Write** a class with a main method that performs a simple calculation:
   ```java
   int x = 10;
   int y = 20;
   System.out.println("Sum: " + (x + y));
   ```
3. **Compile** and run it
4. **Verify** the output shows the correct sum (30)

**Hint**: Remember the class name must match the filename (Calculator.java for class Calculator).

These challenges confirm that your Java setup is complete and working properly.

## Optional: Set Up an IDE

While you can develop Java with a text editor and command line, an **Integrated Development Environment (IDE)** provides code completion, debugging, and other helpful features.

### Popular Java IDEs

**IntelliJ IDEA Community Edition** (Recommended)

- **Download**: [https://www.jetbrains.com/idea/download/](https://www.jetbrains.com/idea/download/)
- **Free** and feature-rich
- Excellent code completion and refactoring
- Built-in Maven and Gradle support

**Eclipse**

- **Download**: [https://www.eclipse.org/downloads/](https://www.eclipse.org/downloads/)
- **Free** and open-source
- Lightweight and highly customizable
- Large plugin ecosystem

**Visual Studio Code**

- **Download**: [https://code.visualstudio.com/](https://code.visualstudio.com/)
- **Free** and lightweight
- Install "Extension Pack for Java" from Microsoft
- Good for beginners, less feature-rich than IntelliJ or Eclipse

**You don't need an IDE right now**. For the Initial Setup and Quick Start tutorials, the command line is sufficient. IDEs become more valuable as projects grow larger.

## Next Steps

Congratulations! You have a working Java development environment.

### Continue Learning

- **[Java Quick Start](quick-start)** - Learn enough Java to explore independently. Perfect if you want a quick overview of core syntax and concepts.

- **[Complete Beginner's Guide to Java](beginner)** - Comprehensive foundation covering Java fundamentals from zero. Choose this if you want in-depth coverage with practice exercises.

### What's Next?

The **Quick Start** tutorial teaches you:

- Variables, data types, and operators
- Methods and control flow
- Basic object-oriented programming
- Simple collections (ArrayList, HashMap)
- Error handling basics

The **Beginner** tutorial provides comprehensive coverage:

- Complete object-oriented programming (classes, inheritance, interfaces)
- Collections Framework with generics
- Exception handling and testing
- Streams and functional programming
- Practice projects and exercises

Choose Quick Start for a fast overview, or jump to Beginner for thorough, comprehensive learning.

### Additional Resources

- **Official Java Documentation**: [https://docs.oracle.com/en/java/javase/21/](https://docs.oracle.com/en/java/javase/21/)
- **Java Language Specification**: [https://docs.oracle.com/javase/specs/](https://docs.oracle.com/javase/specs/)
- **OpenJDK Project**: [https://openjdk.org/](https://openjdk.org/)

---

**Tutorial Complete!** You're now ready to start learning Java programming.
