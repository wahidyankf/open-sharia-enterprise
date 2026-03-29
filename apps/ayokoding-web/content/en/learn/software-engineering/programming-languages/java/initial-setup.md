---
title: "Initial Setup"
date: 2025-01-29T00:00:00+07:00
draft: false
weight: 100001
description: "Get Java installed and running on your system - JDK installation, verification, and your first working program"
tags: ["java", "installation", "setup", "beginner", "jdk"]
---

**Want to start programming in Java?** This initial setup guide gets Java installed and working on your system in minutes. By the end, you'll have the Java Development Kit (JDK) running and will execute your first program.

This tutorial provides 0-5% coverage - just enough to get Java working on your machine. For deeper learning, continue to [Quick Start](/en/learn/software-engineering/programming-languages/java/quick-start) (5-30% coverage).

## Prerequisites

Before installing Java, you need:

- A computer running Windows, macOS, or Linux
- Administrator/sudo access for installation
- A terminal/command prompt
- A text editor (VS Code, IntelliJ IDEA, Eclipse, or any editor)
- Basic command-line navigation skills

No prior programming experience required - this guide starts from zero.

## Learning Objectives

By the end of this tutorial, you will be able to:

1. **Install** the Java Development Kit (JDK) on your operating system
2. **Verify** that Java is installed correctly and check the version
3. **Write** your first Java program (Hello, World!)
4. **Compile** Java source code into bytecode
5. **Execute** Java programs using the JVM

## Understanding Java: JDK, JRE, JVM

Before installation, understand key Java components:

- **JDK (Java Development Kit)**: Complete development environment (compiler, tools, libraries) - this is what you install to write Java programs
- **JRE (Java Runtime Environment)**: Runtime-only (JVM + libraries) - needed to run Java programs (included in JDK)
- **JVM (Java Virtual Machine)**: Executes Java bytecode - makes Java platform-independent

**For development, install the JDK** (includes JRE and JVM).

## Platform-Specific Installation

Choose your operating system and follow the installation steps.

### Windows Installation

**Step 1: Download the JDK**

1. Visit [https://adoptium.net/](https://adoptium.net/) (Eclipse Temurin - open-source JDK)
2. Click **Download** for the latest LTS version (Java 21 recommended)
3. Choose:
   - **Operating System**: Windows
   - **Architecture**: x64 (most common)
   - **Package Type**: JDK
   - **Format**: .msi installer
4. Download the `.msi` file (e.g., `OpenJDK21U-jdk_x64_windows_hotspot_21.X.X.msi`)

**Alternative**: Oracle JDK from [https://www.oracle.com/java/technologies/downloads/](https://www.oracle.com/java/technologies/downloads/) (requires Oracle account)

**Step 2: Run the Installer**

1. Double-click the downloaded `.msi` file
2. Follow the installation wizard:
   - Click **Next** on welcome screen
   - Keep default installation directory (`C:\Program Files\Eclipse Adoptium\jdk-21.X.X-hotspot\`)
   - Check **Add to PATH** option (important!)
   - Check **Set JAVA_HOME** option (important!)
   - Click **Install**
3. Click **Finish** when complete

**Step 3: Verify Installation**

Open Command Prompt or PowerShell and run:

```cmd
java -version
```

Expected output:

```
openjdk version "21.X.X" 2024-XX-XX
OpenJDK Runtime Environment Temurin-21.X.X (build 21.X.X)
OpenJDK 64-Bit Server VM Temurin-21.X.X (build 21.X.X, mixed mode, sharing)
```

Check the compiler:

```cmd
javac -version
```

Expected output:

```
javac 21.X.X
```

**Step 4: Verify Environment Variables**

```cmd
echo %JAVA_HOME%
echo %PATH%
```

`JAVA_HOME` should point to JDK installation directory.
`PATH` should include `%JAVA_HOME%\bin`.

**Troubleshooting Windows**:

- If commands fail, restart terminal or computer to load environment variables
- Manually set `JAVA_HOME`:
  - Right-click **This PC** → **Properties**
  - **Advanced system settings** → **Environment Variables**
  - Add new system variable: `JAVA_HOME` = `C:\Program Files\Eclipse Adoptium\jdk-21.X.X-hotspot`
  - Edit `PATH`: Add `%JAVA_HOME%\bin`

### macOS Installation

**Step 1: Check Pre-installed Java**

macOS may have an outdated Java. Check:

```bash
java -version
```

If it shows Java 17+, you're good! Otherwise, continue installation.

**Step 2: Download the JDK**

1. Visit [https://adoptium.net/](https://adoptium.net/)
2. Click **Download** for Java 21 LTS
3. Choose:
   - **Operating System**: macOS
   - **Architecture**:
     - **aarch64** for Apple Silicon (M1/M2/M3)
     - **x64** for Intel Macs
   - **Package Type**: JDK
   - **Format**: .pkg installer
4. Download the `.pkg` file

Not sure which architecture? Run `uname -m` in Terminal:

- `arm64` → Apple Silicon
- `x86_64` → Intel

**Step 3: Install via Package**

1. Double-click the downloaded `.pkg` file
2. Follow the installer:
   - Click **Continue** through introduction
   - Accept the license
   - Click **Install** (may require password)
   - Click **Close** when complete

**Step 4: Verify Installation**

Open Terminal and run:

```bash
java -version
javac -version
```

Expected output similar to Windows example above.

**Step 5: Set JAVA_HOME (if needed)**

Add to `~/.zshrc` or `~/.bash_profile`:

```bash
export JAVA_HOME=$(/usr/libexec/java_home)
export PATH=$JAVA_HOME/bin:$PATH
```

Reload configuration:

```bash
source ~/.zshrc  # or source ~/.bash_profile
```

**Alternative: Install via Homebrew**

```bash
brew install openjdk@21
```

Link to system:

```bash
sudo ln -sfn /usr/local/opt/openjdk@21/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-21.jdk
```

Set `JAVA_HOME`:

```bash
echo 'export JAVA_HOME=$(/usr/libexec/java_home)' >> ~/.zshrc
source ~/.zshrc
```

**Troubleshooting macOS**:

- Use `/usr/libexec/java_home` to find Java installation: `/usr/libexec/java_home -V`
- If multiple Java versions installed, specify version: `export JAVA_HOME=$(/usr/libexec/java_home -v 21)`

### Linux Installation

**Step 1: Install via Package Manager**

**Ubuntu/Debian**:

```bash
sudo apt update
sudo apt install openjdk-21-jdk
```

**Fedora/RHEL/CentOS**:

```bash
sudo dnf install java-21-openjdk-devel
```

**Arch Linux**:

```bash
sudo pacman -S jdk-openjdk
```

**Step 2: Verify Installation**

```bash
java -version
javac -version
```

**Step 3: Set JAVA_HOME**

Find Java installation:

```bash
update-alternatives --list java
```

Set `JAVA_HOME` in `~/.bashrc` or `~/.zshrc`:

```bash
export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64
export PATH=$JAVA_HOME/bin:$PATH
```

Reload:

```bash
source ~/.bashrc  # or source ~/.zshrc
```

**Alternative: Manual Installation from Adoptium**

```bash
wget https://github.com/adoptium/temurin21-binaries/releases/download/jdk-21.X.X/OpenJDK21U-jdk_x64_linux_hotspot_21.X.X.tar.gz

sudo tar -xzf OpenJDK21U-jdk_x64_linux_hotspot_21.X.X.tar.gz -C /opt/

sudo ln -s /opt/jdk-21.X.X /opt/jdk

echo 'export JAVA_HOME=/opt/jdk' >> ~/.bashrc
echo 'export PATH=$JAVA_HOME/bin:$PATH' >> ~/.bashrc
source ~/.bashrc
```

**Troubleshooting Linux**:

- Multiple Java versions: Use `update-alternatives` to switch:

  ```bash
  sudo update-alternatives --config java
  sudo update-alternatives --config javac
  ```

- Check JAVA_HOME: `echo $JAVA_HOME` should point to JDK directory

## Your First Java Program

Let's write and run your first Java program - the classic "Hello, World!".

### Create a Project Directory

```bash
mkdir -p ~/java-projects/hello
cd ~/java-projects/hello
```

### Write the Program

Create a file named `HelloWorld.java`:

```java
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
```

**CRITICAL**: Filename MUST match class name (`HelloWorld.java` for `class HelloWorld`).

**Code breakdown**:

- `public class HelloWorld`: Declares a public class named HelloWorld
- `public static void main(String[] args)`: Entry point - execution starts here
- `System.out.println(...)`: Prints text to console with newline

### Compile the Program

Java code must be compiled before execution:

```bash
javac HelloWorld.java
```

This creates `HelloWorld.class` (bytecode file).

**Directory contents**:

```
~/java-projects/hello/
├── HelloWorld.java   # Source code
└── HelloWorld.class  # Compiled bytecode
```

### Run the Program

Execute the bytecode with the JVM:

```bash
java HelloWorld
```

**Note**: Use class name (no `.class` extension) when running.

**Output**:

```
Hello, World!
```

**What happened**:

1. `javac` compiled `HelloWorld.java` → `HelloWorld.class` (bytecode)
2. `java` launched JVM to execute bytecode
3. JVM executed `main()` method
4. `System.out.println()` printed output

## Compile and Run in One Step

For simple programs, compile and run together:

```bash
javac HelloWorld.java && java HelloWorld
```

Or use Java 11+ single-file execution (no compilation step visible):

```bash
java HelloWorld.java
```

This compiles and runs in one command (useful for simple scripts).

## More Detailed Example

Let's write a program with user input. Create `Greeting.java`:

```java
import java.util.Scanner;

public class Greeting {
    public static void main(String[] args) {
        // Create Scanner for user input
        Scanner scanner = new Scanner(System.in);

        // Get user's name
        System.out.print("What's your name? ");
        String name = scanner.nextLine();

        // Get user's age
        System.out.print("What's your age? ");
        int age = scanner.nextInt();

        // Display greeting
        System.out.println("Hello, " + name + "! Welcome to Java.");
        System.out.println("In 10 years, you'll be " + (age + 10) + " years old!");

        // Close scanner
        scanner.close();
    }
}
```

Compile and run:

```bash
javac Greeting.java
java Greeting
```

**Interaction**:

```
What's your name? Alice
What's your age? 25
Hello, Alice! Welcome to Java.
In 10 years, you'll be 35 years old!
```

**Code breakdown**:

- `import java.util.Scanner`: Imports Scanner class for input
- `Scanner scanner = new Scanner(System.in)`: Creates Scanner object
- `scanner.nextLine()`: Reads string input
- `scanner.nextInt()`: Reads integer input
- String concatenation with `+` operator

## Understanding Java Compilation Model

Java uses a two-step execution model:

1. **Compile**: `javac` converts `.java` (source) → `.class` (bytecode)
2. **Execute**: `java` runs bytecode on JVM

**Why this matters**:

- **Write Once, Run Anywhere**: Bytecode runs on any platform with JVM
- **Performance**: JVM optimizes bytecode at runtime (JIT compilation)
- **Security**: JVM provides sandboxed execution environment

**Contrast with interpreted languages** (Python, JavaScript):

- Python: `.py` → interpreter executes directly
- Java: `.java` → `.class` → JVM executes

## Summary

**What you've accomplished**:

- Installed Java Development Kit (JDK) on your operating system
- Verified Java installation with version and environment checks
- Understood JDK, JRE, and JVM components
- Wrote and compiled your first Java programs
- Executed Java bytecode using the JVM
- Learned Java's two-step compilation model

**Key commands learned**:

- `java -version` - Check Java runtime version
- `javac -version` - Check Java compiler version
- `javac <file>.java` - Compile Java source to bytecode
- `java <ClassName>` - Execute compiled Java program
- `java <file>.java` - Compile and run in one step (Java 11+)

**Skills gained**:

- Platform-specific JDK installation
- Setting JAVA_HOME environment variable
- Compiling and running Java programs
- Understanding Java compilation model

## Next Steps

**Ready to learn Java syntax and concepts?**

- [Quick Start](/en/learn/software-engineering/programming-languages/java/quick-start) (5-30% coverage) - Touch all core Java concepts in a fast-paced tour

**Want comprehensive fundamentals?**

**Prefer code-first learning?**

- [By-Example Tutorial](/en/learn/software-engineering/programming-languages/java/by-example) - Learn through heavily annotated examples

**Want to understand Java's design philosophy?**

- [Overview](/en/learn/software-engineering/programming-languages/java/overview) - Why Java exists and when to use it

## Troubleshooting Common Issues

### "java: command not found" or "javac: command not found"

**Problem**: Terminal doesn't recognize Java commands.

**Solution**:

- Verify JDK is installed: Check installation directory exists
- Set PATH environment variable to include `$JAVA_HOME/bin`
- Restart terminal after environment changes
- On Windows, ensure "Add to PATH" was checked during installation

### "Error: Could not find or load main class"

**Problem**: JVM can't find the compiled class.

**Solution**:

- Verify `.class` file exists: Run `ls` (Linux/macOS) or `dir` (Windows)
- Ensure class name matches: `java HelloWorld` for `HelloWorld.class`
- Don't include `.class` extension: Use `java HelloWorld`, not `java HelloWorld.class`
- Check current directory: Be in same directory as `.class` file

### Filename doesn't match class name

**Problem**: Compiler error about public class and filename.

**Solution**:

- File `HelloWorld.java` MUST contain `public class HelloWorld`
- Filenames are case-sensitive: `helloWorld.java` ≠ `HelloWorld.java`
- Only one public class per file (filename must match public class name)

### Multiple Java versions installed

**Problem**: `java -version` shows wrong version.

**Solution**:

- **Linux**: Use `update-alternatives`:

  ```bash
  sudo update-alternatives --config java
  sudo update-alternatives --config javac
  ```

- **macOS**: Use specific version with `JAVA_HOME`:

  ```bash
  export JAVA_HOME=$(/usr/libexec/java_home -v 21)
  ```

- **Windows**: Set `JAVA_HOME` to desired JDK directory in environment variables

### JAVA_HOME not set

**Problem**: Build tools (Maven, Gradle) require JAVA_HOME.

**Solution**:

- Set environment variable pointing to JDK installation:
  - **Linux/macOS**: Add to `~/.bashrc`: `export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64`
  - **Windows**: System Properties → Environment Variables → New → `JAVA_HOME` = JDK path

## Further Resources

**Official Java Documentation**:

- [Oracle Java Documentation](https://docs.oracle.com/en/java/) - Comprehensive Java docs
- [Java SE API Documentation](https://docs.oracle.com/en/java/javase/21/docs/api/) - Standard library reference
- [Java Tutorials](https://docs.oracle.com/javase/tutorial/) - Official learning path

**Development Tools**:

- [IntelliJ IDEA](https://www.jetbrains.com/idea/) - Popular Java IDE (Community Edition free)
- [Eclipse](https://www.eclipse.org/) - Open-source Java IDE
- [VS Code](https://code.visualstudio.com/) with [Java Extension Pack](https://marketplace.visualstudio.com/items?itemName=vscjava.vscode-java-pack)

**Build Tools**:

- [Maven](https://maven.apache.org/) - Dependency management and build automation
- [Gradle](https://gradle.org/) - Modern build tool for Java projects

**Community**:

- [Stack Overflow - Java](https://stackoverflow.com/questions/tagged/java) - Q&A community
- [/r/learnjava](https://www.reddit.com/r/learnjava/) - Reddit beginner community
- [Java Discord](https://discord.gg/java) - Real-time chat community
