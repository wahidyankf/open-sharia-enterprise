---
title: "How to Build CLI Applications"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 614
description: "Create command-line applications with picocli, proper argument parsing, and executable JAR packaging"
tags: ["java", "cli", "command-line", "picocli", "jar"]
---

## Problem

Command-line applications need robust argument parsing, help text generation, validation, and error handling. Manual argument parsing becomes complex with multiple options, flags, and subcommands. Users expect standard CLI behaviors like --help, --version, and clear error messages.

This guide shows how to build professional CLI applications in Java.

## Picocli Framework

### Basic CLI Application

```java
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;

import java.util.concurrent.Callable;

// ✅ Simple CLI with picocli
@Command(
  name = "greet",
  mixinStandardHelpOptions = true,
  version = "1.0",
  description = "Greets users with customizable message"
)
public class GreetCommand implements Callable<Integer> {

  @Parameters(
    index = "0",
    description = "Name of the person to greet"
  )
  private String name;

  @Option(
    names = {"-c", "--capitalized"},
    description = "Capitalize the greeting"
  )
  private boolean capitalized;

  @Option(
    names = {"-r", "--repeat"},
    description = "Number of times to repeat greeting",
    defaultValue = "1"
  )
  private int repeat;

  @Override
  public Integer call() {
    String greeting = capitalized
      ? "HELLO, " + name.toUpperCase() + "!"
      : "Hello, " + name + "!";

    for (int i = 0; i < repeat; i++) {
      System.out.println(greeting);
    }

    return 0; // Exit code 0 = success
  }

  public static void main(String[] args) {
    int exitCode = new CommandLine(new GreetCommand()).execute(args);
    System.exit(exitCode);
  }
}
```

**Usage:**

```bash
# Basic usage
java -jar greet.jar Alice
# Output: Hello, Alice!

# With options
java -jar greet.jar --capitalized --repeat 3 Bob
# Output:
# HELLO, BOB!
# HELLO, BOB!
# HELLO, BOB!

# Help text (automatically generated)
java -jar greet.jar --help
# Shows usage, parameters, and options

# Version
java -jar greet.jar --version
# Shows: 1.0
```

**Why picocli**: Annotation-based configuration, automatic help generation, type-safe option handling, subcommands support, and comprehensive validation.

### Argument Types and Validation

```java
@Command(name = "process", description = "Process data files")
public class ProcessCommand implements Callable<Integer> {

  // ✅ File parameter with validation
  @Parameters(
    index = "0",
    description = "Input file to process"
  )
  private File inputFile;

  // ✅ Multiple values (array)
  @Option(
    names = {"-f", "--format"},
    description = "Output formats (valid: ${COMPLETION-CANDIDATES})",
    split = ","
  )
  private Format[] formats = {Format.JSON};

  enum Format { JSON, XML, CSV }

  // ✅ Required option
  @Option(
    names = {"-o", "--output"},
    description = "Output directory",
    required = true
  )
  private File outputDir;

  // ✅ Numeric range validation
  @Option(
    names = {"-t", "--threads"},
    description = "Number of threads (1-16)",
    defaultValue = "4"
  )
  private int threads;

  // ✅ Custom validation
  @Override
  public Integer call() {
    // Validate input file exists
    if (!inputFile.exists()) {
      System.err.println("Error: Input file does not exist: " + inputFile);
      return 1;
    }

    // Validate output directory
    if (!outputDir.exists() && !outputDir.mkdirs()) {
      System.err.println("Error: Cannot create output directory: " + outputDir);
      return 1;
    }

    // Validate thread count
    if (threads < 1 || threads > 16) {
      System.err.println("Error: Thread count must be between 1 and 16");
      return 1;
    }

    processFile(inputFile, outputDir, formats, threads);
    return 0;
  }

  private void processFile(File input, File output, Format[] formats, int threads) {
    System.out.printf("Processing %s with %d threads%n", input, threads);
    System.out.printf("Output directory: %s%n", output);
    System.out.printf("Formats: %s%n", Arrays.toString(formats));
    // Implementation...
  }

  public static void main(String[] args) {
    System.exit(new CommandLine(new ProcessCommand()).execute(args));
  }
}
```

### Subcommands

```java
// ✅ Main command with subcommands
@Command(
  name = "user",
  mixinStandardHelpOptions = true,
  description = "User management CLI",
  subcommands = {
    UserCreateCommand.class,
    UserListCommand.class,
    UserDeleteCommand.class
  }
)
public class UserCommand implements Callable<Integer> {

  @Override
  public Integer call() {
    System.err.println("Please specify a subcommand. Use --help for usage.");
    return 1;
  }

  public static void main(String[] args) {
    System.exit(new CommandLine(new UserCommand()).execute(args));
  }
}

// ✅ Create subcommand
@Command(
  name = "create",
  description = "Create a new user"
)
class UserCreateCommand implements Callable<Integer> {

  @Option(names = {"-e", "--email"}, required = true, description = "User email")
  private String email;

  @Option(names = {"-n", "--name"}, required = true, description = "User name")
  private String name;

  @Option(names = {"--admin"}, description = "Create as admin user")
  private boolean admin;

  @Override
  public Integer call() {
    System.out.printf("Creating user: %s <%s>%s%n",
      name, email, admin ? " (admin)" : "");
    // Create user...
    System.out.println("User created successfully");
    return 0;
  }
}

// ✅ List subcommand
@Command(
  name = "list",
  description = "List all users"
)
class UserListCommand implements Callable<Integer> {

  @Option(names = {"--admin-only"}, description = "Show only admin users")
  private boolean adminOnly;

  @Option(names = {"-l", "--limit"}, description = "Limit number of results", defaultValue = "10")
  private int limit;

  @Override
  public Integer call() {
    System.out.printf("Listing %s users (limit: %d)%n",
      adminOnly ? "admin" : "all", limit);
    // List users...
    return 0;
  }
}

// ✅ Delete subcommand
@Command(
  name = "delete",
  description = "Delete a user"
)
class UserDeleteCommand implements Callable<Integer> {

  @Parameters(index = "0", description = "User ID to delete")
  private String userId;

  @Option(names = {"-f", "--force"}, description = "Skip confirmation")
  private boolean force;

  @Override
  public Integer call() {
    if (!force) {
      System.out.print("Are you sure? (y/N): ");
      Scanner scanner = new Scanner(System.in);
      String response = scanner.nextLine();
      if (!response.equalsIgnoreCase("y")) {
        System.out.println("Cancelled");
        return 0;
      }
    }

    System.out.printf("Deleting user: %s%n", userId);
    // Delete user...
    System.out.println("User deleted successfully");
    return 0;
  }
}
```

**Usage:**

```bash
# List subcommands
java -jar user.jar --help

# Create user
java -jar user.jar create --email alice@example.com --name Alice

# List users
java -jar user.jar list --admin-only --limit 5

# Delete user
java -jar user.jar delete user-123 --force
```

## Exit Codes and Error Handling

### Standard Exit Codes

```java
public class ExitCodes {
  public static final int SUCCESS = 0;
  public static final int GENERAL_ERROR = 1;
  public static final int INVALID_ARGUMENTS = 2;
  public static final int FILE_NOT_FOUND = 3;
  public static final int PERMISSION_DENIED = 4;
  public static final int NETWORK_ERROR = 5;
}

@Command(name = "backup")
public class BackupCommand implements Callable<Integer> {

  @Parameters(index = "0", description = "Source directory")
  private File source;

  @Parameters(index = "1", description = "Destination directory")
  private File destination;

  @Override
  public Integer call() {
    // Validate source exists
    if (!source.exists()) {
      System.err.println("Error: Source directory does not exist: " + source);
      return ExitCodes.FILE_NOT_FOUND;
    }

    // Validate source is readable
    if (!source.canRead()) {
      System.err.println("Error: Cannot read source directory: " + source);
      return ExitCodes.PERMISSION_DENIED;
    }

    // Validate destination is writable
    if (destination.exists() && !destination.canWrite()) {
      System.err.println("Error: Cannot write to destination: " + destination);
      return ExitCodes.PERMISSION_DENIED;
    }

    try {
      performBackup(source, destination);
      System.out.println("Backup completed successfully");
      return ExitCodes.SUCCESS;

    } catch (IOException e) {
      System.err.println("Error during backup: " + e.getMessage());
      return ExitCodes.GENERAL_ERROR;
    }
  }

  private void performBackup(File source, File dest) throws IOException {
    // Implementation...
  }
}
```

### Graceful Error Messages

```java
@Command(name = "convert")
public class ConvertCommand implements Callable<Integer> {

  @Parameters(index = "0", description = "Input file")
  private File inputFile;

  @Option(names = {"-f", "--format"}, description = "Output format", required = true)
  private String format;

  @Override
  public Integer call() {
    try {
      validateInputs();
      convert(inputFile, format);
      return 0;

    } catch (FileNotFoundException e) {
      printError("File not found: " + e.getMessage());
      return 3;

    } catch (UnsupportedFormatException e) {
      printError("Unsupported format: " + format);
      printError("Supported formats: JSON, XML, CSV");
      return 2;

    } catch (Exception e) {
      printError("Conversion failed: " + e.getMessage());
      if (System.getenv("DEBUG") != null) {
        e.printStackTrace(System.err);
      }
      return 1;
    }
  }

  private void validateInputs() throws FileNotFoundException {
    if (!inputFile.exists()) {
      throw new FileNotFoundException(inputFile.getPath());
    }
  }

  private void convert(File input, String format) throws Exception {
    // Implementation...
  }

  private void printError(String message) {
    System.err.println("Error: " + message);
  }
}
```

## stdin/stdout/stderr Usage

### Reading from stdin

```java
@Command(name = "filter", description = "Filter lines from input")
public class FilterCommand implements Callable<Integer> {

  @Option(names = {"-p", "--pattern"}, description = "Pattern to match", required = true)
  private String pattern;

  @Option(names = {"-i", "--input"}, description = "Input file (default: stdin)")
  private File inputFile;

  @Override
  public Integer call() throws IOException {
    Pattern regex = Pattern.compile(pattern);

    try (BufferedReader reader = createReader()) {
      String line;
      while ((line = reader.readLine()) != null) {
        if (regex.matcher(line).find()) {
          System.out.println(line); // stdout for output
        }
      }
    }

    return 0;
  }

  private BufferedReader createReader() throws IOException {
    if (inputFile != null) {
      return new BufferedReader(new FileReader(inputFile));
    } else {
      // Read from stdin
      return new BufferedReader(new InputStreamReader(System.in));
    }
  }
}
```

**Usage:**

```bash
# From file
java -jar filter.jar --pattern "ERROR" --input app.log

# From stdin (pipe)
cat app.log | java -jar filter.jar --pattern "ERROR"

# From stdin (redirect)
java -jar filter.jar --pattern "ERROR" < app.log
```

### Writing to stdout and stderr

```java
@Command(name = "process")
public class ProcessCommand implements Callable<Integer> {

  @Option(names = {"-v", "--verbose"}, description = "Verbose output")
  private boolean verbose;

  @Override
  public Integer call() {
    // ✅ Normal output to stdout
    System.out.println("Processing started");

    // ✅ Errors to stderr
    try {
      processData();
    } catch (Exception e) {
      System.err.println("ERROR: " + e.getMessage());
      return 1;
    }

    // ✅ Verbose/diagnostic info to stderr
    if (verbose) {
      System.err.println("Processed 1000 items in 5 seconds");
    }

    // ✅ Result to stdout
    System.out.println("Processing completed");
    return 0;
  }

  private void processData() throws Exception {
    // Implementation...
  }
}
```

**Why stdout vs stderr**: stdout for program output (can be piped), stderr for diagnostic messages and errors (visible to user but not piped).

## JAR Packaging

### Creating Executable JAR

**Maven configuration (pom.xml):**

```xml
<build>
  <plugins>
    <!-- Create executable JAR -->
    <plugin>
      <groupId>org.apache.maven.plugins</groupId>
      <artifactId>maven-jar-plugin</artifactId>
      <version>3.3.0</version>
      <configuration>
        <archive>
          <manifest>
            <mainClass>com.example.MyCommand</mainClass>
          </manifest>
        </archive>
      </configuration>
    </plugin>

    <!-- Include dependencies in JAR (fat JAR) -->
    <plugin>
      <groupId>org.apache.maven.plugins</groupId>
      <artifactId>maven-shade-plugin</artifactId>
      <version>3.5.0</version>
      <executions>
        <execution>
          <phase>package</phase>
          <goals>
            <goal>shade</goal>
          </goals>
          <configuration>
            <transformers>
              <transformer implementation="org.apache.maven.plugins.shade.resource.ManifestResourceTransformer">
                <mainClass>com.example.MyCommand</mainClass>
              </transformer>
            </transformers>
          </configuration>
        </execution>
      </executions>
    </plugin>
  </plugins>
</build>
```

**Build and run:**

```bash
# Build JAR
mvn clean package

# Run JAR
java -jar target/myapp-1.0.jar --help
```

### Shell Script Wrapper

```bash
#!/bin/bash
# mycli - Wrapper script for Java CLI application

# Find Java
if [ -n "$JAVA_HOME" ]; then
  JAVA="$JAVA_HOME/bin/java"
else
  JAVA="java"
fi

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
JAR_FILE="$SCRIPT_DIR/myapp.jar"

# Check JAR exists
if [ ! -f "$JAR_FILE" ]; then
  echo "Error: JAR file not found: $JAR_FILE" >&2
  exit 1
fi

# Run application
exec "$JAVA" -jar "$JAR_FILE" "$@"
```

**Installation:**

```bash
# Make executable
chmod +x mycli

# Install to PATH
sudo cp mycli /usr/local/bin/

# Use like native command
mycli --help
mycli process input.txt
```

## GraalVM Native Image

### Creating Native Binaries

**Maven configuration:**

```xml
<profiles>
  <profile>
    <id>native</id>
    <build>
      <plugins>
        <plugin>
          <groupId>org.graalvm.buildtools</groupId>
          <artifactId>native-maven-plugin</artifactId>
          <version>0.9.28</version>
          <executions>
            <execution>
              <id>build-native</id>
              <goals>
                <goal>compile-no-fork</goal>
              </goals>
              <phase>package</phase>
            </execution>
          </executions>
          <configuration>
            <mainClass>com.example.MyCommand</mainClass>
            <imageName>mycli</imageName>
          </configuration>
        </plugin>
      </plugins>
    </build>
  </profile>
</profiles>
```

**Build native binary:**

```bash
# Requires GraalVM installed
mvn clean package -Pnative

# Creates native executable
./target/mycli --help

# Fast startup, no JVM required
time ./target/mycli --version
# ~0.01s startup time (vs ~0.5s for JAR)
```

**Benefits of native image:**

- Near-instant startup (important for CLI tools)
- Lower memory footprint
- No JVM required on target system
- Single executable file

## Testing CLI Applications

### Unit Testing Commands

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class GreetCommandTest {

  @Test
  void shouldGreetUser() {
    GreetCommand command = new GreetCommand();
    command.name = "Alice";
    command.capitalized = false;
    command.repeat = 1;

    int exitCode = command.call();

    assertEquals(0, exitCode);
  }

  @Test
  void shouldCapitalizeGreeting() {
    GreetCommand command = new GreetCommand();
    command.name = "Bob";
    command.capitalized = true;
    command.repeat = 1;

    int exitCode = command.call();

    assertEquals(0, exitCode);
  }

  @Test
  void shouldRepeatGreeting() {
    GreetCommand command = new GreetCommand();
    command.name = "Charlie";
    command.capitalized = false;
    command.repeat = 3;

    int exitCode = command.call();

    assertEquals(0, exitCode);
  }
}
```

### Integration Testing with CommandLine

```java
@Test
void shouldParseArguments() {
  GreetCommand command = new GreetCommand();
  CommandLine cmd = new CommandLine(command);

  int exitCode = cmd.execute("Alice", "--capitalized", "--repeat", "2");

  assertEquals(0, exitCode);
  assertEquals("Alice", command.name);
  assertTrue(command.capitalized);
  assertEquals(2, command.repeat);
}

@Test
void shouldShowHelpText() {
  StringWriter sw = new StringWriter();
  CommandLine cmd = new CommandLine(new GreetCommand());
  cmd.setOut(new PrintWriter(sw));

  int exitCode = cmd.execute("--help");

  assertEquals(0, exitCode);
  assertTrue(sw.toString().contains("Greets users"));
}

@Test
void shouldFailOnMissingRequiredArgument() {
  CommandLine cmd = new CommandLine(new GreetCommand());

  int exitCode = cmd.execute(); // Missing name parameter

  assertEquals(2, exitCode); // Picocli returns 2 for usage errors
}
```

## Summary

Building CLI applications in Java requires robust argument parsing, clear help text, proper error handling, and standard exit codes. Picocli simplifies CLI development through annotations that declare parameters, options, and validation rules. The framework automatically generates help text, validates inputs, and handles common CLI patterns.

Command structure uses @Command annotation for configuration, @Parameters for positional arguments, and @Option for named options. Implement Callable<Integer> and return exit codes - 0 for success, non-zero for errors. Picocli handles the parsing and invokes your call() method with populated fields.

Subcommands organize complex CLIs into logical groups. Main command declares subcommands, each subcommand implements its own logic. Users run `app subcommand --options`, mirroring git-style interfaces. Each subcommand has independent parameters, options, and help text.

Exit codes communicate success or failure to shell scripts and calling processes. Use standard conventions - 0 for success, 1 for general errors, 2 for invalid arguments, specific codes for domain errors. Shell scripts check exit codes to determine if operations succeeded.

stdin/stdout/stderr serve different purposes. stdout carries program output suitable for piping to other commands. stderr carries diagnostic messages and errors visible to users but not piped. Reading from stdin enables CLI tools to work in pipelines - `cat file | filter | process`.

JAR packaging with maven-shade-plugin creates fat JARs containing all dependencies. Single JAR file runs anywhere with Java installed. Shell script wrappers make JARs feel like native commands. GraalVM native-image compiles to native binaries with instant startup and no JVM requirement.

Testing CLI applications involves unit testing command logic and integration testing argument parsing. Test that commands parse arguments correctly, validate inputs, and return appropriate exit codes. Capture stdout/stderr to verify output messages.

Professional CLI applications follow Unix conventions - help text with --help, version with --version, exit codes indicating success/failure, stdin/stdout/stderr for proper I/O, and clear error messages. Picocli handles most conventions automatically, letting you focus on business logic.

## Related Content

- [Java Best Practices and Design Principles](/en/learn/swe/prog-lang/java/explanation/best-practices)
- [How to Handle Files and Resources](/en/learn/swe/prog-lang/java/how-to/handle-files-and-resources)
- [How to Implement Proper Exception Handling](/en/learn/swe/prog-lang/java/how-to/exception-handling)
- [How to Write Effective Tests](/en/learn/swe/prog-lang/java/how-to/write-effective-tests)
