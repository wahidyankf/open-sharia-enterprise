---
title: "Building CLI Applications"
date: 2026-02-03T00:00:00+07:00
draft: false
description: Comprehensive guide to building command-line applications in Java with argument parsing, input/output handling, and native compilation
weight: 10000014
tags: ["java", "cli", "command-line", "picocli", "graalvm", "terminal"]
---

## Why CLI Applications Matter

Command-line applications are essential for automation, DevOps tooling, build systems, and system administration. Java's portability and ecosystem make it excellent for building cross-platform CLI tools.

**Core Benefits**:

- **Automation**: Script repetitive tasks
- **DevOps tooling**: Build deployment pipelines
- **Data processing**: Transform and analyze files
- **System administration**: Manage infrastructure
- **Developer tools**: Create custom build and deployment tools

**Problem**: Building robust CLI apps requires argument parsing, error handling, configuration management, and proper exit codes - all tedious with raw System.out.

**Solution**: Use CLI frameworks and libraries to handle common patterns professionally.

## CLI Framework Comparison

| Framework              | Pros                                         | Cons                          | Use When                     |
| ---------------------- | -------------------------------------------- | ----------------------------- | ---------------------------- |
| **picocli**            | Annotation-based, feature-rich, autocomplete | Learning curve                | Complex CLI with subcommands |
| **Apache Commons CLI** | Simple, mature, lightweight                  | Verbose API, limited features | Simple argument parsing      |
| **JCommander**         | Annotation-based, simple                     | Less active development       | Medium complexity            |
| **Args4j**             | Lightweight, annotation-based                | Limited features              | Basic argument parsing       |
| **Raw args[]**         | No dependencies                              | Manual parsing, error-prone   | Trivial one-argument tools   |

**Recommendation**: Use picocli for production CLI applications - it's the modern standard with excellent GraalVM support.

**Recommended progression**: Start with raw args[] to understand CLI fundamentals → Learn System streams and exit codes → Use picocli for production CLIs.

## CLI Building with Standard Library

Java's standard library provides basic CLI building blocks through args[], System streams, and exit codes. Use these fundamentals before introducing frameworks.

### Manual Argument Parsing

Parse command-line arguments manually using the args[] array.

**Basic pattern**:

```java
public class SimpleGreeter {
    public static void main(String[] args) {
        // Check argument count
        if (args.length == 0) {
            System.err.println("Error: Name argument required");
            System.err.println("Usage: java SimpleGreeter <name>");
            System.exit(1);
        }

        // Extract argument
        String name = args[0];

        // Use argument
        System.out.println("Hello, " + name + "!");
        System.exit(0);
    }
}
```

**Usage**:

```bash
java SimpleGreeter Alice
# Output: Hello, Alice!

java SimpleGreeter
# Output: Error: Name argument required
#         Usage: java SimpleGreeter <name>
```

**Before**: No argument validation
**After**: Validated input with clear error messages

### Parsing Options (Flags)

Handle optional flags using string comparison and loops.

**Pattern**:

```java
public class GreeterWithOptions {
    public static void main(String[] args) {
        String name = null;
        int count = 1;
        boolean verbose = false;

        // Parse arguments
        for (int i = 0; i < args.length; i++) {
            String arg = args[i];

            if (arg.equals("-c") || arg.equals("--count")) {
                // Next argument is the count value
                if (i + 1 < args.length) {
                    count = Integer.parseInt(args[++i]);
                } else {
                    System.err.println("Error: -c/--count requires a value");
                    System.exit(1);
                }
            } else if (arg.equals("-v") || arg.equals("--verbose")) {
                verbose = true;
            } else if (arg.startsWith("-")) {
                System.err.println("Error: Unknown option: " + arg);
                System.exit(1);
            } else {
                // Positional argument (name)
                name = arg;
            }
        }

        // Validate required arguments
        if (name == null) {
            System.err.println("Error: Name argument required");
            System.exit(1);
        }

        // Execute command
        if (verbose) {
            System.out.println("Greeting " + name + " " + count + " times");
        }

        for (int i = 0; i < count; i++) {
            System.out.println("Hello, " + name + "!");
        }

        System.exit(0);
    }
}
```

**Usage**:

```bash
java GreeterWithOptions Alice
# Output: Hello, Alice!

java GreeterWithOptions Alice -c 3
# Output: Hello, Alice!
#         Hello, Alice!
#         Hello, Alice!

java GreeterWithOptions -v Alice --count 2
# Output: Greeting Alice 2 times
#         Hello, Alice!
#         Hello, Alice!
```

**Problem**: Manual option parsing requires verbose if/else chains.

**Solution**: Frameworks handle parsing automatically with annotations.

### System.out vs System.err

Distinguish between normal output and error messages using standard streams.

**Pattern**:

```java
public class StreamExample {
    public static void main(String[] args) {
        // Normal output to stdout
        System.out.println("Processing file: input.txt");
        System.out.println("Found 100 records");

        // Errors to stderr
        System.err.println("Warning: Duplicate record at line 42");
        System.err.println("Error: Invalid format at line 99");

        // Status to stdout
        System.out.println("Processing complete");
    }
}
```

**Running with stream redirection**:

```bash
# Redirect stdout to file, stderr to console
java StreamExample > output.txt
# Console shows: Warning: Duplicate record at line 42
#                Error: Invalid format at line 99
# output.txt contains: Processing file: input.txt
#                      Found 100 records
#                      Processing complete

# Redirect both streams separately
java StreamExample > output.txt 2> errors.txt
# output.txt contains normal output
# errors.txt contains error messages
```

**Stream conventions**:

- **stdout (System.out)**: Normal program output, data, results
- **stderr (System.err)**: Errors, warnings, diagnostic messages
- **stdin (System.in)**: User input (covered next)

**Problem**: Mixing output and errors makes filtering difficult.

**Solution**: Use stdout for data, stderr for errors.

### Reading User Input

Read interactive input from stdin using BufferedReader.

**Pattern**:

```java
import java.io.*;

public class InteractiveCLI {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(
            new InputStreamReader(System.in)
        );

        // Prompt for input
        System.out.print("Enter your name: ");
        String name = reader.readLine();

        System.out.print("Enter your age: ");
        String ageStr = reader.readLine();
        int age = Integer.parseInt(ageStr);

        // Process input
        System.out.println("\nHello, " + name + "!");
        System.out.println("You are " + age + " years old.");

        reader.close();
        System.exit(0);
    }
}
```

**Usage**:

```bash
java InteractiveCLI
# Enter your name: Alice
# Enter your age: 30
#
# Hello, Alice!
# You are 30 years old.
```

**Input validation**:

```java
import java.io.*;

public class ValidatedInput {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(
            new InputStreamReader(System.in)
        );

        int age = -1;
        while (age < 0 || age > 120) {
            System.out.print("Enter your age (0-120): ");
            try {
                age = Integer.parseInt(reader.readLine());
                if (age < 0 || age > 120) {
                    System.err.println("Error: Age must be between 0 and 120");
                }
            } catch (NumberFormatException e) {
                System.err.println("Error: Please enter a valid number");
            }
        }

        System.out.println("Valid age entered: " + age);
        reader.close();
    }
}
```

**Problem**: Manual validation requires try-catch and loops.

**Solution**: Frameworks provide built-in type conversion and validation.

### Exit Codes and Conventions

Signal success or failure to calling processes using exit codes.

**Pattern**:

```java
public class ExitCodeExample {
    public static void main(String[] args) {
        if (args.length == 0) {
            System.err.println("Usage: java ExitCodeExample <file>");
            System.exit(1);  // Error: Missing argument
            return;
        }

        String filename = args[0];
        java.io.File file = new java.io.File(filename);

        if (!file.exists()) {
            System.err.println("Error: File not found: " + filename);
            System.exit(2);  // Error: File not found
            return;
        }

        if (!file.canRead()) {
            System.err.println("Error: Cannot read file: " + filename);
            System.exit(3);  // Error: Permission denied
            return;
        }

        // Process file successfully
        System.out.println("Processing: " + filename);
        // ... file processing logic ...
        System.out.println("Success!");

        System.exit(0);  // Success
    }
}
```

**Exit code conventions**:

- **0**: Success (all operations completed)
- **1**: General error (invalid arguments, logic errors)
- **2**: Misuse of command (wrong arguments, missing file)
- **3-125**: Custom application errors
- **126**: Command cannot execute
- **127**: Command not found
- **128+N**: Fatal error signal N

**Use in scripts**:

```bash
#!/bin/bash

java ExitCodeExample myfile.txt
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    echo "Success!"
elif [ $EXIT_CODE -eq 1 ]; then
    echo "Usage error"
elif [ $EXIT_CODE -eq 2 ]; then
    echo "File not found"
else
    echo "Unknown error: $EXIT_CODE"
fi
```

**Problem**: No standard way to signal different error types.

**Solution**: Exit codes enable automation and error handling in scripts.

### Complete Example: File Processor CLI

Combine all standard library CLI patterns in realistic application.

**Pattern**:

```java
import java.io.*;
import java.nio.file.*;

public class FileProcessorCLI {
    public static void main(String[] args) {
        // Parse arguments
        if (args.length == 0) {
            printUsage();
            System.exit(1);
        }

        String inputFile = null;
        String outputFile = null;
        boolean verbose = false;

        for (int i = 0; i < args.length; i++) {
            String arg = args[i];

            if (arg.equals("-o") || arg.equals("--output")) {
                if (i + 1 < args.length) {
                    outputFile = args[++i];
                } else {
                    System.err.println("Error: -o/--output requires a file path");
                    System.exit(1);
                }
            } else if (arg.equals("-v") || arg.equals("--verbose")) {
                verbose = true;
            } else if (arg.equals("-h") || arg.equals("--help")) {
                printUsage();
                System.exit(0);
            } else if (arg.startsWith("-")) {
                System.err.println("Error: Unknown option: " + arg);
                printUsage();
                System.exit(1);
            } else {
                inputFile = arg;
            }
        }

        // Validate required arguments
        if (inputFile == null) {
            System.err.println("Error: Input file required");
            printUsage();
            System.exit(1);
        }

        // Process file
        try {
            processFile(inputFile, outputFile, verbose);
            System.exit(0);  // Success
        } catch (IOException e) {
            System.err.println("Error processing file: " + e.getMessage());
            System.exit(2);  // File error
        }
    }

    private static void processFile(String inputPath, String outputPath, boolean verbose)
            throws IOException {
        if (verbose) {
            System.out.println("Reading: " + inputPath);
        }

        // Read file
        Path input = Paths.get(inputPath);
        String content = Files.readString(input);

        // Process (example: convert to uppercase)
        String processed = content.toUpperCase();

        // Write output
        if (outputPath != null) {
            Files.writeString(Paths.get(outputPath), processed);
            if (verbose) {
                System.out.println("Wrote: " + outputPath);
            }
        } else {
            System.out.println(processed);
        }

        if (verbose) {
            System.out.println("Processing complete");
        }
    }

    private static void printUsage() {
        System.out.println("Usage: java FileProcessorCLI [OPTIONS] <input-file>");
        System.out.println();
        System.out.println("Options:");
        System.out.println("  -o, --output FILE   Write output to FILE instead of stdout");
        System.out.println("  -v, --verbose       Show verbose output");
        System.out.println("  -h, --help          Show this help message");
    }
}
```

**Usage examples**:

```bash
# Process file and print to stdout
java FileProcessorCLI input.txt

# Process file and write to output file
java FileProcessorCLI input.txt -o output.txt

# Verbose mode
java FileProcessorCLI -v input.txt --output output.txt
# Output: Reading: input.txt
#         Wrote: output.txt
#         Processing complete

# Show help
java FileProcessorCLI --help
# Output: Usage: java FileProcessorCLI [OPTIONS] <input-file>
#         ...
```

**Before**: Basic argument handling, no error handling
**After**: Production-ready CLI with options, help, validation, error codes

### Limitations of Standard Library Approach

Manual CLI building becomes unmaintainable for complex applications.

**Critical limitations**:

- **Verbose parsing**: 50+ lines for basic option parsing
- **No type conversion**: Manual parseInt(), parseDouble() everywhere
- **No validation**: Must write custom validation logic
- **No automatic help**: Help text manually synchronized with code
- **No subcommands**: Difficult to organize complex CLIs (like git)
- **Error-prone**: Easy to miss edge cases (missing values, invalid types)
- **No auto-completion**: Cannot generate shell completion scripts
- **Poor maintainability**: Changes require updating parsing logic and help text

**Real-world complexity**:

```java
// A production CLI tool requires:
// - Multiple subcommands (init, build, deploy, status)
// - Options with short/long forms (-v/--verbose)
// - Required vs optional parameters
// - Type conversion (String, int, File, URL)
// - Input validation (ranges, formats, file existence)
// - Automatic help generation
// - Version information
// - Shell completion scripts
// - Error messages with suggestions

// Standard library approach would require 500+ lines of boilerplate.
// Frameworks solve this with annotations and automatic generation.
```

**When standard library is acceptable**:

- Simple utilities (1-2 arguments, no options)
- Learning CLI fundamentals
- No dependencies constraint (embedded systems)
- Trivial automation scripts

**For production**: Use picocli or Apache Commons CLI (covered next).

## picocli - Modern CLI Framework (External Library)

Picocli uses annotations to define CLI structure, handling parsing, validation, and help generation automatically.

### Basic Command Structure

**Pattern**:

```java
import picocli.CommandLine;
import picocli.CommandLine.*;

@Command(name = "greet",
         description = "Greets users",
         version = "1.0.0")
class GreetCommand implements Runnable {

    @Parameters(index = "0", description = "Name to greet")
    private String name;

    @Option(names = {"-c", "--count"}, description = "Repetition count")
    private int count = 1;

    @Override
    public void run() {
        for (int i = 0; i < count; i++) {
            System.out.println("Hello, " + name + "!");
        }
    }

    public static void main(String[] args) {
        int exitCode = new CommandLine(new GreetCommand()).execute(args);
        System.exit(exitCode);
    }
}
```

**Usage**:

```bash
java GreetCommand Alice
# Output: Hello, Alice!

java GreetCommand Alice --count 3
# Output:
# Hello, Alice!
# Hello, Alice!
# Hello, Alice!
```

**Before**: Manual args parsing with if/else chains
**After**: Declarative annotations with automatic validation

### Options and Parameters

**Options** (named arguments):

```java
@Option(names = {"-v", "--verbose"}, description = "Verbose output")
boolean verbose;

@Option(names = {"-o", "--output"}, description = "Output file")
File outputFile;

@Option(names = {"-p", "--port"}, defaultValue = "8080")
int port;
```

**Parameters** (positional arguments):

```java
@Parameters(index = "0", description = "Source file")
File source;

@Parameters(index = "1..*", description = "Target files")
List<File> targets;
```

**Problem**: Manual parsing requires index tracking and type conversion.

**Solution**: Picocli handles parsing, type conversion, and bounds checking automatically.

### Subcommands

Organize complex CLIs with subcommands (like git: `git commit`, `git push`).

**Pattern**:

```java
@Command(name = "db",
         description = "Database operations",
         subcommands = {MigrateCommand.class, SeedCommand.class})
class DbCommand {}

@Command(name = "migrate", description = "Run migrations")
class MigrateCommand implements Runnable {
    @ParentCommand
    private DbCommand parent;

    @Override
    public void run() {
        System.out.println("Running migrations...");
    }
}

@Command(name = "seed", description = "Seed database")
class SeedCommand implements Runnable {
    @Override
    public void run() {
        System.out.println("Seeding database...");
    }
}
```

**Usage**:

```bash
java DbCommand migrate
java DbCommand seed
```

### Help Generation

Picocli generates help automatically from annotations.

**Pattern**:

```java
@Command(name = "app",
         description = "Application CLI",
         mixinStandardHelpOptions = true)  // Adds --help and --version
class App {}
```

**Usage**:

```bash
java App --help
# Output:
# Usage: app [-hV] <command>
# Application CLI
#   -h, --help      Show this help message
#   -V, --version   Print version information
```

## Input/Output Handling

### Reading User Input

**Interactive prompts**:

```java
import java.io.*;

BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

System.out.print("Enter name: ");
String name = reader.readLine();

System.out.print("Enter age: ");
int age = Integer.parseInt(reader.readLine());
```

**With picocli interactive option**:

```java
@Option(names = {"-p", "--password"},
        description = "Password",
        interactive = true)  // Prompts if not provided
char[] password;
```

**Problem**: System.in reading is verbose and requires exception handling.

**Solution**: Use picocli's interactive options or helper methods.

### Writing Output

**Standard output**:

```java
System.out.println("Normal output");      // stdout
System.err.println("Error output");       // stderr
```

**Formatted output**:

```java
System.out.printf("Name: %s, Age: %d%n", name, age);
```

**File output**:

```java
try (PrintWriter writer = new PrintWriter(new FileWriter(outputFile))) {
    writer.println("Output line");
} catch (IOException e) {
    System.err.println("Error writing file: " + e.getMessage());
    System.exit(1);
}
```

### Progress Indicators

**Simple progress**:

```java
for (int i = 0; i < total; i++) {
    processItem(i);
    System.err.printf("\rProcessing: %d/%d", i + 1, total);
}
System.err.println();  // New line after completion
```

**With ProgressBar library** (external):

```java
try (ProgressBar pb = new ProgressBar("Processing", total)) {
    for (int i = 0; i < total; i++) {
        processItem(i);
        pb.step();
    }
}
```

## Exit Codes

Use standard exit codes to communicate success/failure to shell scripts.

**Standard exit codes**:

- **0**: Success
- **1**: General error
- **2**: Misuse of command
- **126**: Command cannot execute
- **127**: Command not found
- **128+n**: Fatal error signal n

**Pattern**:

```java
public static void main(String[] args) {
    try {
        runApplication(args);
        System.exit(0);  // Success
    } catch (ValidationException e) {
        System.err.println("Error: " + e.getMessage());
        System.exit(2);  // Validation error
    } catch (Exception e) {
        System.err.println("Unexpected error: " + e.getMessage());
        e.printStackTrace();
        System.exit(1);  // General error
    }
}
```

**With picocli**:

```java
public static void main(String[] args) {
    int exitCode = new CommandLine(new App()).execute(args);
    System.exit(exitCode);
}
```

**Problem**: Hardcoded System.exit() makes testing difficult.

**Solution**: Picocli returns exit codes, allowing tests to verify without actually exiting.

## Configuration Management

### Command-line Configuration

**Environment variables**:

```java
String dbUrl = System.getenv("DATABASE_URL");
if (dbUrl == null) {
    dbUrl = "jdbc:postgresql://localhost/app";  // Default
}
```

**With picocli**:

```java
@Option(names = "--db-url",
        description = "Database URL",
        defaultValue = "${DATABASE_URL:-jdbc:postgresql://localhost/app}")
String dbUrl;
```

### Configuration Files

**Properties files**:

```java
Properties props = new Properties();
try (InputStream input = new FileInputStream("config.properties")) {
    props.load(input);
    String dbUrl = props.getProperty("db.url");
} catch (IOException e) {
    System.err.println("Error loading config: " + e.getMessage());
}
```

**YAML configuration** (with Jackson or SnakeYAML):

```java
ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
Config config = mapper.readValue(new File("config.yaml"), Config.class);
```

### Configuration Precedence

**Standard precedence order** (highest to lowest):

1. Command-line arguments
2. Environment variables
3. Configuration file
4. Defaults

**Pattern**:

```java
String getValue(String cliArg, String envVar, String configKey, String defaultValue) {
    if (cliArg != null) return cliArg;
    String env = System.getenv(envVar);
    if (env != null) return env;
    String config = loadFromConfig(configKey);
    if (config != null) return config;
    return defaultValue;
}
```

## Error Handling

### User-Friendly Error Messages

**Before** (developer-focused):

```
Exception in thread "main" java.lang.NullPointerException
    at App.run(App.java:42)
```

**After** (user-focused):

```
Error: File 'data.csv' not found
Please check the file path and try again.
```

**Pattern**:

```java
try {
    processFile(file);
} catch (FileNotFoundException e) {
    System.err.println("Error: File '" + file + "' not found");
    System.err.println("Please check the file path and try again.");
    System.exit(1);
} catch (IOException e) {
    System.err.println("Error reading file: " + e.getMessage());
    System.exit(1);
}
```

### Validation

**Input validation**:

```java
@Parameters(index = "0", description = "Port number")
@CommandLine.Range(min = 1, max = 65535)  // Picocli validation
int port;
```

**Custom validation**:

```java
if (!file.exists()) {
    throw new ParameterException(
        spec.commandLine(),
        "File does not exist: " + file);
}
```

## Testing CLI Applications

### Unit Testing Commands

**Pattern**:

```java
@Test
void testGreetCommand() {
    GreetCommand cmd = new GreetCommand();
    cmd.name = "Alice";
    cmd.count = 2;

    ByteArrayOutputStream out = new ByteArrayOutputStream();
    System.setOut(new PrintStream(out));

    cmd.run();

    String output = out.toString();
    assertThat(output).contains("Hello, Alice!");
    assertThat(output.split("\n")).hasSize(2);
}
```

### Integration Testing with picocli

**Pattern**:

```java
@Test
void testCommandLineExecution() {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    ByteArrayOutputStream err = new ByteArrayOutputStream();

    int exitCode = new CommandLine(new GreetCommand())
        .setOut(new PrintWriter(out, true))
        .setErr(new PrintWriter(err, true))
        .execute("Alice", "--count", "2");

    assertThat(exitCode).isEqualTo(0);
    assertThat(out.toString()).contains("Hello, Alice!");
}
```

### Testing Exit Codes

**Pattern**:

```java
@Test
void testInvalidArguments() {
    int exitCode = new CommandLine(new GreetCommand())
        .execute("--invalid-option");

    assertThat(exitCode).isEqualTo(2);  // Misuse of command
}
```

## Native Compilation with GraalVM

GraalVM native-image compiles Java to native executable for faster startup and lower memory usage.

### Why Native Compilation

**Benefits**:

- **Fast startup**: Milliseconds instead of seconds
- **Low memory**: No JVM heap overhead
- **Single executable**: No JRE required
- **Smaller footprint**: Better for containers

**Trade-offs**:

- **Build time**: Native compilation is slow (minutes)
- **Reflection limitations**: Requires configuration
- **No dynamic class loading**: AOT compilation only

### Building Native Image

**Install GraalVM**:

```bash
sdk install java 21.0.1-graalce
sdk use java 21.0.1-graalce
```

**Compile to native**:

```bash
native-image -jar app.jar app
# Creates 'app' executable
```

**With Maven**:

```xml
<plugin>
    <groupId>org.graalvm.buildtools</groupId>
    <artifactId>native-maven-plugin</artifactId>
    <version>0.10.3</version>
    <executions>
        <execution>
            <goals>
                <goal>compile-no-fork</goal>
            </goals>
        </execution>
    </executions>
</plugin>
```

```bash
mvn -Pnative native:compile
```

### Reflection Configuration

GraalVM requires reflection metadata for classes used reflectively.

**Automatic detection** (during build):

```bash
java -agentlib:native-image-agent=config-output-dir=META-INF/native-image \
     -jar app.jar
```

**Manual configuration** (META-INF/native-image/reflect-config.json):

```json
[
  {
    "name": "com.example.Config",
    "allDeclaredFields": true,
    "allDeclaredMethods": true
  }
]
```

**Picocli native support**:

```java
@Command(name = "app")
@GenerateNativeImage  // Picocli annotation for GraalVM
class App {}
```

## Best Practices

### 1. Use Standard Streams Correctly

**stdout** for normal output, **stderr** for errors and progress.

**Before**: All output to stdout
**After**: Data to stdout, errors/progress to stderr

### 2. Support Piping

Accept input from stdin, write output to stdout for Unix pipeline integration.

**Pattern**:

```java
BufferedReader reader;
if (inputFile != null) {
    reader = new BufferedReader(new FileReader(inputFile));
} else {
    reader = new BufferedReader(new InputStreamReader(System.in));
}
```

### 3. Provide Version Information

Always include version flag for debugging.

**Pattern**:

```java
@Command(mixinStandardHelpOptions = true,
         version = "myapp 1.2.3")
```

### 4. Handle Signals Gracefully

Clean up resources on SIGINT (Ctrl+C).

**Pattern**:

```java
Runtime.getRuntime().addShutdownHook(new Thread(() -> {
    System.err.println("\nShutting down gracefully...");
    cleanup();
}));
```

### 5. Use Color Sparingly

Color improves readability but must support plain terminals.

**With picocli ANSI colors**:

```java
System.out.println(Ansi.AUTO.string("@|green Success!|@"));
System.err.println(Ansi.AUTO.string("@|red Error!|@"));
```

### 6. Make Commands Idempotent

Running the same command twice should be safe.

**Example**: `db migrate` should skip already-applied migrations.

### 7. Provide Dry-Run Mode

Allow users to preview changes without applying them.

**Pattern**:

```java
@Option(names = "--dry-run", description = "Preview changes without applying")
boolean dryRun;

if (!dryRun) {
    applyChanges();
}
```

## Common CLI Patterns

### File Processing

**Pattern**:

```java
@Parameters(description = "Input files")
List<File> files;

for (File file : files) {
    processFile(file);
}
```

### Batch Operations

**Pattern**:

```java
@Option(names = {"-b", "--batch"}, description = "Batch size")
int batchSize = 100;

List<Item> batch = new ArrayList<>(batchSize);
for (Item item : items) {
    batch.add(item);
    if (batch.size() >= batchSize) {
        processBatch(batch);
        batch.clear();
    }
}
if (!batch.isEmpty()) {
    processBatch(batch);  // Process remaining
}
```

### Watch Mode

**Pattern**:

```java
@Option(names = {"-w", "--watch"}, description = "Watch for changes")
boolean watch;

do {
    processFiles();
    if (watch) {
        Thread.sleep(1000);
    }
} while (watch);
```

## Example: Complete CLI Application

**Real-world CSV processor**:

```java
@Command(name = "csvtool",
         description = "CSV processing tool",
         mixinStandardHelpOptions = true,
         version = "1.0.0")
class CsvTool implements Runnable {

    @Parameters(description = "Input CSV file")
    File inputFile;

    @Option(names = {"-o", "--output"}, description = "Output file")
    File outputFile;

    @Option(names = {"-f", "--filter"}, description = "Filter column:value")
    String filter;

    @Override
    public void run() {
        try {
            processCsv();
        } catch (IOException e) {
            System.err.println("Error: " + e.getMessage());
            System.exit(1);
        }
    }

    private void processCsv() throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(inputFile));
             PrintWriter writer = outputFile != null
                 ? new PrintWriter(new FileWriter(outputFile))
                 : new PrintWriter(System.out)) {

            String line;
            while ((line = reader.readLine()) != null) {
                if (filter == null || matchesFilter(line)) {
                    writer.println(processLine(line));
                }
            }
        }
    }

    private boolean matchesFilter(String line) {
        // Filter logic
        return true;
    }

    private String processLine(String line) {
        // Processing logic
        return line.toUpperCase();
    }

    public static void main(String[] args) {
        int exitCode = new CommandLine(new CsvTool()).execute(args);
        System.exit(exitCode);
    }
}
```

## Related Content

### Core Java Topics

- **[Java Best Practices](/en/learn/software-engineering/programming-languages/java/in-practice/best-practices)** - General coding standards
- **[Test-Driven Development](/en/learn/software-engineering/programming-languages/java/in-practice/test-driven-development)** - Testing CLI apps

### External Resources

**CLI Frameworks**:

- [Picocli](https://picocli.info/) - Modern CLI framework
- [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/) - Classic CLI parsing
- [JCommander](https://jcommander.org/) - Annotation-based parsing

**Native Compilation**:

- [GraalVM](https://www.graalvm.org/) - Native image compilation
- [GraalVM Native Image](https://www.graalvm.org/latest/reference-manual/native-image/) - Documentation

**Libraries**:

- [JLine](https://github.com/jline/jline3) - Terminal input/output
- [ProgressBar](https://github.com/ctongfei/progressbar) - Progress indicators
- [Jansi](https://github.com/fusesource/jansi) - ANSI color support

---

**Last Updated**: 2026-02-03
**Java Version**: 17+ (baseline), 21+ (recommended)
**Framework Versions**: Picocli 4.7.6, GraalVM 21.0.1
