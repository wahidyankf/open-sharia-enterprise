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

## Picocli Fundamentals

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
