---
title: "Linting and Formatting"
date: 2026-02-03T00:00:00+07:00
draft: false
description: Comprehensive guide to code quality tools from manual reviews to automated linting, formatting, and static analysis
weight: 10000002
tags: ["java", "code-quality", "checkstyle", "spotbugs", "pmd", "sonarqube"]
---

## Why Code Quality Tools Matter

Code quality tools automate detection of bugs, code smells, style violations, and security vulnerabilities. Manual code reviews alone cannot consistently catch all issues.

**Core Benefits**:

- **Early bug detection**: Find bugs before they reach production
- **Consistency**: Enforce coding standards across team
- **Security**: Detect common vulnerabilities automatically
- **Maintainability**: Prevent code smell accumulation
- **Learning**: Teams learn from automated feedback

**Problem**: Manual code reviews are time-consuming, inconsistent, and miss subtle bugs that automated tools catch immediately.

**Solution**: Use automated linting, formatting, and static analysis tools integrated into build process and IDE.

## Code Quality Tool Comparison

| Tool                   | Type                 | Pros                                  | Cons                         | Use When                             |
| ---------------------- | -------------------- | ------------------------------------- | ---------------------------- | ------------------------------------ |
| **Checkstyle**         | Style checker        | Fast, configurable, widely adopted    | Style only (no bugs)         | Enforce coding standards             |
| **SpotBugs**           | Bug detector         | Finds real bugs, low false positives  | Limited to common patterns   | Detect common bugs                   |
| **PMD**                | Code smell detector  | Detects design issues, extensible     | More false positives         | Find code smells                     |
| **Error Prone**        | Compile-time checker | Fast (compile-time), Google-backed    | Limited to javac integration | Catch bugs during compilation        |
| **SonarQube**          | Quality platform     | Comprehensive, metrics, history       | Resource-intensive           | Enterprise quality governance        |
| **Google Java Format** | Formatter            | Deterministic, no config needed       | No customization             | Consistent formatting (Google style) |
| **Manual reviews**     | Human inspection     | Context-aware, architectural insights | Time-consuming, inconsistent | Architecture and design decisions    |

**Recommendation**: Use Checkstyle (style), SpotBugs (bugs), PMD (code smells), and Google Java Format (formatting) in combination. Add SonarQube for centralized quality metrics.

**Recommended progression**: Start with manual reviews to understand quality issues → Add automated tools to catch common problems → Integrate comprehensive quality platform.

## Manual Code Reviews

Human code reviews provide context-aware feedback that automated tools cannot replicate. Use checklists to ensure consistency.

### Code Review Checklist

**Correctness**:

- Logic implements requirements correctly
- Edge cases handled (null, empty, boundary values)
- Error handling appropriate for failure scenarios
- No off-by-one errors in loops or array access

**Design**:

- Single Responsibility Principle followed
- Dependencies injected rather than hard-coded
- Appropriate abstraction level
- No premature optimization

**Testing**:

- Unit tests cover core logic
- Test names clearly describe scenarios
- Test data realistic and varied
- No test interdependencies

**Security**:

- Input validation at system boundaries
- No SQL injection or XSS vulnerabilities
- Sensitive data not logged or exposed
- Authentication and authorization appropriate

**Performance**:

- No obvious performance bottlenecks
- Database queries efficient (no N+1 queries)
- Resources properly closed (files, connections)
- Appropriate data structures chosen

**Maintainability**:

- Code self-documenting with clear names
- Complex logic explained with comments
- No code duplication
- Consistent with codebase style

### Why Manual Reviews Are Insufficient

**Limitations**:

1. **Time-consuming**: Thorough reviews take significant time
2. **Inconsistent**: Different reviewers have different standards
3. **Fatigue**: Humans miss issues in large changesets
4. **Bias**: Reviewers may overlook familiar patterns
5. **Scale**: Cannot review every line in large codebases
6. **Knowledge gaps**: Reviewers may not know all anti-patterns

**Before**: Manual reviews only, inconsistent quality
**After**: Automated tools catch common issues, humans focus on architecture and design

## Checkstyle

Checkstyle enforces coding standards and style conventions. It checks formatting, naming, imports, and other stylistic aspects.

### Adding Checkstyle

**Maven**:

```xml
<plugin>
    <groupId>org.apache.maven.plugins</groupId>
    <artifactId>maven-checkstyle-plugin</artifactId>
    <version>3.3.1</version>
    <configuration>
        <configLocation>checkstyle.xml</configLocation>
        <consoleOutput>true</consoleOutput>
        <failsOnError>true</failsOnError>
    </configuration>
    <executions>
        <execution>
            <goals>
                <goal>check</goal>
            </goals>
        </execution>
    </executions>
</plugin>
```

**Gradle**:

```groovy
plugins {
    id 'checkstyle'
}

checkstyle {
    toolVersion = '10.12.7'
    configFile = file("${rootDir}/checkstyle.xml")
}

// Fail build on violations
tasks.withType(Checkstyle) {
    reports {
        xml.required = true
        html.required = true
    }
}
```

### Checkstyle Configuration

Create checkstyle.xml with rules to enforce.

**Basic checkstyle.xml**:

```xml
<?xml version="1.0"?>
<!DOCTYPE module PUBLIC
    "-//Checkstyle//DTD Checkstyle Configuration 1.3//EN"
    "https://checkstyle.org/dtds/configuration_1_3.dtd">

<module name="Checker">
    <property name="charset" value="UTF-8"/>
    <property name="severity" value="error"/>
    <property name="fileExtensions" value="java"/>

    <!-- TreeWalker checks individual Java source files -->
    <module name="TreeWalker">
        <!-- Naming Conventions -->
        <module name="TypeName">
            <property name="format" value="^[A-Z][a-zA-Z0-9]*$"/>
        </module>
        <module name="MethodName">
            <property name="format" value="^[a-z][a-zA-Z0-9]*$"/>
        </module>
        <module name="ConstantName">
            <property name="format" value="^[A-Z][A-Z0-9]*(_[A-Z0-9]+)*$"/>
        </module>
        <module name="LocalVariableName">
            <property name="format" value="^[a-z][a-zA-Z0-9]*$"/>
        </module>
        <module name="PackageName">
            <property name="format" value="^[a-z]+(\.[a-z][a-z0-9]*)*$"/>
        </module>

        <!-- Imports -->
        <module name="AvoidStarImport"/>
        <module name="UnusedImports"/>
        <module name="RedundantImport"/>
        <module name="ImportOrder">
            <property name="groups" value="java,javax,org,com"/>
            <property name="ordered" value="true"/>
            <property name="separated" value="true"/>
        </module>

        <!-- Whitespace -->
        <module name="WhitespaceAround"/>
        <module name="WhitespaceAfter"/>
        <module name="NoWhitespaceBefore"/>
        <module name="EmptyLineSeparator">
            <property name="allowNoEmptyLineBetweenFields" value="true"/>
        </module>

        <!-- Blocks -->
        <module name="LeftCurly"/>
        <module name="RightCurly"/>
        <module name="NeedBraces"/>
        <module name="EmptyBlock"/>

        <!-- Coding -->
        <module name="EmptyStatement"/>
        <module name="EqualsHashCode"/>
        <module name="SimplifyBooleanExpression"/>
        <module name="SimplifyBooleanReturn"/>
        <module name="StringLiteralEquality"/>
        <module name="OneStatementPerLine"/>
        <module name="MultipleVariableDeclarations"/>

        <!-- Design -->
        <module name="FinalClass"/>
        <module name="HideUtilityClassConstructor"/>
        <module name="InterfaceIsType"/>
        <module name="VisibilityModifier"/>

        <!-- Miscellaneous -->
        <module name="ArrayTypeStyle"/>
        <module name="UpperEll"/>
        <module name="ModifierOrder"/>
    </module>

    <!-- File checks -->
    <module name="FileTabCharacter"/>
    <module name="NewlineAtEndOfFile"/>
</module>
```

### Common Checkstyle Rules

**Naming conventions**:

```java
// GOOD: Class name PascalCase
public class PaymentService { }

// BAD: Class name lowercase
public class paymentService { }

// GOOD: Method name camelCase
public void processPayment() { }

// BAD: Method name PascalCase
public void ProcessPayment() { }

// GOOD: Constant UPPER_SNAKE_CASE
private static final int MAX_RETRIES = 3;

// BAD: Constant camelCase
private static final int maxRetries = 3;
```

**Import rules**:

```java
// BAD: Star imports
import java.util.*;
import com.example.*;

// GOOD: Explicit imports
import java.util.List;
import java.util.ArrayList;
import com.example.PaymentService;

// BAD: Unused import
import java.util.Map; // Not used in code

// GOOD: Only used imports
import java.util.List;
```

**Whitespace rules**:

```java
// GOOD: Proper spacing
public class Example {
    public void method(int x, int y) {
        if (x > 0) {
            return;
        }
    }
}

// BAD: Missing spaces
public class Example{
    public void method(int x,int y){
        if(x>0){
            return;
        }
    }
}
```

### Running Checkstyle

```bash
# Maven
mvn checkstyle:check

# Generate HTML report
mvn checkstyle:checkstyle
# Report: target/site/checkstyle.html

# Gradle
./gradlew checkstyleMain checkstyleTest

# Reports: build/reports/checkstyle/
```

### Suppressing Violations

Suppress specific violations when necessary (use sparingly).

**Suppress in code**:

```java
// Suppress specific check
@SuppressWarnings("checkstyle:MagicNumber")
public void calculate() {
    int result = value * 100; // Magic number acceptable here
}

// Suppress for entire file
// checkstyle:off
public class LegacyCode {
    // Legacy code with many violations
}
// checkstyle:on
```

**Suppress in configuration**:

```xml
<module name="SuppressionFilter">
    <property name="file" value="checkstyle-suppressions.xml"/>
</module>
```

**checkstyle-suppressions.xml**:

```xml
<?xml version="1.0"?>
<!DOCTYPE suppressions PUBLIC
    "-//Checkstyle//DTD SuppressionFilter Configuration 1.2//EN"
    "https://checkstyle.org/dtds/suppressions_1_2.dtd">

<suppressions>
    <!-- Suppress all checks for generated code -->
    <suppress files=".*[/\\]generated[/\\].*" checks=".*"/>

    <!-- Suppress specific check for specific files -->
    <suppress files="LegacyService\.java" checks="MethodLength"/>
</suppressions>
```

## SpotBugs

SpotBugs (successor to FindBugs) detects common bugs through bytecode analysis. It finds real bugs like null pointer dereferences, resource leaks, and threading issues.

### Adding SpotBugs

**Maven**:

```xml
<plugin>
    <groupId>com.github.spotbugs</groupId>
    <artifactId>spotbugs-maven-plugin</artifactId>
    <version>4.8.3.0</version>
    <configuration>
        <effort>Max</effort>
        <threshold>Low</threshold>
        <failOnError>true</failOnError>
    </configuration>
    <executions>
        <execution>
            <goals>
                <goal>check</goal>
            </goals>
        </execution>
    </executions>
</plugin>
```

**Gradle**:

```groovy
plugins {
    id 'com.github.spotbugs' version '6.0.6'
}

spotbugs {
    effort = 'max'
    reportLevel = 'low'
}

spotbugsMain {
    reports {
        html.required = true
        xml.required = false
    }
}
```

### Bug Categories

SpotBugs detects bugs in multiple categories.

**Common bug patterns**:

| Category           | Example Bugs                                         | Severity |
| ------------------ | ---------------------------------------------------- | -------- |
| **Correctness**    | Null pointer dereference, equals() comparison bugs   | High     |
| **Bad practice**   | Ignored return value, equals() without hashCode()    | Medium   |
| **Dodgy code**     | Dead local store, useless control flow               | Low-Med  |
| **Performance**    | Inefficient string concatenation in loops            | Medium   |
| **Multithreaded**  | Inconsistent synchronization, double-checked locking | High     |
| **Security**       | SQL injection, XSS vulnerabilities                   | High     |
| **Malicious code** | Public mutable fields, finalizer vulnerabilities     | Medium   |

**Example bugs detected**:

```java
// BUG: Null pointer dereference
public void process(String value) {
    if (value == null) {
        System.out.println("Null value");
    }
    // SpotBugs: Possible null pointer dereference
    System.out.println(value.length());
}

// FIXED: Proper null handling
public void process(String value) {
    if (value == null) {
        System.out.println("Null value");
        return;
    }
    System.out.println(value.length());
}

// BUG: equals() without hashCode()
public class User {
    private String id;

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof User)) return false;
        return this.id.equals(((User) obj).id);
    }
    // SpotBugs: equals() defined without hashCode()
}

// FIXED: Implement both
public class User {
    private String id;

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof User)) return false;
        return this.id.equals(((User) obj).id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
```

### Running SpotBugs

```bash
# Maven
mvn spotbugs:check

# Generate HTML report
mvn spotbugs:spotbugs
# Report: target/spotbugsXml.xml, target/site/spotbugs.html

# Gradle
./gradlew spotbugsMain spotbugsTest

# Reports: build/reports/spotbugs/
```

### Excluding False Positives

Create exclusion filter for false positives.

**spotbugs-exclude.xml**:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<FindBugsFilter>
    <!-- Exclude specific bug in specific class -->
    <Match>
        <Class name="com.example.LegacyService"/>
        <Bug pattern="NP_NULL_ON_SOME_PATH"/>
    </Match>

    <!-- Exclude entire class from analysis -->
    <Match>
        <Class name="com.example.GeneratedCode"/>
    </Match>

    <!-- Exclude specific bug pattern everywhere -->
    <Match>
        <Bug pattern="URF_UNREAD_FIELD"/>
    </Match>

    <!-- Exclude generated code -->
    <Match>
        <Package name="~.*\.generated\..*"/>
    </Match>
</FindBugsFilter>
```

**Maven configuration**:

```xml
<plugin>
    <groupId>com.github.spotbugs</groupId>
    <artifactId>spotbugs-maven-plugin</artifactId>
    <version>4.8.3.0</version>
    <configuration>
        <excludeFilterFile>spotbugs-exclude.xml</excludeFilterFile>
    </configuration>
</plugin>
```

## PMD

PMD detects code smells, potential bugs, and inefficient code through source code analysis. It's more opinionated than SpotBugs.

### Adding PMD

**Maven**:

```xml
<plugin>
    <groupId>org.apache.maven.plugins</groupId>
    <artifactId>maven-pmd-plugin</artifactId>
    <version>3.21.2</version>
    <configuration>
        <rulesets>
            <ruleset>/rulesets/java/quickstart.xml</ruleset>
        </rulesets>
        <failOnViolation>true</failOnViolation>
        <printFailingErrors>true</printFailingErrors>
    </configuration>
    <executions>
        <execution>
            <goals>
                <goal>check</goal>
            </goals>
        </execution>
    </executions>
</plugin>
```

**Gradle**:

```groovy
plugins {
    id 'pmd'
}

pmd {
    toolVersion = '6.55.0'
    ruleSetFiles = files("pmd-ruleset.xml")
    ignoreFailures = false
}
```

### PMD Rule Categories

**Common rule sets**:

| Rule Set           | Focus                                  | Example Rules                                    |
| ------------------ | -------------------------------------- | ------------------------------------------------ |
| **Best Practices** | General coding best practices          | UnusedPrivateMethod, SystemPrintln               |
| **Code Style**     | Naming and formatting conventions      | ShortVariable, LongVariable                      |
| **Design**         | Design flaws and complexity            | GodClass, TooManyMethods                         |
| **Documentation**  | Comment and documentation requirements | UncommentedEmptyConstructor                      |
| **Error Prone**    | Code likely to cause errors            | EmptyCatchBlock, CloseResource                   |
| **Multithreading** | Threading issues                       | AvoidThreadGroup, AvoidSynchronizedAtMethodLevel |
| **Performance**    | Performance anti-patterns              | AvoidInstantiatingObjectsInLoops                 |
| **Security**       | Security vulnerabilities               | HardCodedCryptoKey                               |

### Custom PMD Ruleset

Create custom ruleset (pmd-ruleset.xml).

**pmd-ruleset.xml**:

```xml
<?xml version="1.0"?>
<ruleset name="Custom Rules"
         xmlns="http://pmd.sourceforge.net/ruleset/2.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://pmd.sourceforge.net/ruleset/2.0.0
                             https://pmd.sourceforge.io/ruleset_2_0_0.xsd">

    <description>Custom PMD ruleset for project</description>

    <!-- Best Practices -->
    <rule ref="category/java/bestpractices.xml">
        <!-- Exclude specific rules -->
        <exclude name="JUnitTestsShouldIncludeAssert"/>
    </rule>

    <!-- Code Style -->
    <rule ref="category/java/codestyle.xml/ShortVariable">
        <properties>
            <property name="minimum" value="3"/>
        </properties>
    </rule>

    <rule ref="category/java/codestyle.xml/LongVariable">
        <properties>
            <property name="maximum" value="40"/>
        </properties>
    </rule>

    <!-- Design -->
    <rule ref="category/java/design.xml">
        <exclude name="LawOfDemeter"/>
    </rule>

    <!-- Error Prone -->
    <rule ref="category/java/errorprone.xml"/>

    <!-- Performance -->
    <rule ref="category/java/performance.xml"/>

    <!-- Security -->
    <rule ref="category/java/security.xml"/>
</ruleset>
```

### Running PMD

```bash
# Maven
mvn pmd:check

# Generate HTML report
mvn pmd:pmd
# Report: target/site/pmd.html

# Gradle
./gradlew pmdMain pmdTest

# Reports: build/reports/pmd/
```

### Common PMD Violations

**Example violations**:

```java
// VIOLATION: EmptyCatchBlock
try {
    riskyOperation();
} catch (Exception e) {
    // Empty catch block
}

// FIXED: Handle or log exception
try {
    riskyOperation();
} catch (Exception e) {
    logger.error("Risk operation failed", e);
    throw new ServiceException("Operation failed", e);
}

// VIOLATION: AvoidInstantiatingObjectsInLoops
for (int i = 0; i < 1000; i++) {
    Object temp = new Object(); // Creates 1000 objects
    process(temp);
}

// FIXED: Reuse object if possible
Object temp = new Object();
for (int i = 0; i < 1000; i++) {
    process(temp);
}

// VIOLATION: UnusedPrivateMethod
public class Service {
    private void helperMethod() {
        // Never called
    }
}

// FIXED: Remove unused method
public class Service {
    // Method removed
}
```

## Error Prone

Error Prone is a compile-time static analysis tool from Google that catches common Java mistakes during compilation.

### Adding Error Prone

**Maven** (with Compiler Plugin):

```xml
<build>
    <plugins>
        <plugin>
            <groupId>org.apache.maven.plugins</groupId>
            <artifactId>maven-compiler-plugin</artifactId>
            <version>3.12.1</version>
            <configuration>
                <source>21</source>
                <target>21</target>
                <compilerArgs>
                    <arg>-XDcompilePolicy=simple</arg>
                    <arg>-Xplugin:ErrorProne</arg>
                </compilerArgs>
                <annotationProcessorPaths>
                    <path>
                        <groupId>com.google.errorprone</groupId>
                        <artifactId>error_prone_core</artifactId>
                        <version>2.24.1</version>
                    </path>
                </annotationProcessorPaths>
            </configuration>
        </plugin>
    </plugins>
</build>
```

**Gradle**:

```groovy
plugins {
    id 'java'
    id 'net.ltgt.errorprone' version '3.1.0'
}

dependencies {
    errorprone 'com.google.errorprone:error_prone_core:2.24.1'
}

tasks.withType(JavaCompile) {
    options.errorprone.enabled = true
}
```

### Error Prone Bug Patterns

Error Prone detects bugs at compile time.

**Common patterns**:

```java
// BUG: Precedence confusion
if (x = y) { // Assignment instead of comparison
    // Error Prone: "Assignment in condition"
}

// FIXED
if (x == y) {
    // Proper comparison
}

// BUG: String comparison
if (str == "test") { // Reference comparison
    // Error Prone: "StringEquality"
}

// FIXED
if (str.equals("test")) {
    // Value comparison
}

// BUG: Collection incompatible type
List<String> list = Arrays.asList("a", "b");
list.contains(123); // Wrong type
// Error Prone: "CollectionIncompatibleType"

// FIXED
list.contains("123"); // Correct type
```

### Configuration

**Disable specific checks**:

```groovy
tasks.withType(JavaCompile) {
    options.errorprone {
        // Disable specific check
        disable("StringSplitter")

        // Set severity
        error("NullAway")
        warn("FallThrough")
    }
}
```

## Code Formatting

Consistent code formatting improves readability and reduces diff noise in version control.

### Google Java Format

Google Java Format is a deterministic formatter with zero configuration.

**Maven**:

```xml
<plugin>
    <groupId>com.spotify.fmt</groupId>
    <artifactId>fmt-maven-plugin</artifactId>
    <version>2.21.1</version>
    <executions>
        <execution>
            <goals>
                <goal>format</goal>
            </goals>
        </execution>
    </executions>
</plugin>
```

```bash
# Format code
mvn fmt:format

# Check formatting
mvn fmt:check
```

**Gradle**:

```groovy
plugins {
    id 'com.diffplug.spotless' version '6.23.3'
}

spotless {
    java {
        googleJavaFormat('1.18.1')
    }
}
```

```bash
# Format code
./gradlew spotlessApply

# Check formatting
./gradlew spotlessCheck
```

**Benefits**:

- **Zero config**: No configuration debates
- **Deterministic**: Always produces same output
- **Fast**: Quick formatting
- **Google style**: Follows Google Java Style Guide

### EditorConfig

EditorConfig maintains consistent coding styles across different editors.

**.editorconfig**:

```ini
# Root configuration
root = true

# Java files
[*.java]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true
indent_style = space
indent_size = 4
max_line_length = 100

# XML files
[*.xml]
indent_style = space
indent_size = 2

# Properties files
[*.properties]
charset = utf-8
```

## SonarQube

SonarQube is an enterprise code quality platform providing centralized metrics, issue tracking, and quality gates.

### SonarQube Architecture

**Components**:

- **SonarQube Server**: Web interface, database, analysis engine
- **Scanner**: Analyzes code and sends results to server
- **Quality Gate**: Pass/fail criteria for code quality
- **Quality Profile**: Rule configuration for analysis

### Adding SonarQube Scanner

**Maven**:

```xml
<plugin>
    <groupId>org.sonarsource.scanner.maven</groupId>
    <artifactId>sonar-maven-plugin</artifactId>
    <version>3.10.0.2594</version>
</plugin>
```

```bash
# Run analysis (requires SonarQube server)
mvn clean verify sonar:sonar \
  -Dsonar.projectKey=myproject \
  -Dsonar.host.url=http://localhost:9000 \
  -Dsonar.login=sqp_token
```

**Gradle**:

```groovy
plugins {
    id 'org.sonarqube' version '4.4.1.3373'
}

sonar {
    properties {
        property "sonar.projectKey", "myproject"
        property "sonar.host.url", "http://localhost:9000"
    }
}
```

```bash
./gradlew sonar \
  -Dsonar.login=sqp_token
```

### Quality Metrics

SonarQube tracks multiple quality metrics.

**Key metrics**:

| Metric                   | Description                          | Target         |
| ------------------------ | ------------------------------------ | -------------- |
| **Coverage**             | Percentage of code covered by tests  | ≥ 80%          |
| **Duplications**         | Percentage of duplicated code blocks | < 3%           |
| **Bugs**                 | Reliability issues                   | 0 (or minimal) |
| **Vulnerabilities**      | Security issues                      | 0              |
| **Code Smells**          | Maintainability issues               | < 0.5% density |
| **Technical Debt**       | Estimated time to fix all issues     | < 5% ratio     |
| **Cognitive Complexity** | How hard code is to understand       | Low            |

### Quality Gates

Quality gates define pass/fail criteria for builds.

**Example quality gate**:

```yaml
Conditions:
  - Coverage ≥ 80%
  - Duplicated Lines < 3%
  - New Bugs = 0
  - New Vulnerabilities = 0
  - New Code Smells ≤ 5
  - Security Rating = A
  - Maintainability Rating ≤ A
```

**Fail build on quality gate**:

```bash
# Maven
mvn verify sonar:sonar -Dsonar.qualitygate.wait=true

# Gradle
./gradlew sonar -Dsonar.qualitygate.wait=true
```

### CI Integration

**GitHub Actions example**:

```yaml
name: SonarQube Analysis

on: [push, pull_request]

jobs:
  sonarqube:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0 # Shallow clones disabled for better analysis

      - name: Set up JDK 21
        uses: actions/setup-java@v4
        with:
          java-version: "21"
          distribution: "temurin"

      - name: Cache SonarQube packages
        uses: actions/cache@v3
        with:
          path: ~/.sonar/cache
          key: ${{ runner.os }}-sonar

      - name: Build and analyze
        env:
          SONAR_TOKEN: ${{ secrets.SONAR_TOKEN }}
          SONAR_HOST_URL: ${{ secrets.SONAR_HOST_URL }}
        run: mvn verify sonar:sonar -Dsonar.qualitygate.wait=true
```

## Best Practices

### Failing Builds on Violations

Configure tools to fail builds when violations occur.

**Checkstyle**:

```xml
<configuration>
    <failOnViolation>true</failOnViolation>
</configuration>
```

**SpotBugs**:

```xml
<configuration>
    <failOnError>true</failOnError>
</configuration>
```

**PMD**:

```xml
<configuration>
    <failOnViolation>true</failOnViolation>
</configuration>
```

### Gradual Adoption

Don't enable all rules at once on existing codebases.

**Adoption strategy**:

1. **Start with errors only**: Enable critical bug detection
2. **Add style checks**: Enforce on new code only
3. **Refactor gradually**: Fix violations in modules being modified
4. **Increase strictness**: Tighten rules as codebase improves
5. **Full enforcement**: Apply all rules to entire codebase

**Example** (gradual Checkstyle):

```xml
<!-- Phase 1: Critical only -->
<module name="NullPointerException"/>
<module name="EmptyCatchBlock"/>

<!-- Phase 2: Add naming -->
<module name="TypeName"/>
<module name="MethodName"/>

<!-- Phase 3: Add formatting -->
<module name="WhitespaceAround"/>
<module name="Indentation"/>
```

### Team Consensus on Rules

**Process**:

1. **Review defaults**: Examine default rule sets
2. **Discuss preferences**: Team meeting to discuss controversial rules
3. **Document decisions**: Record rationale for rule choices
4. **Trial period**: Test rules for 2-4 weeks
5. **Adjust**: Fine-tune based on feedback
6. **Lock**: Commit final configuration to version control

**Avoid**:

- Individual developers disabling rules without discussion
- Excessive suppression comments
- Ignoring tool output

### CI Integration

Run quality checks in continuous integration.

**Benefits**:

- **Automatic enforcement**: Every commit checked
- **Pull request feedback**: Violations shown in PR
- **Quality trends**: Track improvements over time
- **Fail fast**: Catch issues before code review

**GitHub Actions example** (all tools):

```yaml
name: Code Quality

on: [push, pull_request]

jobs:
  quality:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Set up JDK 21
        uses: actions/setup-java@v4
        with:
          java-version: "21"
          distribution: "temurin"
          cache: "maven"

      - name: Checkstyle
        run: mvn checkstyle:check

      - name: SpotBugs
        run: mvn spotbugs:check

      - name: PMD
        run: mvn pmd:check

      - name: Tests with Coverage
        run: mvn test jacoco:report

      - name: SonarQube
        env:
          SONAR_TOKEN: ${{ secrets.SONAR_TOKEN }}
        run: mvn sonar:sonar
```

## Related Content

- [Best Practices](/en/learn/software-engineering/programming-languages/java/in-the-field/best-practices) - Coding standards and conventions
- [Build Tools](/en/learn/software-engineering/programming-languages/java/in-the-field/build-tools) - Maven and Gradle integration
- [CI/CD Pipelines](/en/learn/software-engineering/programming-languages/java/in-the-field/ci-cd) - Continuous integration patterns
- [Security Practices](/en/learn/software-engineering/programming-languages/java/in-the-field/security-practices) - Security vulnerability detection
- [Test-Driven Development](/en/learn/software-engineering/programming-languages/java/in-the-field/test-driven-development) - Test coverage metrics
