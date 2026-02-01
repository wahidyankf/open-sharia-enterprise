---
title: "Initial Setup"
weight: 100001
date: 2025-01-29T00:00:00+07:00
draft: false
description: "Step-by-step guide to installing Java/Kotlin, Maven/Gradle, and creating your first Spring Framework project structure"
tags: ["spring", "java", "kotlin", "maven", "gradle", "setup", "installation"]
---

## Overview

This guide walks through installing Java Development Kit (JDK), build tools, and creating a Spring Framework project from scratch. You'll set up both Java and Kotlin development environments.

## Prerequisites

**Operating System**: Windows, macOS, or Linux

**Terminal Access**: Command line interface (bash, zsh, PowerShell, or cmd)

**Internet Connection**: For downloading JDK, build tools, and dependencies

## Installing Java Development Kit

### Java 17 Installation

**Why Java 17?**: Long-Term Support (LTS) release with modern features, recommended for enterprise applications.

#### Windows Installation

**Download JDK**:

1. Visit [Adoptium Temurin](https://adoptium.net/temurin/releases/)
2. Select Java 17 LTS
3. Download Windows x64 MSI installer
4. Run installer with default settings

**Verify Installation**:

```bash
# => Check Java version
java -version
# => Output: openjdk version "17.0.x" (or similar)

# => Check Java compiler
javac -version
# => Output: javac 17.0.x
```

**Set JAVA_HOME** (if not set by installer):

```powershell
# => Windows PowerShell
# => Set environment variable permanently
[System.Environment]::SetEnvironmentVariable("JAVA_HOME", "C:\Program Files\Eclipse Adoptium\jdk-17.0.x", "User")
# => JAVA_HOME now points to JDK installation
```

#### macOS Installation

**Using Homebrew**:

```bash
# => Install Homebrew (if not already installed)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
# => Package manager for macOS

# => Install Java 17
brew install openjdk@17
# => Downloads and installs JDK 17

# => Create symlink for system Java
sudo ln -sfn /opt/homebrew/opt/openjdk@17/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-17.jdk
# => Makes JDK 17 available system-wide

# => Add to PATH (add to ~/.zshrc or ~/.bash_profile)
echo 'export PATH="/opt/homebrew/opt/openjdk@17/bin:$PATH"' >> ~/.zshrc
# => JDK binaries now in PATH
```

**Verify Installation**:

```bash
java -version     # => openjdk version "17.0.x"
javac -version    # => javac 17.0.x
```

#### Linux Installation (Ubuntu/Debian)

```bash
# => Update package index
sudo apt update
# => Refresh available packages

# => Install OpenJDK 17
sudo apt install openjdk-17-jdk
# => Downloads and installs JDK 17

# => Verify installation
java -version     # => openjdk version "17.0.x"
javac -version    # => javac 17.0.x

# => Set JAVA_HOME (add to ~/.bashrc or ~/.zshrc)
echo 'export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64' >> ~/.bashrc
# => JAVA_HOME environment variable set
echo 'export PATH=$JAVA_HOME/bin:$PATH' >> ~/.bashrc
# => JDK binaries in PATH

# => Reload shell configuration
source ~/.bashrc
# => Apply changes
```

### Kotlin Installation (Optional)

If you prefer Kotlin over Java, install Kotlin compiler:

```bash
# => Using SDKMAN (recommended for managing JVM tools)
curl -s "https://get.sdkman.io" | bash
# => Install SDKMAN package manager

source "$HOME/.sdkman/bin/sdkman-init.sh"
# => Initialize SDKMAN in current shell

# => Install Kotlin compiler
sdk install kotlin
# => Downloads and installs latest Kotlin

# => Verify installation
kotlin -version
# => Output: Kotlin version 1.9.x
```

## Installing Build Tools

### Maven Installation

**Maven** is a popular build automation and dependency management tool for Java projects.

#### Windows (Maven)

**Download and Install**:

1. Download Maven from [Apache Maven](https://maven.apache.org/download.cgi)
2. Extract ZIP to `C:\Program Files\Apache\Maven\apache-maven-3.9.x`
3. Add to PATH:

```powershell
# => Add Maven bin directory to PATH
[System.Environment]::SetEnvironmentVariable("Path", $env:Path + ";C:\Program Files\Apache\Maven\apache-maven-3.9.x\bin", "User")
# => Maven commands now available globally
```

**Verify**:

```bash
mvn -version
# => Apache Maven 3.9.x
# => Java version: 17.0.x
```

#### macOS/Linux (Maven)

```bash
# => macOS (using Homebrew)
brew install maven
# => Installs latest Maven

# => Linux (Ubuntu/Debian)
sudo apt install maven
# => Installs Maven from package repository

# => Verify installation
mvn -version
# => Apache Maven 3.9.x
# => Java version: 17.0.x
```

### Gradle Installation

**Gradle** is an alternative build tool with Kotlin DSL support and faster builds.

#### Windows (Gradle)

**Using SDKMAN**:

```powershell
# => Install SDKMAN for Windows
# => Download from https://sdkman.io/install

# => Install Gradle
sdk install gradle
# => Downloads and installs latest Gradle

# => Verify
gradle -version
# => Gradle 8.x
```

#### macOS/Linux (Gradle)

```bash
# => macOS (using Homebrew)
brew install gradle
# => Installs latest Gradle

# => Linux (using SDKMAN - recommended)
sdk install gradle
# => Downloads and installs Gradle

# => Verify installation
gradle -version
# => Gradle 8.x
# => JVM version: 17.0.x
```

## Creating Project Structure

### Maven Project Setup

**Create Maven project using archetype**:

```bash
# => Generate Maven project from archetype
mvn archetype:generate \
  -DgroupId=com.ayokoding.zakat \
  -DartifactId=zakat-calculator \
  -DarchetypeArtifactId=maven-archetype-quickstart \
  -DarchetypeVersion=1.4 \
  -DinteractiveMode=false
# => Creates project directory with standard structure
# => groupId: com.ayokoding.zakat (package namespace)
# => artifactId: zakat-calculator (project name)

# => Navigate to project
cd zakat-calculator
# => Project root directory
```

**Project structure created**:

```
zakat-calculator/
├── pom.xml                    # => Maven configuration (dependencies, plugins)
└── src/
    ├── main/
    │   └── java/
    │       └── com/ayokoding/zakat/
    │           └── App.java   # => Main application class
    └── test/
        └── java/
            └── com/ayokoding/zakat/
                └── AppTest.java  # => Test class
```

**Configure pom.xml for Spring Framework**:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    <!-- => Project coordinates -->
    <groupId>com.ayokoding.zakat</groupId>
    <artifactId>zakat-calculator</artifactId>
    <version>1.0-SNAPSHOT</version>

    <properties>
        <!-- => Java version configuration -->
        <maven.compiler.source>17</maven.compiler.source>
        <maven.compiler.target>17</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        <!-- => Spring Framework version -->
        <spring.version>6.1.3</spring.version>
    </properties>

    <dependencies>
        <!-- => Spring Core (IoC Container, DI) -->
        <dependency>
            <groupId>org.springframework</groupId>
            <artifactId>spring-context</artifactId>
            <version>${spring.version}</version>
            <!-- => Transitively includes spring-core, spring-beans -->
        </dependency>

        <!-- => Spring JDBC (Data Access) -->
        <dependency>
            <groupId>org.springframework</groupId>
            <artifactId>spring-jdbc</artifactId>
            <version>${spring.version}</version>
            <!-- => JdbcTemplate, transaction support -->
        </dependency>

        <!-- => Spring Web MVC -->
        <dependency>
            <groupId>org.springframework</groupId>
            <artifactId>spring-webmvc</artifactId>
            <version>${spring.version}</version>
            <!-- => Controllers, request mapping -->
        </dependency>

        <!-- => H2 Database (embedded, for development) -->
        <dependency>
            <groupId>com.h2database</groupId>
            <artifactId>h2</artifactId>
            <version>2.2.224</version>
            <!-- => In-memory database for quick prototyping -->
        </dependency>

        <!-- => SLF4J Logging -->
        <dependency>
            <groupId>org.slf4j</groupId>
            <artifactId>slf4j-simple</artifactId>
            <version>2.0.11</version>
            <!-- => Simple logging implementation -->
        </dependency>

        <!-- => JUnit 5 (Testing) -->
        <dependency>
            <groupId>org.junit.jupiter</groupId>
            <artifactId>junit-jupiter</artifactId>
            <version>5.10.1</version>
            <scope>test</scope>
            <!-- => Only available in test scope -->
        </dependency>

        <!-- => Spring Test -->
        <dependency>
            <groupId>org.springframework</groupId>
            <artifactId>spring-test</artifactId>
            <version>${spring.version}</version>
            <scope>test</scope>
            <!-- => Spring testing utilities -->
        </dependency>
    </dependencies>
</project>
```

### Gradle Project Setup (Kotlin DSL)

**Create Gradle project**:

```bash
# => Create project directory
mkdir zakat-calculator-gradle
cd zakat-calculator-gradle
# => Project root

# => Initialize Gradle project
gradle init \
  --type java-application \
  --dsl kotlin \
  --test-framework junit-jupiter \
  --package com.ayokoding.zakat \
  --project-name zakat-calculator
# => Creates Gradle project with Kotlin DSL
# => type: java-application (generates main class)
# => dsl: kotlin (uses build.gradle.kts)
# => test-framework: junit-jupiter (JUnit 5)
```

**Project structure created**:

```
zakat-calculator-gradle/
├── build.gradle.kts           # => Gradle build configuration (Kotlin DSL)
├── settings.gradle.kts        # => Project settings
├── gradlew                    # => Gradle wrapper (Unix)
├── gradlew.bat                # => Gradle wrapper (Windows)
└── src/
    ├── main/
    │   ├── java/
    │   │   └── com/ayokoding/zakat/
    │   │       └── App.java
    │   └── resources/         # => Application resources
    └── test/
        └── java/
            └── com/ayokoding/zakat/
                └── AppTest.java
```

**Configure build.gradle.kts for Spring Framework**:

```kotlin
plugins {
    java                                    // => Java plugin
    application                             // => Application plugin (main class support)
}

repositories {
    mavenCentral()                          // => Use Maven Central for dependencies
}

// => Spring Framework version
val springVersion = "6.1.3"

dependencies {
    // => Spring Framework dependencies
    implementation("org.springframework:spring-context:${springVersion}")
    // => IoC container, DI
    implementation("org.springframework:spring-jdbc:${springVersion}")
    // => Data access
    implementation("org.springframework:spring-webmvc:${springVersion}")
    // => Web MVC

    // => H2 Database
    implementation("com.h2database:h2:2.2.224")
    // => Embedded database

    // => Logging
    implementation("org.slf4j:slf4j-simple:2.0.11")
    // => Simple logging

    // => Testing dependencies
    testImplementation("org.junit.jupiter:junit-jupiter:5.10.1")
    // => JUnit 5
    testImplementation("org.springframework:spring-test:${springVersion}")
    // => Spring testing support
}

application {
    mainClass.set("com.ayokoding.zakat.App")  // => Application entry point
}

tasks.test {
    useJUnitPlatform()                        // => Enable JUnit 5 support
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))  // => Java 17
    }
}
```

## Creating ApplicationContext Configuration

### Java-Based Configuration

Create configuration class to bootstrap Spring container:

**Create file**: `src/main/java/com/ayokoding/zakat/AppConfig.java`

```java
package com.ayokoding.zakat;

import org.springframework.context.annotation.Bean;             // => Bean declaration
import org.springframework.context.annotation.ComponentScan;    // => Component scanning
import org.springframework.context.annotation.Configuration;    // => Configuration class
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;
import javax.sql.DataSource;

// => @Configuration marks this as Spring config class
// => Provides bean definitions to IoC container
@Configuration
@ComponentScan(basePackages = "com.ayokoding.zakat")  // => Scan for @Component classes
public class AppConfig {

    // => @Bean declares Spring-managed bean
    // => Method return type is bean type
    // => Method name is bean ID (unless specified otherwise)
    @Bean
    public DataSource dataSource() {
        return new EmbeddedDatabaseBuilder()
            .setType(EmbeddedDatabaseType.H2)        // => H2 embedded database
            .addScript("classpath:schema.sql")       // => Initialize schema
            .build();                                 // => Returns DataSource instance
    }                                                 // => Spring manages lifecycle
}
```

**Create schema file**: `src/main/resources/schema.sql`

```sql
-- => Create zakat_records table
-- => Stores Zakat payment records
CREATE TABLE IF NOT EXISTS zakat_records (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,    -- => Auto-increment ID
    amount DECIMAL(15, 2) NOT NULL,          -- => Zakat amount (15 digits, 2 decimals)
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP  -- => Timestamp
);
-- => Table created when ApplicationContext initializes
```

### Kotlin Configuration (Alternative)

If using Kotlin, create configuration in Kotlin:

**Create file**: `src/main/kotlin/com/ayokoding/zakat/AppConfig.kt`

```kotlin
package com.ayokoding.zakat

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType
import javax.sql.DataSource

// => @Configuration annotation (Kotlin style)
@Configuration
@ComponentScan(basePackages = ["com.ayokoding.zakat"])  // => Kotlin array syntax
class AppConfig {

    // => @Bean function (returns DataSource)
    @Bean
    fun dataSource(): DataSource {
        return EmbeddedDatabaseBuilder()
            .setType(EmbeddedDatabaseType.H2)     // => H2 database
            .addScript("classpath:schema.sql")    // => Schema initialization
            .build()                               // => Returns DataSource
    }  // => Managed by Spring container
}
```

## Verifying Installation

### Test ApplicationContext Creation

**Create test class**: `src/test/java/com/ayokoding/zakat/AppConfigTest.java`

```java
package com.ayokoding.zakat;

import org.junit.jupiter.api.Test;                          // => JUnit 5 test
import org.springframework.context.ApplicationContext;      // => Spring container
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import javax.sql.DataSource;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class AppConfigTest {

    @Test
    public void testApplicationContextLoads() {
        // => Create ApplicationContext from Java config
        ApplicationContext context = new AnnotationConfigApplicationContext(AppConfig.class);
        // => Loads @Configuration class
        // => Initializes IoC container
        // => Scans for @Component classes

        // => Retrieve bean from container
        DataSource dataSource = context.getBean(DataSource.class);
        // => Looks up bean by type
        // => Returns configured DataSource bean

        // => Verify bean exists
        assertNotNull(dataSource);  // => DataSource should not be null
        // => Test passes if ApplicationContext successfully created and configured
    }
}
```

**Run test**:

```bash
# => Maven
mvn test
# => Compiles code, runs tests
# => Output: Tests run: 1, Failures: 0, Errors: 0

# => Gradle
./gradlew test
# => Executes test task
# => Output: BUILD SUCCESSFUL
```

**Expected output**:

```
[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0
# => ApplicationContext loaded successfully
# => DataSource bean configured
# => Spring Framework working correctly
```

## Common Issues and Solutions

### Issue: JAVA_HOME not set

**Error**: `JAVA_HOME is not set and no 'java' command could be found`

**Solution**:

```bash
# => Set JAVA_HOME to JDK installation directory
export JAVA_HOME=/path/to/jdk-17
# => Add to ~/.bashrc or ~/.zshrc for persistence

# => Windows PowerShell
[System.Environment]::SetEnvironmentVariable("JAVA_HOME", "C:\Program Files\Java\jdk-17", "User")
```

### Issue: Maven dependencies not downloading

**Error**: `Could not resolve dependencies`

**Solution**:

```bash
# => Clear Maven cache
rm -rf ~/.m2/repository
# => Deletes cached dependencies

# => Re-download dependencies
mvn clean install
# => Downloads all dependencies fresh
```

### Issue: Gradle daemon failures

**Error**: `Gradle daemon stopped unexpectedly`

**Solution**:

```bash
# => Stop all Gradle daemons
./gradlew --stop
# => Terminates background processes

# => Clean build
./gradlew clean build
# => Fresh build
```

## Next Steps

Setup complete! You now have:

✅ Java 17 JDK installed
✅ Maven or Gradle build tool configured
✅ Spring Framework project structure created
✅ ApplicationContext configuration verified

**Continue to**:

**[Quick Start](/en/learn/software-engineering/platform-web/jvm-spring/quick-start)** - Build a complete Zakat Calculator application demonstrating Spring Framework core features

**[By Example](/en/learn/software-engineering/platform-web/jvm-spring/by-example)** - Learn through heavily annotated code examples
