---
title: "Initial Setup"
date: 2025-01-29T00:00:00+07:00
draft: false
weight: 100001
description: "Get Spring Boot installed and running - JDK setup, Spring Initializr, Maven/Gradle, and your first REST API"
tags: ["spring-boot", "java", "kotlin", "setup", "installation", "web-framework", "beginner"]
---

**Want to start building with Spring Boot?** This initial setup guide gets Spring Boot installed and working on your system in minutes. By the end, you'll have Spring Boot running and will create your first web application.

This tutorial provides 0-5% coverage - just enough to get Spring Boot working on your machine. For deeper learning, continue to [Quick Start](/en/learn/software-engineering/web-platform/jvm-spring-boot/quick-start) (5-30% coverage).

## Prerequisites

Before installing Spring Boot, you need:

- A computer running Windows, macOS, or Linux
- Administrator/sudo access for installation
- A terminal/command prompt
- A text editor or IDE (IntelliJ IDEA recommended, VS Code, Eclipse, or any editor)
- Basic command-line navigation skills
- Java Development Kit (JDK) 17 or later (we'll install this)

No prior Spring or enterprise framework experience required - this guide starts from zero.

## Learning Objectives

By the end of this tutorial, you will be able to:

1. **Install** Java Development Kit (JDK 17 or later)
2. **Create** a Spring Boot project using Spring Initializr
3. **Understand** Maven and Gradle build tools
4. **Run** your first Spring Boot application
5. **Access** a REST API endpoint in your browser

## Java Development Kit Installation

Spring Boot 3.x requires Java 17 or later (Java 21 recommended).

### Verify Existing Java Installation

Check if Java is already installed:

```bash
java -version
```

If you see output like this with version 17+, Java is installed:

```
openjdk version "17.0.9" 2023-10-17
```

**Required**: Java 17 or later (Java 21 recommended for latest features)

If Java not installed or version below 17, continue to installation steps.

### Windows Java Installation

**Step 1: Download JDK**

Visit [Adoptium](https://adoptium.net/) (Eclipse Temurin):

1. Go to [https://adoptium.net/temurin/releases/](https://adoptium.net/temurin/releases/)
2. Select **Java Version: 21** (LTS)
3. Select **Operating System: Windows**
4. Select **Architecture: x64** (or ARM for ARM Windows)
5. Download `.msi` installer

**Step 2: Install JDK**

1. Double-click downloaded `.msi` file
2. Follow installer wizard:
   - Click **Next** through introduction
   - Keep default installation path
   - **IMPORTANT**: Select "Set JAVA_HOME variable" option
   - **IMPORTANT**: Select "Add to PATH" option
   - Click **Install**
3. Click **Finish**

**Step 3: Verify Installation**

Open new Command Prompt or PowerShell:

```cmd
java -version
javac -version
```

Expected output:

```
openjdk version "21.0.1" 2023-10-17 LTS
javac 21.0.1
```

**Verify JAVA_HOME**:

```cmd
echo %JAVA_HOME%
```

Should output: `C:\Program Files\Eclipse Adoptium\jdk-21.0.1+12`

**Troubleshooting Windows**:

- If `java` not found, restart Command Prompt to load new PATH
- Manually set JAVA_HOME in System Environment Variables if installer didn't
- Ensure JDK bin directory in PATH: `%JAVA_HOME%\bin`

### macOS Java Installation

**Step 1: Install via Homebrew (Recommended)**

```bash
brew install openjdk@21
```

**Step 2: Create Symlink**

```bash
sudo ln -sfn /usr/local/opt/openjdk@21/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-21.jdk
```

**Step 3: Set JAVA_HOME**

Add to `~/.zshrc` or `~/.bash_profile`:

```bash
export JAVA_HOME=$(/usr/libexec/java_home -v 21)
export PATH="$JAVA_HOME/bin:$PATH"
```

Reload shell:

```bash
source ~/.zshrc  # or source ~/.bash_profile
```

**Step 4: Verify Installation**

```bash
java -version
javac -version
echo $JAVA_HOME
```

Expected output shows Java 21.

**Alternative: Download from Adoptium**

1. Visit [https://adoptium.net/temurin/releases/](https://adoptium.net/temurin/releases/)
2. Download `.pkg` for macOS
3. Install and follow verification steps above

**Troubleshooting macOS**:

- If multiple Java versions exist, use `/usr/libexec/java_home -V` to list
- Select specific version: `export JAVA_HOME=$(/usr/libexec/java_home -v 21)`

### Linux Java Installation

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

**Verify Installation**:

```bash
java -version
javac -version
```

Expected output:

```
openjdk version "21.0.1" 2023-10-17 LTS
javac 21.0.1
```

**Set JAVA_HOME**:

Add to `~/.bashrc` or `~/.zshrc`:

```bash
export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64  # Ubuntu/Debian
export JAVA_HOME=/usr/lib/jvm/java-21-openjdk  # Fedora/Arch
export PATH="$JAVA_HOME/bin:$PATH"
```

Reload shell:

```bash
source ~/.bashrc  # or source ~/.zshrc
```

**Troubleshooting Linux**:

- Find Java installation: `sudo update-alternatives --config java` (Ubuntu/Debian)
- JAVA_HOME path varies by distribution - check actual installation

## Create Spring Boot Project with Spring Initializr

Spring Initializr generates pre-configured Spring Boot projects.

### Using Spring Initializr Web Interface

**Step 1: Visit Spring Initializr**

Open browser to [https://start.spring.io/](https://start.spring.io/)

**Step 2: Configure Project**

Select these options:

- **Project**: Maven (or Gradle if preferred)
- **Language**: Java (or Kotlin)
- **Spring Boot**: 3.2.x (latest stable version)
- **Project Metadata**:
  - Group: `com.example`
  - Artifact: `demo`
  - Name: `demo`
  - Description: Demo project for Spring Boot
  - Package name: `com.example.demo`
  - Packaging: Jar
  - Java: 21 (or 17)

**Step 3: Add Dependencies**

Click "Add Dependencies" and search for:

- **Spring Web** - For REST APIs and web applications
- **Spring Boot DevTools** - For hot reload during development

**Step 4: Generate Project**

Click **Generate** button. This downloads `demo.zip` file.

**Step 5: Extract Project**

Extract `demo.zip` to your projects directory:

```bash
unzip demo.zip -d ~/projects/
cd ~/projects/demo

cd C:\projects\demo
```

### Using Spring Initializr Command Line (Alternative)

Use `curl` to generate project via API:

```bash
curl https://start.spring.io/starter.zip \
  -d dependencies=web,devtools \
  -d type=maven-project \
  -d language=java \
  -d bootVersion=3.2.0 \
  -d baseDir=demo \
  -d groupId=com.example \
  -d artifactId=demo \
  -d javaVersion=21 \
  -o demo.zip

unzip demo.zip
cd demo
```

### Explore Project Structure

Your Spring Boot project structure:

```
demo/
├── src/
│   ├── main/
│   │   ├── java/
│   │   │   └── com/example/demo/
│   │   │       └── DemoApplication.java  # Main class
│   │   └── resources/
│   │       ├── application.properties    # Configuration
│   │       ├── static/                   # Static files (CSS, JS)
│   │       └── templates/                # HTML templates
│   └── test/
│       └── java/
│           └── com/example/demo/
│               └── DemoApplicationTests.java
├── pom.xml              # Maven configuration (dependencies, build)
├── mvnw                 # Maven wrapper (Linux/macOS)
├── mvnw.cmd             # Maven wrapper (Windows)
└── README.md
```

**Key files**:

- **pom.xml** - Maven project configuration with dependencies
- **DemoApplication.java** - Spring Boot entry point
- **application.properties** - Application configuration

### View pom.xml

```bash
cat pom.xml
```

Maven configuration includes:

```xml
<dependencies>
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-web</artifactId>
    </dependency>
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-devtools</artifactId>
        <scope>runtime</scope>
        <optional>true</optional>
    </dependency>
    <!-- Test dependencies -->
</dependencies>
```

### View Main Application Class

```bash
cat src/main/java/com/example/demo/DemoApplication.java
```

Generated code:

```java
package com.example.demo;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class DemoApplication {

    public static void main(String[] args) {
        SpringApplication.run(DemoApplication.class, args);
    }

}
```

**Code explanation**:

- `@SpringBootApplication` - Enables auto-configuration, component scanning, configuration
- `SpringApplication.run()` - Starts embedded web server and Spring context
- `main()` - Standard Java entry point

## Run Your First Spring Boot Application

### Start Application with Maven Wrapper

Spring Boot includes Maven wrapper (`mvnw`) - no need to install Maven separately.

**Linux/macOS**:

```bash
./mvnw spring-boot:run
```

**Windows**:

```cmd
mvnw.cmd spring-boot:run
```

First run downloads dependencies (takes 2-5 minutes). Output:

```
[INFO] Scanning for projects...
[INFO]
[INFO] -----------------------< com.example:demo >-----------------------
[INFO] Building demo 0.0.1-SNAPSHOT
[INFO] --------------------------------[ jar ]---------------------------------
...
  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::                (v3.2.0)

2025-01-29T10:00:00.000+07:00  INFO 12345 --- [           main] c.example.demo.DemoApplication           : Starting DemoApplication
2025-01-29T10:00:01.500+07:00  INFO 12345 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat started on port 8080 (http)
2025-01-29T10:00:01.510+07:00  INFO 12345 --- [           main] c.example.demo.DemoApplication           : Started DemoApplication in 2.345 seconds
```

**Application is ready when you see**:

```
Tomcat started on port 8080 (http)
Started DemoApplication in X.XXX seconds
```

### Access Application

Open browser to:

```
http://localhost:8080
```

You'll see error page:

```
Whitelabel Error Page
This application has no explicit mapping for /error, so you are seeing this as a fallback.
```

This is normal - we haven't created any endpoints yet. The application works; it just has no routes defined.

### Stop the Application

Press `Ctrl+C` in terminal to stop Spring Boot.

## Create Your First REST API Endpoint

Let's add a simple REST controller.

### Create Controller Class

Create `src/main/java/com/example/demo/HelloController.java`:

```java
package com.example.demo;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class HelloController {

    @GetMapping("/hello")
    public String hello(@RequestParam(defaultValue = "World") String name) {
        return "Hello, " + name + "!";
    }

    @GetMapping("/")
    public String home() {
        return "Welcome to Spring Boot!";
    }
}
```

**Code explanation**:

- `@RestController` - Marks class as REST controller (combines @Controller + @ResponseBody)
- `@GetMapping("/hello")` - Maps HTTP GET requests to `/hello` endpoint
- `@RequestParam` - Extracts query parameter `name` from URL
- Method return value automatically serialized to JSON/text

### Restart Application

```bash
./mvnw spring-boot:run
```

With DevTools installed, Spring Boot hot-reloads code changes automatically (no manual restart needed after initial start).

### Test Endpoints

**Visit home endpoint**:

```
http://localhost:8080/
```

Output:

```
Welcome to Spring Boot!
```

**Visit hello endpoint**:

```
http://localhost:8080/hello
```

Output:

```
Hello, World!
```

**With query parameter**:

```
http://localhost:8080/hello?name=Alice
```

Output:

```
Hello, Alice!
```

### Test with curl (Command Line)

```bash
curl http://localhost:8080/

curl http://localhost:8080/hello

curl http://localhost:8080/hello?name=Bob
```

## Create JSON API Endpoint

Let's create an endpoint returning JSON.

### Create Model Class

Create `src/main/java/com/example/demo/Greeting.java`:

```java
package com.example.demo;

public class Greeting {
    private String message;
    private long timestamp;

    public Greeting(String message) {
        this.message = message;
        this.timestamp = System.currentTimeMillis();
    }

    public String getMessage() {
        return message;
    }

    public long getTimestamp() {
        return timestamp;
    }
}
```

### Update Controller

Add JSON endpoint to `HelloController.java`:

```java
@GetMapping("/api/greeting")
public Greeting greeting(@RequestParam(defaultValue = "World") String name) {
    return new Greeting("Hello, " + name + "!");
}
```

### Test JSON Endpoint

Visit:

```
http://localhost:8080/api/greeting?name=Alice
```

Output (JSON):

```json
{
  "message": "Hello, Alice!",
  "timestamp": 1706508123456
}
```

Spring Boot automatically serializes Java objects to JSON using Jackson library (included in spring-boot-starter-web).

## Understanding Spring Boot Auto-Configuration

Spring Boot configures components automatically based on dependencies.

### What Gets Auto-Configured?

With `spring-boot-starter-web` dependency:

- **Embedded Tomcat server** - No external server installation needed
- **Jackson** - JSON serialization/deserialization
- **Spring MVC** - Web framework for REST APIs
- **Error handling** - Default error pages and error responses
- **Static content serving** - Serves files from `/static`, `/public`, `/resources`

### View Auto-Configuration Report

Add `--debug` flag to see auto-configuration:

```bash
./mvnw spring-boot:run --debug
```

Output shows:

- Positive matches (auto-configured beans)
- Negative matches (skipped configurations)
- Exclusions

Or add to `application.properties`:

```properties
logging.level.org.springframework.boot.autoconfigure=DEBUG
```

## Understanding Maven vs Gradle

Spring Boot supports two build tools.

### Maven (pom.xml)

**Advantages**:

- Widely adopted in enterprise Java
- XML configuration (explicit, verbose)
- Established ecosystem and plugins
- Better documentation for beginners

**Basic commands**:

```bash
./mvnw clean              # Clean build artifacts
./mvnw compile            # Compile source code
./mvnw test               # Run tests
./mvnw package            # Build JAR/WAR
./mvnw spring-boot:run    # Run Spring Boot app
```

### Gradle (build.gradle or build.gradle.kts)

**Advantages**:

- Faster builds (incremental compilation, build cache)
- Groovy/Kotlin DSL (less verbose than XML)
- Flexible and powerful for complex builds
- Better for Android and multi-module projects

**Basic commands**:

```bash
./gradlew clean           # Clean build artifacts
./gradlew build           # Build project
./gradlew test            # Run tests
./gradlew bootRun         # Run Spring Boot app
```

### Creating Gradle Project

Use Spring Initializr and select "Gradle - Groovy" or "Gradle - Kotlin" instead of Maven.

For this tutorial, we use Maven (more common for beginners).

## Verify Your Setup Works

Let's confirm everything is functioning correctly.

### Test 1: Java Installed

```bash
java -version
javac -version
```

Should show Java 17 or later.

### Test 2: Maven Wrapper Works

```bash
./mvnw --version
```

Should show Maven version.

### Test 3: Application Starts

```bash
./mvnw spring-boot:run
```

Should start without errors and show "Started DemoApplication".

### Test 4: Endpoints Respond

Visit `http://localhost:8080/hello` in browser or:

```bash
curl http://localhost:8080/hello
```

Should return "Hello, World!"

### Test 5: JSON Endpoint Works

```bash
curl http://localhost:8080/api/greeting
```

Should return JSON with message and timestamp.

**All tests passed?** Your Spring Boot setup is complete!

## Summary

**What you've accomplished**:

- Installed Java Development Kit (JDK 17 or 21)
- Created Spring Boot project using Spring Initializr
- Understood Maven project structure and configuration
- Started Spring Boot embedded server
- Created REST API endpoints (text and JSON)
- Tested endpoints in browser and with curl
- Understood Spring Boot auto-configuration

**Key commands learned**:

- `java -version` - Check Java version
- `./mvnw spring-boot:run` - Run Spring Boot application
- `./mvnw clean package` - Build JAR file
- `curl http://localhost:8080/endpoint` - Test API endpoint

**Annotations learned**:

- `@SpringBootApplication` - Enable Spring Boot auto-configuration
- `@RestController` - Mark class as REST API controller
- `@GetMapping("/path")` - Map HTTP GET request to method
- `@RequestParam` - Extract query parameter from URL

**Skills gained**:

- Java JDK installation and configuration
- Spring Initializr project generation
- Maven project structure navigation
- REST API endpoint creation
- JSON serialization with Spring Boot
- Spring Boot auto-configuration understanding

## Next Steps

**Ready to learn Spring Boot fundamentals?**

- [Quick Start](/en/learn/software-engineering/web-platform/jvm-spring-boot/quick-start) (5-30% coverage) - Touch all core Spring Boot concepts in a fast-paced tour

**Want comprehensive Spring Boot mastery?**

- [Beginner Tutorial](/en/learn/software-engineering/web-platform/jvm-spring-boot/by-concept/beginner) (0-60% coverage) - Deep dive into Spring Boot with extensive practice

**Prefer code-first learning?**

- [By-Example Tutorial](/en/learn/software-engineering/web-platform/jvm-spring-boot/by-example) - Learn through heavily annotated examples in Java and Kotlin

**Want to understand Spring Boot's design philosophy?**

- [Overview](/en/learn/software-engineering/web-platform/jvm-spring-boot/overview) - Why Spring Boot exists and when to use it

## Troubleshooting Common Issues

### "java: command not found"

**Problem**: Java not installed or not in PATH.

**Solution**:

- Install JDK following platform-specific instructions above
- Verify PATH includes Java bin directory
- Restart terminal after installation

### "JAVA_HOME not set"

**Problem**: JAVA_HOME environment variable missing.

**Solution**:

- **Windows**: Set in System Environment Variables
- **Linux/macOS**: Add `export JAVA_HOME=/path/to/jdk` to shell config
- Verify: `echo $JAVA_HOME` (Linux/macOS) or `echo %JAVA_HOME%` (Windows)

### "mvnw: Permission denied" (Linux/macOS)

**Problem**: Maven wrapper not executable.

**Solution**:

```bash
chmod +x mvnw
./mvnw spring-boot:run
```

### Port 8080 already in use

**Problem**: Another application using port 8080.

**Solution**:

- Stop other application on port 8080
- Or change port in `application.properties`:

  ```properties
  server.port=8081
  ```

- Restart application

### Slow first build

**Problem**: Maven downloads dependencies on first run.

**Solution**:

- This is normal - first build downloads JARs from Maven Central
- Subsequent builds much faster (uses local cache)
- Wait patiently during first `./mvnw spring-boot:run`

### "Whitelabel Error Page" on localhost:8080

**Problem**: No root mapping defined.

**Solution**:

- This is expected if you haven't created controller with `@GetMapping("/")`
- Add home endpoint to controller (as shown in tutorial)
- Or ignore - error page means application running correctly

### DevTools hot reload not working

**Problem**: Code changes don't reload automatically.

**Solution**:

- Ensure `spring-boot-devtools` in `pom.xml`
- Save file in IDE (IntelliJ auto-saves, others may not)
- Some changes require full restart (configuration files)
- Check IDE settings: "Build project automatically" enabled

## Further Resources

**Official Spring Boot Documentation**:

- [Spring Boot Reference](https://docs.spring.io/spring-boot/docs/current/reference/html/) - Complete documentation
- [Spring Guides](https://spring.io/guides) - Step-by-step tutorials
- [Spring Initializr](https://start.spring.io/) - Project generator

**Learning Resources**:

- [Spring Boot Quick Start](https://spring.io/quickstart) - Official quick start
- [Baeldung Spring Boot](https://www.baeldung.com/spring-boot) - In-depth tutorials
- [Spring Academy](https://spring.academy/) - Official training

**API Documentation**:

- [Spring Boot API Docs](https://docs.spring.io/spring-boot/docs/current/api/) - Javadoc reference
- [Spring Framework API](https://docs.spring.io/spring-framework/docs/current/javadoc-api/) - Core framework docs

**Community**:

- [Stack Overflow Spring Boot](https://stackoverflow.com/questions/tagged/spring-boot) - Q&A
- [Spring Community Forum](https://github.com/spring-projects/spring-boot/discussions) - GitHub discussions
- [Spring Blog](https://spring.io/blog) - Official blog with announcements

**Tools**:

- [IntelliJ IDEA](https://www.jetbrains.com/idea/) - Best IDE for Spring Boot (Ultimate Edition recommended)
- [VS Code Spring Boot Extension Pack](https://marketplace.visualstudio.com/items?itemName=vmware.vscode-boot-dev-pack) - VS Code support
- [Postman](https://www.postman.com/) - API testing tool
