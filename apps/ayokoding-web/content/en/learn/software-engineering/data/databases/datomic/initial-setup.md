---
title: "Initial Setup"
date: 2026-01-29T00:00:00+07:00
draft: false
weight: 100001
description: "Get Datomic installed and running on your system - installation, verification, and your first working program"
tags: ["datomic", "installation", "setup", "beginner", "database"]
---

**Want to start working with Datomic?** This initial setup guide gets Datomic Free installed and working on your system. By the end, you'll have Datomic running and will create your first database with time-travel queries.

This tutorial provides 0-5% coverage - just enough to get Datomic working on your machine. For deeper learning, continue to [Quick Start](/en/learn/software-engineering/data/databases/datomic/quick-start) (5-30% coverage).

## Prerequisites

Before installing Datomic, you need:

- Java 8 or later installed (Datomic runs on the JVM)
- A computer running Windows, macOS, or Linux
- A terminal/command prompt
- A text editor or IDE (IntelliJ IDEA, VS Code, Emacs)
- Basic understanding of Java or Clojure (helpful but not required)

No prior Datomic or database experience required - this guide starts from zero.

## Learning Objectives

By the end of this tutorial, you will be able to:

1. **Install** Java and verify JVM compatibility for Datomic
2. **Download** and configure Datomic Free edition
3. **Verify** that Datomic is installed correctly and can create databases
4. **Write** your first Datomic program (Java or Clojure)
5. **Execute** queries and understand basic datalog syntax

## Install Java (JVM)

Datomic requires Java 8 or later. Check if Java is already installed.

### Verify Java Installation

Open your terminal and run:

```bash
java -version
```

**Expected output** (Java 8+):

```
openjdk version "17.0.9" 2023-10-17
OpenJDK Runtime Environment (build 17.0.9+9)
OpenJDK 64-Bit Server VM (build 17.0.9+9, mixed mode)
```

If Java is not installed or version is below 8, continue to platform-specific installation.

### Windows Java Installation

**Step 1: Download Java**

1. Visit [Adoptium](https://adoptium.net/) (recommended OpenJDK distribution)
2. Download the latest LTS version (Java 17 or 21)
3. Choose Windows installer (`.msi` file)

**Step 2: Install Java**

1. Run the downloaded `.msi` installer
2. Follow installation wizard:
   - Accept license agreement
   - Keep default installation path
   - Check "Set JAVA_HOME variable" option
   - Check "Add to PATH" option
3. Click **Install** and wait for completion

**Step 3: Verify Installation**

Open new Command Prompt and run:

```cmd
java -version
javac -version
```

Both commands should display Java version information.

**Troubleshooting Windows**:

- If `java -version` fails, restart terminal to reload PATH
- Manually set JAVA_HOME: Control Panel → System → Advanced → Environment Variables
- Add `%JAVA_HOME%\bin` to PATH if not already present

### macOS Java Installation

**Using Homebrew** (recommended):

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew install openjdk@17

sudo ln -sfn $(brew --prefix)/opt/openjdk@17/libexec/openjdk.jdk \
  /Library/Java/JavaVirtualMachines/openjdk-17.jdk

echo 'export PATH="/usr/local/opt/openjdk@17/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

**Using Package** (alternative):

1. Download `.dmg` from [Adoptium](https://adoptium.net/)
2. Open `.dmg` file and run installer
3. Follow installation prompts
4. Restart terminal

**Verify installation**:

```bash
java -version
```

### Linux Java Installation

**Ubuntu/Debian**:

```bash
sudo apt update

sudo apt install -y openjdk-17-jdk

java -version
javac -version
```

**Fedora/RHEL/CentOS**:

```bash
sudo dnf install -y java-17-openjdk-devel

java -version
javac -version
```

**Arch Linux**:

```bash
sudo pacman -S jdk-openjdk

java -version
```

**Set JAVA_HOME** (all distributions):

Add to `~/.bashrc` or `~/.zshrc`:

```bash
export JAVA_HOME=$(dirname $(dirname $(readlink -f $(which java))))
export PATH=$JAVA_HOME/bin:$PATH
```

Reload shell:

```bash
source ~/.bashrc  # or source ~/.zshrc
```

## Download and Install Datomic Free

Datomic Free is the free edition suitable for development, evaluation, and small projects.

### Register for My Datomic Account

1. Visit [my.datomic.com](https://my.datomic.com/)
2. Sign up for free account (email verification required)
3. Log in to access downloads

### Download Datomic Free

**Step 1: Get Download Link**

1. Log into [my.datomic.com](https://my.datomic.com/)
2. Navigate to **Releases** → **Datomic Free**
3. Copy download URL for latest version (e.g., `0.9.5697`)

**Step 2: Download and Extract**

**macOS/Linux**:

```bash
mkdir -p ~/datomic
cd ~/datomic

wget https://datomic-free-downloads.s3.amazonaws.com/0.9.5697/datomic-free-0.9.5697.zip

curl -O https://datomic-free-downloads.s3.amazonaws.com/0.9.5697/datomic-free-0.9.5697.zip

unzip datomic-free-0.9.5697.zip

cd datomic-free-0.9.5697
```

**Windows**:

```cmd
REM Create directory for Datomic
mkdir C:\datomic
cd C:\datomic

REM Download using browser or PowerShell
REM Extract .zip file using Windows Explorer or 7-Zip

REM Navigate to extracted directory
cd datomic-free-0.9.5697
```

### Directory Structure

After extraction, you'll see:

```
datomic-free-0.9.5697/
├── bin/                    # Executable scripts
│   ├── transactor          # Database server (Unix)
│   ├── transactor.bat      # Database server (Windows)
│   └── repl                # Clojure REPL
├── lib/                    # JAR dependencies
├── samples/                # Example code
├── config/                 # Configuration files
└── README.txt              # Documentation
```

## Verify Datomic Installation

### Start Datomic Transactor (Database Server)

The transactor is Datomic's database server that handles writes and coordinates transactions.

**macOS/Linux**:

```bash
bin/transactor config/samples/free-transactor-template.properties
```

**Windows**:

```cmd
REM From datomic-free-0.9.5697 directory
bin\transactor.bat config\samples\free-transactor-template.properties
```

**Expected output**:

```
Launching with Java options -server -Xms1g -Xmx1g ...
Starting datomic:free://localhost:4334/<DB-NAME>, storing data in: data ...
System started datomic:free://localhost:4334
```

**Important**: Keep this terminal window open - transactor must run while you work with Datomic.

### Verify Transactor Health

Open a new terminal and check transactor status:

```bash
netstat -an | grep 4334

netstat -an | findstr 4334
```

**Expected output**:

```
tcp46      0      0  *.4334                 *.*                    LISTEN
```

Port 4334 listening means transactor is running successfully.

## Project Setup: Java

Set up a Java project to use Datomic as a library.

### Maven Project Setup

Create a Maven project with Datomic dependency.

**Step 1: Create Project Structure**

```bash
mkdir -p datomic-tutorial
cd datomic-tutorial
mkdir -p src/main/java/com/example
```

**Step 2: Create `pom.xml`**

```xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>com.example</groupId>
    <artifactId>datomic-tutorial</artifactId>
    <version>1.0-SNAPSHOT</version>

    <properties>
        <maven.compiler.source>17</maven.compiler.source>
        <maven.compiler.target>17</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>

    <dependencies>
        <!-- Datomic Free -->
        <dependency>
            <groupId>com.datomic</groupId>
            <artifactId>datomic-free</artifactId>
            <version>0.9.5697</version>
        </dependency>
    </dependencies>
</project>
```

**Step 3: Download Dependencies**

```bash
mvn dependency:resolve
```

This downloads Datomic and all its dependencies.

### Gradle Project Setup (Alternative)

**Step 1: Create Project Structure**

```bash
mkdir -p datomic-tutorial
cd datomic-tutorial
mkdir -p src/main/java/com/example
```

**Step 2: Create `build.gradle`**

```gradle
plugins {
    id 'java'
}

group = 'com.example'
version = '1.0-SNAPSHOT'

repositories {
    mavenCentral()
}

dependencies {
    implementation 'com.datomic:datomic-free:0.9.5697'
}

java {
    sourceCompatibility = JavaVersion.VERSION_17
    targetCompatibility = JavaVersion.VERSION_17
}
```

**Step 3: Download Dependencies**

```bash
gradle build
```

## Your First Datomic Program (Java)

Let's create a simple Java program that connects to Datomic, creates a database, and runs a query.

### Write the Program

Create `src/main/java/com/example/DatomicDemo.java`:

```java
package com.example;

import datomic.Peer;
import datomic.Connection;
import datomic.Database;

import java.util.List;
import java.util.Collection;

public class DatomicDemo {
    public static void main(String[] args) {
        // Database URI (in-memory database)
        String uri = "datomic:mem://hello";

        // Step 1: Create database
        boolean created = Peer.createDatabase(uri);
        System.out.println("Database created: " + created);

        // Step 2: Connect to database
        Connection conn = Peer.connect(uri);
        System.out.println("Connected to database");

        // Step 3: Get current database value
        Database db = conn.db();
        System.out.println("Database value retrieved");

        // Step 4: Simple query (count all datoms)
        String query = "[:find (count ?e) :where [?e :db/ident]]";
        Collection results = Peer.q(query, db);
        System.out.println("Schema attributes count: " + results);

        // Success!
        System.out.println("\nDatomic is working!");

        // Release connection
        conn.release();
    }
}
```

**Code breakdown**:

- `String uri = "datomic:mem://hello"`: In-memory database URI (data lost on exit)
- `Peer.createDatabase(uri)`: Creates new database
- `Peer.connect(uri)`: Returns connection to database
- `conn.db()`: Gets immutable database value (snapshot)
- `Peer.q(query, db)`: Executes datalog query
- `[:find (count ?e) :where [?e :db/ident]]`: Datalog query counting schema attributes

### Run the Program

**Using Maven**:

```bash
mvn compile exec:java -Dexec.mainClass="com.example.DatomicDemo"
```

**Using Gradle**:

```bash
gradle run --args="com.example.DatomicDemo"
```

**Expected output**:

```
Database created: true
Connected to database
Database value retrieved
Schema attributes count: [[87]]

Datomic is working!
```

The number `87` represents built-in schema attributes Datomic provides by default.

**Troubleshooting Java**:

- If compilation fails, verify Java 8+ and Maven/Gradle are installed
- If connection fails, ensure transactor is running (see "Start Datomic Transactor")
- If dependencies fail, check internet connection and Maven Central access

## Project Setup: Clojure

Set up a Clojure project using Datomic (native Clojure API).

### Leiningen Project Setup

**Step 1: Install Leiningen**

**macOS/Linux**:

```bash
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > lein

chmod +x lein

sudo mv lein /usr/local/bin/

lein
```

**Windows**:

1. Download `lein.bat` from [leiningen.org](https://leiningen.org/)
2. Place in a directory on PATH
3. Run `lein.bat` to self-install

**Step 2: Create Project**

```bash
lein new app datomic-tutorial
cd datomic-tutorial
```

**Step 3: Add Datomic Dependency**

Edit `project.clj`:

```clojure
(defproject datomic-tutorial "0.1.0-SNAPSHOT"
  :description "Datomic tutorial project"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [com.datomic/datomic-free "0.9.5697"]]
  :main ^:skip-aot datomic-tutorial.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
```

**Step 4: Download Dependencies**

```bash
lein deps
```

### Clojure CLI Project Setup (Alternative)

**Step 1: Install Clojure CLI**

**macOS/Linux**:

```bash
brew install clojure/tools/clojure

curl -O https://download.clojure.org/install/linux-install-1.11.1.1435.sh
chmod +x linux-install-1.11.1.1435.sh
sudo ./linux-install-1.11.1.1435.sh
```

**Windows**:

Download installer from [clojure.org](https://clojure.org/guides/install_clojure#_windows) and run.

**Step 2: Create Project**

```bash
mkdir datomic-tutorial
cd datomic-tutorial
```

**Step 3: Create `deps.edn`**

```clojure
{:deps {org.clojure/clojure {:mvn/version "1.11.1"}
        com.datomic/datomic-free {:mvn/version "0.9.5697"}}}
```

## Your First Datomic Program (Clojure)

Create a simple Clojure program demonstrating Datomic basics.

### Write the Program

**Using Leiningen** - Edit `src/datomic_tutorial/core.clj`:

```clojure
(ns datomic-tutorial.core
  (:require [datomic.api :as d]))

(defn -main
  [& args]
  ;; Database URI (in-memory database)
  (def uri "datomic:mem://hello")

  ;; Step 1: Create database
  (def created (d/create-database uri))
  (println "Database created:" created)

  ;; Step 2: Connect to database
  (def conn (d/connect uri))
  (println "Connected to database")

  ;; Step 3: Get current database value
  (def db (d/db conn))
  (println "Database value retrieved")

  ;; Step 4: Simple query (count all schema attributes)
  (def results (d/q '[:find (count ?e)
                      :where [?e :db/ident]]
                    db))
  (println "Schema attributes count:" results)

  ;; Success!
  (println "\nDatomic is working!")

  ;; Release connection
  (d/release conn))
```

**Using Clojure CLI** - Create `src/demo.clj`:

```clojure
(ns demo
  (:require [datomic.api :as d]))

;; Database URI
(def uri "datomic:mem://hello")

;; Create and connect
(d/create-database uri)
(def conn (d/connect uri))
(def db (d/db conn))

;; Query schema
(def results (d/q '[:find (count ?e)
                    :where [?e :db/ident]]
                  db))

(println "Schema attributes count:" results)
(println "Datomic is working!")

(d/release conn)
```

**Code breakdown**:

- `(require [datomic.api :as d])`: Import Datomic API as `d` namespace
- `(d/create-database uri)`: Creates new database
- `(d/connect uri)`: Returns connection
- `(d/db conn)`: Gets immutable database value
- `(d/q query db)`: Executes datalog query
- `'[:find (count ?e) :where [?e :db/ident]]`: Datalog query as quoted list

### Run the Program

**Using Leiningen**:

```bash
lein run
```

**Using Clojure CLI**:

```bash
clj -M -m demo
```

**Expected output**:

```
Database created: true
Connected to database
Database value retrieved
Schema attributes count: #{[87]}

Datomic is working!
```

**Troubleshooting Clojure**:

- If Leiningen fails, ensure Java 8+ is installed
- If connection fails, verify transactor is running
- If dependencies fail, check internet connection

## Understanding Datomic URIs

Datomic uses URIs to specify database storage and location.

### URI Format

```
datomic:<storage-backend>://<host>/<database-name>
```

**Storage backends**:

- **`mem`**: In-memory (data lost on exit, development only)
- **`free`**: Free persistent storage (local filesystem)
- **`dev`**: Development storage with in-memory cache
- **`sql`**: SQL database backend (PostgreSQL, etc.)
- **`ddb`**: AWS DynamoDB backend (Pro edition only)

### Common URI Examples

**In-memory** (development, data not persisted):

```
datomic:mem://my-database
```

**Free persistent** (local filesystem):

```
datomic:free://localhost:4334/my-database
```

**Dev mode** (transactor required):

```
datomic:dev://localhost:4334/my-database
```

## Environment Variables

Configure Datomic behavior using environment variables.

### Key Environment Variables

**DATOMIC_HOME**: Path to Datomic installation

```bash
export DATOMIC_HOME=~/datomic/datomic-free-0.9.5697
```

**PATH**: Add Datomic bin directory

```bash
export PATH=$DATOMIC_HOME/bin:$PATH
```

**Memory settings** (transactor):

Edit transactor properties file:

```properties
memory-index-max=256m
memory-index-threshold=32m
object-cache-max=128m
```

### Verify Environment

**Check Datomic installation**:

```bash
ls -la $DATOMIC_HOME
```

**Check transactor availability**:

```bash
which transactor  # Unix
where transactor  # Windows
```

## Next Steps

You now have Datomic installed and working. Here's what to learn next:

1. **[Quick Start](/en/learn/software-engineering/data/databases/datomic/quick-start)** - Build a complete application with schema, transactions, and queries (5-30% coverage)
2. **[By-Example Tutorial](/en/learn/software-engineering/data/databases/datomic/by-example)** - Learn through 80 annotated examples covering 95% of Datomic
3. **[Official Datomic Documentation](https://docs.datomic.com/)** - Comprehensive reference and guides

## Summary

In this initial setup tutorial, you learned how to:

1. Install Java (JVM) required by Datomic
2. Download and extract Datomic Free edition
3. Start the Datomic transactor (database server)
4. Create a Maven or Leiningen project with Datomic dependency
5. Write and run your first Datomic program in Java or Clojure
6. Execute datalog queries and understand database URIs

You're now ready to explore Datomic's powerful features: immutable facts, time-travel queries, and flexible datalog. Continue to the Quick Start tutorial to build a real application.

## Common Issues and Solutions

### Transactor Won't Start

**Problem**: Transactor fails with memory error

**Solution**: Reduce memory settings in transactor properties file

```properties
memory-index-max=128m  # Reduced from 256m
```

### Connection Timeout

**Problem**: Program can't connect to transactor

**Solutions**:

1. Verify transactor is running: `netstat -an | grep 4334`
2. Check firewall allows port 4334
3. Use `datomic:mem://` URI for in-memory database (no transactor needed)

### Dependency Download Fails

**Problem**: Maven/Gradle can't download Datomic

**Solutions**:

1. Check internet connection
2. Verify Maven Central is accessible
3. Try using Datomic Maven repository directly:

```xml
<repositories>
    <repository>
        <id>datomic</id>
        <url>https://my.datomic.com/repo</url>
    </repository>
</repositories>
```

### Java Version Incompatibility

**Problem**: Datomic fails with "Unsupported class file version"

**Solution**: Upgrade to Java 8 or later (Datomic requires Java 8+)

```bash
java -version  # Should show 1.8.0 or higher
```

## Additional Resources

- [Official Datomic Documentation](https://docs.datomic.com/)
- [Datomic Forum](https://forum.datomic.com/)
- [Day of Datomic Tutorial](https://github.com/Datomic/day-of-datomic)
- [Learn Datalog Today](http://www.learndatalogtoday.org/)
- [Datomic Mailing List](https://groups.google.com/g/datomic)
