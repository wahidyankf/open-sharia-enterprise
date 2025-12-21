---
title: "How to Set Up Gradle with Kotlin DSL"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 1000020
description: "Configure Gradle builds using type-safe Kotlin DSL"
tags: ["kotlin", "gradle", "build-tools", "kotlin-dsl"]
categories: ["learn"]
---

## Problem

Gradle's Groovy DSL lacks type safety and IDE support. Kotlin DSL (build.gradle.kts) provides compile-time checking, auto-completion, and refactoring support. However, the syntax differs from Groovy.

This guide shows how to configure Gradle projects with Kotlin DSL.

## Basic Setup

### Creating build.gradle.kts

Initialize a Kotlin project with Gradle Kotlin DSL.

```kotlin
// ✅ build.gradle.kts - basic structure
plugins {
  kotlin("jvm") version "2.3.0"
  application
}

group = "com.example"
version = "1.0.0"

repositories {
  mavenCentral()
}

dependencies {
  implementation(kotlin("stdlib"))
  testImplementation(kotlin("test"))
}

kotlin {
  jvmToolchain(17)
}

application {
  mainClass.set("com.example.MainKt")
}
```

**Key differences from Groovy**:

- Parentheses for function calls: `implementation()` not `implementation`
- String literals in quotes: `"junit:junit:4.13.2"`
- Type-safe accessors for extensions

### Settings File

Configure multi-project builds.

```kotlin
// ✅ settings.gradle.kts
rootProject.name = "my-kotlin-app"

include("app")
include("lib-common")
include("lib-data")
```

## Dependency Management

### Adding Dependencies

Type-safe dependency declarations.

```kotlin
// ✅ Dependencies with version catalogs
dependencies {
  // Standard library
  implementation(kotlin("stdlib"))

  // Coroutines
  implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.8.0")

  // Ktor
  implementation("io.ktor:ktor-server-core:2.3.7")
  implementation("io.ktor:ktor-server-netty:2.3.7")

  // Serialization
  implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.6.2")

  // Testing
  testImplementation(kotlin("test"))
  testImplementation("io.mockk:mockk:1.13.9")
  testImplementation("org.jetbrains.kotlinx:kotlinx-coroutines-test:1.8.0")
}
```

### Version Catalogs

Centralize dependency versions.

```toml
# ✅ gradle/libs.versions.toml
[versions]
kotlin = "2.3.0"
ktor = "2.3.7"
coroutines = "1.8.0"

[libraries]
kotlin-stdlib = { module = "org.jetbrains.kotlin:kotlin-stdlib", version.ref = "kotlin" }
ktor-server-core = { module = "io.ktor:ktor-server-core", version.ref = "ktor" }
ktor-server-netty = { module = "io.ktor:ktor-server-netty", version.ref = "ktor" }
coroutines-core = { module = "org.jetbrains.kotlinx:kotlinx-coroutines-core", version.ref = "coroutines" }

[plugins]
kotlin-jvm = { id = "org.jetbrains.kotlin.jvm", version.ref = "kotlin" }
```

```kotlin
// ✅ build.gradle.kts with version catalog
plugins {
  alias(libs.plugins.kotlin.jvm)
}

dependencies {
  implementation(libs.kotlin.stdlib)
  implementation(libs.ktor.server.core)
  implementation(libs.ktor.server.netty)
  implementation(libs.coroutines.core)
}
```

## Plugin Configuration

### Applying Plugins

Type-safe plugin application.

```kotlin
// ✅ Plugins block (preferred)
plugins {
  kotlin("jvm") version "2.3.0"
  kotlin("plugin.serialization") version "2.3.0"
  id("io.ktor.plugin") version "2.3.7"
  application
}

// ✅ Configure plugins
kotlin {
  jvmToolchain(17)
}

application {
  mainClass.set("com.example.MainKt")
}

ktor {
  fatJar {
    archiveFileName.set("app.jar")
  }
}
```

### Kotlin Compiler Options

Configure compiler settings.

```kotlin
// ✅ Kotlin compiler options
tasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
  kotlinOptions {
    jvmTarget = "17"
    freeCompilerArgs = listOf(
      "-Xjsr305=strict",  // Strict null safety
      "-Xcontext-receivers"  // Enable context receivers
    )
  }
}
```

## Task Configuration

### Creating Custom Tasks

Define tasks with Kotlin DSL.

```kotlin
// ✅ Custom task
tasks.register("hello") {
  doLast {
    println("Hello from Kotlin DSL!")
  }
}

// ✅ Typed task
tasks.register<Jar>("customJar") {
  archiveFileName.set("custom.jar")
  from(sourceSets["main"].output)
}

// ✅ Task with dependencies
tasks.register("integrationTest") {
  dependsOn("test")
  doLast {
    println("Running integration tests...")
  }
}
```

### Configuring Existing Tasks

Modify built-in tasks.

```kotlin
// ✅ Configure test task
tasks.test {
  useJUnitPlatform()

  testLogging {
    events("passed", "skipped", "failed")
    showStandardStreams = true
  }

  maxParallelForks = Runtime.getRuntime().availableProcessors()
}

// ✅ Configure jar task
tasks.jar {
  manifest {
    attributes(
      "Main-Class" to "com.example.MainKt",
      "Implementation-Version" to version
    )
  }

  // Include dependencies (fat jar)
  from(configurations.runtimeClasspath.get().map { if (it.isDirectory) it else zipTree(it) })
  duplicatesStrategy = DuplicatesStrategy.EXCLUDE
}
```

## Source Sets

### Configuring Source Sets

Define source and resource directories.

```kotlin
// ✅ Custom source sets
sourceSets {
  main {
    kotlin.srcDirs("src/main/kotlin", "src/main/generated")
    resources.srcDir("src/main/resources")
  }

  test {
    kotlin.srcDir("src/test/kotlin")
    resources.srcDir("src/test/resources")
  }

  // Custom source set
  create("integrationTest") {
    kotlin.srcDir("src/integrationTest/kotlin")
    resources.srcDir("src/integrationTest/resources")
    compileClasspath += sourceSets["main"].output
    runtimeClasspath += sourceSets["main"].output
  }
}
```

## Multi-Project Builds

### Configuring Subprojects

Organize multi-module projects.

```kotlin
// ✅ Root build.gradle.kts
plugins {
  kotlin("jvm") version "2.3.0" apply false
}

allprojects {
  group = "com.example"
  version = "1.0.0"

  repositories {
    mavenCentral()
  }
}

subprojects {
  apply(plugin = "kotlin")

  dependencies {
    implementation(kotlin("stdlib"))
    testImplementation(kotlin("test"))
  }

  tasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
    kotlinOptions.jvmTarget = "17"
  }
}
```

### Project Dependencies

Reference other modules.

```kotlin
// ✅ app/build.gradle.kts
dependencies {
  implementation(project(":lib-common"))
  implementation(project(":lib-data"))
}
```

## Common Pitfalls

### String vs Function Syntax

```kotlin
// ❌ Groovy syntax doesn't work
dependencies {
  implementation 'com.example:library:1.0.0'  // ❌ Wrong
}

// ✅ Kotlin DSL requires parentheses and quotes
dependencies {
  implementation("com.example:library:1.0.0")  // ✅ Correct
}
```

### Plugin Application

```kotlin
// ❌ Old apply plugin syntax
apply plugin: 'kotlin'  // ❌ Groovy syntax

// ✅ Plugins block
plugins {
  kotlin("jvm") version "2.3.0"
}

// ✅ Or apply for buildSrc plugins
apply(plugin = "kotlin")
```

### Configuration vs Task Execution

```kotlin
// ❌ Wrong - executes during configuration
tasks.register("wrong") {
  println("This runs during configuration!")  // ❌ Bad
}

// ✅ Correct - executes when task runs
tasks.register("correct") {
  doLast {
    println("This runs when task executes")  // ✅ Good
  }
}
```

## Variations

### buildSrc for Custom Logic

Share build logic across projects.

```kotlin
// ✅ buildSrc/src/main/kotlin/Dependencies.kt
object Versions {
  const val kotlin = "2.3.0"
  const val ktor = "2.3.7"
  const val coroutines = "1.8.0"
}

object Libs {
  const val ktorCore = "io.ktor:ktor-server-core:${Versions.ktor}"
  const val coroutines = "org.jetbrains.kotlinx:kotlinx-coroutines-core:${Versions.coroutines}"
}

// ✅ Use in build.gradle.kts
dependencies {
  implementation(Libs.ktorCore)
  implementation(Libs.coroutines)
}
```

### Composite Builds

Include external projects.

```kotlin
// ✅ settings.gradle.kts
includeBuild("../shared-library")

// Use in dependencies
dependencies {
  implementation("com.example:shared-library")
}
```

## Related Patterns

**Learn more**:

- [Initial Setup Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/initial-setup) - Gradle installation and basics
- [Intermediate Tutorial - REST API](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate#rest-api-with-ktor) - Gradle with Ktor
- [Managing Dependencies](/en/learn/swe/prog-lang/kotlin/how-to/manage-dependencies-gradle) - Advanced dependency management

**Cookbook recipes**:

- [JDBC Extensions](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#jdbc-extensions) - Database dependencies
- [REST API with Ktor](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#rest-api-ktor) - Ktor dependencies
