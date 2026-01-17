---
title: "Initial Setup"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100001
description: "Install Spring Boot and create your first web application"
tags:
  - java
  - spring-boot
  - web-framework
  - installation
---

Get Spring Boot installed and create your first web application. This guide walks you through installation and building your first Spring Boot app.

## 🎯 What You'll Accomplish

By the end of this tutorial, you'll have:

- ✅ Java and Spring Boot set up
- ✅ Your first Spring Boot project created
- ✅ REST API endpoint running

## 📋 Prerequisites

- Java 17 or later
- Maven or Gradle
- IDE (IntelliJ IDEA, Eclipse, or VS Code)

## 💾 Step 1: Create Spring Boot Project

Visit [start.spring.io](https://start.spring.io/) and create a project with:

- Spring Boot 3.2.x
- Dependencies: Spring Web

Or use command line:

```bash
curl https://start.spring.io/starter.zip \
  -d dependencies=web \
  -d bootVersion=3.2.0 \
  -d javaVersion=17 \
  -o spring-boot-demo.zip
unzip spring-boot-demo.zip
cd spring-boot-demo
```

## 🚀 Step 2: Create Your First Controller

Create `src/main/java/com/example/demo/HelloController.java`:

```java
package com.example.demo;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class HelloController {

    @GetMapping("/")
    public String hello() {
        return "Hello, Spring Boot!";
    }

    @GetMapping("/api/greeting")
    public Greeting greeting() {
        return new Greeting("Hello, World!");
    }
}

record Greeting(String message) {}
```

## 📊 Step 3: Run the Application

```bash
./mvnw spring-boot:run

# Or with Gradle
./gradlew bootRun
```

Visit:

- `http://localhost:8080/` - See hello message
- `http://localhost:8080/api/greeting` - See JSON response

## ✅ Verification Checklist

Before moving forward, verify:

- [ ] Project created successfully
- [ ] Application starts without errors
- [ ] Endpoints respond correctly

## 🎉 You're Done

You've successfully created your first Spring Boot web application.

## 📚 What's Next?

**Quick learner**: [Java Spring Boot Quick Start](/en/learn/software-engineering/web-platform/java-spring-boot/tutorials/quick-start)

**Code-first learner**: [Java Spring Boot By Example](/en/learn/software-engineering/web-platform/java-spring-boot/tutorials/by-example)
