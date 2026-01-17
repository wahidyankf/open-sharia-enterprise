---
title: "Initial Setup"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100001
description: "Install Spring Data JPA and create your first repository"
tags:
  - java
  - spring
  - jpa
  - database
  - orm
---

Get Spring Data JPA installed and create your first repository. This guide walks you through setting up Spring Data JPA for database operations in Java.

## 🎯 What You'll Accomplish

By the end of this tutorial, you'll have:

- ✅ Spring Boot project with Spring Data JPA
- ✅ Database connection configured
- ✅ Your first entity and repository created
- ✅ Basic CRUD operations working

## 📋 Prerequisites

- Java 17 or later installed
- Maven or Gradle installed
- Basic familiarity with Java and Spring Boot

## 💾 Step 1: Create Spring Boot Project

Visit [start.spring.io](https://start.spring.io/) and create a project with:

- Spring Boot 3.x
- Dependencies: Spring Data JPA, H2 Database (for testing), PostgreSQL Driver

Or use command line:

```bash
curl https://start.spring.io/starter.zip \
  -d dependencies=data-jpa,h2,postgresql \
  -d bootVersion=3.2.0 \
  -d javaVersion=17 \
  -o spring-jpa-demo.zip
unzip spring-jpa-demo.zip
cd spring-jpa-demo
```

## ⚙️ Step 2: Configure Database

Edit `src/main/resources/application.properties`:

```properties
spring.datasource.url=jdbc:h2:mem:testdb
spring.datasource.driverClassName=org.h2.Driver
spring.jpa.database-platform=org.hibernate.dialect.H2Dialect
spring.h2.console.enabled=true
spring.jpa.hibernate.ddl-auto=create-drop
spring.jpa.show-sql=true
```

## 📦 Step 3: Create Your First Entity

Create `src/main/java/com/example/demo/User.java`:

```java
@Entity
@Table(name = "users")
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String name;

    @Column(nullable = false, unique = true)
    private String email;

    private Integer age;

    // Getters and setters
}
```

## 🔧 Step 4: Create Repository

Create `src/main/java/com/example/demo/UserRepository.java`:

```java
public interface UserRepository extends JpaRepository<User, Long> {
    Optional<User> findByEmail(String email);
    List<User> findByAgeGreaterThan(Integer age);
}
```

## 🚀 Step 5: Test CRUD Operations

Create a test or command line runner:

```java
@SpringBootApplication
public class DemoApplication implements CommandLineRunner {

    @Autowired
    private UserRepository userRepository;

    public static void main(String[] args) {
        SpringApplication.run(DemoApplication.class, args);
    }

    @Override
    public void run(String... args) {
        // Create
        User user = new User();
        user.setName("Alice");
        user.setEmail("alice@example.com");
        user.setAge(30);
        userRepository.save(user);

        // Read
        List<User> users = userRepository.findAll();
        users.forEach(u -> System.out.println(u.getName()));
    }
}
```

## ✅ Verification Checklist

Before moving forward, verify:

- [ ] Spring Boot project created with Spring Data JPA
- [ ] Database connection configured
- [ ] Entity and repository created
- [ ] CRUD operations work

## 🎉 You're Done

You've successfully set up Spring Data JPA and created your first repository. You're ready to work with database operations.

## 📚 What's Next?

**Quick learner**: [Spring Data JPA Quick Start](/en/learn/software-engineering/data/tools/spring-data-jpa/tutorials/quick-start)

**Code-first learner**: [Spring Data JPA By Example](/en/learn/software-engineering/data/tools/spring-data-jpa/tutorials/by-example)
