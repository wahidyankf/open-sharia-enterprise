---
title: "Quick Start"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100002
description: "Learn core Spring Boot concepts and web development patterns"
tags:
  - java
  - spring-boot
  - web-framework
  - quick-start
---

Learn core Spring Boot concepts and web development patterns. This Quick Start teaches essential Spring Boot skills.

## 🎯 What You'll Learn

By the end of this tutorial, you'll understand:

- REST controllers and request mapping
- Dependency injection
- Configuration properties
- JPA and database access

## 📋 Prerequisites

- Spring Boot installed (see [Initial Setup](/en/learn/software-engineering/platforms/web/java-spring-boot/tutorials/initial-setup))

## 🛣️ REST Controllers

```java
@RestController
@RequestMapping("/api/users")
public class UserController {

    @GetMapping
    public List<User> getAllUsers() {
        return userService.findAll();
    }

    @GetMapping("/{id}")
    public User getUser(@PathVariable Long id) {
        return userService.findById(id);
    }

    @PostMapping
    public User createUser(@RequestBody User user) {
        return userService.save(user);
    }

    @PutMapping("/{id}")
    public User updateUser(@PathVariable Long id, @RequestBody User user) {
        return userService.update(id, user);
    }

    @DeleteMapping("/{id}")
    public void deleteUser(@PathVariable Long id) {
        userService.delete(id);
    }
}
```

## 🔧 Dependency Injection

```java
@Service
public class UserService {
    private final UserRepository userRepository;

    @Autowired
    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    public List<User> findAll() {
        return userRepository.findAll();
    }
}
```

## ⚙️ Configuration Properties

Create `src/main/resources/application.properties`:

```properties
server.port=8080
spring.application.name=myapp

# Database
spring.datasource.url=jdbc:postgresql://localhost:5432/mydb
spring.datasource.username=postgres
spring.datasource.password=secret

# JPA
spring.jpa.hibernate.ddl-auto=update
spring.jpa.show-sql=true
```

Or use `application.yml`:

```yaml
server:
  port: 8080

spring:
  application:
    name: myapp
  datasource:
    url: jdbc:postgresql://localhost:5432/mydb
    username: postgres
    password: secret
  jpa:
    hibernate:
      ddl-auto: update
    show-sql: true
```

## 📊 JPA and Database

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

    // Getters and setters
}

public interface UserRepository extends JpaRepository<User, Long> {
    Optional<User> findByEmail(String email);
    List<User> findByNameContaining(String name);
}
```

## ✅ Next Steps

You now understand Spring Boot essentials!

1. **Try the examples**: Build REST APIs and services
2. **Explore By Example**: [Java Spring Boot By Example](/en/learn/software-engineering/platforms/web/java-spring-boot/tutorials/by-example)

## 🎯 Self-Assessment

After completing this Quick Start, you should be able to:

- [ ] Create REST controllers
- [ ] Use dependency injection
- [ ] Configure application properties
- [ ] Work with JPA and databases
