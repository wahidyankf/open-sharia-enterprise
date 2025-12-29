---
title: "Quick Start"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100002
description: "Learn essential Spring Data JPA operations and query patterns"
tags:
  - java
  - spring
  - jpa
  - database
  - quick-start
---

Learn essential Spring Data JPA operations and query patterns for effective database work with Spring Boot. This Quick Start teaches core Spring Data JPA concepts.

## 🎯 What You'll Learn

By the end of this tutorial, you'll understand:

- Repository methods and query derivation
- Custom queries with @Query
- Relationships and associations
- Pagination and sorting

## 📋 Prerequisites

- Spring Data JPA installed (see [Initial Setup](/en/learn/software-engineering/data/tools/spring-data-jpa/tutorials/initial-setup))
- Basic Java and Spring Boot knowledge

## 📊 Repository Methods

### Built-in Methods

```java
// JpaRepository provides:
userRepository.findAll();
userRepository.findById(1L);
userRepository.save(user);
userRepository.delete(user);
userRepository.count();
```

### Query Method Derivation

```java
public interface UserRepository extends JpaRepository<User, Long> {
    List<User> findByName(String name);
    List<User> findByAgeGreaterThan(Integer age);
    List<User> findByNameAndAge(String name, Integer age);
    List<User> findByEmailContaining(String emailPart);
    List<User> findByOrderByNameAsc();
}
```

## 🔍 Custom Queries

### JPQL Queries

```java
public interface UserRepository extends JpaRepository<User, Long> {
    @Query("SELECT u FROM User u WHERE u.email LIKE %:domain%")
    List<User> findByEmailDomain(@Param("domain") String domain);

    @Query("SELECT u FROM User u WHERE u.age > :age ORDER BY u.name")
    List<User> findUsersOlderThan(@Param("age") Integer age);
}
```

### Native SQL Queries

```java
@Query(value = "SELECT * FROM users WHERE age > ?1", nativeQuery = true)
List<User> findUsersOlderThanNative(Integer age);
```

## 🔗 Relationships

### One-to-Many

```java
@Entity
public class User {
    @Id
    @GeneratedValue
    private Long id;

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL)
    private List<Post> posts;
}

@Entity
public class Post {
    @Id
    @GeneratedValue
    private Long id;

    @ManyToOne
    @JoinColumn(name = "user_id")
    private User user;
}
```

### Many-to-Many

```java
@Entity
public class User {
    @ManyToMany
    @JoinTable(
        name = "user_roles",
        joinColumns = @JoinColumn(name = "user_id"),
        inverseJoinColumns = @JoinColumn(name = "role_id")
    )
    private Set<Role> roles;
}
```

## 📄 Pagination and Sorting

```java
// Pagination
Pageable pageable = PageRequest.of(0, 10);
Page<User> users = userRepository.findAll(pageable);

// Sorting
Sort sort = Sort.by(Sort.Direction.DESC, "name");
List<User> users = userRepository.findAll(sort);

// Both
Pageable pageable = PageRequest.of(0, 10, Sort.by("name").ascending());
Page<User> users = userRepository.findAll(pageable);
```

## ✅ Next Steps

You now understand Spring Data JPA essentials! To deepen your knowledge:

1. **Try the examples**: Create repositories and execute operations
2. **Explore By Example**: [Spring Data JPA By Example](/en/learn/software-engineering/data/tools/spring-data-jpa/tutorials/by-example)

## 🎯 Self-Assessment

After completing this Quick Start, you should be able to:

- [ ] Use built-in repository methods
- [ ] Create query methods with derivation
- [ ] Write custom JPQL and native queries
- [ ] Define entity relationships
- [ ] Implement pagination and sorting
