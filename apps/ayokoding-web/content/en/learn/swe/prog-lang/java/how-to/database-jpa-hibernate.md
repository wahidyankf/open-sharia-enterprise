---
title: Database Access with JPA and Hibernate
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 628
description: Master JPA entities, repositories, transactions, and query optimization
tags: ["java", "jpa", "hibernate", "database", "orm"]
---

## Entity Mapping

### Basic Entity

```java
@Entity
@Table(name = "users")
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false, unique = true)
    private String username;

    @Column(nullable = false)
    private String email;

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<Order> orders = new ArrayList<>();

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "department_id")
    private Department department;

    // Getters and setters
}
```

### Entity Relationships

```java
// One-to-One
@Entity
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @OneToOne(mappedBy = "user", cascade = CascadeType.ALL)
    private UserProfile profile;
}

@Entity
public class UserProfile {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @OneToOne
    @JoinColumn(name = "user_id")
    private User user;

    private String bio;
    private String avatarUrl;
}

// Many-to-Many
@Entity
public class Student {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToMany
    @JoinTable(
        name = "student_course",
        joinColumns = @JoinColumn(name = "student_id"),
        inverseJoinColumns = @JoinColumn(name = "course_id")
    )
    private Set<Course> courses = new HashSet<>();
}

@Entity
public class Course {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToMany(mappedBy = "courses")
    private Set<Student> students = new HashSet<>();
}
```

### Embedded and Embeddable

```java
@Embeddable
public class Address {
    private String street;
    private String city;
    private String zipCode;
    private String country;

    // Getters and setters
}

@Entity
public class Company {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String name;

    @Embedded
    private Address address;

    @Embedded
    @AttributeOverrides({
        @AttributeOverride(name = "street", column = @Column(name = "billing_street")),
        @AttributeOverride(name = "city", column = @Column(name = "billing_city"))
    })
    private Address billingAddress;
}
```

## Spring Data JPA Repositories

```java
public interface UserRepository extends JpaRepository<User, Long> {
    Optional<User> findByUsername(String username);
    List<User> findByEmailContaining(String email);

    @Query("SELECT u FROM User u WHERE u.email = :email")
    Optional<User> findByEmailCustom(@Param("email") String email);

    @Query(value = "SELECT * FROM users WHERE created_at > ?1", nativeQuery = true)
    List<User> findRecentUsers(LocalDateTime since);
}
```

## Transaction Management

```java
@Service
public class OrderService {
    @Transactional
    public Order createOrder(CreateOrderRequest request) {
        Order order = new Order();
        order.setItems(request.getItems());
        return orderRepository.save(order);
    }

    @Transactional(readOnly = true)
    public List<Order> findAll() {
        return orderRepository.findAll();
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void logActivity(String message) {
        // Runs in separate transaction
        activityLog.save(new Activity(message));
    }
}
```

## Query Optimization

### Solving N+1 Problem

```java
// N+1 problem solution
@Query("SELECT u FROM User u JOIN FETCH u.orders WHERE u.id = :id")
Optional<User> findByIdWithOrders(@Param("id") Long id);

// Multiple joins
@Query("SELECT u FROM User u " +
       "LEFT JOIN FETCH u.orders o " +
       "LEFT JOIN FETCH o.items " +
       "WHERE u.id = :id")
Optional<User> findByIdWithOrdersAndItems(@Param("id") Long id);

// Entity graph
@EntityGraph(attributePaths = {"orders", "orders.items"})
@Query("SELECT u FROM User u WHERE u.id = :id")
Optional<User> findByIdWithGraph(@Param("id") Long id);
```

### Pagination and Sorting

```java
// Pagination
Page<User> findAll(Pageable pageable);

// Usage
Pageable pageable = PageRequest.of(0, 20, Sort.by("username"));
Page<User> users = userRepository.findAll(pageable);

// Custom pagination query
@Query("SELECT u FROM User u WHERE u.createdAt > :date")
Page<User> findRecentUsers(@Param("date") LocalDateTime date, Pageable pageable);

// Multiple sort fields
Pageable pageable = PageRequest.of(
    0,
    20,
    Sort.by("lastName").ascending()
        .and(Sort.by("firstName").ascending())
);
```

### Projection and DTO

```java
// Interface projection
public interface UserSummary {
    String getUsername();
    String getEmail();
    Long getOrderCount();
}

@Query("SELECT u.username as username, u.email as email, COUNT(o) as orderCount " +
       "FROM User u LEFT JOIN u.orders o GROUP BY u.id")
List<UserSummary> findUserSummaries();

// Class-based DTO
public record UserDTO(String username, String email, int orderCount) {}

@Query("SELECT new com.example.dto.UserDTO(u.username, u.email, SIZE(u.orders)) " +
       "FROM User u")
List<UserDTO> findUserDTOs();
```

## Caching Strategies

### Second-Level Cache

```java
@Entity
@Cacheable
@org.hibernate.annotations.Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String username;
    private String email;

    @OneToMany(mappedBy = "user")
    @org.hibernate.annotations.Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
    private List<Order> orders;
}
```

### Query Cache

```java
@Repository
public interface UserRepository extends JpaRepository<User, Long> {
    @QueryHints(@QueryHint(name = "org.hibernate.cacheable", value = "true"))
    @Query("SELECT u FROM User u WHERE u.email = :email")
    Optional<User> findByEmail(@Param("email") String email);
}
```

### Spring Cache

```java
@Service
public class UserService {
    @Cacheable(value = "users", key = "#id")
    public Optional<User> findById(Long id) {
        return userRepository.findById(id);
    }

    @CachePut(value = "users", key = "#user.id")
    public User update(User user) {
        return userRepository.save(user);
    }

    @CacheEvict(value = "users", key = "#id")
    public void delete(Long id) {
        userRepository.deleteById(id);
    }

    @CacheEvict(value = "users", allEntries = true)
    public void evictAllUsers() {
        // Clear all user cache
    }
}
```

## Advanced Transaction Patterns

### Transaction Isolation

```java
@Service
public class OrderService {
    @Transactional(isolation = Isolation.SERIALIZABLE)
    public void processPayment(Long orderId) {
        Order order = orderRepository.findById(orderId)
            .orElseThrow(() -> new OrderNotFoundException(orderId));

        if (order.getStatus() != OrderStatus.PENDING) {
            throw new InvalidOrderStateException("Order already processed");
        }

        order.setStatus(OrderStatus.PAID);
        orderRepository.save(order);
    }

    @Transactional(isolation = Isolation.READ_COMMITTED)
    public List<Order> findPendingOrders() {
        return orderRepository.findByStatus(OrderStatus.PENDING);
    }
}
```

### Programmatic Transactions

```java
@Service
public class TransferService {
    private final PlatformTransactionManager transactionManager;
    private final TransactionTemplate transactionTemplate;

    public TransferService(PlatformTransactionManager transactionManager) {
        this.transactionManager = transactionManager;
        this.transactionTemplate = new TransactionTemplate(transactionManager);
    }

    public void transfer(Long fromId, Long toId, BigDecimal amount) {
        transactionTemplate.execute(status -> {
            try {
                Account from = accountRepository.findById(fromId)
                    .orElseThrow(() -> new AccountNotFoundException(fromId));
                Account to = accountRepository.findById(toId)
                    .orElseThrow(() -> new AccountNotFoundException(toId));

                from.withdraw(amount);
                to.deposit(amount);

                accountRepository.save(from);
                accountRepository.save(to);

                return null;
            } catch (Exception e) {
                status.setRollbackOnly();
                throw e;
            }
        });
    }
}
```

## Related Resources

**Learn more**:

- [Glossary](/en/learn/swe/prog-lang/java/reference/glossary) - JPA/ORM terms
- [Resources](/en/learn/swe/prog-lang/java/reference/resources) - Hibernate docs
