---
title: Intermediate Java - Production-Ready Development
description: Learn production techniques and patterns - design patterns, SOLID principles, concurrency, performance, and database integration for professional development
category: tutorial
tags:
	- java
	- intermediate
	- design-patterns
	- concurrency
	- performance
	- production
	- jdbc
created: 2025-12-04
updated: 2025-12-04
---

# Intermediate Java - Production-Ready Development

**Master production-ready Java techniques in 4-8 hours.** This tutorial bridges the gap between basic understanding and professional development. You'll learn design patterns, advanced concurrency, performance optimization, and integration with databases and build tools.

## What You'll Achieve

By the end of this tutorial (4-8 hours), you will:

- ✅ Understand and apply common design patterns
- ✅ Apply SOLID principles to real code
- ✅ Work with multiple threads safely
- ✅ Profile and optimize Java applications
- ✅ Use Maven and Gradle for project management
- ✅ Connect to databases with JDBC
- ✅ Implement security best practices
- ✅ Use advanced collections techniques
- ✅ Design and architect medium-scale systems
- ✅ Know what to learn for expert mastery

## Prerequisites

- Completed [Beginner's Guide to Java](./tu-se-pl-ja__beginner.md) - Comfortable with OOP, collections, testing
- Or equivalent Java experience with classes, inheritance, interfaces

## Learning Path Overview

This tutorial covers **60-85% of Java knowledge** needed for production development. It's focused on practical, real-world techniques.

We'll progress through:

1. **Design Patterns** - Reusable solutions to common problems
2. **SOLID Principles in Practice** - Applying to real code
3. **Advanced Concurrency** - Multiple threads and synchronization
4. **Build Tools** - Maven and Gradle for project management
5. **Performance and Profiling** - Making Java fast
6. **Database Integration** - JDBC and data persistence
7. **Advanced Collections** - Beyond the basics
8. **Security Best Practices** - Protecting applications
9. **System Design** - Architecting larger systems
10. **Production Patterns** - Real-world techniques

---

## Part 1: Design Patterns

Design patterns are proven solutions to common problems. They make code more reusable, maintainable, and understandable.

### 1.1 Singleton Pattern

**Singleton** ensures a class has only one instance.

#### Example: Singleton

```java
// Thread-safe singleton using eager initialization
public class DatabaseConnection {
    private static final DatabaseConnection instance = new DatabaseConnection();

    private DatabaseConnection() {
        System.out.println("DatabaseConnection initialized");
    }

    public static DatabaseConnection getInstance() {
        return instance;
    }

    public void connect(String url) {
        System.out.println("Connecting to: " + url);
    }
}

// Usage
public class SingletonDemo {
    public static void main(String[] args) {
        DatabaseConnection db1 = DatabaseConnection.getInstance();
        DatabaseConnection db2 = DatabaseConnection.getInstance();

        System.out.println(db1 == db2);  // true - same instance
    }
}
```

**Use Singleton for**:

- Database connections
- Logger instances
- Configuration managers
- Thread pools

### 1.2 Factory Pattern

**Factory** creates objects without exposing creation logic.

#### Example: Factory Pattern

```java
// Interface
public interface DataSource {
    void connect();
    void query(String sql);
}

// Implementations
public class MySQLDataSource implements DataSource {
    @Override
    public void connect() {
        System.out.println("Connecting to MySQL");
    }

    @Override
    public void query(String sql) {
        System.out.println("MySQL Query: " + sql);
    }
}

public class PostgresDataSource implements DataSource {
    @Override
    public void connect() {
        System.out.println("Connecting to PostgreSQL");
    }

    @Override
    public void query(String sql) {
        System.out.println("PostgreSQL Query: " + sql);
    }
}

// Factory
public class DataSourceFactory {
    public static DataSource create(String type) {
        switch (type.toLowerCase()) {
            case "mysql":
                return new MySQLDataSource();
            case "postgres":
                return new PostgresDataSource();
            default:
                throw new IllegalArgumentException("Unknown type: " + type);
        }
    }
}

// Usage
public class FactoryDemo {
    public static void main(String[] args) {
        DataSource mysql = DataSourceFactory.create("mysql");
        DataSource postgres = DataSourceFactory.create("postgres");

        mysql.connect();
        mysql.query("SELECT * FROM users");
    }
}
```

**Use Factory for**:

- Creating objects based on configuration
- Abstracting creation logic
- Supporting multiple implementations

### 1.3 Observer Pattern

**Observer** notifies multiple objects about state changes.

#### Example: Observer Pattern

```java
import java.util.*;

// Observer interface
public interface Observer {
    void update(String event);
}

// Subject (observable)
public class EventBus {
    private List<Observer> observers = new ArrayList<>();

    public void subscribe(Observer observer) {
        observers.add(observer);
    }

    public void unsubscribe(Observer observer) {
        observers.remove(observer);
    }

    public void publish(String event) {
        for (Observer observer : observers) {
            observer.update(event);
        }
    }
}

// Concrete observers
public class EmailNotifier implements Observer {
    @Override
    public void update(String event) {
        System.out.println("Email: " + event);
    }
}

public class LogNotifier implements Observer {
    @Override
    public void update(String event) {
        System.out.println("Log: " + event);
    }
}

// Usage
public class ObserverDemo {
    public static void main(String[] args) {
        EventBus eventBus = new EventBus();

        eventBus.subscribe(new EmailNotifier());
        eventBus.subscribe(new LogNotifier());

        eventBus.publish("User registered");
        // Output:
        // Email: User registered
        // Log: User registered
    }
}
```

### 1.4 Strategy Pattern

**Strategy** encapsulates algorithms as interchangeable objects.

#### Example: Strategy Pattern

```java
// Strategy interface
public interface PaymentStrategy {
    void pay(double amount);
}

// Concrete strategies
public class CreditCardPayment implements PaymentStrategy {
    @Override
    public void pay(double amount) {
        System.out.println("Paying $" + amount + " with credit card");
    }
}

public class PayPalPayment implements PaymentStrategy {
    @Override
    public void pay(double amount) {
        System.out.println("Paying $" + amount + " via PayPal");
    }
}

public class CryptocurrencyPayment implements PaymentStrategy {
    @Override
    public void pay(double amount) {
        System.out.println("Paying $" + amount + " with cryptocurrency");
    }
}

// Context
public class ShoppingCart {
    private PaymentStrategy strategy;

    public void setPaymentStrategy(PaymentStrategy strategy) {
        this.strategy = strategy;
    }

    public void checkout(double total) {
        if (strategy == null) {
            throw new IllegalStateException("Payment strategy not set");
        }
        strategy.pay(total);
    }
}

// Usage
public class StrategyDemo {
    public static void main(String[] args) {
        ShoppingCart cart = new ShoppingCart();

        cart.setPaymentStrategy(new CreditCardPayment());
        cart.checkout(100.00);

        cart.setPaymentStrategy(new PayPalPayment());
        cart.checkout(50.00);
    }
}
```

**Other Useful Patterns**:

- **Decorator** - Add behavior to objects
- **Adapter** - Convert interfaces
- **Builder** - Construct complex objects
- **Template Method** - Define algorithm skeleton

---

## Part 2: SOLID Principles in Practice

### 2.1 Dependency Injection

**Dependency Injection** provides dependencies rather than creating them internally.

#### Bad: Tight Coupling

```java
public class OrderService {
    private EmailService emailService = new EmailService();  // Tightly coupled!

    public void placeOrder(Order order) {
        // Process order
        emailService.sendConfirmation(order);
    }
}
```

#### Good: Dependency Injection

```java
// Service interface
public interface NotificationService {
    void sendConfirmation(Order order);
}

// Implementations
public class EmailNotificationService implements NotificationService {
    @Override
    public void sendConfirmation(Order order) {
        System.out.println("Email sent");
    }
}

public class SMSNotificationService implements NotificationService {
    @Override
    public void sendConfirmation(Order order) {
        System.out.println("SMS sent");
    }
}

// Service with injected dependency
public class OrderService {
    private NotificationService notificationService;

    // Constructor injection
    public OrderService(NotificationService notificationService) {
        this.notificationService = notificationService;
    }

    public void placeOrder(Order order) {
        // Process order
        notificationService.sendConfirmation(order);
    }
}

// Usage
public class DIDemo {
    public static void main(String[] args) {
        NotificationService emailService = new EmailNotificationService();
        OrderService orderService = new OrderService(emailService);

        orderService.placeOrder(new Order(123));
    }
}
```

**Benefits**:

- Easy to test (inject mock services)
- Loosely coupled
- Easy to swap implementations

### 2.2 Interface Segregation

**Don't** force clients to depend on methods they don't use.

#### Bad: Fat Interface

```java
public interface Worker {
    void work();
    void eat();
    void manage();
}

// Robot shouldn't implement eat()
public class Robot implements Worker {
    @Override
    public void work() { /* code */ }

    @Override
    public void eat() { /* doesn't apply */ }

    @Override
    public void manage() { /* doesn't apply */ }
}
```

#### Good: Segregated Interfaces

```java
public interface Workable {
    void work();
}

public interface Eatable {
    void eat();
}

public interface Manageable {
    void manage();
}

public class Robot implements Workable {
    @Override
    public void work() { /* code */ }
}

public class Human implements Workable, Eatable, Manageable {
    @Override
    public void work() { /* code */ }

    @Override
    public void eat() { /* code */ }

    @Override
    public void manage() { /* code */ }
}
```

---

## Part 3: Advanced Concurrency

### 3.1 Threads and Synchronization

**Concurrency** allows multiple tasks to execute simultaneously.

#### Example: Creating Threads

```java
// Method 1: Extend Thread
public class CounterThread extends Thread {
    private int id;

    public CounterThread(int id) {
        this.id = id;
    }

    @Override
    public void run() {
        for (int i = 1; i <= 5; i++) {
            System.out.println("Thread " + id + ": " + i);
            try {
                Thread.sleep(1000);  // Sleep 1 second
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }
}

// Method 2: Implement Runnable (preferred)
public class CounterRunnable implements Runnable {
    private int id;

    public CounterRunnable(int id) {
        this.id = id;
    }

    @Override
    public void run() {
        for (int i = 1; i <= 5; i++) {
            System.out.println("Thread " + id + ": " + i);
        }
    }
}

// Usage
public class ThreadDemo {
    public static void main(String[] args) {
        // Method 2: More flexible
        Thread t1 = new Thread(new CounterRunnable(1));
        Thread t2 = new Thread(new CounterRunnable(2));

        t1.start();
        t2.start();

        try {
            t1.join();  // Wait for t1 to finish
            t2.join();  // Wait for t2 to finish
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }

        System.out.println("All threads done");
    }
}
```

### 3.2 Thread Safety with Synchronized

**Synchronization** prevents race conditions when multiple threads access shared data.

#### Example: Synchronization

```java
// NOT thread-safe
public class UnsafeCounter {
    private int count = 0;

    public void increment() {
        count++;  // NOT atomic! Race condition possible
    }

    public int getCount() {
        return count;
    }
}

// Thread-safe with synchronized
public class SafeCounter {
    private int count = 0;

    // Synchronized method
    public synchronized void increment() {
        count++;
    }

    public synchronized int getCount() {
        return count;
    }
}

// Better: Synchronized block
public class OptimizedCounter {
    private int count = 0;

    public void increment() {
        // Only lock what's necessary
        synchronized (this) {
            count++;
        }
    }

    public int getCount() {
        synchronized (this) {
            return count;
        }
    }
}

// Best: Use atomic classes
import java.util.concurrent.atomic.AtomicInteger;

public class AtomicCounter {
    private AtomicInteger count = new AtomicInteger(0);

    public void increment() {
        count.incrementAndGet();
    }

    public int getCount() {
        return count.get();
    }
}
```

### 3.3 Concurrent Collections

**Concurrent collections** are thread-safe without explicit synchronization.

#### Example: Concurrent Collections

```java
import java.util.concurrent.*;

public class ConcurrentCollectionsDemo {
    public static void main(String[] args) throws InterruptedException {
        // ConcurrentHashMap - thread-safe without locking entire map
        ConcurrentHashMap<String, Integer> map = new ConcurrentHashMap<>();

        // Multiple threads can put/get safely
        ExecutorService executor = Executors.newFixedThreadPool(3);

        for (int i = 0; i < 10; i++) {
            final int id = i;
            executor.submit(() -> {
                map.put("key" + id, id * 100);
            });
        }

        executor.shutdown();
        executor.awaitTermination(10, TimeUnit.SECONDS);

        System.out.println("Size: " + map.size());

        // BlockingQueue - safe queue for producer-consumer
        BlockingQueue<String> queue = new LinkedBlockingQueue<>();

        // Producer thread
        new Thread(() -> {
            try {
                queue.put("item1");
                queue.put("item2");
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }).start();

        // Consumer thread
        new Thread(() -> {
            try {
                System.out.println(queue.take());  // Waits if empty
                System.out.println(queue.take());
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }).start();
    }
}
```

---

## Part 4: Build Tools

### 4.1 Maven Basics

**Maven** manages dependencies and builds.

#### Example: Maven POM

```xml
<project>
    <modelVersion>4.0.0</modelVersion>
    <groupId>com.example</groupId>
    <artifactId>my-app</artifactId>
    <version>1.0.0</version>

    <properties>
        <maven.compiler.source>21</maven.compiler.source>
        <maven.compiler.target>21</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>

    <dependencies>
        <!-- JUnit 5 for testing -->
        <dependency>
            <groupId>org.junit.jupiter</groupId>
            <artifactId>junit-jupiter</artifactId>
            <version>5.10.0</version>
            <scope>test</scope>
        </dependency>

        <!-- JSON processing -->
        <dependency>
            <groupId>com.google.code.gson</groupId>
            <artifactId>gson</artifactId>
            <version>2.10.1</version>
        </dependency>
    </dependencies>

    <build>
        <plugins>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-surefire-plugin</artifactId>
                <version>3.0.0</version>
            </plugin>
        </plugins>
    </build>
</project>
```

**Common Commands**:

- `mvn clean` - Remove build artifacts
- `mvn compile` - Compile source code
- `mvn test` - Run tests
- `mvn package` - Create JAR
- `mvn install` - Install to local repository

### 4.2 Gradle Basics

**Gradle** is a modern build tool (alternative to Maven).

#### Example: Gradle Build File

```gradle
plugins {
    id 'java'
}

java {
    sourceCompatibility = '21'
    targetCompatibility = '21'
}

repositories {
    mavenCentral()
}

dependencies {
    // JUnit 5
    testImplementation 'org.junit.jupiter:junit-jupiter:5.10.0'

    // JSON
    implementation 'com.google.code.gson:gson:2.10.1'
}

tasks.named('test') {
    useJUnitPlatform()
}
```

**Common Commands**:

- `gradle build` - Build project
- `gradle test` - Run tests
- `gradle clean` - Remove build files
- `gradle jar` - Create JAR

---

## Part 5: Performance and Profiling

### 5.1 Performance Best Practices

#### String Operations

```java
// Bad: Creates many String objects
String result = "";
for (int i = 0; i < 10000; i++) {
    result += "item" + i + ",";  // Inefficient!
}

// Good: Use StringBuilder
StringBuilder sb = new StringBuilder();
for (int i = 0; i < 10000; i++) {
    sb.append("item").append(i).append(",");
}
String result = sb.toString();
```

#### Collection Sizing

```java
// Bad: ArrayList grows repeatedly
List<String> items = new ArrayList<>();
for (int i = 0; i < 100000; i++) {
    items.add("item " + i);  // Reallocates internally
}

// Good: Size ArrayList upfront
List<String> items = new ArrayList<>(100000);
for (int i = 0; i < 100000; i++) {
    items.add("item " + i);
}
```

#### Caching

```java
// Bad: Recalculate repeatedly
public int expensiveOperation(int n) {
    int result = 0;
    for (int i = 0; i < 1000000; i++) {
        result += heavyCalculation(n);
    }
    return result;
}

// Good: Cache results
private Map<Integer, Integer> cache = new HashMap<>();

public int expensiveOperation(int n) {
    if (cache.containsKey(n)) {
        return cache.get(n);
    }
    int result = 0;
    for (int i = 0; i < 1000000; i++) {
        result += heavyCalculation(n);
    }
    cache.put(n, result);
    return result;
}
```

### 5.2 Profiling with JVisualVM

JVisualVM comes with Java and helps identify performance bottlenecks.

```bash
# Start your Java application
java -jar myapp.jar

# In another terminal, launch JVisualVM
jvisualvm

# Monitor CPU, memory, threads
```

---

## Part 6: Database Integration with JDBC

### 6.1 Basic JDBC

**JDBC** connects Java to databases.

#### Example: JDBC Connection

```java
import java.sql.*;

public class JDBCExample {
    public static void main(String[] args) {
        String url = "jdbc:mysql://localhost:3306/mydb";
        String user = "root";
        String password = "password";

        try {
            // Load driver
            Class.forName("com.mysql.cj.jdbc.Driver");

            // Get connection
            Connection conn = DriverManager.getConnection(url, user, password);

            // Create statement
            Statement stmt = conn.createStatement();

            // Execute query
            String sql = "SELECT * FROM users WHERE age > 18";
            ResultSet rs = stmt.executeQuery(sql);

            // Process results
            while (rs.next()) {
                int id = rs.getInt("id");
                String name = rs.getString("name");
                int age = rs.getInt("age");

                System.out.println(id + ": " + name + " (" + age + ")");
            }

            // Close resources
            rs.close();
            stmt.close();
            conn.close();

        } catch (ClassNotFoundException | SQLException e) {
            e.printStackTrace();
        }
    }
}
```

### 6.2 Prepared Statements (SQL Injection Prevention)

```java
// Bad: SQL Injection vulnerability
String sql = "SELECT * FROM users WHERE username = '" + userInput + "'";

// Good: Use PreparedStatement
String sql = "SELECT * FROM users WHERE username = ?";
PreparedStatement pstmt = conn.prepareStatement(sql);
pstmt.setString(1, userInput);  // Safely bind parameter
ResultSet rs = pstmt.executeQuery();
```

---

## Part 7: Security Best Practices

### 7.1 Password Handling

```java
import java.security.MessageDigest;
import java.util.Arrays;

public class PasswordSecurity {
    // Hash password with salt
    public static String hashPassword(String password, byte[] salt) throws Exception {
        MessageDigest md = MessageDigest.getInstance("SHA-256");
        md.update(salt);
        byte[] hashedPassword = md.digest(password.getBytes());
        return bytesToHex(hashedPassword);
    }

    private static String bytesToHex(byte[] bytes) {
        StringBuilder sb = new StringBuilder();
        for (byte b : bytes) {
            sb.append(String.format("%02x", b));
        }
        return sb.toString();
    }
}
```

### 7.2 Input Validation

```java
public class InputValidation {
    public static boolean isValidEmail(String email) {
        return email.matches("^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}$");
    }

    public static int parseIntSafely(String value, int defaultValue) {
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }
}
```

---

## Part 8: System Design Principles

### 8.1 Scalability

```java
// Use connection pooling for database
import org.apache.commons.dbcp2.BasicDataSource;

public class ConnectionPool {
    private static BasicDataSource ds = new BasicDataSource();

    static {
        ds.setUrl("jdbc:mysql://localhost/mydb");
        ds.setUsername("root");
        ds.setPassword("password");
        ds.setInitialSize(5);
        ds.setMaxTotal(10);
    }

    public static Connection getConnection() throws SQLException {
        return ds.getConnection();
    }
}
```

### 8.2 Caching Layer

```java
public class CachingUserRepository {
    private Map<Integer, User> cache = new ConcurrentHashMap<>();
    private UserRepository repository;

    public User getUser(int id) {
        // Check cache first
        if (cache.containsKey(id)) {
            return cache.get(id);
        }

        // Fetch from database
        User user = repository.findById(id);

        // Store in cache
        cache.put(id, user);

        return user;
    }
}
```

---

## Part 9: Production Patterns

### 9.1 Logging

Use proper logging instead of `System.out.println()`.

```java
import java.util.logging.Logger;

public class UserService {
    private static final Logger logger = Logger.getLogger(UserService.class.getName());

    public void createUser(String name) {
        try {
            logger.info("Creating user: " + name);
            // Create user
            logger.info("User created successfully");
        } catch (Exception e) {
            logger.severe("Failed to create user: " + e.getMessage());
        }
    }
}
```

### 9.2 Configuration Management

```java
import java.io.FileInputStream;
import java.util.Properties;

public class Config {
    private static Properties props = new Properties();

    static {
        try {
            props.load(new FileInputStream("config.properties"));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static String get(String key) {
        return props.getProperty(key);
    }

    public static String get(String key, String defaultValue) {
        return props.getProperty(key, defaultValue);
    }
}

// config.properties
// database.url=jdbc:mysql://localhost/mydb
// database.user=root
// logging.level=INFO
```

---

## What to Learn Next

Excellent progress! You've covered 60-85% of Java knowledge for professional development.

### Next: Advanced Java

Continue with [Advanced Java](./tu-se-pl-ja__advanced.md) for:

- JVM internals and architecture
- Garbage collection tuning
- Advanced concurrency patterns
- Reflection and annotations
- Bytecode analysis

### For Day-to-Day Solutions

Use [Java Cookbook](./tu-se-pl-ja__cookbook.md) for:

- Quick patterns and recipes
- Common problems and solutions
- Copy-paste code snippets

---

**Intermediate Tutorial Complete!** You're ready for professional Java development.
