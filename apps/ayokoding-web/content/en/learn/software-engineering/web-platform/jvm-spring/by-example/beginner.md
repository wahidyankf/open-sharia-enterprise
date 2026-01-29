---
title: "Beginner"
weight: 11000001
date: 2025-01-29T00:00:00+07:00
draft: false
description: "Beginner-level Spring Framework examples covering IoC container, dependency injection, component scanning, and bean lifecycle (Coverage: 0-40%)"
tags: ["spring", "java", "kotlin", "beginner", "examples", "ioc", "dependency-injection"]
---

This tutorial provides 25 heavily annotated Spring Framework examples for experienced developers learning Spring. Each example is self-contained and demonstrates core IoC container, dependency injection, and bean management patterns.

**Coverage**: 0-40% of Spring Framework features
**Target Audience**: Developers familiar with Java/Kotlin wanting to learn Spring fundamentals

## Basic Operations (Examples 1-5)

### Example 1: Creating Spring ApplicationContext (Coverage: 1.0%)

Demonstrates the most basic Spring setup - creating an ApplicationContext to manage beans.

**Java Implementation**:

```java
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Configuration;

@Configuration  // => Marks this as Spring configuration class
                // => Spring scans for @Bean methods here
class AppConfig {
    // => Empty config for now, just bootstrapping Spring
}

public class Example01 {
    public static void main(String[] args) {
        // => Creates Spring IoC container from Java config
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => context is now initialized, ready to manage beans

        System.out.println("Spring Context ID: " + context.getId());
        // => Output: Spring Context ID: [unique-id]

        context.close();  // => Releases resources, calls bean destruction callbacks
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Configuration

@Configuration  // => Marks this as Spring configuration class
                // => Spring scans for @Bean methods here
class AppConfig {
    // => Empty config for now, just bootstrapping Spring
}

fun main() {
    // => Creates Spring IoC container from Kotlin config
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => context is now initialized, ready to manage beans

    println("Spring Context ID: ${context.id}")
    // => Output: Spring Context ID: [unique-id]

    context.close()  // => Releases resources, calls bean destruction callbacks
}
```

**Expected Output**:

```
Spring Context ID: org.springframework.context.annotation.AnnotationConfigApplicationContext@5fd0d5ae
```

**Key Takeaways**:

- `@Configuration` marks classes as Spring configuration sources
- `AnnotationConfigApplicationContext` creates IoC container from Java config
- Always close context to release resources (or use try-with-resources)
- Context manages entire bean lifecycle

**Related Documentation**:

- [Spring IoC Container Documentation](https://docs.spring.io/spring-framework/reference/core/beans.html)

---

### Example 2: Defining and Retrieving Simple Bean (Coverage: 3.0%)

Demonstrates defining a bean with `@Bean` and retrieving it from the context.

**Java Implementation**:

```java
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

class ZakatCalculator {  // => Simple POJO for Zakat calculations
    public double calculateZakat(double wealth) {
        return wealth * 0.025;  // => 2.5% of wealth
    }
}

@Configuration
class AppConfig {
    @Bean  // => Tells Spring to manage this object as a bean
           // => Bean name defaults to method name: "zakatCalculator"
    public ZakatCalculator zakatCalculator() {
        return new ZakatCalculator();  // => Spring calls this to create bean
        // => Bean stored in container, reused for subsequent requests
    }
}

public class Example02 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Context initialized, zakatCalculator bean created

        ZakatCalculator calc = context.getBean(ZakatCalculator.class);
        // => Retrieves bean by type
        // => calc references the singleton instance

        double zakat = calc.calculateZakat(100000);  // => Calculates 2.5%
        System.out.println("Zakat: " + zakat);       // => Output: Zakat: 2500.0

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

class ZakatCalculator {  // => Simple Kotlin class for Zakat calculations
    fun calculateZakat(wealth: Double): Double {
        return wealth * 0.025  // => 2.5% of wealth
    }
}

@Configuration
class AppConfig {
    @Bean  // => Tells Spring to manage this object as a bean
           // => Bean name defaults to method name: "zakatCalculator"
    fun zakatCalculator(): ZakatCalculator {
        return ZakatCalculator()  // => Spring calls this to create bean
        // => Bean stored in container, reused for subsequent requests
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Context initialized, zakatCalculator bean created

    val calc = context.getBean(ZakatCalculator::class.java)
    // => Retrieves bean by type
    // => calc references the singleton instance

    val zakat = calc.calculateZakat(100000.0)  // => Calculates 2.5%
    println("Zakat: $zakat")                    // => Output: Zakat: 2500.0

    context.close()
}
```

**Expected Output**:

```
Zakat: 2500.0
```

**Key Takeaways**:

- `@Bean` methods define beans managed by Spring container
- Bean names default to method names (can be customized)
- `getBean(Class)` retrieves bean by type
- Beans are singletons by default (same instance returned)

**Related Documentation**:

- [Bean Definition Documentation](https://docs.spring.io/spring-framework/reference/core/beans/definition.html)

---

### Example 3: Constructor Dependency Injection (Coverage: 6.0%)

Demonstrates constructor-based dependency injection - Spring's recommended DI approach.

**Java Implementation**:

```java
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

class SadaqahRepository {  // => Data access layer for Sadaqah donations
    public void save(String donation) {
        System.out.println("Saved: " + donation);  // => Simulates database save
    }
}

class SadaqahService {  // => Business logic layer
    private final SadaqahRepository repository;  // => Dependency (immutable)

    // => Constructor injection - Spring injects dependency here
    public SadaqahService(SadaqahRepository repository) {
        this.repository = repository;  // => Assigns injected dependency
        // => Constructor called by Spring, not by you
    }

    public void recordDonation(String donor, double amount) {
        String donation = donor + ": $" + amount;
        repository.save(donation);  // => Uses injected dependency
    }
}

@Configuration
class AppConfig {
    @Bean
    public SadaqahRepository sadaqahRepository() {
        return new SadaqahRepository();  // => Creates repository bean
    }

    @Bean
    // => Spring sees SadaqahService constructor needs SadaqahRepository
    // => Automatically finds and injects sadaqahRepository bean
    public SadaqahService sadaqahService(SadaqahRepository repository) {
        return new SadaqahService(repository);  // => Spring passes dependency
        // => Method parameter resolved from container
    }
}

public class Example03 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Spring creates repository bean first, then service bean

        SadaqahService service = context.getBean(SadaqahService.class);
        service.recordDonation("Ahmad", 500.0);
        // => Output: Saved: Ahmad: $500.0

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

class SadaqahRepository {  // => Data access layer for Sadaqah donations
    fun save(donation: String) {
        println("Saved: $donation")  // => Simulates database save
    }
}

// => Primary constructor with dependency parameter
class SadaqahService(private val repository: SadaqahRepository) {
    // => Constructor injection - Spring injects dependency here
    // => 'val' makes dependency immutable

    fun recordDonation(donor: String, amount: Double) {
        val donation = "$donor: $$amount"
        repository.save(donation)  // => Uses injected dependency
    }
}

@Configuration
class AppConfig {
    @Bean
    fun sadaqahRepository(): SadaqahRepository {
        return SadaqahRepository()  // => Creates repository bean
    }

    @Bean
    // => Spring sees SadaqahService constructor needs SadaqahRepository
    // => Automatically finds and injects sadaqahRepository bean
    fun sadaqahService(repository: SadaqahRepository): SadaqahService {
        return SadaqahService(repository)  // => Spring passes dependency
        // => Method parameter resolved from container
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Spring creates repository bean first, then service bean

    val service = context.getBean(SadaqahService::class.java)
    service.recordDonation("Ahmad", 500.0)
    // => Output: Saved: Ahmad: $500.0

    context.close()
}
```

**Expected Output**:

```
Saved: Ahmad: $500.0
```

**Key Takeaways**:

- Constructor injection is Spring's recommended DI approach
- Dependencies are immutable (final/val), improving safety
- Spring resolves dependencies from `@Bean` method parameters
- Bean creation order determined by dependency graph

**Related Documentation**:

- [Dependency Injection Documentation](https://docs.spring.io/spring-framework/reference/core/beans/dependencies/factory-collaborators.html)

---

### Example 4: Component Scanning with @Component (Coverage: 10.0%)

Demonstrates automatic bean discovery using `@Component` and `@ComponentScan`.

**Java Implementation**:

```java
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;

@Component  // => Marks class as Spring-managed component
            // => Spring automatically creates bean (no @Bean method needed)
class QardHassanCalculator {  // => Qard Hassan (interest-free loan) calculator
    public double calculateMonthlyPayment(double principal, int months) {
        return principal / months;  // => No interest, simple division
    }
}

@Configuration
@ComponentScan  // => Tells Spring to scan current package for @Component classes
                // => Automatically discovers and registers beans
class AppConfig {
    // => No @Bean methods needed for @Component classes
}

public class Example04 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => @ComponentScan discovers QardHassanCalculator
        // => Bean created automatically with name "qardHassanCalculator"

        QardHassanCalculator calc = context.getBean(QardHassanCalculator.class);
        double payment = calc.calculateMonthlyPayment(12000, 12);
        System.out.println("Monthly Payment: " + payment);
        // => Output: Monthly Payment: 1000.0

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration
import org.springframework.stereotype.Component

@Component  // => Marks class as Spring-managed component
            // => Spring automatically creates bean (no @Bean method needed)
class QardHassanCalculator {  // => Qard Hassan (interest-free loan) calculator
    fun calculateMonthlyPayment(principal: Double, months: Int): Double {
        return principal / months  // => No interest, simple division
    }
}

@Configuration
@ComponentScan  // => Tells Spring to scan current package for @Component classes
                // => Automatically discovers and registers beans
class AppConfig {
    // => No @Bean methods needed for @Component classes
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => @ComponentScan discovers QardHassanCalculator
    // => Bean created automatically with name "qardHassanCalculator"

    val calc = context.getBean(QardHassanCalculator::class.java)
    val payment = calc.calculateMonthlyPayment(12000.0, 12)
    println("Monthly Payment: $payment")
    // => Output: Monthly Payment: 1000.0

    context.close()
}
```

**Expected Output**:

```
Monthly Payment: 1000.0
```

**Key Takeaways**:

- `@Component` enables automatic bean discovery
- `@ComponentScan` scans packages for annotated classes
- Bean names default to camelCase class names
- Reduces boilerplate compared to `@Bean` methods

**Related Documentation**:

- [Component Scanning Documentation](https://docs.spring.io/spring-framework/reference/core/beans/classpath-scanning.html)

---

### Example 5: @Autowired Constructor Injection (Coverage: 13.0%)

Demonstrates `@Autowired` for automatic dependency injection with components.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Repository;
import org.springframework.stereotype.Service;

@Repository  // => Specialized @Component for data access layer
             // => Semantically indicates database/storage interaction
class MurabahaRepository {
    public void save(String contract) {
        System.out.println("Saved contract: " + contract);
    }
}

@Service  // => Specialized @Component for business logic layer
          // => Semantically indicates service/business operations
class MurabahaService {
    private final MurabahaRepository repository;

    @Autowired  // => Tells Spring to inject dependency via constructor
                // => Optional in Spring 4.3+ if only one constructor
    public MurabahaService(MurabahaRepository repository) {
        this.repository = repository;  // => Injected by Spring
        // => Spring finds MurabahaRepository bean and passes it
    }

    public void createContract(String client, double amount) {
        String contract = "Murabaha for " + client + ": $" + amount;
        repository.save(contract);  // => Uses injected dependency
    }
}

@Configuration
@ComponentScan  // => Discovers @Repository and @Service beans
class AppConfig {
}

public class Example05 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Spring discovers repository and service beans
        // => Automatically wires repository into service

        MurabahaService service = context.getBean(MurabahaService.class);
        service.createContract("Fatimah", 50000.0);
        // => Output: Saved contract: Murabaha for Fatimah: $50000.0

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration
import org.springframework.stereotype.Repository
import org.springframework.stereotype.Service

@Repository  // => Specialized @Component for data access layer
             // => Semantically indicates database/storage interaction
class MurabahaRepository {
    fun save(contract: String) {
        println("Saved contract: $contract")
    }
}

@Service  // => Specialized @Component for business logic layer
          // => Semantically indicates service/business operations
// => Kotlin primary constructor - @Autowired automatic for single constructor
class MurabahaService @Autowired constructor(
    private val repository: MurabahaRepository
    // => Spring finds MurabahaRepository bean and passes it
) {
    fun createContract(client: String, amount: Double) {
        val contract = "Murabaha for $client: $$amount"
        repository.save(contract)  // => Uses injected dependency
    }
}

@Configuration
@ComponentScan  // => Discovers @Repository and @Service beans
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Spring discovers repository and service beans
    // => Automatically wires repository into service

    val service = context.getBean(MurabahaService::class.java)
    service.createContract("Fatimah", 50000.0)
    // => Output: Saved contract: Murabaha for Fatimah: $50000.0

    context.close()
}
```

**Expected Output**:

```
Saved contract: Murabaha for Fatimah: $50000.0
```

**Key Takeaways**:

- `@Autowired` enables automatic dependency injection
- `@Service` and `@Repository` are specialized `@Component` variants
- `@Autowired` optional for single-constructor classes (Spring 4.3+)
- Stereotypes (@Service, @Repository) provide semantic clarity

**Related Documentation**:

- [Stereotype Annotations Documentation](https://docs.spring.io/spring-framework/reference/core/beans/classpath-scanning.html#beans-stereotype-annotations)

---

## Bean Configuration (Examples 6-10)

### Example 6: Custom Bean Names (Coverage: 16.0%)

Demonstrates specifying custom names for beans instead of defaults.

**Java Implementation**:

```java
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

class Calculator {
    public double add(double a, double b) {
        return a + b;
    }
}

@Configuration
class AppConfig {
    @Bean(name = "primaryCalculator")  // => Custom bean name
                                         // => Overrides default "calculator"
    public Calculator calculator() {
        return new Calculator();  // => Bean registered as "primaryCalculator"
    }

    @Bean("backupCalculator")  // => Shorthand for name attribute
    public Calculator anotherCalculator() {
        return new Calculator();  // => Bean registered as "backupCalculator"
    }
}

public class Example06 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        Calculator primary = context.getBean("primaryCalculator", Calculator.class);
        // => Retrieves by custom name
        System.out.println("5 + 3 = " + primary.add(5, 3));
        // => Output: 5 + 3 = 8.0

        Calculator backup = context.getBean("backupCalculator", Calculator.class);
        // => Retrieves second bean by its name
        System.out.println("10 + 2 = " + backup.add(10, 2));
        // => Output: 10 + 2 = 12.0

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

class Calculator {
    fun add(a: Double, b: Double): Double = a + b
}

@Configuration
class AppConfig {
    @Bean(name = ["primaryCalculator"])  // => Custom bean name (array syntax)
                                          // => Overrides default "calculator"
    fun calculator(): Calculator {
        return Calculator()  // => Bean registered as "primaryCalculator"
    }

    @Bean("backupCalculator")  // => Shorthand for name attribute
    fun anotherCalculator(): Calculator {
        return Calculator()  // => Bean registered as "backupCalculator"
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val primary = context.getBean("primaryCalculator", Calculator::class.java)
    // => Retrieves by custom name
    println("5 + 3 = ${primary.add(5.0, 3.0)}")
    // => Output: 5 + 3 = 8.0

    val backup = context.getBean("backupCalculator", Calculator::class.java)
    // => Retrieves second bean by its name
    println("10 + 2 = ${backup.add(10.0, 2.0)}")
    // => Output: 10 + 2 = 12.0

    context.close()
}
```

**Expected Output**:

```
5 + 3 = 8.0
10 + 2 = 12.0
```

**Key Takeaways**:

- `@Bean(name = "...")` specifies custom bean name
- Multiple beans of same type require unique names
- `getBean(String, Class)` retrieves by name and type
- Bean names must be unique within container

**Related Documentation**:

- [Bean Naming Documentation](https://docs.spring.io/spring-framework/reference/core/beans/definition.html#beans-beanname)

---

### Example 7: Bean Aliases (Coverage: 18.0%)

Demonstrates defining multiple names (aliases) for the same bean.

**Java Implementation**:

```java
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

class ZakatCalculator {
    public double calculate(double wealth) {
        return wealth * 0.025;
    }
}

@Configuration
class AppConfig {
    @Bean(name = {"zakatCalc", "zakatCalculator", "zakahService"})
    // => Defines three aliases for same bean
    // => All names reference the SAME instance
    public ZakatCalculator calculator() {
        return new ZakatCalculator();  // => Single bean, multiple names
    }
}

public class Example07 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        ZakatCalculator calc1 = context.getBean("zakatCalc", ZakatCalculator.class);
        ZakatCalculator calc2 = context.getBean("zakatCalculator", ZakatCalculator.class);
        ZakatCalculator calc3 = context.getBean("zakahService", ZakatCalculator.class);
        // => All three retrieve the SAME bean instance

        System.out.println("Same instance? " + (calc1 == calc2 && calc2 == calc3));
        // => Output: Same instance? true

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

class ZakatCalculator {
    fun calculate(wealth: Double): Double = wealth * 0.025
}

@Configuration
class AppConfig {
    @Bean(name = ["zakatCalc", "zakatCalculator", "zakahService"])
    // => Defines three aliases for same bean
    // => All names reference the SAME instance
    fun calculator(): ZakatCalculator {
        return ZakatCalculator()  // => Single bean, multiple names
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val calc1 = context.getBean("zakatCalc", ZakatCalculator::class.java)
    val calc2 = context.getBean("zakatCalculator", ZakatCalculator::class.java)
    val calc3 = context.getBean("zakahService", ZakatCalculator::class.java)
    // => All three retrieve the SAME bean instance

    println("Same instance? ${calc1 === calc2 && calc2 === calc3}")
    // => Output: Same instance? true

    context.close()
}
```

**Expected Output**:

```
Same instance? true
```

**Key Takeaways**:

- Bean aliases provide alternative names for same bean
- All aliases reference identical singleton instance
- Useful for compatibility or alternative naming conventions
- Primary name is first in array

**Related Documentation**:

- [Bean Aliasing Documentation](https://docs.spring.io/spring-framework/reference/core/beans/definition.html#beans-beanname-alias)

---

### Example 8: Setter Injection (Coverage: 20.0%)

Demonstrates setter-based dependency injection as alternative to constructor injection.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

@Component
class EmailService {
    public void send(String message) {
        System.out.println("Email sent: " + message);
    }
}

@Service
class NotificationService {
    private EmailService emailService;  // => Dependency (not final)

    @Autowired  // => Spring calls setter after construction
                // => Injects emailService bean
    public void setEmailService(EmailService emailService) {
        this.emailService = emailService;  // => Setter injection
        // => Allows changing dependency after construction
    }

    public void notifyDonation(String donor) {
        emailService.send("Thank you, " + donor);
    }
}

@Configuration
@ComponentScan
class AppConfig {
}

public class Example08 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Spring creates NotificationService, then calls setEmailService

        NotificationService service = context.getBean(NotificationService.class);
        service.notifyDonation("Ali");
        // => Output: Email sent: Thank you, Ali

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration
import org.springframework.stereotype.Component
import org.springframework.stereotype.Service

@Component
class EmailService {
    fun send(message: String) {
        println("Email sent: $message")
    }
}

@Service
class NotificationService {
    private lateinit var emailService: EmailService  // => Late-init dependency

    @Autowired  // => Spring calls setter after construction
                // => Injects emailService bean
    fun setEmailService(emailService: EmailService) {
        this.emailService = emailService  // => Setter injection
        // => lateinit allows setting after construction
    }

    fun notifyDonation(donor: String) {
        emailService.send("Thank you, $donor")
    }
}

@Configuration
@ComponentScan
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Spring creates NotificationService, then calls setEmailService

    val service = context.getBean(NotificationService::class.java)
    service.notifyDonation("Ali")
    // => Output: Email sent: Thank you, Ali

    context.close()
}
```

**Expected Output**:

```
Email sent: Thank you, Ali
```

**Key Takeaways**:

- Setter injection allows optional or changeable dependencies
- Dependencies not final (mutable after construction)
- Constructor injection preferred for required dependencies
- Setter injection useful for optional configuration

**Related Documentation**:

- [Setter Injection Documentation](https://docs.spring.io/spring-framework/reference/core/beans/dependencies/factory-collaborators.html#beans-setter-injection)

---

### Example 9: Field Injection (Coverage: 22.0%)

Demonstrates field-based injection - simplest but least recommended approach.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

@Component
class AuditLogger {
    public void log(String action) {
        System.out.println("AUDIT: " + action);
    }
}

@Service
class TransactionService {
    @Autowired  // => Spring injects directly into field via reflection
                // => No constructor or setter needed
    private AuditLogger logger;  // => Field injection (simplest)

    public void processTransaction(String type, double amount) {
        logger.log(type + " transaction: $" + amount);
        // => Uses injected dependency
    }
}

@Configuration
@ComponentScan
class AppConfig {
}

public class Example09 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Spring creates beans, injects logger via reflection

        TransactionService service = context.getBean(TransactionService.class);
        service.processTransaction("Sadaqah", 200.0);
        // => Output: AUDIT: Sadaqah transaction: $200.0

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration
import org.springframework.stereotype.Component
import org.springframework.stereotype.Service

@Component
class AuditLogger {
    fun log(action: String) {
        println("AUDIT: $action")
    }
}

@Service
class TransactionService {
    @Autowired  // => Spring injects directly into field via reflection
                // => No constructor or setter needed
    private lateinit var logger: AuditLogger  // => Field injection with lateinit

    fun processTransaction(type: String, amount: Double) {
        logger.log("$type transaction: $$amount")
        // => Uses injected dependency
    }
}

@Configuration
@ComponentScan
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Spring creates beans, injects logger via reflection

    val service = context.getBean(TransactionService::class.java)
    service.processTransaction("Sadaqah", 200.0)
    // => Output: AUDIT: Sadaqah transaction: $200.0

    context.close()
}
```

**Expected Output**:

```
AUDIT: Sadaqah transaction: $200.0
```

**Key Takeaways**:

- Field injection uses reflection (no constructor/setter)
- Simplest syntax but hardest to test (can't inject mocks easily)
- Cannot be used with final/val fields
- Constructor injection preferred for testability

**Related Documentation**:

- [Field Injection Discussion](https://docs.spring.io/spring-framework/reference/core/beans/dependencies/factory-collaborators.html)

---

### Example 10: @Qualifier for Disambiguation (Coverage: 25.0%)

Demonstrates using `@Qualifier` when multiple beans of same type exist.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

interface PaymentProcessor {
    void process(double amount);
}

class CashPayment implements PaymentProcessor {
    public void process(double amount) {
        System.out.println("Cash payment: $" + amount);
    }
}

class CardPayment implements PaymentProcessor {
    public void process(double amount) {
        System.out.println("Card payment: $" + amount);
    }
}

@Configuration
class AppConfig {
    @Bean
    @Qualifier("cash")  // => Tags this bean as "cash" processor
    public PaymentProcessor cashProcessor() {
        return new CashPayment();  // => Registered with "cash" qualifier
    }

    @Bean
    @Qualifier("card")  // => Tags this bean as "card" processor
    public PaymentProcessor cardProcessor() {
        return new CardPayment();  // => Registered with "card" qualifier
    }

    @Bean
    public DonationService donationService(
        @Qualifier("cash") PaymentProcessor processor
        // => Specifies which bean to inject when multiple exist
        // => Injects cashProcessor, not cardProcessor
    ) {
        return new DonationService(processor);
    }
}

class DonationService {
    private final PaymentProcessor processor;

    public DonationService(PaymentProcessor processor) {
        this.processor = processor;
    }

    public void acceptDonation(double amount) {
        processor.process(amount);
    }
}

public class Example10 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        DonationService service = context.getBean(DonationService.class);
        service.acceptDonation(100.0);
        // => Output: Cash payment: $100.0
        // => Uses cashProcessor due to @Qualifier

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Qualifier
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

interface PaymentProcessor {
    fun process(amount: Double)
}

class CashPayment : PaymentProcessor {
    override fun process(amount: Double) {
        println("Cash payment: $$amount")
    }
}

class CardPayment : PaymentProcessor {
    override fun process(amount: Double) {
        println("Card payment: $$amount")
    }
}

@Configuration
class AppConfig {
    @Bean
    @Qualifier("cash")  // => Tags this bean as "cash" processor
    fun cashProcessor(): PaymentProcessor {
        return CashPayment()  // => Registered with "cash" qualifier
    }

    @Bean
    @Qualifier("card")  // => Tags this bean as "card" processor
    fun cardProcessor(): PaymentProcessor {
        return CardPayment()  // => Registered with "card" qualifier
    }

    @Bean
    fun donationService(
        @Qualifier("cash") processor: PaymentProcessor
        // => Specifies which bean to inject when multiple exist
        // => Injects cashProcessor, not cardProcessor
    ): DonationService {
        return DonationService(processor)
    }
}

class DonationService(private val processor: PaymentProcessor) {
    fun acceptDonation(amount: Double) {
        processor.process(amount)
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val service = context.getBean(DonationService::class.java)
    service.acceptDonation(100.0)
    // => Output: Cash payment: $100.0
    // => Uses cashProcessor due to @Qualifier

    context.close()
}
```

**Expected Output**:

```
Cash payment: $100.0
```

**Key Takeaways**:

- `@Qualifier` disambiguates when multiple beans of same type exist
- Qualifier names are strings (use constants to avoid typos)
- Without qualifiers, Spring throws `NoUniqueBeanDefinitionException`
- Combines with `@Autowired` for precise injection

**Related Documentation**:

- [Qualifier Documentation](https://docs.spring.io/spring-framework/reference/core/beans/annotation-config/autowired-qualifiers.html)

---

## Bean Scopes and Lifecycle (Examples 11-15)

### Example 11: Singleton Scope (Default) (Coverage: 27.0%)

Demonstrates singleton scope - Spring's default bean scope.

**Java Implementation**:

```java
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

class Counter {
    private int count = 0;  // => Mutable state

    public void increment() {
        count++;  // => Modifies state
    }

    public int getCount() {
        return count;
    }
}

@Configuration
class AppConfig {
    @Bean  // => Default scope is singleton
           // => Only ONE instance created per container
    public Counter counter() {
        return new Counter();  // => Called once during context initialization
    }
}

public class Example11 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        Counter c1 = context.getBean(Counter.class);
        Counter c2 = context.getBean(Counter.class);
        // => Both reference the SAME instance

        c1.increment();  // => Modifies shared instance, count = 1
        c1.increment();  // => count = 2

        System.out.println("c1 count: " + c1.getCount());  // => Output: c1 count: 2
        System.out.println("c2 count: " + c2.getCount());  // => Output: c2 count: 2
        System.out.println("Same? " + (c1 == c2));         // => Output: Same? true

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

class Counter {
    private var count = 0  // => Mutable state

    fun increment() {
        count++  // => Modifies state
    }

    fun getCount(): Int = count
}

@Configuration
class AppConfig {
    @Bean  // => Default scope is singleton
           // => Only ONE instance created per container
    fun counter(): Counter {
        return Counter()  // => Called once during context initialization
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val c1 = context.getBean(Counter::class.java)
    val c2 = context.getBean(Counter::class.java)
    // => Both reference the SAME instance

    c1.increment()  // => Modifies shared instance, count = 1
    c1.increment()  // => count = 2

    println("c1 count: ${c1.getCount()}")  // => Output: c1 count: 2
    println("c2 count: ${c2.getCount()}")  // => Output: c2 count: 2
    println("Same? ${c1 === c2}")           // => Output: Same? true

    context.close()
}
```

**Expected Output**:

```
c1 count: 2
c2 count: 2
Same? true
```

**Key Takeaways**:

- Singleton is default scope (one instance per container)
- All `getBean()` calls return same instance
- State shared across all usages
- Best for stateless services

**Related Documentation**:

- [Bean Scopes Documentation](https://docs.spring.io/spring-framework/reference/core/beans/factory-scopes.html)

---

### Example 12: Prototype Scope (Coverage: 30.0%)

Demonstrates prototype scope - new instance per request.

**Java Implementation**:

```java
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

class Transaction {
    private final String id;

    public Transaction() {
        this.id = java.util.UUID.randomUUID().toString();
        // => Unique ID per instance
    }

    public String getId() {
        return id;
    }
}

@Configuration
class AppConfig {
    @Bean
    @Scope("prototype")  // => Creates NEW instance for each getBean() call
                          // => Not cached in container
    public Transaction transaction() {
        return new Transaction();  // => Called multiple times
    }
}

public class Example12 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        Transaction t1 = context.getBean(Transaction.class);
        Transaction t2 = context.getBean(Transaction.class);
        // => Two DIFFERENT instances created

        System.out.println("t1 ID: " + t1.getId());  // => Output: t1 ID: [uuid-1]
        System.out.println("t2 ID: " + t2.getId());  // => Output: t2 ID: [uuid-2]
        System.out.println("Same? " + (t1 == t2));   // => Output: Same? false

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.context.annotation.Scope
import java.util.UUID

class Transaction {
    val id: String = UUID.randomUUID().toString()
    // => Unique ID per instance
}

@Configuration
class AppConfig {
    @Bean
    @Scope("prototype")  // => Creates NEW instance for each getBean() call
                          // => Not cached in container
    fun transaction(): Transaction {
        return Transaction()  // => Called multiple times
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val t1 = context.getBean(Transaction::class.java)
    val t2 = context.getBean(Transaction::class.java)
    // => Two DIFFERENT instances created

    println("t1 ID: ${t1.id}")  // => Output: t1 ID: [uuid-1]
    println("t2 ID: ${t2.id}")  // => Output: t2 ID: [uuid-2]
    println("Same? ${t1 === t2}")  // => Output: Same? false

    context.close()
}
```

**Expected Output** (IDs will vary):

```
t1 ID: 3f8b4c9e-1234-5678-90ab-cdef12345678
t2 ID: 7a2d1e5f-9876-5432-10fe-dcba87654321
Same? false
```

**Key Takeaways**:

- Prototype scope creates new instance per request
- No caching in container
- Useful for stateful objects or per-request beans
- Container doesn't manage destruction (caller responsible)

**Related Documentation**:

- [Prototype Scope Documentation](https://docs.spring.io/spring-framework/reference/core/beans/factory-scopes.html#beans-factory-scopes-prototype)

---

### Example 13: @PostConstruct Lifecycle Callback (Coverage: 33.0%)

Demonstrates `@PostConstruct` for post-initialization logic.

**Java Implementation**:

```java
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;
import javax.annotation.PostConstruct;

@Component
class DatabaseConnection {
    private boolean connected = false;

    public DatabaseConnection() {
        System.out.println("1. Constructor called");
        // => Constructor runs first
        // => Dependencies not yet injected
    }

    @PostConstruct  // => Called AFTER dependencies injected
                    // => Runs once per bean initialization
    public void initialize() {
        System.out.println("2. @PostConstruct called");
        this.connected = true;  // => Setup logic after DI complete
        System.out.println("   Database connected: " + connected);
    }

    public boolean isConnected() {
        return connected;
    }
}

@Configuration
@ComponentScan
class AppConfig {
}

public class Example13 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Constructor → DI → @PostConstruct sequence

        DatabaseConnection db = context.getBean(DatabaseConnection.class);
        System.out.println("3. Bean ready, connected: " + db.isConnected());
        // => Output: 3. Bean ready, connected: true

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration
import org.springframework.stereotype.Component
import javax.annotation.PostConstruct

@Component
class DatabaseConnection {
    private var connected = false

    init {
        println("1. Constructor called")
        // => Constructor runs first
        // => Dependencies not yet injected
    }

    @PostConstruct  // => Called AFTER dependencies injected
                    // => Runs once per bean initialization
    fun initialize() {
        println("2. @PostConstruct called")
        this.connected = true  // => Setup logic after DI complete
        println("   Database connected: $connected")
    }

    fun isConnected(): Boolean = connected
}

@Configuration
@ComponentScan
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Constructor → DI → @PostConstruct sequence

    val db = context.getBean(DatabaseConnection::class.java)
    println("3. Bean ready, connected: ${db.isConnected()}")
    // => Output: 3. Bean ready, connected: true

    context.close()
}
```

**Expected Output**:

```
1. Constructor called
2. @PostConstruct called
   Database connected: true
3. Bean ready, connected: true
```

**Key Takeaways**:

- `@PostConstruct` runs after dependency injection completes
- Lifecycle order: Constructor → DI → @PostConstruct
- Use for initialization requiring dependencies
- Runs exactly once per bean

**Related Documentation**:

- [Lifecycle Callbacks Documentation](https://docs.spring.io/spring-framework/reference/core/beans/annotation-config/postconstruct-and-predestroy-annotations.html)

---

### Example 14: @PreDestroy Lifecycle Callback (Coverage: 36.0%)

Demonstrates `@PreDestroy` for pre-destruction cleanup.

**Java Implementation**:

```java
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;
import javax.annotation.PreDestroy;

@Component
class FileWriter {
    public FileWriter() {
        System.out.println("FileWriter created");
        // => Constructor called when bean created
    }

    public void write(String data) {
        System.out.println("Writing: " + data);
    }

    @PreDestroy  // => Called BEFORE bean destroyed
                 // => Runs when context.close() called
    public void cleanup() {
        System.out.println("@PreDestroy: Closing file handles");
        // => Cleanup logic: close files, connections, release resources
    }
}

@Configuration
@ComponentScan
class AppConfig {
}

public class Example14 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        FileWriter writer = context.getBean(FileWriter.class);
        writer.write("Zakat record");
        // => Output: Writing: Zakat record

        System.out.println("Closing context...");
        context.close();  // => Triggers @PreDestroy methods
        // => Output: @PreDestroy: Closing file handles
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration
import org.springframework.stereotype.Component
import javax.annotation.PreDestroy

@Component
class FileWriter {
    init {
        println("FileWriter created")
        // => Constructor called when bean created
    }

    fun write(data: String) {
        println("Writing: $data")
    }

    @PreDestroy  // => Called BEFORE bean destroyed
                 // => Runs when context.close() called
    fun cleanup() {
        println("@PreDestroy: Closing file handles")
        // => Cleanup logic: close files, connections, release resources
    }
}

@Configuration
@ComponentScan
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val writer = context.getBean(FileWriter::class.java)
    writer.write("Zakat record")
    // => Output: Writing: Zakat record

    println("Closing context...")
    context.close()  // => Triggers @PreDestroy methods
    // => Output: @PreDestroy: Closing file handles
}
```

**Expected Output**:

```
FileWriter created
Writing: Zakat record
Closing context...
@PreDestroy: Closing file handles
```

**Key Takeaways**:

- `@PreDestroy` runs before bean destruction
- Triggered by `context.close()` or JVM shutdown
- Use for cleanup: close connections, release resources
- Only called for singleton beans (not prototypes)

**Related Documentation**:

- [Destruction Callbacks Documentation](https://docs.spring.io/spring-framework/reference/core/beans/annotation-config/postconstruct-and-predestroy-annotations.html)

---

### Example 15: @Primary for Default Bean (Coverage: 38.0%)

Demonstrates `@Primary` to mark default bean when multiple exist.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

interface Notifier {
    void notify(String message);
}

class EmailNotifier implements Notifier {
    public void notify(String message) {
        System.out.println("Email: " + message);
    }
}

class SmsNotifier implements Notifier {
    public void notify(String message) {
        System.out.println("SMS: " + message);
    }
}

@Configuration
class AppConfig {
    @Bean
    @Primary  // => Marks this as default bean for Notifier type
              // => Used when no @Qualifier specified
    public Notifier emailNotifier() {
        return new EmailNotifier();  // => Primary (default) notifier
    }

    @Bean
    public Notifier smsNotifier() {
        return new SmsNotifier();  // => Alternative notifier
    }

    @Bean
    public AlertService alertService(Notifier notifier) {
        // => @Primary makes emailNotifier inject here
        // => No @Qualifier needed
        return new AlertService(notifier);
    }
}

class AlertService {
    private final Notifier notifier;

    public AlertService(Notifier notifier) {
        this.notifier = notifier;
    }

    public void sendAlert(String message) {
        notifier.notify(message);
    }
}

public class Example15 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        AlertService service = context.getBean(AlertService.class);
        service.sendAlert("Donation received");
        // => Output: Email: Donation received
        // => Uses emailNotifier (marked @Primary)

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.context.annotation.Primary

interface Notifier {
    fun notify(message: String)
}

class EmailNotifier : Notifier {
    override fun notify(message: String) {
        println("Email: $message")
    }
}

class SmsNotifier : Notifier {
    override fun notify(message: String) {
        println("SMS: $message")
    }
}

@Configuration
class AppConfig {
    @Bean
    @Primary  // => Marks this as default bean for Notifier type
              // => Used when no @Qualifier specified
    fun emailNotifier(): Notifier {
        return EmailNotifier()  // => Primary (default) notifier
    }

    @Bean
    fun smsNotifier(): Notifier {
        return SmsNotifier()  // => Alternative notifier
    }

    @Bean
    fun alertService(notifier: Notifier): AlertService {
        // => @Primary makes emailNotifier inject here
        // => No @Qualifier needed
        return AlertService(notifier)
    }
}

class AlertService(private val notifier: Notifier) {
    fun sendAlert(message: String) {
        notifier.notify(message)
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val service = context.getBean(AlertService::class.java)
    service.sendAlert("Donation received")
    // => Output: Email: Donation received
    // => Uses emailNotifier (marked @Primary)

    context.close()
}
```

**Expected Output**:

```
Email: Donation received
```

**Key Takeaways**:

- `@Primary` marks default bean when multiple candidates exist
- Used when injection point doesn't specify `@Qualifier`
- Only one `@Primary` per type allowed
- `@Qualifier` overrides `@Primary` when both present

**Related Documentation**:

- [Primary Beans Documentation](https://docs.spring.io/spring-framework/reference/core/beans/annotation-config/autowired-qualifiers.html#beans-autowired-annotation-primary)

---

## Property Management (Examples 16-20)

### Example 16: @Value with Literal Values (Coverage: 40.0%)

Demonstrates injecting literal values using `@Value` annotation.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;

@Component
class ZakatConfig {
    @Value("0.025")  // => Injects literal double value (2.5%)
                      // => Converted from String to double
    private double rate;

    @Value("85")  // => Injects literal int value (85 grams of gold nisab)
                   // => Converted from String to int
    private int nisabGrams;

    public double getRate() {
        return rate;
    }

    public int getNisabGrams() {
        return nisabGrams;
    }
}

@Configuration
@ComponentScan
class AppConfig {
}

public class Example16 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        ZakatConfig config = context.getBean(ZakatConfig.class);
        System.out.println("Rate: " + config.getRate());        // => Output: Rate: 0.025
        System.out.println("Nisab: " + config.getNisabGrams());  // => Output: Nisab: 85

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Value
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration
import org.springframework.stereotype.Component

@Component
class ZakatConfig {
    @Value("0.025")  // => Injects literal double value (2.5%)
                      // => Converted from String to Double
    private var rate: Double = 0.0

    @Value("85")  // => Injects literal int value (85 grams of gold nisab)
                   // => Converted from String to Int
    private var nisabGrams: Int = 0

    fun getRate(): Double = rate
    fun getNisabGrams(): Int = nisabGrams
}

@Configuration
@ComponentScan
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val config = context.getBean(ZakatConfig::class.java)
    println("Rate: ${config.getRate()}")        // => Output: Rate: 0.025
    println("Nisab: ${config.getNisabGrams()}")  // => Output: Nisab: 85

    context.close()
}
```

**Expected Output**:

```
Rate: 0.025
Nisab: 85
```

**Key Takeaways**:

- `@Value` injects literal values into fields
- Spring auto-converts String to target type
- Supports primitives, wrappers, String
- Useful for simple configuration constants

**Related Documentation**:

- [Value Annotation Documentation](https://docs.spring.io/spring-framework/reference/core/beans/annotation-config/value-annotations.html)

---

### Example 17: @Value with Property Placeholders (Coverage: 42.0%)

Demonstrates injecting values from property files using placeholders.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Component;

// Assume application.properties exists with:
// app.name=Zakat Management System
// app.version=1.0.0
// app.enabled=true

@Component
class AppInfo {
    @Value("${app.name}")  // => Reads property "app.name" from properties file
                            // => Placeholder syntax: ${property.key}
    private String name;

    @Value("${app.version}")  // => Reads "app.version"
    private String version;

    @Value("${app.enabled}")  // => Reads boolean property
                               // => Converted from String "true" to boolean
    private boolean enabled;

    public void printInfo() {
        System.out.println("App: " + name + " v" + version);
        System.out.println("Enabled: " + enabled);
    }
}

@Configuration
@ComponentScan
@PropertySource("classpath:application.properties")
// => Loads properties file into Spring Environment
// => Makes properties available for ${...} placeholders
class AppConfig {
}

public class Example17 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        AppInfo info = context.getBean(AppInfo.class);
        info.printInfo();
        // => Output: App: Zakat Management System v1.0.0
        // => Output: Enabled: true

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Value
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration
import org.springframework.context.annotation.PropertySource
import org.springframework.stereotype.Component

// Assume application.properties exists with:
// app.name=Zakat Management System
// app.version=1.0.0
// app.enabled=true

@Component
class AppInfo {
    @Value("\${app.name}")  // => Reads property "app.name" from properties file
                             // => Placeholder syntax: \${property.key}
                             // => Escaped in Kotlin strings
    private lateinit var name: String

    @Value("\${app.version}")  // => Reads "app.version"
    private lateinit var version: String

    @Value("\${app.enabled}")  // => Reads boolean property
                                // => Converted from String "true" to Boolean
    private var enabled: Boolean = false

    fun printInfo() {
        println("App: $name v$version")
        println("Enabled: $enabled")
    }
}

@Configuration
@ComponentScan
@PropertySource("classpath:application.properties")
// => Loads properties file into Spring Environment
// => Makes properties available for \${...} placeholders
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val info = context.getBean(AppInfo::class.java)
    info.printInfo()
    // => Output: App: Zakat Management System v1.0.0
    // => Output: Enabled: true

    context.close()
}
```

**Expected Output**:

```
App: Zakat Management System v1.0.0
Enabled: true
```

**Key Takeaways**:

- `@PropertySource` loads properties files into Spring Environment
- `${key}` syntax resolves properties
- Type conversion automatic (String → primitives, wrappers)
- Missing properties throw exception (use defaults to avoid)

**Related Documentation**:

- [Property Sources Documentation](https://docs.spring.io/spring-framework/reference/core/beans/environment.html#beans-property-source-abstraction)

---

### Example 18: @Value with Default Values (Coverage: 44.0%)

Demonstrates providing default values when properties missing.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;

@Component
class ServiceConfig {
    @Value("${service.host:localhost}")
    // => Syntax: ${property:default}
    // => If "service.host" property missing, use "localhost"
    private String host;

    @Value("${service.port:8080}")
    // => If "service.port" missing, use 8080
    // => Converted from String "8080" to int
    private int port;

    @Value("${service.timeout:5000}")
    // => Default 5000ms if property missing
    private int timeout;

    public void printConfig() {
        System.out.println("Host: " + host);
        System.out.println("Port: " + port);
        System.out.println("Timeout: " + timeout + "ms");
    }
}

@Configuration
@ComponentScan
class AppConfig {
    // => No @PropertySource - properties missing
    // => Defaults will be used
}

public class Example18 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        ServiceConfig config = context.getBean(ServiceConfig.class);
        config.printConfig();
        // => Output: Host: localhost
        // => Output: Port: 8080
        // => Output: Timeout: 5000ms
        // => All defaults used since properties missing

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Value
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration
import org.springframework.stereotype.Component

@Component
class ServiceConfig {
    @Value("\${service.host:localhost}")
    // => Syntax: \${property:default}
    // => If "service.host" property missing, use "localhost"
    private lateinit var host: String

    @Value("\${service.port:8080}")
    // => If "service.port" missing, use 8080
    // => Converted from String "8080" to Int
    private var port: Int = 0

    @Value("\${service.timeout:5000}")
    // => Default 5000ms if property missing
    private var timeout: Int = 0

    fun printConfig() {
        println("Host: $host")
        println("Port: $port")
        println("Timeout: ${timeout}ms")
    }
}

@Configuration
@ComponentScan
class AppConfig {
    // => No @PropertySource - properties missing
    // => Defaults will be used
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val config = context.getBean(ServiceConfig::class.java)
    config.printConfig()
    // => Output: Host: localhost
    // => Output: Port: 8080
    // => Output: Timeout: 5000ms
    // => All defaults used since properties missing

    context.close()
}
```

**Expected Output**:

```
Host: localhost
Port: 8080
Timeout: 5000ms
```

**Key Takeaways**:

- Syntax `${property:default}` provides fallback values
- Prevents exceptions when properties missing
- Defaults must match target type
- Useful for optional configuration

**Related Documentation**:

- [Default Values Documentation](https://docs.spring.io/spring-framework/reference/core/beans/annotation-config/value-annotations.html)

---

### Example 19: Profile-Based Configuration (@Profile) (Coverage: 46.0%)

Demonstrates environment-specific bean registration using `@Profile`.

**Java Implementation**:

```java
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

interface DatabaseConnection {
    void connect();
}

class DevDatabase implements DatabaseConnection {
    public void connect() {
        System.out.println("Connected to DEV database (H2 in-memory)");
    }
}

class ProdDatabase implements DatabaseConnection {
    public void connect() {
        System.out.println("Connected to PROD database (PostgreSQL)");
    }
}

@Configuration
class AppConfig {
    @Bean
    @Profile("dev")  // => Only active when "dev" profile active
                      // => Bean registered conditionally
    public DatabaseConnection devDatabase() {
        return new DevDatabase();  // => Created only in dev profile
    }

    @Bean
    @Profile("prod")  // => Only active when "prod" profile active
    public DatabaseConnection prodDatabase() {
        return new ProdDatabase();  // => Created only in prod profile
    }
}

public class Example19 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext();

        context.getEnvironment().setActiveProfiles("dev");
        // => Activates "dev" profile
        // => Only @Profile("dev") beans registered

        context.register(AppConfig.class);
        context.refresh();
        // => Context initialized with dev profile

        DatabaseConnection db = context.getBean(DatabaseConnection.class);
        db.connect();
        // => Output: Connected to DEV database (H2 in-memory)
        // => Uses devDatabase bean

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.context.annotation.Profile

interface DatabaseConnection {
    fun connect()
}

class DevDatabase : DatabaseConnection {
    override fun connect() {
        println("Connected to DEV database (H2 in-memory)")
    }
}

class ProdDatabase : DatabaseConnection {
    override fun connect() {
        println("Connected to PROD database (PostgreSQL)")
    }
}

@Configuration
class AppConfig {
    @Bean
    @Profile("dev")  // => Only active when "dev" profile active
                      // => Bean registered conditionally
    fun devDatabase(): DatabaseConnection {
        return DevDatabase()  // => Created only in dev profile
    }

    @Bean
    @Profile("prod")  // => Only active when "prod" profile active
    fun prodDatabase(): DatabaseConnection {
        return ProdDatabase()  // => Created only in prod profile
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext()

    context.environment.setActiveProfiles("dev")
    // => Activates "dev" profile
    // => Only @Profile("dev") beans registered

    context.register(AppConfig::class.java)
    context.refresh()
    // => Context initialized with dev profile

    val db = context.getBean(DatabaseConnection::class.java)
    db.connect()
    // => Output: Connected to DEV database (H2 in-memory)
    // => Uses devDatabase bean

    context.close()
}
```

**Expected Output**:

```
Connected to DEV database (H2 in-memory)
```

**Key Takeaways**:

- `@Profile` enables conditional bean registration
- Activate profiles via `setActiveProfiles()` or properties
- Multiple profiles can be active simultaneously
- Use for environment-specific configuration (dev, prod, test)

**Related Documentation**:

- [Bean Definition Profiles Documentation](https://docs.spring.io/spring-framework/reference/core/beans/environment.html#beans-definition-profiles)

---

### Example 20: Environment Abstraction (Coverage: 48.0%)

Demonstrates programmatic property access using Spring's Environment abstraction.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

@Component
class ConfigReader {
    @Autowired
    private Environment env;  // => Injected Environment object
                               // => Provides programmatic property access

    public void readConfig() {
        String name = env.getProperty("app.name");
        // => Reads property, returns null if missing

        String version = env.getProperty("app.version", "0.0.0");
        // => Second parameter is default value

        int timeout = env.getProperty("app.timeout", Integer.class, 3000);
        // => Reads with type conversion and default

        boolean enabled = env.getProperty("app.enabled", Boolean.class);
        // => Type-safe property reading

        System.out.println("Name: " + name);
        System.out.println("Version: " + version);
        System.out.println("Timeout: " + timeout);
        System.out.println("Enabled: " + enabled);
    }
}

@Configuration
@ComponentScan
@PropertySource("classpath:application.properties")
class AppConfig {
}

public class Example20 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        ConfigReader reader = context.getBean(ConfigReader.class);
        reader.readConfig();

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration
import org.springframework.context.annotation.PropertySource
import org.springframework.core.env.Environment
import org.springframework.stereotype.Component

@Component
class ConfigReader {
    @Autowired
    private lateinit var env: Environment  // => Injected Environment object
                                            // => Provides programmatic property access

    fun readConfig() {
        val name = env.getProperty("app.name")
        // => Reads property, returns null if missing

        val version = env.getProperty("app.version", "0.0.0")
        // => Second parameter is default value

        val timeout = env.getProperty("app.timeout", Int::class.java, 3000)
        // => Reads with type conversion and default

        val enabled = env.getProperty("app.enabled", Boolean::class.java)
        // => Type-safe property reading

        println("Name: $name")
        println("Version: $version")
        println("Timeout: $timeout")
        println("Enabled: $enabled")
    }
}

@Configuration
@ComponentScan
@PropertySource("classpath:application.properties")
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val reader = context.getBean(ConfigReader::class.java)
    reader.readConfig()

    context.close()
}
```

**Expected Output** (assuming application.properties exists):

```
Name: Zakat Management System
Version: 1.0.0
Timeout: 3000
Enabled: true
```

**Key Takeaways**:

- `Environment` provides programmatic property access
- `getProperty()` supports type conversion and defaults
- Alternative to `@Value` for dynamic property reading
- Access active profiles, system properties, environment variables

**Related Documentation**:

- [Environment Abstraction Documentation](https://docs.spring.io/spring-framework/reference/core/beans/environment.html)

---

## Resource Loading and Collections (Examples 21-25)

### Example 21: Loading Resources (Coverage: 50.0%)

Demonstrates loading files and resources using Spring's Resource abstraction.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.stream.Collectors;

@Component
class ResourceLoader {
    @Value("classpath:zakat-rates.txt")
    // => Loads resource from classpath
    // => Spring converts path string to Resource object
    private Resource resource;

    public void readResource() throws Exception {
        BufferedReader reader = new BufferedReader(
            new InputStreamReader(resource.getInputStream())
            // => Opens InputStream from Resource
        );

        String content = reader.lines().collect(Collectors.joining("\n"));
        // => Reads all lines

        System.out.println("Resource content:");
        System.out.println(content);

        reader.close();
    }
}

@Configuration
@ComponentScan
class AppConfig {
}

public class Example21 {
    public static void main(String[] args) throws Exception {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        ResourceLoader loader = context.getBean(ResourceLoader.class);
        loader.readResource();
        // => Reads and prints file content

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Value
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration
import org.springframework.core.io.Resource
import org.springframework.stereotype.Component
import java.io.BufferedReader
import java.io.InputStreamReader

@Component
class ResourceLoader {
    @Value("classpath:zakat-rates.txt")
    // => Loads resource from classpath
    // => Spring converts path string to Resource object
    private lateinit var resource: Resource

    fun readResource() {
        val reader = BufferedReader(InputStreamReader(resource.inputStream))
        // => Opens InputStream from Resource

        val content = reader.readLines().joinToString("\n")
        // => Reads all lines

        println("Resource content:")
        println(content)

        reader.close()
    }
}

@Configuration
@ComponentScan
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val loader = context.getBean(ResourceLoader::class.java)
    loader.readResource()
    // => Reads and prints file content

    context.close()
}
```

**Expected Output**:

```
Resource content:
Gold: 2.5%
Silver: 2.5%
Cash: 2.5%
```

**Key Takeaways**:

- `Resource` abstraction unifies classpath, file, URL resources
- `@Value` converts resource paths to Resource objects
- Support for classpath:, file:, http: protocols
- Consistent API across resource types

**Related Documentation**:

- [Resources Documentation](https://docs.spring.io/spring-framework/reference/core/resources.html)

---

### Example 22: Injecting Collections (Coverage: 52.0%)

Demonstrates injecting collections of beans (List, Set, Map).

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import java.util.List;

interface PaymentMethod {
    String getName();
}

class CashPayment implements PaymentMethod {
    public String getName() { return "Cash"; }
}

class CardPayment implements PaymentMethod {
    public String getName() { return "Card"; }
}

class BankTransfer implements PaymentMethod {
    public String getName() { return "Bank Transfer"; }
}

@Configuration
class AppConfig {
    @Bean
    public PaymentMethod cashPayment() {
        return new CashPayment();  // => Bean 1
    }

    @Bean
    public PaymentMethod cardPayment() {
        return new CardPayment();  // => Bean 2
    }

    @Bean
    public PaymentMethod bankTransfer() {
        return new BankTransfer();  // => Bean 3
    }

    @Bean
    public PaymentRegistry registry(List<PaymentMethod> methods) {
        // => Spring automatically collects ALL beans of type PaymentMethod
        // => Injects as List
        return new PaymentRegistry(methods);
    }
}

class PaymentRegistry {
    private final List<PaymentMethod> methods;

    public PaymentRegistry(List<PaymentMethod> methods) {
        this.methods = methods;  // => Receives all PaymentMethod beans
    }

    public void listMethods() {
        System.out.println("Available payment methods:");
        for (PaymentMethod method : methods) {
            System.out.println("- " + method.getName());
        }
    }
}

public class Example22 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        PaymentRegistry registry = context.getBean(PaymentRegistry.class);
        registry.listMethods();
        // => Output: Available payment methods:
        // => Output: - Cash
        // => Output: - Card
        // => Output: - Bank Transfer

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

interface PaymentMethod {
    fun getName(): String
}

class CashPayment : PaymentMethod {
    override fun getName(): String = "Cash"
}

class CardPayment : PaymentMethod {
    override fun getName(): String = "Card"
}

class BankTransfer : PaymentMethod {
    override fun getName(): String = "Bank Transfer"
}

@Configuration
class AppConfig {
    @Bean
    fun cashPayment(): PaymentMethod = CashPayment()  // => Bean 1

    @Bean
    fun cardPayment(): PaymentMethod = CardPayment()  // => Bean 2

    @Bean
    fun bankTransfer(): PaymentMethod = BankTransfer()  // => Bean 3

    @Bean
    fun registry(methods: List<PaymentMethod>): PaymentRegistry {
        // => Spring automatically collects ALL beans of type PaymentMethod
        // => Injects as List
        return PaymentRegistry(methods)
    }
}

class PaymentRegistry(private val methods: List<PaymentMethod>) {
    // => Receives all PaymentMethod beans

    fun listMethods() {
        println("Available payment methods:")
        methods.forEach { println("- ${it.getName()}") }
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val registry = context.getBean(PaymentRegistry::class.java)
    registry.listMethods()
    // => Output: Available payment methods:
    // => Output: - Cash
    // => Output: - Card
    // => Output: - Bank Transfer

    context.close()
}
```

**Expected Output**:

```
Available payment methods:
- Cash
- Card
- Bank Transfer
```

**Key Takeaways**:

- Spring auto-collects beans of matching type into collections
- Supports List, Set, Map injection
- Order preserved for List (registration order)
- Useful for plugin/strategy pattern implementations

**Related Documentation**:

- [Collection Injection Documentation](https://docs.spring.io/spring-framework/reference/core/beans/dependencies/factory-autowire.html#beans-autowired-annotation-collection)

---

### Example 23: Conditional Bean Registration (@Conditional) (Coverage: 54.0%)

Demonstrates conditional bean registration using `@Conditional` annotation.

**Java Implementation**:

```java
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Conditional;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Condition;
import org.springframework.context.annotation.ConditionContext;
import org.springframework.core.type.AnnotatedTypeMetadata;

class LinuxCondition implements Condition {
    // => Custom condition checking if OS is Linux
    @Override
    public boolean matches(ConditionContext context, AnnotatedTypeMetadata metadata) {
        String os = System.getProperty("os.name").toLowerCase();
        // => Reads system property
        return os.contains("linux");  // => Returns true if Linux
        // => Bean registered only if this returns true
    }
}

class FileService {
    private final String basePath;

    public FileService(String basePath) {
        this.basePath = basePath;
    }

    public void printPath() {
        System.out.println("Base path: " + basePath);
    }
}

@Configuration
class AppConfig {
    @Bean
    @Conditional(LinuxCondition.class)
    // => Bean only registered if LinuxCondition.matches() returns true
    public FileService linuxFileService() {
        return new FileService("/var/data");
        // => Linux-specific path
    }

    @Bean
    @Conditional(WindowsCondition.class)
    public FileService windowsFileService() {
        return new FileService("C:\\Data");
        // => Windows-specific path
    }
}

class WindowsCondition implements Condition {
    @Override
    public boolean matches(ConditionContext context, AnnotatedTypeMetadata metadata) {
        String os = System.getProperty("os.name").toLowerCase();
        return os.contains("windows");
    }
}

public class Example23 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        FileService service = context.getBean(FileService.class);
        service.printPath();
        // => Output on Linux: Base path: /var/data
        // => Output on Windows: Base path: C:\Data
        // => Only one bean registered based on OS

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Conditional
import org.springframework.context.annotation.Configuration
import org.springframework.context.annotation.Condition
import org.springframework.context.annotation.ConditionContext
import org.springframework.core.type.AnnotatedTypeMetadata

class LinuxCondition : Condition {
    // => Custom condition checking if OS is Linux
    override fun matches(context: ConditionContext, metadata: AnnotatedTypeMetadata): Boolean {
        val os = System.getProperty("os.name").lowercase()
        // => Reads system property
        return os.contains("linux")  // => Returns true if Linux
        // => Bean registered only if this returns true
    }
}

class WindowsCondition : Condition {
    override fun matches(context: ConditionContext, metadata: AnnotatedTypeMetadata): Boolean {
        val os = System.getProperty("os.name").lowercase()
        return os.contains("windows")
    }
}

class FileService(private val basePath: String) {
    fun printPath() {
        println("Base path: $basePath")
    }
}

@Configuration
class AppConfig {
    @Bean
    @Conditional(LinuxCondition::class)
    // => Bean only registered if LinuxCondition.matches() returns true
    fun linuxFileService(): FileService {
        return FileService("/var/data")
        // => Linux-specific path
    }

    @Bean
    @Conditional(WindowsCondition::class)
    fun windowsFileService(): FileService {
        return FileService("C:\\Data")
        // => Windows-specific path
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val service = context.getBean(FileService::class.java)
    service.printPath()
    // => Output on Linux: Base path: /var/data
    // => Output on Windows: Base path: C:\Data
    // => Only one bean registered based on OS

    context.close()
}
```

**Expected Output** (varies by OS):

```
Base path: /var/data
```

**Key Takeaways**:

- `@Conditional` enables runtime bean registration decisions
- Implement `Condition` interface for custom logic
- Check system properties, environment, bean presence
- More flexible than `@Profile` for complex conditions

**Related Documentation**:

- [Conditional Beans Documentation](https://docs.spring.io/spring-framework/reference/core/beans/java/bean-annotation.html#beans-java-conditional)

---

### Example 24: Lazy Bean Initialization (@Lazy) (Coverage: 56.0%)

Demonstrates lazy initialization to defer bean creation until first use.

**Java Implementation**:

```java
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;

class ExpensiveService {
    public ExpensiveService() {
        System.out.println("ExpensiveService created (expensive initialization)");
        // => Constructor called when bean created
        // => Simulates expensive operation (database connection, etc.)
    }

    public void doWork() {
        System.out.println("ExpensiveService working");
    }
}

@Configuration
class AppConfig {
    @Bean
    @Lazy  // => Bean NOT created during context initialization
           // => Created on FIRST getBean() call
    public ExpensiveService expensiveService() {
        return new ExpensiveService();
        // => Called lazily, not eagerly
    }
}

public class Example24 {
    public static void main(String[] args) {
        System.out.println("Creating context...");
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        System.out.println("Context created");
        // => Output: Creating context...
        // => Output: Context created
        // => ExpensiveService NOT yet created

        System.out.println("Requesting bean...");
        ExpensiveService service = context.getBean(ExpensiveService.class);
        // => NOW bean is created
        // => Output: ExpensiveService created (expensive initialization)

        service.doWork();
        // => Output: ExpensiveService working

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.context.annotation.Lazy

class ExpensiveService {
    init {
        println("ExpensiveService created (expensive initialization)")
        // => Constructor called when bean created
        // => Simulates expensive operation (database connection, etc.)
    }

    fun doWork() {
        println("ExpensiveService working")
    }
}

@Configuration
class AppConfig {
    @Bean
    @Lazy  // => Bean NOT created during context initialization
           // => Created on FIRST getBean() call
    fun expensiveService(): ExpensiveService {
        return ExpensiveService()
        // => Called lazily, not eagerly
    }
}

fun main() {
    println("Creating context...")
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    println("Context created")
    // => Output: Creating context...
    // => Output: Context created
    // => ExpensiveService NOT yet created

    println("Requesting bean...")
    val service = context.getBean(ExpensiveService::class.java)
    // => NOW bean is created
    // => Output: ExpensiveService created (expensive initialization)

    service.doWork()
    // => Output: ExpensiveService working

    context.close()
}
```

**Expected Output**:

```
Creating context...
Context created
Requesting bean...
ExpensiveService created (expensive initialization)
ExpensiveService working
```

**Key Takeaways**:

- `@Lazy` defers bean creation until first use
- Reduces startup time for rarely-used beans
- Default is eager initialization (all beans created at startup)
- Useful for expensive initializations

**Related Documentation**:

- [Lazy Initialization Documentation](https://docs.spring.io/spring-framework/reference/core/beans/dependencies/factory-lazy-init.html)

---

### Example 25: DependsOn for Bean Creation Order (Coverage: 58.0%)

Demonstrates controlling bean initialization order using `@DependsOn`.

**Java Implementation**:

```java
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;

class ConfigLoader {
    public ConfigLoader() {
        System.out.println("1. ConfigLoader created (loads configuration)");
        // => Must be created FIRST
    }
}

class CacheWarmer {
    public CacheWarmer() {
        System.out.println("2. CacheWarmer created (warms cache)");
        // => Should be created AFTER ConfigLoader
    }
}

class ApplicationService {
    public ApplicationService() {
        System.out.println("3. ApplicationService created");
        // => Should be created LAST
    }
}

@Configuration
class AppConfig {
    @Bean
    public ConfigLoader configLoader() {
        return new ConfigLoader();  // => Created first
    }

    @Bean
    @DependsOn("configLoader")
    // => Ensures configLoader bean created BEFORE this bean
    // => Even without injection relationship
    public CacheWarmer cacheWarmer() {
        return new CacheWarmer();  // => Created second
    }

    @Bean
    @DependsOn({"configLoader", "cacheWarmer"})
    // => Multiple dependencies (both must be created first)
    public ApplicationService applicationService() {
        return new ApplicationService();  // => Created last
    }
}

public class Example25 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Beans created in order: ConfigLoader → CacheWarmer → ApplicationService
        // => Output shows controlled initialization order

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.context.annotation.DependsOn

class ConfigLoader {
    init {
        println("1. ConfigLoader created (loads configuration)")
        // => Must be created FIRST
    }
}

class CacheWarmer {
    init {
        println("2. CacheWarmer created (warms cache)")
        // => Should be created AFTER ConfigLoader
    }
}

class ApplicationService {
    init {
        println("3. ApplicationService created")
        // => Should be created LAST
    }
}

@Configuration
class AppConfig {
    @Bean
    fun configLoader(): ConfigLoader {
        return ConfigLoader()  // => Created first
    }

    @Bean
    @DependsOn("configLoader")
    // => Ensures configLoader bean created BEFORE this bean
    // => Even without injection relationship
    fun cacheWarmer(): CacheWarmer {
        return CacheWarmer()  // => Created second
    }

    @Bean
    @DependsOn("configLoader", "cacheWarmer")
    // => Multiple dependencies (both must be created first)
    fun applicationService(): ApplicationService {
        return ApplicationService()  // => Created last
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Beans created in order: ConfigLoader → CacheWarmer → ApplicationService
    // => Output shows controlled initialization order

    context.close()
}
```

**Expected Output**:

```
1. ConfigLoader created (loads configuration)
2. CacheWarmer created (warms cache)
3. ApplicationService created
```

**Key Takeaways**:

- `@DependsOn` controls bean initialization order
- Works without actual dependency injection
- Accepts array of bean names
- Useful for startup sequence requirements

**Related Documentation**:

- [DependsOn Documentation](https://docs.spring.io/spring-framework/reference/core/beans/dependencies/factory-dependson.html)

---

## Summary

This beginner tutorial covered **25 fundamental Spring Framework examples** (0-40% coverage):

**Basic Operations (1-5)**:

- ApplicationContext creation
- Bean definition and retrieval
- Constructor dependency injection
- Component scanning
- @Autowired annotation

**Bean Configuration (6-10)**:

- Custom bean names
- Bean aliases
- Setter injection
- Field injection
- @Qualifier disambiguation

**Bean Scopes and Lifecycle (11-15)**:

- Singleton scope (default)
- Prototype scope
- @PostConstruct lifecycle
- @PreDestroy cleanup
- @Primary default beans

**Property Management (16-20)**:

- @Value with literals
- Property placeholders
- Default values
- @Profile-based config
- Environment abstraction

**Resource Loading and Collections (21-25)**:

- Resource loading
- Collection injection
- @Conditional registration
- @Lazy initialization
- @DependsOn ordering

**Next Steps**: Progress to [Intermediate](/en/learn/software-engineering/web-platform/jvm-spring/by-example/intermediate) (40-75% coverage) covering advanced DI, AOP, transactions, and data access.
