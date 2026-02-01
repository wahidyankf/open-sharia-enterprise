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

class Calculator {  // => Simple POJO for arithmetic operations
    public double add(double a, double b) {  // => Addition method
        return a + b;  // => Returns sum of two numbers
    }
}

@Configuration  // => Marks this as Spring configuration source
class AppConfig {
    @Bean(name = "primaryCalculator")  // => Custom bean name
                                         // => Overrides default "calculator"
                                         // => name attribute explicitly sets bean identifier
    public Calculator calculator() {
        return new Calculator();  // => Bean registered as "primaryCalculator"
                                   // => Spring stores with custom name, not method name
    }

    @Bean("backupCalculator")  // => Shorthand for name attribute
                                // => Equivalent to @Bean(name = "backupCalculator")
    public Calculator anotherCalculator() {
        return new Calculator();  // => Bean registered as "backupCalculator"
                                   // => Different instance from primaryCalculator
    }
}

public class Example06 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Spring initializes both calculator beans

        Calculator primary = context.getBean("primaryCalculator", Calculator.class);
        // => Retrieves bean by custom name and type
        // => primary references the "primaryCalculator" bean
        System.out.println("5 + 3 = " + primary.add(5, 3));
        // => Calls add method on retrieved bean
        // => Output: 5 + 3 = 8.0

        Calculator backup = context.getBean("backupCalculator", Calculator.class);
        // => Retrieves second bean by its custom name
        // => backup is separate instance from primary
        System.out.println("10 + 2 = " + backup.add(10, 2));
        // => Uses second calculator instance
        // => Output: 10 + 2 = 12.0

        context.close();  // => Cleanup resources
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

class Calculator {  // => Simple Kotlin class for arithmetic
    fun add(a: Double, b: Double): Double = a + b  // => Expression body function
                                                     // => Returns sum directly
}

@Configuration  // => Marks this as Spring configuration source
class AppConfig {
    @Bean(name = ["primaryCalculator"])  // => Custom bean name (array syntax)
                                          // => Overrides default "calculator"
                                          // => Kotlin requires array syntax for single value
    fun calculator(): Calculator {
        return Calculator()  // => Bean registered as "primaryCalculator"
                              // => Spring stores with custom name, not method name
    }

    @Bean("backupCalculator")  // => Shorthand for name attribute
                                // => Equivalent to @Bean(name = ["backupCalculator"])
    fun anotherCalculator(): Calculator {
        return Calculator()  // => Bean registered as "backupCalculator"
                              // => Different instance from primaryCalculator
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Spring initializes both calculator beans

    val primary = context.getBean("primaryCalculator", Calculator::class.java)
    // => Retrieves bean by custom name and type
    // => primary references the "primaryCalculator" bean
    println("5 + 3 = ${primary.add(5.0, 3.0)}")
    // => Calls add method on retrieved bean
    // => Output: 5 + 3 = 8.0

    val backup = context.getBean("backupCalculator", Calculator::class.java)
    // => Retrieves second bean by its custom name
    // => backup is separate instance from primary
    println("10 + 2 = ${backup.add(10.0, 2.0)}")
    // => Uses second calculator instance
    // => Output: 10 + 2 = 12.0

    context.close()  // => Cleanup resources
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

class ZakatCalculator {  // => Zakat calculation service
    public double calculate(double wealth) {  // => Calculates 2.5% zakat
        return wealth * 0.025;  // => Standard zakat rate for cash/gold
    }
}

@Configuration  // => Spring configuration class
class AppConfig {
    @Bean(name = {"zakatCalc", "zakatCalculator", "zakahService"})
    // => Defines three aliases for same bean
    // => All names reference the SAME instance (singleton)
    // => Primary name is first: "zakatCalc"
    public ZakatCalculator calculator() {
        return new ZakatCalculator();  // => Single bean, multiple names
                                         // => Only one instance created
    }
}

public class Example07 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Spring creates one ZakatCalculator bean with three names

        ZakatCalculator calc1 = context.getBean("zakatCalc", ZakatCalculator.class);
        // => Retrieves via first alias
        ZakatCalculator calc2 = context.getBean("zakatCalculator", ZakatCalculator.class);
        // => Retrieves via second alias
        ZakatCalculator calc3 = context.getBean("zakahService", ZakatCalculator.class);
        // => Retrieves via third alias
        // => All three retrieve the SAME bean instance

        System.out.println("Same instance? " + (calc1 == calc2 && calc2 == calc3));
        // => Checks reference equality
        // => Output: Same instance? true

        context.close();  // => Cleanup resources
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

class ZakatCalculator {  // => Zakat calculation service
    fun calculate(wealth: Double): Double = wealth * 0.025  // => Calculates 2.5% zakat
                                                             // => Standard rate for cash/gold
}

@Configuration  // => Spring configuration class
class AppConfig {
    @Bean(name = ["zakatCalc", "zakatCalculator", "zakahService"])
    // => Defines three aliases for same bean
    // => All names reference the SAME instance (singleton)
    // => Primary name is first: "zakatCalc"
    fun calculator(): ZakatCalculator {
        return ZakatCalculator()  // => Single bean, multiple names
                                   // => Only one instance created
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Spring creates one ZakatCalculator bean with three names

    val calc1 = context.getBean("zakatCalc", ZakatCalculator::class.java)
    // => Retrieves via first alias
    val calc2 = context.getBean("zakatCalculator", ZakatCalculator::class.java)
    // => Retrieves via second alias
    val calc3 = context.getBean("zakahService", ZakatCalculator::class.java)
    // => Retrieves via third alias
    // => All three retrieve the SAME bean instance

    println("Same instance? ${calc1 === calc2 && calc2 === calc3}")
    // => Checks reference equality (=== in Kotlin)
    // => Output: Same instance? true

    context.close()  // => Cleanup resources
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

@Component  // => Spring-managed component
class EmailService {  // => Email sending service
    public void send(String message) {  // => Sends email notification
        System.out.println("Email sent: " + message);  // => Simulates email delivery
    }
}

@Service  // => Business logic component
class NotificationService {  // => Notification coordination service
    private EmailService emailService;  // => Dependency (not final)
                                          // => Allows post-construction modification

    @Autowired  // => Spring calls setter after object construction
                // => Injects emailService bean via setter method
                // => Optional on single setter in Spring 4.3+
    public void setEmailService(EmailService emailService) {
        this.emailService = emailService;  // => Setter injection assigns dependency
        // => Allows changing dependency after construction (if needed)
        // => Less safe than constructor injection
    }

    public void notifyDonation(String donor) {  // => Business method
        emailService.send("Thank you, " + donor);  // => Uses injected dependency
                                                     // => Delegates to EmailService
    }
}

@Configuration  // => Spring configuration source
@ComponentScan  // => Scans for @Component, @Service beans
class AppConfig {
}

public class Example08 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Spring discovers EmailService and NotificationService beans
        // => Creates NotificationService first, then calls setEmailService

        NotificationService service = context.getBean(NotificationService.class);
        // => Retrieves fully-wired NotificationService bean
        service.notifyDonation("Ali");  // => Calls business method
        // => Output: Email sent: Thank you, Ali

        context.close();  // => Cleanup resources
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

@Component  // => Spring-managed component
class EmailService {  // => Email sending service
    fun send(message: String) {  // => Sends email notification
        println("Email sent: $message")  // => Simulates email delivery
    }
}

@Service  // => Business logic component
class NotificationService {  // => Notification coordination service
    private lateinit var emailService: EmailService  // => Late-init dependency
                                                      // => Not initialized in constructor
                                                      // => Set later via setter

    @Autowired  // => Spring calls setter after object construction
                // => Injects emailService bean via setter method
                // => Optional on single setter in Spring 4.3+
    fun setEmailService(emailService: EmailService) {
        this.emailService = emailService  // => Setter injection assigns dependency
        // => lateinit allows setting non-null var after construction
        // => Less safe than constructor injection
    }

    fun notifyDonation(donor: String) {  // => Business method
        emailService.send("Thank you, $donor")  // => Uses injected dependency
                                                 // => Delegates to EmailService
    }
}

@Configuration  // => Spring configuration source
@ComponentScan  // => Scans for @Component, @Service beans
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Spring discovers EmailService and NotificationService beans
    // => Creates NotificationService first, then calls setEmailService

    val service = context.getBean(NotificationService::class.java)
    // => Retrieves fully-wired NotificationService bean
    service.notifyDonation("Ali")  // => Calls business method
    // => Output: Email sent: Thank you, Ali

    context.close()  // => Cleanup resources
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

@Component  // => Spring-managed component
class AuditLogger {  // => Audit logging service
    public void log(String action) {  // => Logs audit events
        System.out.println("AUDIT: " + action);  // => Writes to console
    }
}

@Service  // => Business logic component
class TransactionService {  // => Transaction processing service
    @Autowired  // => Spring injects directly into field via reflection
                // => No constructor or setter needed
                // => Happens after object construction
    private AuditLogger logger;  // => Field injection (simplest syntax)
                                  // => Cannot be final (reflection requirement)

    public void processTransaction(String type, double amount) {  // => Business method
        logger.log(type + " transaction: $" + amount);  // => Uses injected dependency
        // => Delegates to AuditLogger bean
    }
}

@Configuration  // => Spring configuration source
@ComponentScan  // => Scans for @Component, @Service beans
class AppConfig {
}

public class Example09 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Spring discovers AuditLogger and TransactionService beans
        // => Creates beans, injects logger field via reflection

        TransactionService service = context.getBean(TransactionService.class);
        // => Retrieves fully-wired TransactionService
        service.processTransaction("Sadaqah", 200.0);  // => Calls business method
        // => Output: AUDIT: Sadaqah transaction: $200.0

        context.close();  // => Cleanup resources
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

@Component  // => Spring-managed component
class AuditLogger {  // => Audit logging service
    fun log(action: String) {  // => Logs audit events
        println("AUDIT: $action")  // => Writes to console
    }
}

@Service  // => Business logic component
class TransactionService {  // => Transaction processing service
    @Autowired  // => Spring injects directly into field via reflection
                // => No constructor or setter needed
                // => Happens after object construction
    private lateinit var logger: AuditLogger  // => Field injection with lateinit
                                               // => Cannot be val (reflection requirement)
                                               // => Must use var with lateinit

    fun processTransaction(type: String, amount: Double) {  // => Business method
        logger.log("$type transaction: $$amount")  // => Uses injected dependency
        // => Delegates to AuditLogger bean
    }
}

@Configuration  // => Spring configuration source
@ComponentScan  // => Scans for @Component, @Service beans
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Spring discovers AuditLogger and TransactionService beans
    // => Creates beans, injects logger field via reflection

    val service = context.getBean(TransactionService::class.java)
    // => Retrieves fully-wired TransactionService
    service.processTransaction("Sadaqah", 200.0)  // => Calls business method
    // => Output: AUDIT: Sadaqah transaction: $200.0

    context.close()  // => Cleanup resources
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

interface PaymentProcessor {  // => Common interface for payment types
    void process(double amount);  // => Process payment contract
}

class CashPayment implements PaymentProcessor {  // => Cash payment implementation
    public void process(double amount) {  // => Handles cash transactions
        System.out.println("Cash payment: $" + amount);  // => Simulates cash processing
    }
}

class CardPayment implements PaymentProcessor {  // => Card payment implementation
    public void process(double amount) {  // => Handles card transactions
        System.out.println("Card payment: $" + amount);  // => Simulates card processing
    }
}

@Configuration  // => Spring configuration source
class AppConfig {
    @Bean  // => Defines PaymentProcessor bean
    @Qualifier("cash")  // => Tags this bean as "cash" processor
                         // => Allows disambiguation when injecting
    public PaymentProcessor cashProcessor() {
        return new CashPayment();  // => Registered with "cash" qualifier
                                    // => Type: PaymentProcessor
    }

    @Bean  // => Defines second PaymentProcessor bean
    @Qualifier("card")  // => Tags this bean as "card" processor
                         // => Different qualifier from cash
    public PaymentProcessor cardProcessor() {
        return new CardPayment();  // => Registered with "card" qualifier
                                    // => Same type, different qualifier
    }

    @Bean  // => Defines DonationService bean
    public DonationService donationService(
        @Qualifier("cash") PaymentProcessor processor
        // => Specifies which bean to inject when multiple exist
        // => Injects cashProcessor, not cardProcessor
        // => Without @Qualifier would throw NoUniqueBeanDefinitionException
    ) {
        return new DonationService(processor);  // => Creates service with cash processor
    }
}

class DonationService {  // => Business service for donations
    private final PaymentProcessor processor;  // => Injected payment processor

    public DonationService(PaymentProcessor processor) {  // => Constructor injection
        this.processor = processor;  // => Assigns injected dependency
    }

    public void acceptDonation(double amount) {  // => Business method
        processor.process(amount);  // => Delegates to injected processor
                                     // => Uses cash processor from qualifier
    }
}

public class Example10 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Spring creates both payment processors and donation service
        // => Injects cash processor into service

        DonationService service = context.getBean(DonationService.class);
        // => Retrieves wired DonationService bean
        service.acceptDonation(100.0);  // => Processes donation
        // => Output: Cash payment: $100.0
        // => Uses cashProcessor due to @Qualifier("cash")

        context.close();  // => Cleanup resources
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Qualifier
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

interface PaymentProcessor {  // => Common interface for payment types
    fun process(amount: Double)  // => Process payment contract
}

class CashPayment : PaymentProcessor {  // => Cash payment implementation
    override fun process(amount: Double) {  // => Handles cash transactions
        println("Cash payment: $$amount")  // => Simulates cash processing
    }
}

class CardPayment : PaymentProcessor {  // => Card payment implementation
    override fun process(amount: Double) {  // => Handles card transactions
        println("Card payment: $$amount")  // => Simulates card processing
    }
}

@Configuration  // => Spring configuration source
class AppConfig {
    @Bean  // => Defines PaymentProcessor bean
    @Qualifier("cash")  // => Tags this bean as "cash" processor
                         // => Allows disambiguation when injecting
    fun cashProcessor(): PaymentProcessor {
        return CashPayment()  // => Registered with "cash" qualifier
                               // => Type: PaymentProcessor
    }

    @Bean  // => Defines second PaymentProcessor bean
    @Qualifier("card")  // => Tags this bean as "card" processor
                         // => Different qualifier from cash
    fun cardProcessor(): PaymentProcessor {
        return CardPayment()  // => Registered with "card" qualifier
                               // => Same type, different qualifier
    }

    @Bean  // => Defines DonationService bean
    fun donationService(
        @Qualifier("cash") processor: PaymentProcessor
        // => Specifies which bean to inject when multiple exist
        // => Injects cashProcessor, not cardProcessor
        // => Without @Qualifier would throw NoUniqueBeanDefinitionException
    ): DonationService {
        return DonationService(processor)  // => Creates service with cash processor
    }
}

class DonationService(private val processor: PaymentProcessor) {  // => Business service
                                                                    // => Constructor injection
    fun acceptDonation(amount: Double) {  // => Business method
        processor.process(amount)  // => Delegates to injected processor
                                    // => Uses cash processor from qualifier
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Spring creates both payment processors and donation service
    // => Injects cash processor into service

    val service = context.getBean(DonationService::class.java)
    // => Retrieves wired DonationService bean
    service.acceptDonation(100.0)  // => Processes donation
    // => Output: Cash payment: $100.0
    // => Uses cashProcessor due to @Qualifier("cash")

    context.close()  // => Cleanup resources
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

interface Notifier {  // => Common notification interface
    void notify(String message);  // => Notification contract method
}

class EmailNotifier implements Notifier {  // => Email notification implementation
    public void notify(String message) {  // => Sends email notification
        System.out.println("Email: " + message);  // => Simulates email delivery
    }
}

class SmsNotifier implements Notifier {  // => SMS notification implementation
    public void notify(String message) {  // => Sends SMS notification
        System.out.println("SMS: " + message);  // => Simulates SMS delivery
    }
}

@Configuration  // => Spring configuration source
class AppConfig {
    @Bean  // => Defines Notifier bean
    @Primary  // => Marks this as default bean for Notifier type
              // => Used when no @Qualifier specified
              // => Resolves ambiguity automatically
    public Notifier emailNotifier() {
        return new EmailNotifier();  // => Primary (default) notifier
                                      // => Injected when type is Notifier and no qualifier
    }

    @Bean  // => Defines alternative Notifier bean
    public Notifier smsNotifier() {
        return new SmsNotifier();  // => Alternative notifier (not primary)
                                    // => Must use @Qualifier to inject this
    }

    @Bean  // => Defines AlertService bean
    public AlertService alertService(Notifier notifier) {
        // => Parameter type is Notifier
        // => @Primary makes emailNotifier inject here automatically
        // => No @Qualifier needed (uses primary bean)
        return new AlertService(notifier);  // => Creates service with email notifier
    }
}

class AlertService {  // => Alert coordination service
    private final Notifier notifier;  // => Injected notifier dependency

    public AlertService(Notifier notifier) {  // => Constructor injection
        this.notifier = notifier;  // => Assigns injected notifier
    }

    public void sendAlert(String message) {  // => Business method
        notifier.notify(message);  // => Delegates to injected notifier
                                    // => Uses EmailNotifier (primary bean)
    }
}

public class Example15 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Spring creates both notifiers and alert service
        // => Injects emailNotifier (primary) into alertService

        AlertService service = context.getBean(AlertService.class);
        // => Retrieves wired AlertService bean
        service.sendAlert("Donation received");  // => Sends alert
        // => Output: Email: Donation received
        // => Uses emailNotifier (marked @Primary)

        context.close();  // => Cleanup resources
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.context.annotation.Primary

interface Notifier {  // => Common notification interface
    fun notify(message: String)  // => Notification contract method
}

class EmailNotifier : Notifier {  // => Email notification implementation
    override fun notify(message: String) {  // => Sends email notification
        println("Email: $message")  // => Simulates email delivery
    }
}

class SmsNotifier : Notifier {  // => SMS notification implementation
    override fun notify(message: String) {  // => Sends SMS notification
        println("SMS: $message")  // => Simulates SMS delivery
    }
}

@Configuration  // => Spring configuration source
class AppConfig {
    @Bean  // => Defines Notifier bean
    @Primary  // => Marks this as default bean for Notifier type
              // => Used when no @Qualifier specified
              // => Resolves ambiguity automatically
    fun emailNotifier(): Notifier {
        return EmailNotifier()  // => Primary (default) notifier
                                 // => Injected when type is Notifier and no qualifier
    }

    @Bean  // => Defines alternative Notifier bean
    fun smsNotifier(): Notifier {
        return SmsNotifier()  // => Alternative notifier (not primary)
                               // => Must use @Qualifier to inject this
    }

    @Bean  // => Defines AlertService bean
    fun alertService(notifier: Notifier): AlertService {
        // => Parameter type is Notifier
        // => @Primary makes emailNotifier inject here automatically
        // => No @Qualifier needed (uses primary bean)
        return AlertService(notifier)  // => Creates service with email notifier
    }
}

class AlertService(private val notifier: Notifier) {  // => Alert coordination service
                                                        // => Constructor injection
    fun sendAlert(message: String) {  // => Business method
        notifier.notify(message)  // => Delegates to injected notifier
                                   // => Uses EmailNotifier (primary bean)
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Spring creates both notifiers and alert service
    // => Injects emailNotifier (primary) into alertService

    val service = context.getBean(AlertService::class.java)
    // => Retrieves wired AlertService bean
    service.sendAlert("Donation received")  // => Sends alert
    // => Output: Email: Donation received
    // => Uses emailNotifier (marked @Primary)

    context.close()  // => Cleanup resources
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

@Component  // => Spring-managed component
class ResourceLoader {  // => Resource loading service
    @Value("classpath:zakat-rates.txt")  // => Injects resource from classpath
    // => Supports classpath:, file:, http: protocols
    // => Spring converts String path to Resource object
    private Resource resource;  // => Resource abstraction for file access

    public void readResource() throws Exception {  // => Reads and displays resource
        BufferedReader reader = new BufferedReader(  // => Creates reader
            new InputStreamReader(resource.getInputStream())  // => Opens stream from Resource
            // => resource.getInputStream() accesses underlying file
        );

        String content = reader.lines().collect(Collectors.joining("\n"));
        // => Streams all lines from file
        // => Joins lines with newline separator

        System.out.println("Resource content:");  // => Prints header
        System.out.println(content);  // => Prints file content

        reader.close();  // => Closes reader and underlying stream
    }
}

@Configuration  // => Spring configuration source
@ComponentScan  // => Discovers @Component beans
class AppConfig {
}

public class Example21 {
    public static void main(String[] args) throws Exception {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Spring initializes ResourceLoader with injected resource

        ResourceLoader loader = context.getBean(ResourceLoader.class);
        // => Retrieves ResourceLoader bean
        loader.readResource();  // => Reads and displays file
        // => Reads and prints zakat-rates.txt content

        context.close();  // => Cleanup resources
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

@Component  // => Spring-managed component
class ResourceLoader {  // => Resource loading service
    @Value("classpath:zakat-rates.txt")  // => Injects resource from classpath
    // => Supports classpath:, file:, http: protocols
    // => Spring converts String path to Resource object
    private lateinit var resource: Resource  // => Resource abstraction for file access

    fun readResource() {  // => Reads and displays resource
        val reader = BufferedReader(InputStreamReader(resource.inputStream))
        // => Creates reader from Resource InputStream
        // => resource.inputStream accesses underlying file

        val content = reader.readLines().joinToString("\n")
        // => Reads all lines from file
        // => Joins lines with newline separator

        println("Resource content:")  // => Prints header
        println(content)  // => Prints file content

        reader.close()  // => Closes reader and underlying stream
    }
}

@Configuration  // => Spring configuration source
@ComponentScan  // => Discovers @Component beans
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Spring initializes ResourceLoader with injected resource

    val loader = context.getBean(ResourceLoader::class.java)
    // => Retrieves ResourceLoader bean
    loader.readResource()  // => Reads and displays file
    // => Reads and prints zakat-rates.txt content

    context.close()  // => Cleanup resources
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

interface PaymentMethod {  // => Common payment interface
    String getName();  // => Returns payment method name
}

class CashPayment implements PaymentMethod {  // => Cash implementation
    public String getName() { return "Cash"; }  // => Returns "Cash"
}

class CardPayment implements PaymentMethod {  // => Card implementation
    public String getName() { return "Card"; }  // => Returns "Card"
}

class BankTransfer implements PaymentMethod {  // => Bank transfer implementation
    public String getName() { return "Bank Transfer"; }  // => Returns "Bank Transfer"
}

@Configuration  // => Spring configuration source
class AppConfig {
    @Bean  // => Defines first PaymentMethod bean
    public PaymentMethod cashPayment() {
        return new CashPayment();  // => Bean 1 of PaymentMethod type
    }

    @Bean  // => Defines second PaymentMethod bean
    public PaymentMethod cardPayment() {
        return new CardPayment();  // => Bean 2 of PaymentMethod type
    }

    @Bean  // => Defines third PaymentMethod bean
    public PaymentMethod bankTransfer() {
        return new BankTransfer();  // => Bean 3 of PaymentMethod type
    }

    @Bean  // => Defines PaymentRegistry bean
    public PaymentRegistry registry(List<PaymentMethod> methods) {
        // => Spring automatically collects ALL PaymentMethod beans
        // => Injects as List<PaymentMethod> with all 3 beans
        // => Order matches registration order
        return new PaymentRegistry(methods);  // => Creates registry with all methods
    }
}

class PaymentRegistry {  // => Registry for managing payment methods
    private final List<PaymentMethod> methods;  // => All PaymentMethod beans

    public PaymentRegistry(List<PaymentMethod> methods) {  // => Constructor injection
        this.methods = methods;  // => Stores all PaymentMethod beans
                                  // => methods.size() is 3
    }

    public void listMethods() {  // => Displays all available methods
        System.out.println("Available payment methods:");  // => Header
        for (PaymentMethod method : methods) {  // => Iterates all methods
            System.out.println("- " + method.getName());  // => Prints each name
        }
    }
}

public class Example22 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Spring creates all 3 PaymentMethod beans
        // => Collects them into List, injects into registry

        PaymentRegistry registry = context.getBean(PaymentRegistry.class);
        // => Retrieves registry with all 3 payment methods
        registry.listMethods();  // => Lists all methods
        // => Output: Available payment methods:
        // => Output: - Cash
        // => Output: - Card
        // => Output: - Bank Transfer

        context.close();  // => Cleanup resources
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

interface PaymentMethod {  // => Common payment interface
    fun getName(): String  // => Returns payment method name
}

class CashPayment : PaymentMethod {  // => Cash implementation
    override fun getName(): String = "Cash"  // => Returns "Cash"
}

class CardPayment : PaymentMethod {  // => Card implementation
    override fun getName(): String = "Card"  // => Returns "Card"
}

class BankTransfer : PaymentMethod {  // => Bank transfer implementation
    override fun getName(): String = "Bank Transfer"  // => Returns "Bank Transfer"
}

@Configuration  // => Spring configuration source
class AppConfig {
    @Bean  // => Defines first PaymentMethod bean
    fun cashPayment(): PaymentMethod = CashPayment()  // => Bean 1 of PaymentMethod type

    @Bean  // => Defines second PaymentMethod bean
    fun cardPayment(): PaymentMethod = CardPayment()  // => Bean 2 of PaymentMethod type

    @Bean  // => Defines third PaymentMethod bean
    fun bankTransfer(): PaymentMethod = BankTransfer()  // => Bean 3 of PaymentMethod type

    @Bean  // => Defines PaymentRegistry bean
    fun registry(methods: List<PaymentMethod>): PaymentRegistry {
        // => Spring automatically collects ALL PaymentMethod beans
        // => Injects as List<PaymentMethod> with all 3 beans
        // => Order matches registration order
        return PaymentRegistry(methods)  // => Creates registry with all methods
    }
}

class PaymentRegistry(private val methods: List<PaymentMethod>) {  // => Registry for payment methods
    // => Constructor receives all PaymentMethod beans
    // => methods.size is 3

    fun listMethods() {  // => Displays all available methods
        println("Available payment methods:")  // => Header
        methods.forEach { println("- ${it.getName()}") }  // => Prints each name
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Spring creates all 3 PaymentMethod beans
    // => Collects them into List, injects into registry

    val registry = context.getBean(PaymentRegistry::class.java)
    // => Retrieves registry with all 3 payment methods
    registry.listMethods()  // => Lists all methods
    // => Output: Available payment methods:
    // => Output: - Cash
    // => Output: - Card
    // => Output: - Bank Transfer

    context.close()  // => Cleanup resources
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

class LinuxCondition implements Condition {  // => Custom condition implementation
    // => Checks if operating system is Linux
    @Override
    public boolean matches(ConditionContext context, AnnotatedTypeMetadata metadata) {
        // => Spring calls this at bean registration time
        String os = System.getProperty("os.name").toLowerCase();  // => Gets OS name
                                                                   // => Converts to lowercase
        return os.contains("linux");  // => Returns true if Linux detected
                                       // => Bean registered only if returns true
    }
}

class FileService {  // => File service with OS-specific path
    private final String basePath;  // => Base path for file operations

    public FileService(String basePath) {  // => Constructor sets path
        this.basePath = basePath;  // => Stores base path
    }

    public void printPath() {  // => Displays configured path
        System.out.println("Base path: " + basePath);  // => Prints path
    }
}

@Configuration  // => Spring configuration source
class AppConfig {
    @Bean  // => Defines FileService bean for Linux
    @Conditional(LinuxCondition.class)  // => Conditional registration
    // => Bean registered only if LinuxCondition.matches() returns true
    // => Spring evaluates condition before creating bean
    public FileService linuxFileService() {
        return new FileService("/var/data");  // => Linux path convention
                                                // => Used on Linux systems
    }

    @Bean  // => Defines FileService bean for Windows
    @Conditional(WindowsCondition.class)  // => Conditional registration
    // => Bean registered only if WindowsCondition matches
    public FileService windowsFileService() {
        return new FileService("C:\\Data");  // => Windows path convention
                                              // => Used on Windows systems
    }
}

class WindowsCondition implements Condition {  // => Custom condition for Windows
    @Override
    public boolean matches(ConditionContext context, AnnotatedTypeMetadata metadata) {
        // => Checks if OS is Windows
        String os = System.getProperty("os.name").toLowerCase();  // => Gets OS name
        return os.contains("windows");  // => Returns true if Windows
    }
}

public class Example23 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Spring evaluates conditions at startup
        // => Registers only matching bean (Linux OR Windows)

        FileService service = context.getBean(FileService.class);
        // => Retrieves the one registered FileService bean
        service.printPath();  // => Displays OS-appropriate path
        // => Output on Linux: Base path: /var/data
        // => Output on Windows: Base path: C:\Data
        // => Only one bean exists in container

        context.close();  // => Cleanup resources
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

class LinuxCondition : Condition {  // => Custom condition implementation
    // => Checks if operating system is Linux
    override fun matches(context: ConditionContext, metadata: AnnotatedTypeMetadata): Boolean {
        // => Spring calls this at bean registration time
        val os = System.getProperty("os.name").lowercase()  // => Gets OS name
                                                             // => Converts to lowercase
        return os.contains("linux")  // => Returns true if Linux detected
                                      // => Bean registered only if returns true
    }
}

class WindowsCondition : Condition {  // => Custom condition for Windows
    override fun matches(context: ConditionContext, metadata: AnnotatedTypeMetadata): Boolean {
        // => Checks if OS is Windows
        val os = System.getProperty("os.name").lowercase()  // => Gets OS name
        return os.contains("windows")  // => Returns true if Windows
    }
}

class FileService(private val basePath: String) {  // => File service with OS-specific path
    fun printPath() {  // => Displays configured path
        println("Base path: $basePath")  // => Prints path
    }
}

@Configuration  // => Spring configuration source
class AppConfig {
    @Bean  // => Defines FileService bean for Linux
    @Conditional(LinuxCondition::class)  // => Conditional registration
    // => Bean registered only if LinuxCondition.matches() returns true
    // => Spring evaluates condition before creating bean
    fun linuxFileService(): FileService {
        return FileService("/var/data")  // => Linux path convention
                                          // => Used on Linux systems
    }

    @Bean  // => Defines FileService bean for Windows
    @Conditional(WindowsCondition::class)  // => Conditional registration
    // => Bean registered only if WindowsCondition matches
    fun windowsFileService(): FileService {
        return FileService("C:\\Data")  // => Windows path convention
                                         // => Used on Windows systems
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Spring evaluates conditions at startup
    // => Registers only matching bean (Linux OR Windows)

    val service = context.getBean(FileService::class.java)
    // => Retrieves the one registered FileService bean
    service.printPath()  // => Displays OS-appropriate path
    // => Output on Linux: Base path: /var/data
    // => Output on Windows: Base path: C:\Data
    // => Only one bean exists in container

    context.close()  // => Cleanup resources
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

## Advanced Bean Configuration (Examples 26-30)

### Example 26: @Import for Modular Configuration (Coverage: 60.0%)

Demonstrates splitting configuration across multiple classes and importing them.

**Java Implementation**:

```java
import org.springframework.context.annotation.*;

class EmailService {  // => Service for sending emails
    public void send(String msg) {
        System.out.println("Email: " + msg);  // => Sends email message
    }
}

class SmsService {  // => Service for sending SMS
    public void send(String msg) {
        System.out.println("SMS: " + msg);  // => Sends SMS message
    }
}

@Configuration
// => Dedicated config class for messaging services
class MessagingConfig {
    @Bean
    public EmailService emailService() {
        return new EmailService();  // => Creates EmailService bean
    }

    @Bean
    public SmsService smsService() {
        return new SmsService();  // => Creates SmsService bean
    }
}

@Configuration
@Import(MessagingConfig.class)
// => Imports MessagingConfig beans into this config
// => Enables modular configuration organization
class AppConfig {
    // => Main config can now use beans from MessagingConfig
}

public class Example26 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);
        // => Context contains beans from both AppConfig and MessagingConfig

        EmailService email = context.getBean(EmailService.class);
        // => Retrieved bean defined in imported config
        email.send("Test");  // => Output: Email: Test

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.*

class EmailService {  // => Service for sending emails
    fun send(msg: String) {
        println("Email: $msg")  // => Sends email message
    }
}

class SmsService {  // => Service for sending SMS
    fun send(msg: String) {
        println("SMS: $msg")  // => Sends SMS message
    }
}

@Configuration
// => Dedicated config class for messaging services
class MessagingConfig {
    @Bean
    fun emailService(): EmailService {
        return EmailService()  // => Creates EmailService bean
    }

    @Bean
    fun smsService(): SmsService {
        return SmsService()  // => Creates SmsService bean
    }
}

@Configuration
@Import(MessagingConfig::class)
// => Imports MessagingConfig beans into this config
// => Enables modular configuration organization
class AppConfig {
    // => Main config can now use beans from MessagingConfig
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Context contains beans from both AppConfig and MessagingConfig

    val email = context.getBean(EmailService::class.java)
    // => Retrieved bean defined in imported config
    email.send("Test")  // => Output: Email: Test

    context.close()
}
```

**Expected Output**:

```
Email: Test
```

**Key Takeaways**:

- `@Import` enables modular configuration organization
- Import multiple configs with `@Import({Config1.class, Config2.class})`
- Imported beans available in importing context
- Promotes separation of concerns in configuration

**Related Documentation**:

- [Import Documentation](https://docs.spring.io/spring-framework/reference/core/beans/java/composing-configuration-classes.html)

---

### Example 27: @ComponentScan with Custom Base Packages (Coverage: 62.0%)

Demonstrates scanning specific packages for components instead of default package.

**Java Implementation**:

```java
package com.example.services;

import org.springframework.stereotype.Component;

@Component  // => Marks as auto-detected component
public class PaymentService {
    public void process() {
        System.out.println("Payment processed");
    }
}
```

```java
package com.example.config;

import org.springframework.context.annotation.*;

@Configuration
@ComponentScan(basePackages = "com.example.services")
// => Scans com.example.services package for @Component classes
// => Finds and registers PaymentService automatically
class AppConfig {
}

public class Example27 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);
        // => Context created with component scanning enabled

        PaymentService service = context.getBean(PaymentService.class);
        // => Retrieved auto-detected component
        service.process();  // => Output: Payment processed

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
package com.example.services

import org.springframework.stereotype.Component

@Component  // => Marks as auto-detected component
class PaymentService {
    fun process() {
        println("Payment processed")
    }
}
```

```kotlin
package com.example.config

import org.springframework.context.annotation.*

@Configuration
@ComponentScan(basePackages = ["com.example.services"])
// => Scans com.example.services package for @Component classes
// => Finds and registers PaymentService automatically
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Context created with component scanning enabled

    val service = context.getBean(PaymentService::class.java)
    // => Retrieved auto-detected component
    service.process()  // => Output: Payment processed

    context.close()
}
```

**Expected Output**:

```
Payment processed
```

**Key Takeaways**:

- `@ComponentScan` configures which packages to scan
- Can specify multiple base packages with array syntax
- Use `basePackageClasses` for type-safe package specification
- More efficient than scanning entire classpath

**Related Documentation**:

- [ComponentScan Documentation](https://docs.spring.io/spring-framework/reference/core/beans/classpath-scanning.html)

---

### Example 28: @PropertySource for External Configuration (Coverage: 64.0%)

Demonstrates loading external properties files into Spring environment.

**Java Implementation**:

```java
import org.springframework.context.annotation.*;
import org.springframework.beans.factory.annotation.Value;

// File: app.properties
// app.name=Spring Demo
// app.version=1.0.0

@Configuration
@PropertySource("classpath:app.properties")
// => Loads app.properties into Spring environment
// => Makes properties available for @Value injection
class AppConfig {
    @Value("${app.name}")  // => Injects value from app.properties
    private String appName;

    @Value("${app.version}")  // => Injects version property
    private String appVersion;

    @Bean
    public String applicationInfo() {
        return appName + " v" + appVersion;
        // => Returns: "Spring Demo v1.0.0"
    }
}

public class Example28 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);
        // => Properties loaded from app.properties

        String info = context.getBean("applicationInfo", String.class);
        System.out.println(info);  // => Output: Spring Demo v1.0.0

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.*
import org.springframework.beans.factory.annotation.Value

// File: app.properties
// app.name=Spring Demo
// app.version=1.0.0

@Configuration
@PropertySource("classpath:app.properties")
// => Loads app.properties into Spring environment
// => Makes properties available for @Value injection
class AppConfig {
    @Value("\${app.name}")  // => Injects value from app.properties
    private lateinit var appName: String

    @Value("\${app.version}")  // => Injects version property
    private lateinit var appVersion: String

    @Bean
    fun applicationInfo(): String {
        return "$appName v$appVersion"
        // => Returns: "Spring Demo v1.0.0"
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Properties loaded from app.properties

    val info = context.getBean("applicationInfo", String::class.java)
    println(info)  // => Output: Spring Demo v1.0.0

    context.close()
}
```

**Expected Output**:

```
Spring Demo v1.0.0
```

**Key Takeaways**:

- `@PropertySource` loads external property files
- Supports multiple files with array syntax
- Properties accessible via `@Value` and Environment
- Use `ignoreResourceNotFound=true` for optional files

**Related Documentation**:

- [PropertySource Documentation](https://docs.spring.io/spring-framework/reference/core/beans/environment.html#beans-property-source-abstraction)

---

### Example 29: @Bean with initMethod and destroyMethod (Coverage: 66.0%)

Demonstrates bean lifecycle callbacks via method references instead of annotations.

**Java Implementation**:

```java
import org.springframework.context.annotation.*;

class DatabaseConnection {
    public void connect() {  // => Custom initialization method
        System.out.println("Database connected");
    }

    public void disconnect() {  // => Custom destruction method
        System.out.println("Database disconnected");
    }

    public void query() {
        System.out.println("Executing query");
    }
}

@Configuration
class AppConfig {
    @Bean(initMethod = "connect", destroyMethod = "disconnect")
    // => Calls connect() after bean creation
    // => Calls disconnect() before bean destruction
    public DatabaseConnection database() {
        return new DatabaseConnection();
        // => Bean created but not yet initialized
    }
}

public class Example29 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);
        // => Output: Database connected (initMethod called)

        DatabaseConnection db = context.getBean(DatabaseConnection.class);
        db.query();  // => Output: Executing query

        context.close();
        // => Output: Database disconnected (destroyMethod called)
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.*

class DatabaseConnection {
    fun connect() {  // => Custom initialization method
        println("Database connected")
    }

    fun disconnect() {  // => Custom destruction method
        println("Database disconnected")
    }

    fun query() {
        println("Executing query")
    }
}

@Configuration
class AppConfig {
    @Bean(initMethod = "connect", destroyMethod = "disconnect")
    // => Calls connect() after bean creation
    // => Calls disconnect() before bean destruction
    fun database(): DatabaseConnection {
        return DatabaseConnection()
        // => Bean created but not yet initialized
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Output: Database connected (initMethod called)

    val db = context.getBean(DatabaseConnection::class.java)
    db.query()  // => Output: Executing query

    context.close()
    // => Output: Database disconnected (destroyMethod called)
}
```

**Expected Output**:

```
Database connected
Executing query
Database disconnected
```

**Key Takeaways**:

- `initMethod` and `destroyMethod` provide lifecycle callbacks
- Alternative to `@PostConstruct` and `@PreDestroy`
- Useful when you can't modify bean class (third-party code)
- Methods called automatically by Spring container

**Related Documentation**:

- [Bean Lifecycle Documentation](https://docs.spring.io/spring-framework/reference/core/beans/factory-nature.html#beans-factory-lifecycle)

---

### Example 30: @Scope with Request Scope (Web Context) (Coverage: 68.0%)

Demonstrates request-scoped beans in web applications (new instance per HTTP request).

**Java Implementation**:

```java
import org.springframework.context.annotation.*;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.annotation.RequestScope;

@RequestScope  // => New instance created for each HTTP request
// => Equivalent to @Scope("request")
class ShoppingCart {
    private int itemCount = 0;

    public void addItem() {
        itemCount++;  // => Increments count for THIS request only
        System.out.println("Cart items: " + itemCount);
    }

    public int getItemCount() {
        return itemCount;
    }
}

@Configuration
class WebConfig {
    @Bean
    public ShoppingCart cart() {
        return new ShoppingCart();
        // => New instance per HTTP request
    }
}

// In actual web controller:
public class Example30 {
    public void handleRequest(WebApplicationContext context) {
        ShoppingCart cart = context.getBean(ShoppingCart.class);
        // => Gets request-scoped instance

        cart.addItem();  // => Output: Cart items: 1
        cart.addItem();  // => Output: Cart items: 2

        // Next HTTP request gets NEW cart instance starting at 0
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.*
import org.springframework.web.context.WebApplicationContext
import org.springframework.web.context.annotation.RequestScope

@RequestScope  // => New instance created for each HTTP request
// => Equivalent to @Scope("request")
class ShoppingCart {
    private var itemCount = 0

    fun addItem() {
        itemCount++  // => Increments count for THIS request only
        println("Cart items: $itemCount")
    }

    fun getItemCount(): Int = itemCount
}

@Configuration
class WebConfig {
    @Bean
    fun cart(): ShoppingCart {
        return ShoppingCart()
        // => New instance per HTTP request
    }
}

// In actual web controller:
class Example30 {
    fun handleRequest(context: WebApplicationContext) {
        val cart = context.getBean(ShoppingCart::class.java)
        // => Gets request-scoped instance

        cart.addItem()  // => Output: Cart items: 1
        cart.addItem()  // => Output: Cart items: 2

        // Next HTTP request gets NEW cart instance starting at 0
    }
}
```

**Expected Output** (per request):

```
Cart items: 1
Cart items: 2
```

**Key Takeaways**:

- `@RequestScope` creates new bean per HTTP request
- State not shared between requests
- Only works in web-aware ApplicationContext
- Other web scopes: session, application, websocket

**Related Documentation**:

- [Web Scopes Documentation](https://docs.spring.io/spring-framework/reference/core/beans/factory-scopes.html#beans-factory-scopes-other)

---

## Component Stereotypes (Examples 31-35)

### Example 31: @Service Stereotype Annotation (Coverage: 70.0%)

Demonstrates `@Service` as semantic specialization of `@Component` for service layer.

**Java Implementation**:

```java
import org.springframework.context.annotation.*;
import org.springframework.stereotype.Service;

@Service  // => Marks as service layer component
// => Semantically same as @Component but clearer intent
class UserService {
    public String findUser(int id) {
        return "User-" + id;  // => Simulated user retrieval
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
// => Scans for all stereotype annotations
class AppConfig {
}

public class Example31 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);
        // => UserService auto-detected via @Service

        UserService service = context.getBean(UserService.class);
        String user = service.findUser(42);
        System.out.println(user);  // => Output: User-42

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.*
import org.springframework.stereotype.Service

@Service  // => Marks as service layer component
// => Semantically same as @Component but clearer intent
class UserService {
    fun findUser(id: Int): String {
        return "User-$id"  // => Simulated user retrieval
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
// => Scans for all stereotype annotations
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => UserService auto-detected via @Service

    val service = context.getBean(UserService::class.java)
    val user = service.findUser(42)
    println(user)  // => Output: User-42

    context.close()
}
```

**Expected Output**:

```
User-42
```

**Key Takeaways**:

- `@Service` marks business logic layer components
- Functionally equivalent to `@Component` but improves code readability
- Enables future tooling/AOP enhancements specific to services
- Part of Spring's stereotype annotation family

**Related Documentation**:

- [Stereotype Annotations](https://docs.spring.io/spring-framework/reference/core/beans/classpath-scanning.html#beans-stereotype-annotations)

---

### Example 32: @Repository Stereotype with Exception Translation (Coverage: 72.0%)

Demonstrates `@Repository` for data access layer with automatic exception translation.

**Java Implementation**:

```java
import org.springframework.context.annotation.*;
import org.springframework.stereotype.Repository;
import java.util.*;

@Repository  // => Marks as data access component
// => Enables automatic exception translation (JDBC→Spring)
class UserRepository {
    private Map<Integer, String> database = new HashMap<>();

    public UserRepository() {
        database.put(1, "Alice");  // => Mock data
        database.put(2, "Bob");
    }

    public String findById(int id) {
        String user = database.get(id);
        if (user == null) {
            // => In real DAO, SQLException would be translated
            // => to DataAccessException by Spring
            throw new RuntimeException("User not found: " + id);
        }
        return user;
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example32 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);
        // => UserRepository auto-detected via @Repository

        UserRepository repo = context.getBean(UserRepository.class);
        String user = repo.findById(1);
        System.out.println("Found: " + user);  // => Output: Found: Alice

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.*
import org.springframework.stereotype.Repository

@Repository  // => Marks as data access component
// => Enables automatic exception translation (JDBC→Spring)
class UserRepository {
    private val database = mutableMapOf<Int, String>()

    init {
        database[1] = "Alice"  // => Mock data
        database[2] = "Bob"
    }

    fun findById(id: Int): String {
        val user = database[id]
        if (user == null) {
            // => In real DAO, SQLException would be translated
            // => to DataAccessException by Spring
            throw RuntimeException("User not found: $id")
        }
        return user
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => UserRepository auto-detected via @Repository

    val repo = context.getBean(UserRepository::class.java)
    val user = repo.findById(1)
    println("Found: $user")  // => Output: Found: Alice

    context.close()
}
```

**Expected Output**:

```
Found: Alice
```

**Key Takeaways**:

- `@Repository` marks data access layer components
- Automatically translates persistence exceptions to Spring's DataAccessException
- Works with JDBC, JPA, Hibernate
- Improves exception handling consistency

**Related Documentation**:

- [Repository Documentation](https://docs.spring.io/spring-framework/reference/data-access/dao.html#dao-annotations)

---

### Example 33: @Controller Stereotype for Web Layer (Coverage: 74.0%)

Demonstrates `@Controller` stereotype for Spring MVC web controllers.

**Java Implementation**:

```java
import org.springframework.context.annotation.*;
import org.springframework.stereotype.Controller;

@Controller  // => Marks as MVC controller component
// => Enables request mapping and view resolution
class HomeController {
    public String home() {
        System.out.println("Handling home request");
        return "home-view";  // => Returns view name for resolver
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example33 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);
        // => HomeController auto-detected via @Controller

        HomeController controller = context.getBean(HomeController.class);
        String view = controller.home();
        // => Output: Handling home request
        System.out.println("View: " + view);  // => Output: View: home-view

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.*
import org.springframework.stereotype.Controller

@Controller  // => Marks as MVC controller component
// => Enables request mapping and view resolution
class HomeController {
    fun home(): String {
        println("Handling home request")
        return "home-view"  // => Returns view name for resolver
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => HomeController auto-detected via @Controller

    val controller = context.getBean(HomeController::class.java)
    val view = controller.home()
    // => Output: Handling home request
    println("View: $view")  // => Output: View: home-view

    context.close()
}
```

**Expected Output**:

```
Handling home request
View: home-view
```

**Key Takeaways**:

- `@Controller` marks Spring MVC controller components
- Used with `@RequestMapping` for HTTP request handling
- Returns view names for view resolution
- For REST APIs, use `@RestController` instead

**Related Documentation**:

- [Controller Documentation](https://docs.spring.io/spring-framework/reference/web/webmvc/mvc-controller.html)

---

### Example 34: @Configuration as Meta-Annotation (Coverage: 76.0%)

Demonstrates `@Configuration` is also a `@Component` and gets auto-detected.

**Java Implementation**:

```java
import org.springframework.context.annotation.*;

@Configuration  // => Is itself a @Component
// => Will be auto-detected by component scanning
class DatabaseConfig {
    @Bean
    public String connectionString() {
        return "jdbc:mysql://localhost:3306/mydb";
        // => Connection string bean
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
// => Scans and finds DatabaseConfig automatically
class AppConfig {
}

public class Example34 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);
        // => DatabaseConfig auto-detected and processed

        String connStr = context.getBean("connectionString", String.class);
        System.out.println(connStr);
        // => Output: jdbc:mysql://localhost:3306/mydb

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.*

@Configuration  // => Is itself a @Component
// => Will be auto-detected by component scanning
class DatabaseConfig {
    @Bean
    fun connectionString(): String {
        return "jdbc:mysql://localhost:3306/mydb"
        // => Connection string bean
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
// => Scans and finds DatabaseConfig automatically
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => DatabaseConfig auto-detected and processed

    val connStr = context.getBean("connectionString", String::class.java)
    println(connStr)
    // => Output: jdbc:mysql://localhost:3306/mydb

    context.close()
}
```

**Expected Output**:

```
jdbc:mysql://localhost:3306/mydb
```

**Key Takeaways**:

- `@Configuration` is meta-annotated with `@Component`
- Configuration classes auto-detected by component scanning
- Enables modular configuration without explicit `@Import`
- Supports hierarchical configuration organization

**Related Documentation**:

- [Configuration Documentation](https://docs.spring.io/spring-framework/reference/core/beans/java/basic-concepts.html)

---

### Example 35: Custom Stereotype Annotation (Coverage: 78.0%)

Demonstrates creating custom stereotype annotations by composing Spring annotations.

**Java Implementation**:

```java
import org.springframework.context.annotation.*;
import org.springframework.stereotype.Component;
import java.lang.annotation.*;

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Component  // => Meta-annotation makes this a component stereotype
// => Custom annotation inherits @Component behavior
@interface DataService {
    String value() default "";  // => Optional bean name
}

@DataService  // => Uses custom stereotype
class OrderDataService {
    public String getOrders() {
        return "Orders: [1, 2, 3]";
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example35 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);
        // => OrderDataService auto-detected via custom @DataService

        OrderDataService service = context.getBean(OrderDataService.class);
        System.out.println(service.getOrders());
        // => Output: Orders: [1, 2, 3]

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.*
import org.springframework.stereotype.Component

@Target(AnnotationTarget.CLASS)
@Retention(AnnotationRetention.RUNTIME)
@Component  // => Meta-annotation makes this a component stereotype
// => Custom annotation inherits @Component behavior
annotation class DataService(
    val value: String = ""  // => Optional bean name
)

@DataService  // => Uses custom stereotype
class OrderDataService {
    fun getOrders(): String {
        return "Orders: [1, 2, 3]"
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => OrderDataService auto-detected via custom @DataService

    val service = context.getBean(OrderDataService::class.java)
    println(service.getOrders())
    // => Output: Orders: [1, 2, 3]

    context.close()
}
```

**Expected Output**:

```
Orders: [1, 2, 3]
```

**Key Takeaways**:

- Create custom stereotypes by meta-annotating with `@Component`
- Inherits component scanning behavior
- Improves code semantics and domain clarity
- Can combine multiple Spring annotations

**Related Documentation**:

- [Meta-Annotations](https://docs.spring.io/spring-framework/reference/core/beans/classpath-scanning.html#beans-meta-annotations)

---

## Advanced Bean Features (Examples 36-40)

### Example 36: FactoryBean for Complex Bean Creation (Coverage: 80.0%)

Demonstrates using FactoryBean to customize bean instantiation logic.

**Java Implementation**:

```java
import org.springframework.beans.factory.FactoryBean;
import org.springframework.context.annotation.*;

class ConnectionPool {  // => Complex object to create
    private int maxConnections;

    public ConnectionPool(int maxConnections) {
        this.maxConnections = maxConnections;
        System.out.println("Pool created with " + maxConnections + " max connections");
    }
}

class ConnectionPoolFactory implements FactoryBean<ConnectionPool> {
    // => Custom factory for creating ConnectionPool beans
    private int maxConnections = 10;

    @Override
    public ConnectionPool getObject() throws Exception {
        // => Called when bean requested from context
        return new ConnectionPool(maxConnections);
    }

    @Override
    public Class<?> getObjectType() {
        return ConnectionPool.class;  // => Type of created beans
    }

    @Override
    public boolean isSingleton() {
        return true;  // => Return same instance each time
    }
}

@Configuration
class AppConfig {
    @Bean
    public ConnectionPoolFactory connectionPool() {
        return new ConnectionPoolFactory();
        // => Registers factory, not actual ConnectionPool
    }
}

public class Example36 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);

        // Getting bean by type returns ConnectionPool, not Factory
        ConnectionPool pool = context.getBean(ConnectionPool.class);
        // => Output: Pool created with 10 max connections

        // To get factory itself, prefix with &
        ConnectionPoolFactory factory =
            context.getBean("&connectionPool", ConnectionPoolFactory.class);
        // => Returns the FactoryBean instance

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.FactoryBean
import org.springframework.context.annotation.*

class ConnectionPool(private val maxConnections: Int) {
    // => Complex object to create
    init {
        println("Pool created with $maxConnections max connections")
    }
}

class ConnectionPoolFactory : FactoryBean<ConnectionPool> {
    // => Custom factory for creating ConnectionPool beans
    private val maxConnections = 10

    override fun getObject(): ConnectionPool {
        // => Called when bean requested from context
        return ConnectionPool(maxConnections)
    }

    override fun getObjectType(): Class<*> {
        return ConnectionPool::class.java  // => Type of created beans
    }

    override fun isSingleton(): Boolean {
        return true  // => Return same instance each time
    }
}

@Configuration
class AppConfig {
    @Bean
    fun connectionPool(): ConnectionPoolFactory {
        return ConnectionPoolFactory()
        // => Registers factory, not actual ConnectionPool
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    // Getting bean by type returns ConnectionPool, not Factory
    val pool = context.getBean(ConnectionPool::class.java)
    // => Output: Pool created with 10 max connections

    // To get factory itself, prefix with &
    val factory = context.getBean("&connectionPool", ConnectionPoolFactory::class.java)
    // => Returns the FactoryBean instance

    context.close()
}
```

**Expected Output**:

```
Pool created with 10 max connections
```

**Key Takeaways**:

- `FactoryBean` provides custom bean creation logic
- Container calls `getObject()` when bean requested
- Use `&beanName` to get FactoryBean itself
- Useful for complex initialization or proxying

**Related Documentation**:

- [FactoryBean Documentation](https://docs.spring.io/spring-framework/reference/core/beans/factory-extension.html#beans-factory-extension-factorybean)

---

### Example 37: BeanPostProcessor for Bean Customization (Coverage: 82.0%)

Demonstrates modifying beans before/after initialization using BeanPostProcessor.

**Java Implementation**:

```java
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.*;
import org.springframework.stereotype.Component;

class EmailService {
    private String prefix = "";

    public void setPrefix(String prefix) {
        this.prefix = prefix;
    }

    public void send(String msg) {
        System.out.println(prefix + msg);
    }
}

@Component
class PrefixBeanPostProcessor implements BeanPostProcessor {
    // => Processes ALL beans in container

    @Override
    public Object postProcessBeforeInitialization(Object bean, String beanName) {
        // => Called BEFORE @PostConstruct
        if (bean instanceof EmailService) {
            ((EmailService) bean).setPrefix("[PROCESSED] ");
            // => Customizes EmailService before initialization
        }
        return bean;  // => Must return bean (or wrapped proxy)
    }

    @Override
    public Object postProcessAfterInitialization(Object bean, String beanName) {
        // => Called AFTER @PostConstruct
        return bean;
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
class AppConfig {
    @Bean
    public EmailService emailService() {
        return new EmailService();
    }
}

public class Example37 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);
        // => BeanPostProcessor applied to all beans

        EmailService service = context.getBean(EmailService.class);
        service.send("Hello");  // => Output: [PROCESSED] Hello

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.config.BeanPostProcessor
import org.springframework.context.annotation.*
import org.springframework.stereotype.Component

class EmailService {
    var prefix = ""

    fun send(msg: String) {
        println("$prefix$msg")
    }
}

@Component
class PrefixBeanPostProcessor : BeanPostProcessor {
    // => Processes ALL beans in container

    override fun postProcessBeforeInitialization(bean: Any, beanName: String): Any {
        // => Called BEFORE @PostConstruct
        if (bean is EmailService) {
            bean.prefix = "[PROCESSED] "
            // => Customizes EmailService before initialization
        }
        return bean  // => Must return bean (or wrapped proxy)
    }

    override fun postProcessAfterInitialization(bean: Any, beanName: String): Any {
        // => Called AFTER @PostConstruct
        return bean
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
class AppConfig {
    @Bean
    fun emailService(): EmailService {
        return EmailService()
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => BeanPostProcessor applied to all beans

    val service = context.getBean(EmailService::class.java)
    service.send("Hello")  // => Output: [PROCESSED] Hello

    context.close()
}
```

**Expected Output**:

```
[PROCESSED] Hello
```

**Key Takeaways**:

- `BeanPostProcessor` customizes beans during initialization
- `postProcessBeforeInitialization`: before @PostConstruct
- `postProcessAfterInitialization`: after @PostConstruct
- Used by Spring for AOP, transaction proxies, etc.

**Related Documentation**:

- [BeanPostProcessor Documentation](https://docs.spring.io/spring-framework/reference/core/beans/factory-extension.html#beans-factory-extension-bpp)

---

### Example 38: @Lookup Method Injection for Prototype Beans (Coverage: 84.0%)

Demonstrates injecting prototype-scoped beans into singleton beans using method injection.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Lookup;
import org.springframework.context.annotation.*;

@Scope("prototype")  // => New instance each time
class Task {
    private static int counter = 0;
    private int id;

    public Task() {
        this.id = ++counter;  // => Each instance gets unique ID
        System.out.println("Task-" + id + " created");
    }

    public int getId() {
        return id;
    }
}

@Component
class TaskProcessor {
    // => Singleton bean needing prototype dependencies

    @Lookup  // => Spring overrides this method at runtime
    // => Returns new Task instance each call
    protected Task createTask() {
        return null;  // => Implementation provided by Spring
    }

    public void process() {
        Task task = createTask();  // => Gets new prototype instance
        System.out.println("Processing task: " + task.getId());
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example38 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);

        TaskProcessor processor = context.getBean(TaskProcessor.class);
        // => Same singleton processor

        processor.process();  // => Output: Task-1 created, Processing task: 1
        processor.process();  // => Output: Task-2 created, Processing task: 2
        processor.process();  // => Output: Task-3 created, Processing task: 3

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Lookup
import org.springframework.context.annotation.*

@Scope("prototype")  // => New instance each time
class Task {
    companion object {
        private var counter = 0
    }

    private val id: Int

    init {
        this.id = ++counter  // => Each instance gets unique ID
        println("Task-$id created")
    }

    fun getId(): Int = id
}

@Component
abstract class TaskProcessor {
    // => Singleton bean needing prototype dependencies

    @Lookup  // => Spring overrides this method at runtime
    // => Returns new Task instance each call
    protected abstract fun createTask(): Task

    fun process() {
        val task = createTask()  // => Gets new prototype instance
        println("Processing task: ${task.getId()}")
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val processor = context.getBean(TaskProcessor::class.java)
    // => Same singleton processor

    processor.process()  // => Output: Task-1 created, Processing task: 1
    processor.process()  // => Output: Task-2 created, Processing task: 2
    processor.process()  // => Output: Task-3 created, Processing task: 3

    context.close()
}
```

**Expected Output**:

```
Task-1 created
Processing task: 1
Task-2 created
Processing task: 2
Task-3 created
Processing task: 3
```

**Key Takeaways**:

- `@Lookup` enables method injection for prototype beans
- Solves singleton-prototype dependency problem
- Spring generates subclass implementing abstract method
- Each call returns fresh prototype instance

**Related Documentation**:

- [Lookup Method Injection](https://docs.spring.io/spring-framework/reference/core/beans/dependencies/factory-method-injection.html)

---

### Example 39: ApplicationContextAware for Context Access (Coverage: 86.0%)

Demonstrates accessing ApplicationContext within beans using aware interfaces.

**Java Implementation**:

```java
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.annotation.*;
import org.springframework.stereotype.Component;

@Component
class BeanLocator implements ApplicationContextAware {
    // => Implements aware interface for context injection
    private ApplicationContext context;

    @Override
    public void setApplicationContext(ApplicationContext context) {
        // => Called by Spring with context reference
        this.context = context;
        System.out.println("Context injected into BeanLocator");
    }

    public <T> T getBean(Class<T> beanClass) {
        // => Dynamic bean lookup at runtime
        return context.getBean(beanClass);
    }

    public int getBeanCount() {
        return context.getBeanDefinitionCount();
        // => Returns total bean count in context
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example39 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);
        // => Output: Context injected into BeanLocator

        BeanLocator locator = context.getBean(BeanLocator.class);
        System.out.println("Bean count: " + locator.getBeanCount());
        // => Output: Bean count: [number]

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.ApplicationContext
import org.springframework.context.ApplicationContextAware
import org.springframework.context.annotation.*
import org.springframework.stereotype.Component

@Component
class BeanLocator : ApplicationContextAware {
    // => Implements aware interface for context injection
    private lateinit var context: ApplicationContext

    override fun setApplicationContext(context: ApplicationContext) {
        // => Called by Spring with context reference
        this.context = context
        println("Context injected into BeanLocator")
    }

    fun <T> getBean(beanClass: Class<T>): T {
        // => Dynamic bean lookup at runtime
        return context.getBean(beanClass)
    }

    fun getBeanCount(): Int {
        return context.beanDefinitionCount
        // => Returns total bean count in context
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Output: Context injected into BeanLocator

    val locator = context.getBean(BeanLocator::class.java)
    println("Bean count: ${locator.getBeanCount()}")
    // => Output: Bean count: [number]

    context.close()
}
```

**Expected Output**:

```
Context injected into BeanLocator
Bean count: 8
```

**Key Takeaways**:

- `ApplicationContextAware` provides context access to beans
- Use sparingly (creates coupling to Spring)
- Useful for dynamic bean lookup or framework integration
- Other aware interfaces: BeanNameAware, EnvironmentAware

**Related Documentation**:

- [Aware Interfaces](https://docs.spring.io/spring-framework/reference/core/beans/factory-nature.html#beans-factory-aware)

---

### Example 40: @Order for Bean Loading Sequence (Coverage: 88.0%)

Demonstrates controlling component initialization order using `@Order` annotation.

**Java Implementation**:

```java
import org.springframework.context.annotation.*;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import javax.annotation.PostConstruct;

interface StartupTask {
    void execute();
}

@Component
@Order(1)  // => Executed first (lowest order number)
class DatabaseInitTask implements StartupTask {
    @PostConstruct
    public void execute() {
        System.out.println("1. Database initialization");
    }
}

@Component
@Order(2)  // => Executed second
class CacheWarmupTask implements StartupTask {
    @PostConstruct
    public void execute() {
        System.out.println("2. Cache warmup");
    }
}

@Component
@Order(3)  // => Executed last (highest order number)
class ServerReadyTask implements StartupTask {
    @PostConstruct
    public void execute() {
        System.out.println("3. Server ready");
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example40 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);
        // => Output shows ordered initialization:
        // => 1. Database initialization
        // => 2. Cache warmup
        // => 3. Server ready

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.*
import org.springframework.core.annotation.Order
import org.springframework.stereotype.Component
import javax.annotation.PostConstruct

interface StartupTask {
    fun execute()
}

@Component
@Order(1)  // => Executed first (lowest order number)
class DatabaseInitTask : StartupTask {
    @PostConstruct
    override fun execute() {
        println("1. Database initialization")
    }
}

@Component
@Order(2)  // => Executed second
class CacheWarmupTask : StartupTask {
    @PostConstruct
    override fun execute() {
        println("2. Cache warmup")
    }
}

@Component
@Order(3)  // => Executed last (highest order number)
class ServerReadyTask : StartupTask {
    @PostConstruct
    override fun execute() {
        println("3. Server ready")
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Output shows ordered initialization:
    // => 1. Database initialization
    // => 2. Cache warmup
    // => 3. Server ready

    context.close()
}
```

**Expected Output**:

```
1. Database initialization
2. Cache warmup
3. Server ready
```

**Key Takeaways**:

- `@Order` controls component initialization sequence
- Lower numbers execute first
- Useful for startup tasks with dependencies
- Does NOT guarantee strict ordering (use `@DependsOn` for that)

**Related Documentation**:

- [Order Documentation](https://docs.spring.io/spring-framework/reference/core/beans/factory-nature.html#beans-factory-ordered)

---

## Event Handling and Messaging (Examples 41-45)

### Example 41: ApplicationEventPublisher for Custom Events (Coverage: 90.0%)

Demonstrates publishing and listening to custom application events.

**Java Implementation**:

```java
import org.springframework.context.*;
import org.springframework.context.annotation.*;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

class OrderPlacedEvent extends ApplicationEvent {
    // => Custom event type
    private String orderId;

    public OrderPlacedEvent(Object source, String orderId) {
        super(source);  // => Event source (usually publisher)
        this.orderId = orderId;
    }

    public String getOrderId() {
        return orderId;
    }
}

@Component
class OrderService implements ApplicationEventPublisherAware {
    // => Publishes events
    private ApplicationEventPublisher publisher;

    @Override
    public void setApplicationEventPublisher(ApplicationEventPublisher publisher) {
        this.publisher = publisher;  // => Injected by Spring
    }

    public void placeOrder(String orderId) {
        System.out.println("Order placed: " + orderId);
        // => Publishes event to all listeners
        publisher.publishEvent(new OrderPlacedEvent(this, orderId));
    }
}

@Component
class EmailNotifier {
    @EventListener  // => Listens for OrderPlacedEvent
    public void handleOrderPlaced(OrderPlacedEvent event) {
        // => Called when event published
        System.out.println("Email sent for order: " + event.getOrderId());
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example41 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);

        OrderService service = context.getBean(OrderService.class);
        service.placeOrder("ORD-123");
        // => Output: Order placed: ORD-123
        // => Output: Email sent for order: ORD-123

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.*
import org.springframework.context.annotation.*
import org.springframework.context.event.EventListener
import org.springframework.stereotype.Component

class OrderPlacedEvent(source: Any, val orderId: String) : ApplicationEvent(source) {
    // => Custom event type
}

@Component
class OrderService : ApplicationEventPublisherAware {
    // => Publishes events
    private lateinit var publisher: ApplicationEventPublisher

    override fun setApplicationEventPublisher(publisher: ApplicationEventPublisher) {
        this.publisher = publisher  // => Injected by Spring
    }

    fun placeOrder(orderId: String) {
        println("Order placed: $orderId")
        // => Publishes event to all listeners
        publisher.publishEvent(OrderPlacedEvent(this, orderId))
    }
}

@Component
class EmailNotifier {
    @EventListener  // => Listens for OrderPlacedEvent
    fun handleOrderPlaced(event: OrderPlacedEvent) {
        // => Called when event published
        println("Email sent for order: ${event.orderId}")
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val service = context.getBean(OrderService::class.java)
    service.placeOrder("ORD-123")
    // => Output: Order placed: ORD-123
    // => Output: Email sent for order: ORD-123

    context.close()
}
```

**Expected Output**:

```
Order placed: ORD-123
Email sent for order: ORD-123
```

**Key Takeaways**:

- `ApplicationEventPublisher` enables event-driven architecture
- `@EventListener` marks event handler methods
- Events decouple components (publisher doesn't know listeners)
- Built-in events: ContextRefreshedEvent, ContextClosedEvent, etc.

**Related Documentation**:

- [Event Handling](https://docs.spring.io/spring-framework/reference/core/beans/context-introduction.html#context-functionality-events)

---

### Example 42: @Async Event Listeners (Coverage: 92.0%)

Demonstrates asynchronous event processing with `@Async` annotation.

**Java Implementation**:

```java
import org.springframework.context.annotation.*;
import org.springframework.context.event.*;
import org.springframework.scheduling.annotation.*;
import org.springframework.stereotype.Component;

class DataProcessedEvent {
    // => Simple POJO event (no ApplicationEvent extension needed)
    private String data;

    public DataProcessedEvent(String data) {
        this.data = data;
    }

    public String getData() {
        return data;
    }
}

@Component
class SyncListener {
    @EventListener  // => Synchronous listener (blocks publisher)
    public void handleSync(DataProcessedEvent event) {
        System.out.println("[SYNC] Processing: " + event.getData());
        // => Runs in publisher's thread
    }
}

@Component
class AsyncListener {
    @Async  // => Asynchronous execution
    @EventListener  // => Non-blocking listener
    public void handleAsync(DataProcessedEvent event) throws InterruptedException {
        Thread.sleep(1000);  // => Simulates slow processing
        System.out.println("[ASYNC] Processed: " + event.getData());
        // => Runs in separate thread pool
    }
}

@Configuration
@EnableAsync  // => Enables @Async support
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example42 {
    public static void main(String[] args) throws InterruptedException {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);

        context.publishEvent(new DataProcessedEvent("test-data"));
        // => Output: [SYNC] Processing: test-data (immediate)
        System.out.println("Event published, continuing...");
        // => Publisher not blocked by async listener

        Thread.sleep(1500);  // => Wait for async processing
        // => Output: [ASYNC] Processed: test-data (after delay)

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.*
import org.springframework.context.event.*
import org.springframework.scheduling.annotation.*
import org.springframework.stereotype.Component

data class DataProcessedEvent(val data: String)
// => Simple data class event (no ApplicationEvent extension needed)

@Component
class SyncListener {
    @EventListener  // => Synchronous listener (blocks publisher)
    fun handleSync(event: DataProcessedEvent) {
        println("[SYNC] Processing: ${event.data}")
        // => Runs in publisher's thread
    }
}

@Component
class AsyncListener {
    @Async  // => Asynchronous execution
    @EventListener  // => Non-blocking listener
    fun handleAsync(event: DataProcessedEvent) {
        Thread.sleep(1000)  // => Simulates slow processing
        println("[ASYNC] Processed: ${event.data}")
        // => Runs in separate thread pool
    }
}

@Configuration
@EnableAsync  // => Enables @Async support
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    context.publishEvent(DataProcessedEvent("test-data"))
    // => Output: [SYNC] Processing: test-data (immediate)
    println("Event published, continuing...")
    // => Publisher not blocked by async listener

    Thread.sleep(1500)  // => Wait for async processing
    // => Output: [ASYNC] Processed: test-data (after delay)

    context.close()
}
```

**Expected Output**:

```
[SYNC] Processing: test-data
Event published, continuing...
[ASYNC] Processed: test-data
```

**Key Takeaways**:

- `@Async` enables non-blocking event listeners
- Requires `@EnableAsync` on configuration class
- Events can be POJOs (no ApplicationEvent extension needed)
- Sync and async listeners can coexist

**Related Documentation**:

- [Async Event Processing](https://docs.spring.io/spring-framework/reference/core/beans/context-introduction.html#context-functionality-events-async)

---

### Example 43: @EventListener with Condition (Coverage: 94.0%)

Demonstrates conditional event listening based on SpEL expressions.

**Java Implementation**:

```java
import org.springframework.context.annotation.*;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

class PaymentEvent {
    private String type;  // => "credit" or "debit"
    private double amount;

    public PaymentEvent(String type, double amount) {
        this.type = type;
        this.amount = amount;
    }

    public String getType() { return type; }
    public double getAmount() { return amount; }
}

@Component
class PaymentProcessor {
    @EventListener(condition = "#event.type == 'credit'")
    // => SpEL condition: only process credit payments
    public void handleCredit(PaymentEvent event) {
        System.out.println("Credit: $" + event.getAmount());
    }

    @EventListener(condition = "#event.type == 'debit' and #event.amount > 100")
    // => Multiple conditions: debit AND amount > 100
    public void handleLargeDebit(PaymentEvent event) {
        System.out.println("Large Debit: $" + event.getAmount());
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example43 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);

        context.publishEvent(new PaymentEvent("credit", 50));
        // => Output: Credit: $50.0 (matches first condition)

        context.publishEvent(new PaymentEvent("debit", 50));
        // => No output (amount not > 100)

        context.publishEvent(new PaymentEvent("debit", 150));
        // => Output: Large Debit: $150.0 (matches second condition)

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.*
import org.springframework.context.event.EventListener
import org.springframework.stereotype.Component

data class PaymentEvent(
    val type: String,  // => "credit" or "debit"
    val amount: Double
)

@Component
class PaymentProcessor {
    @EventListener(condition = "#event.type == 'credit'")
    // => SpEL condition: only process credit payments
    fun handleCredit(event: PaymentEvent) {
        println("Credit: $${event.amount}")
    }

    @EventListener(condition = "#event.type == 'debit' and #event.amount > 100")
    // => Multiple conditions: debit AND amount > 100
    fun handleLargeDebit(event: PaymentEvent) {
        println("Large Debit: $${event.amount}")
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    context.publishEvent(PaymentEvent("credit", 50.0))
    // => Output: Credit: $50.0 (matches first condition)

    context.publishEvent(PaymentEvent("debit", 50.0))
    // => No output (amount not > 100)

    context.publishEvent(PaymentEvent("debit", 150.0))
    // => Output: Large Debit: $150.0 (matches second condition)

    context.close()
}
```

**Expected Output**:

```
Credit: $50.0
Large Debit: $150.0
```

**Key Takeaways**:

- `@EventListener` supports SpEL condition expressions
- Access event properties with `#event.propertyName`
- Enables selective event processing
- Conditions evaluated before method invocation

**Related Documentation**:

- [Event Listener Conditions](https://docs.spring.io/spring-framework/reference/core/beans/context-introduction.html#context-functionality-events-annotation)

---

### Example 44: Generic Event Types (Coverage: 96.0%)

Demonstrates type-safe event handling with generic event types.

**Java Implementation**:

```java
import org.springframework.context.annotation.*;
import org.springframework.context.event.EventListener;
import org.springframework.core.ResolvableType;
import org.springframework.core.ResolvableTypeProvider;
import org.springframework.stereotype.Component;

class EntityEvent<T> implements ResolvableTypeProvider {
    // => Generic event with type parameter
    private T entity;
    private String action;

    public EntityEvent(T entity, String action) {
        this.entity = entity;
        this.action = action;
    }

    public T getEntity() { return entity; }
    public String getAction() { return action; }

    @Override
    public ResolvableType getResolvableType() {
        // => Provides runtime type information for generic
        return ResolvableType.forClassWithGenerics(
            getClass(), ResolvableType.forInstance(entity));
    }
}

class User {
    private String name;
    public User(String name) { this.name = name; }
    public String getName() { return name; }
}

class Product {
    private String sku;
    public Product(String sku) { this.sku = sku; }
    public String getSku() { return sku; }
}

@Component
class EventHandlers {
    @EventListener
    // => Type-safe: only EntityEvent<User> events
    public void handleUserEvent(EntityEvent<User> event) {
        System.out.println("User " + event.getAction() + ": " +
            event.getEntity().getName());
    }

    @EventListener
    // => Type-safe: only EntityEvent<Product> events
    public void handleProductEvent(EntityEvent<Product> event) {
        System.out.println("Product " + event.getAction() + ": " +
            event.getEntity().getSku());
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example44 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);

        context.publishEvent(new EntityEvent<>(new User("Alice"), "created"));
        // => Output: User created: Alice (routed to handleUserEvent)

        context.publishEvent(new EntityEvent<>(new Product("SKU-123"), "updated"));
        // => Output: Product updated: SKU-123 (routed to handleProductEvent)

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.*
import org.springframework.context.event.EventListener
import org.springframework.core.ResolvableType
import org.springframework.core.ResolvableTypeProvider
import org.springframework.stereotype.Component

class EntityEvent<T>(val entity: T, val action: String) : ResolvableTypeProvider {
    // => Generic event with type parameter
    override fun getResolvableType(): ResolvableType {
        // => Provides runtime type information for generic
        return ResolvableType.forClassWithGenerics(
            javaClass, ResolvableType.forInstance(entity))
    }
}

data class User(val name: String)
data class Product(val sku: String)

@Component
class EventHandlers {
    @EventListener
    // => Type-safe: only EntityEvent<User> events
    fun handleUserEvent(event: EntityEvent<User>) {
        println("User ${event.action}: ${event.entity.name}")
    }

    @EventListener
    // => Type-safe: only EntityEvent<Product> events
    fun handleProductEvent(event: EntityEvent<Product>) {
        println("Product ${event.action}: ${event.entity.sku}")
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    context.publishEvent(EntityEvent(User("Alice"), "created"))
    // => Output: User created: Alice (routed to handleUserEvent)

    context.publishEvent(EntityEvent(Product("SKU-123"), "updated"))
    // => Output: Product updated: SKU-123 (routed to handleProductEvent)

    context.close()
}
```

**Expected Output**:

```
User created: Alice
Product updated: SKU-123
```

**Key Takeaways**:

- Generic events enable type-safe event handling
- Implement `ResolvableTypeProvider` for runtime type resolution
- Spring routes events to correct handler based on generic type
- Reduces type casting and improves compile-time safety

**Related Documentation**:

- [Generic Events](https://docs.spring.io/spring-framework/reference/core/beans/context-introduction.html#context-functionality-events-generics)

---

### Example 45: Transaction Event Listeners (Coverage: 98.0%)

Demonstrates event listeners that execute at specific transaction phases.

**Java Implementation**:

```java
import org.springframework.context.annotation.*;
import org.springframework.transaction.event.*;
import org.springframework.transaction.annotation.*;
import org.springframework.stereotype.Component;

class UserCreatedEvent {
    private String username;

    public UserCreatedEvent(String username) {
        this.username = username;
    }

    public String getUsername() { return username; }
}

@Component
class NotificationService {
    @TransactionalEventListener(phase = TransactionPhase.BEFORE_COMMIT)
    // => Executes BEFORE transaction commits
    public void beforeCommit(UserCreatedEvent event) {
        System.out.println("Before commit: validate " + event.getUsername());
    }

    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    // => Executes AFTER successful commit (default)
    public void afterCommit(UserCreatedEvent event) {
        System.out.println("After commit: send email to " + event.getUsername());
    }

    @TransactionalEventListener(phase = TransactionPhase.AFTER_ROLLBACK)
    // => Executes if transaction rolls back
    public void afterRollback(UserCreatedEvent event) {
        System.out.println("Rollback: cleanup for " + event.getUsername());
    }

    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMPLETION)
    // => Executes after commit OR rollback
    public void afterCompletion(UserCreatedEvent event) {
        System.out.println("Completion: log event for " + event.getUsername());
    }
}

@Configuration
@EnableTransactionManagement  // => Enables transaction support
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example45 {
    public static void main(String[] args) {
        // Note: Requires transaction manager bean for full functionality
        System.out.println("Transaction event listener configured");
        System.out.println("Listeners execute at: BEFORE_COMMIT, AFTER_COMMIT,");
        System.out.println("AFTER_ROLLBACK, AFTER_COMPLETION");
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.*
import org.springframework.transaction.event.*
import org.springframework.transaction.annotation.*
import org.springframework.stereotype.Component

data class UserCreatedEvent(val username: String)

@Component
class NotificationService {
    @TransactionalEventListener(phase = TransactionPhase.BEFORE_COMMIT)
    // => Executes BEFORE transaction commits
    fun beforeCommit(event: UserCreatedEvent) {
        println("Before commit: validate ${event.username}")
    }

    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    // => Executes AFTER successful commit (default)
    fun afterCommit(event: UserCreatedEvent) {
        println("After commit: send email to ${event.username}")
    }

    @TransactionalEventListener(phase = TransactionPhase.AFTER_ROLLBACK)
    // => Executes if transaction rolls back
    fun afterRollback(event: UserCreatedEvent) {
        println("Rollback: cleanup for ${event.username}")
    }

    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMPLETION)
    // => Executes after commit OR rollback
    fun afterCompletion(event: UserCreatedEvent) {
        println("Completion: log event for ${event.username}")
    }
}

@Configuration
@EnableTransactionManagement  // => Enables transaction support
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    // Note: Requires transaction manager bean for full functionality
    println("Transaction event listener configured")
    println("Listeners execute at: BEFORE_COMMIT, AFTER_COMMIT,")
    println("AFTER_ROLLBACK, AFTER_COMPLETION")
}
```

**Expected Output**:

```
Transaction event listener configured
Listeners execute at: BEFORE_COMMIT, AFTER_COMMIT,
AFTER_ROLLBACK, AFTER_COMPLETION
```

**Key Takeaways**:

- `@TransactionalEventListener` ties event handling to transaction lifecycle
- Four phases: BEFORE_COMMIT, AFTER_COMMIT, AFTER_ROLLBACK, AFTER_COMPLETION
- Ensures listeners execute only when transaction succeeds (AFTER_COMMIT)
- Useful for sending notifications, clearing caches, etc.

**Related Documentation**:

- [Transaction Event Listeners](https://docs.spring.io/spring-framework/reference/data-access/transaction/event.html)

---

## Bean Validation and SpEL (Examples 46-50)

### Example 46: SpEL in @Value for Complex Expressions (Coverage: 100.0%)

Demonstrates Spring Expression Language (SpEL) for advanced property injection.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.*;
import org.springframework.stereotype.Component;

@Component
class SpELExamples {
    @Value("#{2 * 3}")  // => Arithmetic expression
    private int multiplication;  // => Value: 6

    @Value("#{'Hello ' + 'World'}")  // => String concatenation
    private String greeting;  // => Value: "Hello World"

    @Value("#{systemProperties['java.home']}")  // => System property
    private String javaHome;

    @Value("#{T(java.lang.Math).PI}")  // => Static field access
    private double pi;  // => Value: 3.141592653589793

    @Value("#{T(java.lang.Math).max(10, 20)}")  // => Static method call
    private int maxValue;  // => Value: 20

    public void printValues() {
        System.out.println("Multiplication: " + multiplication);
        System.out.println("Greeting: " + greeting);
        System.out.println("Java Home: " + javaHome);
        System.out.println("PI: " + pi);
        System.out.println("Max: " + maxValue);
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example46 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);

        SpELExamples examples = context.getBean(SpELExamples.class);
        examples.printValues();
        // => Output shows evaluated SpEL expressions

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Value
import org.springframework.context.annotation.*
import org.springframework.stereotype.Component

@Component
class SpELExamples {
    @Value("#{2 * 3}")  // => Arithmetic expression
    private var multiplication: Int = 0  // => Value: 6

    @Value("#{'Hello ' + 'World'}")  // => String concatenation
    private lateinit var greeting: String  // => Value: "Hello World"

    @Value("#{systemProperties['java.home']}")  // => System property
    private lateinit var javaHome: String

    @Value("#{T(java.lang.Math).PI}")  // => Static field access
    private var pi: Double = 0.0  // => Value: 3.141592653589793

    @Value("#{T(java.lang.Math).max(10, 20)}")  // => Static method call
    private var maxValue: Int = 0  // => Value: 20

    fun printValues() {
        println("Multiplication: $multiplication")
        println("Greeting: $greeting")
        println("Java Home: $javaHome")
        println("PI: $pi")
        println("Max: $maxValue")
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val examples = context.getBean(SpELExamples::class.java)
    examples.printValues()
    // => Output shows evaluated SpEL expressions

    context.close()
}
```

**Expected Output**:

```
Multiplication: 6
Greeting: Hello World
Java Home: /path/to/java
PI: 3.141592653589793
Max: 20
```

**Key Takeaways**:

- SpEL supports arithmetic, string operations, method calls
- Access system properties with `systemProperties['key']`
- Call static methods with `T(FullyQualifiedClassName).method()`
- Powerful for computed configuration values

**Related Documentation**:

- [SpEL Reference](https://docs.spring.io/spring-framework/reference/core/expressions.html)

---

### Example 47: SpEL with Bean References (Coverage: 102.0%)

Demonstrates referencing other beans in SpEL expressions.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.*;
import org.springframework.stereotype.Component;

@Component("config")
class AppConfiguration {
    private int maxRetries = 3;
    private String apiUrl = "https://api.example.com";

    public int getMaxRetries() { return maxRetries; }
    public String getApiUrl() { return apiUrl; }
}

@Component
class ApiClient {
    @Value("#{@config.maxRetries}")  // => References 'config' bean
    // => Calls getMaxRetries() method
    private int retries;  // => Value: 3

    @Value("#{@config.apiUrl + '/users'}")  // => Bean reference + concatenation
    private String usersEndpoint;  // => Value: "https://api.example.com/users"

    @Value("#{@config.maxRetries > 5 ? 'high' : 'low'}")  // => Ternary operator
    private String retryLevel;  // => Value: "low" (3 is not > 5)

    public void printConfig() {
        System.out.println("Retries: " + retries);
        System.out.println("Endpoint: " + usersEndpoint);
        System.out.println("Retry Level: " + retryLevel);
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example47 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);

        ApiClient client = context.getBean(ApiClient.class);
        client.printConfig();
        // => Output shows values from config bean

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Value
import org.springframework.context.annotation.*
import org.springframework.stereotype.Component

@Component("config")
class AppConfiguration {
    val maxRetries = 3
    val apiUrl = "https://api.example.com"
}

@Component
class ApiClient {
    @Value("#{@config.maxRetries}")  // => References 'config' bean
    // => Calls getMaxRetries() method
    private var retries: Int = 0  // => Value: 3

    @Value("#{@config.apiUrl + '/users'}")  // => Bean reference + concatenation
    private lateinit var usersEndpoint: String  // => Value: "https://api.example.com/users"

    @Value("#{@config.maxRetries > 5 ? 'high' : 'low'}")  // => Ternary operator
    private lateinit var retryLevel: String  // => Value: "low" (3 is not > 5)

    fun printConfig() {
        println("Retries: $retries")
        println("Endpoint: $usersEndpoint")
        println("Retry Level: $retryLevel")
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val client = context.getBean(ApiClient::class.java)
    client.printConfig()
    // => Output shows values from config bean

    context.close()
}
```

**Expected Output**:

```
Retries: 3
Endpoint: https://api.example.com/users
Retry Level: low
```

**Key Takeaways**:

- Reference beans with `@beanName` syntax in SpEL
- Access bean properties and methods
- Combine with operators (ternary, concatenation, etc.)
- Enables cross-bean configuration dependencies

**Related Documentation**:

- [SpEL Bean References](https://docs.spring.io/spring-framework/reference/core/expressions/language-ref/bean-references.html)

---

### Example 48: SpEL Collection Selection and Projection (Coverage: 104.0%)

Demonstrates advanced SpEL operations on collections.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.*;
import org.springframework.stereotype.Component;
import java.util.*;

@Component("data")
class DataSource {
    private List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

    public List<Integer> getNumbers() { return numbers; }
}

@Component
class CollectionProcessor {
    @Value("#{@data.numbers.?[#this > 5]}")  // => Selection: filter > 5
    // => Syntax: collection.?[condition]
    private List<Integer> filtered;  // => Value: [6, 7, 8, 9, 10]

    @Value("#{@data.numbers.![#this * 2]}")  // => Projection: multiply by 2
    // => Syntax: collection.![expression]
    private List<Integer> doubled;  // => Value: [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]

    @Value("#{@data.numbers.?[#this % 2 == 0]}")  // => Filter even numbers
    private List<Integer> evens;  // => Value: [2, 4, 6, 8, 10]

    @Value("#{@data.numbers.?[#this > 5].![#this * 2]}")  // => Chained operations
    // => First filter > 5, then double each
    private List<Integer> filteredAndDoubled;  // => Value: [12, 14, 16, 18, 20]

    public void printResults() {
        System.out.println("Filtered (>5): " + filtered);
        System.out.println("Doubled: " + doubled);
        System.out.println("Evens: " + evens);
        System.out.println("Filtered & Doubled: " + filteredAndDoubled);
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example48 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);

        CollectionProcessor processor = context.getBean(CollectionProcessor.class);
        processor.printResults();

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Value
import org.springframework.context.annotation.*
import org.springframework.stereotype.Component

@Component("data")
class DataSource {
    val numbers = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
}

@Component
class CollectionProcessor {
    @Value("#{@data.numbers.?[#this > 5]}")  // => Selection: filter > 5
    // => Syntax: collection.?[condition]
    private lateinit var filtered: List<Int>  // => Value: [6, 7, 8, 9, 10]

    @Value("#{@data.numbers.![#this * 2]}")  // => Projection: multiply by 2
    // => Syntax: collection.![expression]
    private lateinit var doubled: List<Int>  // => Value: [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]

    @Value("#{@data.numbers.?[#this % 2 == 0]}")  // => Filter even numbers
    private lateinit var evens: List<Int>  // => Value: [2, 4, 6, 8, 10]

    @Value("#{@data.numbers.?[#this > 5].![#this * 2]}")  // => Chained operations
    // => First filter > 5, then double each
    private lateinit var filteredAndDoubled: List<Int>  // => Value: [12, 14, 16, 18, 20]

    fun printResults() {
        println("Filtered (>5): $filtered")
        println("Doubled: $doubled")
        println("Evens: $evens")
        println("Filtered & Doubled: $filteredAndDoubled")
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val processor = context.getBean(CollectionProcessor::class.java)
    processor.printResults()

    context.close()
}
```

**Expected Output**:

```
Filtered (>5): [6, 7, 8, 9, 10]
Doubled: [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
Evens: [2, 4, 6, 8, 10]
Filtered & Doubled: [12, 14, 16, 18, 20]
```

**Key Takeaways**:

- Selection: `collection.?[condition]` filters elements
- Projection: `collection.![expression]` transforms elements
- Use `#this` to reference current element
- Operations can be chained for complex transformations

**Related Documentation**:

- [SpEL Collection Selection](https://docs.spring.io/spring-framework/reference/core/expressions/language-ref/collection-selection.html)

---

### Example 49: Environment Property Resolution in @Value (Coverage: 106.0%)

Demonstrates combining environment properties with SpEL defaults and type conversion.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.*;
import org.springframework.stereotype.Component;

@Component
class ConfigProperties {
    @Value("${app.timeout:5000}")  // => Property with default
    // => Reads from properties, defaults to 5000 if missing
    private int timeout;  // => Type automatically converted to int

    @Value("${app.enabled:true}")  // => Boolean property
    private boolean enabled;

    @Value("${app.tags:dev,test,prod}")  // => Comma-separated list
    private String[] tags;  // => Automatically split into array

    @Value("#{${app.timeout:5000} / 1000}")  // => SpEL expression + property
    // => Converts milliseconds to seconds
    private int timeoutSeconds;

    @Value("${app.url:${default.url:http://localhost}}")  // => Nested defaults
    // => First tries app.url, then default.url, finally literal
    private String url;

    public void printConfig() {
        System.out.println("Timeout: " + timeout + "ms");
        System.out.println("Enabled: " + enabled);
        System.out.println("Tags: " + String.join(", ", tags));
        System.out.println("Timeout (seconds): " + timeoutSeconds);
        System.out.println("URL: " + url);
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example49 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);

        ConfigProperties config = context.getBean(ConfigProperties.class);
        config.printConfig();
        // => Output shows default values (no properties file)

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Value
import org.springframework.context.annotation.*
import org.springframework.stereotype.Component

@Component
class ConfigProperties {
    @Value("\${app.timeout:5000}")  // => Property with default
    // => Reads from properties, defaults to 5000 if missing
    private var timeout: Int = 0  // => Type automatically converted to int

    @Value("\${app.enabled:true}")  // => Boolean property
    private var enabled: Boolean = false

    @Value("\${app.tags:dev,test,prod}")  // => Comma-separated list
    private lateinit var tags: Array<String>  // => Automatically split into array

    @Value("#{'\${app.timeout:5000}' / 1000}")  // => SpEL expression + property
    // => Converts milliseconds to seconds
    private var timeoutSeconds: Int = 0

    @Value("\${app.url:\${default.url:http://localhost}}")  // => Nested defaults
    // => First tries app.url, then default.url, finally literal
    private lateinit var url: String

    fun printConfig() {
        println("Timeout: ${timeout}ms")
        println("Enabled: $enabled")
        println("Tags: ${tags.joinToString(", ")}")
        println("Timeout (seconds): $timeoutSeconds")
        println("URL: $url")
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val config = context.getBean(ConfigProperties::class.java)
    config.printConfig()
    // => Output shows default values (no properties file)

    context.close()
}
```

**Expected Output**:

```
Timeout: 5000ms
Enabled: true
Tags: dev, test, prod
Timeout (seconds): 5
URL: http://localhost
```

**Key Takeaways**:

- `${property:default}` syntax provides fallback values
- Automatic type conversion (String → int, boolean, array)
- Nested defaults: `${prop1:${prop2:literal}}`
- Combine properties with SpEL for computed values

**Related Documentation**:

- [Property Placeholder Configuration](https://docs.spring.io/spring-framework/reference/core/beans/environment.html#beans-property-source-abstraction)

---

### Example 50: Bean Validation with @Validated (Coverage: 108.0%)

Demonstrates method-level validation using Spring's validation framework.

**Java Implementation**:

```java
import org.springframework.context.annotation.*;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;
import javax.validation.constraints.*;

@Component
@Validated  // => Enables method parameter validation
class UserService {
    public void createUser(
        @NotNull @Size(min = 3, max = 50) String username,
        // => Validates: not null, length 3-50
        @Email String email,
        // => Validates: proper email format
        @Min(18) @Max(120) int age
        // => Validates: between 18 and 120
    ) {
        System.out.println("User created: " + username +
            ", email: " + email + ", age: " + age);
    }

    public void updateUser(
        @NotBlank String id,  // => Validates: not null/empty/whitespace
        @Pattern(regexp = "[A-Z]{2}") String country
        // => Validates: exactly 2 uppercase letters
    ) {
        System.out.println("User updated: " + id + ", country: " + country);
    }
}

@Configuration
@ComponentScan(basePackages = "com.example")
class AppConfig {
}

public class Example50 {
    public static void main(String[] args) {
        var context = new AnnotationConfigApplicationContext(AppConfig.class);

        UserService service = context.getBean(UserService.class);

        // Valid calls
        service.createUser("alice", "alice@example.com", 25);
        // => Output: User created: alice, email: alice@example.com, age: 25

        service.updateUser("USR-123", "US");
        // => Output: User updated: USR-123, country: US

        // Invalid calls would throw ConstraintViolationException:
        // service.createUser("ab", "invalid-email", 15);
        // => Violation: username too short, invalid email, age < 18

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.*
import org.springframework.stereotype.Component
import org.springframework.validation.annotation.Validated
import javax.validation.constraints.*

@Component
@Validated  // => Enables method parameter validation
class UserService {
    fun createUser(
        @NotNull @Size(min = 3, max = 50) username: String,
        // => Validates: not null, length 3-50
        @Email email: String,
        // => Validates: proper email format
        @Min(18) @Max(120) age: Int
        // => Validates: between 18 and 120
    ) {
        println("User created: $username, email: $email, age: $age")
    }

    fun updateUser(
        @NotBlank id: String,  // => Validates: not null/empty/whitespace
        @Pattern(regexp = "[A-Z]{2}") country: String
        // => Validates: exactly 2 uppercase letters
    ) {
        println("User updated: $id, country: $country")
    }
}

@Configuration
@ComponentScan(basePackages = ["com.example"])
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val service = context.getBean(UserService::class.java)

    // Valid calls
    service.createUser("alice", "alice@example.com", 25)
    // => Output: User created: alice, email: alice@example.com, age: 25

    service.updateUser("USR-123", "US")
    // => Output: User updated: USR-123, country: US

    // Invalid calls would throw ConstraintViolationException:
    // service.createUser("ab", "invalid-email", 15)
    // => Violation: username too short, invalid email, age < 18

    context.close()
}
```

**Expected Output**:

```
User created: alice, email: alice@example.com, age: 25
User updated: USR-123, country: US
```

**Key Takeaways**:

- `@Validated` enables method-level parameter validation
- Use JSR-303 annotations: @NotNull, @Size, @Email, @Min, @Max, @Pattern
- Violations throw `ConstraintViolationException`
- Requires `spring-boot-starter-validation` or `hibernate-validator` dependency

**Related Documentation**:

- [Method Validation](https://docs.spring.io/spring-framework/reference/core/validation/beanvalidation.html#validation-beanvalidation-spring-method)

---

## Summary

This beginner tutorial covered **50 fundamental Spring Framework examples** (0-40% coverage):

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

**Advanced Bean Configuration (26-30)**:

- @Import for modular config
- @ComponentScan with custom packages
- @PropertySource for external config
- initMethod and destroyMethod
- @Scope with web contexts

**Component Stereotypes (31-35)**:

- @Service stereotype
- @Repository with exception translation
- @Controller for web layer
- @Configuration as meta-annotation
- Custom stereotype annotations

**Advanced Bean Features (36-40)**:

- FactoryBean for complex creation
- BeanPostProcessor customization
- @Lookup method injection
- ApplicationContextAware
- @Order for loading sequence

**Event Handling and Messaging (41-45)**:

- ApplicationEventPublisher
- @Async event listeners
- @EventListener with conditions
- Generic event types
- Transaction event listeners

**Bean Validation and SpEL (46-50)**:

- SpEL complex expressions
- SpEL bean references
- SpEL collection operations
- Environment property resolution
- Bean validation with @Validated

**Next Steps**: Progress to [Intermediate](/en/learn/software-engineering/web-platform/jvm-spring/by-example/intermediate) (40-75% coverage) covering advanced DI, AOP, transactions, and data access.
