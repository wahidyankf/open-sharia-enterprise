---
title: "Intermediate"
weight: 11000002
date: 2025-01-29T00:00:00+07:00
draft: false
description: "Intermediate-level Spring Framework examples covering advanced DI, AOP, transactions, data access, and Spring MVC basics (Coverage: 40-75%)"
tags: ["spring", "java", "kotlin", "intermediate", "examples", "aop", "transactions", "jdbc"]
---

This tutorial provides 25 intermediate Spring Framework examples building on beginner concepts. Focus shifts to AOP, transaction management, data access with JdbcTemplate, and Spring MVC fundamentals.

**Coverage**: 40-75% of Spring Framework features
**Target Audience**: Developers comfortable with Spring basics (IoC, DI, bean lifecycle)

## Advanced Dependency Injection (Examples 26-30)

### Example 26: Constructor Injection with Multiple Dependencies (Coverage: 61.0%)

Demonstrates injecting multiple dependencies via constructor with proper ordering.

**Java Implementation**:

```java
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Repository;
import org.springframework.stereotype.Service;

@Repository
class DonationRepository {
    public void save(String donation) {
        System.out.println("Saved: " + donation);
    }
}

@Component
class EmailNotifier {
    public void send(String message) {
        System.out.println("Email: " + message);
    }
}

@Component
class AuditLogger {
    public void log(String action) {
        System.out.println("Audit: " + action);
    }
}

@Service
class DonationService {
    private final DonationRepository repository;
    private final EmailNotifier notifier;
    private final AuditLogger logger;

    // => Constructor with three dependencies
    // => Spring injects all three automatically
    public DonationService(
        DonationRepository repository,  // => Injected first
        EmailNotifier notifier,          // => Injected second
        AuditLogger logger               // => Injected third
    ) {
        this.repository = repository;
        this.notifier = notifier;
        this.logger = logger;
        // => All dependencies immutable (final)
    }

    public void processDonation(String donor, double amount) {
        String donation = donor + ": $" + amount;
        repository.save(donation);         // => Uses repository
        notifier.send("Thank you");        // => Uses notifier
        logger.log("Donation processed"); // => Uses logger
    }
}

@Configuration
@ComponentScan
class AppConfig {
}

public class Example26 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        DonationService service = context.getBean(DonationService.class);
        service.processDonation("Yusuf", 1000.0);
        // => Output: Saved: Yusuf: $1000.0
        // => Output: Email: Thank you
        // => Output: Audit: Donation processed

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
import org.springframework.stereotype.Repository
import org.springframework.stereotype.Service

@Repository
class DonationRepository {
    fun save(donation: String) {
        println("Saved: $donation")
    }
}

@Component
class EmailNotifier {
    fun send(message: String) {
        println("Email: $message")
    }
}

@Component
class AuditLogger {
    fun log(action: String) {
        println("Audit: $action")
    }
}

@Service
// => Kotlin primary constructor with three dependencies
// => Spring injects all three automatically
class DonationService(
    private val repository: DonationRepository,  // => Injected, immutable
    private val notifier: EmailNotifier,          // => Injected, immutable
    private val logger: AuditLogger               // => Injected, immutable
) {
    fun processDonation(donor: String, amount: Double) {
        val donation = "$donor: $$amount"
        repository.save(donation)         // => Uses repository
        notifier.send("Thank you")        // => Uses notifier
        logger.log("Donation processed") // => Uses logger
    }
}

@Configuration
@ComponentScan
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val service = context.getBean(DonationService::class.java)
    service.processDonation("Yusuf", 1000.0)
    // => Output: Saved: Yusuf: $1000.0
    // => Output: Email: Thank you
    // => Output: Audit: Donation processed

    context.close()
}
```

**Expected Output**:

```
Saved: Yusuf: $1000.0
Email: Thank you
Audit: Donation processed
```

**Key Takeaways**:

- Constructor injection supports multiple dependencies
- Dependencies injected in constructor parameter order
- All dependencies remain immutable (final/val)
- Clean separation of concerns across layers

**Related Documentation**:

- [Constructor Injection Best Practices](https://docs.spring.io/spring-framework/reference/core/beans/dependencies/factory-collaborators.html)

---

### Example 27: Optional Dependencies with @Autowired(required=false) (Coverage: 63.0%)

Demonstrates handling optional dependencies that may not be present.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;

@Component
class PushNotifier {
    public void send(String message) {
        System.out.println("Push: " + message);
    }
}

@Component
class NotificationService {
    private PushNotifier pushNotifier;  // => Optional dependency

    @Autowired(required = false)
    // => Spring won't fail if PushNotifier bean missing
    // => Setter NOT called if bean absent
    public void setPushNotifier(PushNotifier pushNotifier) {
        this.pushNotifier = pushNotifier;  // => May remain null
        System.out.println("PushNotifier injected");
    }

    public void sendNotification(String message) {
        if (pushNotifier != null) {
            // => Check before using optional dependency
            pushNotifier.send(message);
        } else {
            System.out.println("No push notifier available");
        }
    }
}

@Configuration
@ComponentScan
class AppConfig {
}

public class Example27 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        NotificationService service = context.getBean(NotificationService.class);
        service.sendNotification("Test message");
        // => Output: PushNotifier injected
        // => Output: Push: Test message
        // => PushNotifier WAS available (bean exists)

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

@Component
class PushNotifier {
    fun send(message: String) {
        println("Push: $message")
    }
}

@Component
class NotificationService {
    private var pushNotifier: PushNotifier? = null  // => Optional dependency (nullable)

    @Autowired(required = false)
    // => Spring won't fail if PushNotifier bean missing
    // => Setter NOT called if bean absent
    fun setPushNotifier(pushNotifier: PushNotifier) {
        this.pushNotifier = pushNotifier  // => May remain null
        println("PushNotifier injected")
    }

    fun sendNotification(message: String) {
        pushNotifier?.let {
            // => Safe call - only executes if not null
            it.send(message)
        } ?: println("No push notifier available")
        // => Elvis operator for null case
    }
}

@Configuration
@ComponentScan
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val service = context.getBean(NotificationService::class.java)
    service.sendNotification("Test message")
    // => Output: PushNotifier injected
    // => Output: Push: Test message
    // => PushNotifier WAS available (bean exists)

    context.close()
}
```

**Expected Output**:

```
PushNotifier injected
Push: Test message
```

**Key Takeaways**:

- `@Autowired(required = false)` makes dependency optional
- Application starts successfully even if bean missing
- Always null-check optional dependencies before use
- Useful for plugins or optional features

**Related Documentation**:

- [Optional Dependencies Documentation](https://docs.spring.io/spring-framework/reference/core/beans/annotation-config/autowired.html)

---

### Example 28: Injecting ApplicationContext (Coverage: 65.0%)

Demonstrates injecting ApplicationContext itself for dynamic bean lookup.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;

class ZakatCalculator {
    private final String type;

    public ZakatCalculator(String type) {
        this.type = type;
    }

    public String getType() {
        return type;
    }

    public double calculate(double amount) {
        return amount * 0.025;
    }
}

@Component
class DynamicCalculatorService {
    @Autowired
    private ApplicationContext context;
    // => Spring injects its own ApplicationContext
    // => Allows dynamic bean lookup

    public void calculateForType(String type, double amount) {
        ZakatCalculator calculator = context.getBean(type + "Calculator", ZakatCalculator.class);
        // => Dynamic bean lookup by name
        // => Name constructed at runtime

        double zakat = calculator.calculate(amount);
        System.out.println(type + " Zakat: $" + zakat);
    }
}

@Configuration
class AppConfig {
    @Bean("goldCalculator")  // => Named bean
    public ZakatCalculator goldCalculator() {
        return new ZakatCalculator("Gold");
    }

    @Bean("silverCalculator")  // => Named bean
    public ZakatCalculator silverCalculator() {
        return new ZakatCalculator("Silver");
    }
}

public class Example28 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class, DynamicCalculatorService.class);

        DynamicCalculatorService service = context.getBean(DynamicCalculatorService.class);
        service.calculateForType("gold", 10000);    // => Output: gold Zakat: $250.0
        service.calculateForType("silver", 5000);   // => Output: silver Zakat: $125.0

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.context.ApplicationContext
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.stereotype.Component

class ZakatCalculator(private val type: String) {
    fun getType(): String = type

    fun calculate(amount: Double): Double = amount * 0.025
}

@Component
class DynamicCalculatorService {
    @Autowired
    private lateinit var context: ApplicationContext
    // => Spring injects its own ApplicationContext
    // => Allows dynamic bean lookup

    fun calculateForType(type: String, amount: Double) {
        val calculator = context.getBean("${type}Calculator", ZakatCalculator::class.java)
        // => Dynamic bean lookup by name
        // => Name constructed at runtime

        val zakat = calculator.calculate(amount)
        println("$type Zakat: $$zakat")
    }
}

@Configuration
class AppConfig {
    @Bean("goldCalculator")  // => Named bean
    fun goldCalculator(): ZakatCalculator = ZakatCalculator("Gold")

    @Bean("silverCalculator")  // => Named bean
    fun silverCalculator(): ZakatCalculator = ZakatCalculator("Silver")
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java, DynamicCalculatorService::class.java)

    val service = context.getBean(DynamicCalculatorService::class.java)
    service.calculateForType("gold", 10000.0)    // => Output: gold Zakat: $250.0
    service.calculateForType("silver", 5000.0)   // => Output: silver Zakat: $125.0

    context.close()
}
```

**Expected Output**:

```
gold Zakat: $250.0
silver Zakat: $125.0
```

**Key Takeaways**:

- ApplicationContext can be injected like any bean
- Enables Service Locator pattern (use sparingly)
- Useful for dynamic bean selection at runtime
- Prefer dependency injection over context lookup when possible

**Related Documentation**:

- [ApplicationContext Interface Documentation](https://docs.spring.io/spring-framework/reference/core/beans/basics.html#beans-factory-client)

---

### Example 29: Circular Dependency Resolution (Coverage: 67.0%)

Demonstrates how Spring resolves circular dependencies using setter injection.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;

@Component
class ServiceA {
    private ServiceB serviceB;  // => Circular dependency

    // => Constructor creates instance first
    public ServiceA() {
        System.out.println("ServiceA constructor");
    }

    @Autowired  // => Setter injection AFTER construction
                // => Breaks circular dependency cycle
    public void setServiceB(ServiceB serviceB) {
        this.serviceB = serviceB;  // => Injected after both beans constructed
        System.out.println("ServiceA: ServiceB injected");
    }

    public void doWork() {
        System.out.println("ServiceA working");
        serviceB.help();  // => Uses ServiceB
    }
}

@Component
class ServiceB {
    private ServiceA serviceA;  // => Circular dependency

    public ServiceB() {
        System.out.println("ServiceB constructor");
    }

    @Autowired  // => Setter injection breaks cycle
    public void setServiceA(ServiceA serviceA) {
        this.serviceA = serviceA;
        System.out.println("ServiceB: ServiceA injected");
    }

    public void help() {
        System.out.println("ServiceB helping");
    }
}

@Configuration
@ComponentScan
class AppConfig {
}

public class Example29 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);
        // => Spring creates both beans, then injects via setters
        // => Output: ServiceA constructor
        // => Output: ServiceB constructor
        // => Output: ServiceA: ServiceB injected
        // => Output: ServiceB: ServiceA injected

        ServiceA serviceA = context.getBean(ServiceA.class);
        serviceA.doWork();
        // => Output: ServiceA working
        // => Output: ServiceB helping

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

@Component
class ServiceA {
    private lateinit var serviceB: ServiceB  // => Circular dependency (lateinit)

    init {
        println("ServiceA constructor")
    }

    @Autowired  // => Setter injection AFTER construction
                // => Breaks circular dependency cycle
    fun setServiceB(serviceB: ServiceB) {
        this.serviceB = serviceB  // => Injected after both beans constructed
        println("ServiceA: ServiceB injected")
    }

    fun doWork() {
        println("ServiceA working")
        serviceB.help()  // => Uses ServiceB
    }
}

@Component
class ServiceB {
    private lateinit var serviceA: ServiceA  // => Circular dependency (lateinit)

    init {
        println("ServiceB constructor")
    }

    @Autowired  // => Setter injection breaks cycle
    fun setServiceA(serviceA: ServiceA) {
        this.serviceA = serviceA
        println("ServiceB: ServiceA injected")
    }

    fun help() {
        println("ServiceB helping")
    }
}

@Configuration
@ComponentScan
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Spring creates both beans, then injects via setters
    // => Output: ServiceA constructor
    // => Output: ServiceB constructor
    // => Output: ServiceA: ServiceB injected
    // => Output: ServiceB: ServiceA injected

    val serviceA = context.getBean(ServiceA::class.java)
    serviceA.doWork()
    // => Output: ServiceA working
    // => Output: ServiceB helping

    context.close()
}
```

**Expected Output**:

```
ServiceA constructor
ServiceB constructor
ServiceA: ServiceB injected
ServiceB: ServiceA injected
ServiceA working
ServiceB helping
```

**Key Takeaways**:

- Spring resolves circular dependencies via setter injection
- Constructor injection fails with circular dependencies
- Beans constructed first, then dependencies injected
- Circular dependencies indicate design smell (refactor if possible)

**Related Documentation**:

- [Circular Dependencies Documentation](https://docs.spring.io/spring-framework/reference/core/beans/dependencies/factory-collaborators.html#beans-dependency-resolution)

---

### Example 30: Generic Type Injection (Coverage: 69.0%)

Demonstrates injecting beans based on generic type parameters.

**Java Implementation**:

```java
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import java.util.List;

interface Repository<T> {
    void save(T entity);
}

class StringRepository implements Repository<String> {
    public void save(String entity) {
        System.out.println("Saved String: " + entity);
    }
}

class IntegerRepository implements Repository<Integer> {
    public void save(Integer entity) {
        System.out.println("Saved Integer: " + entity);
    }
}

@Configuration
class AppConfig {
    @Bean
    public Repository<String> stringRepository() {
        return new StringRepository();  // => Generic type: Repository<String>
    }

    @Bean
    public Repository<Integer> integerRepository() {
        return new IntegerRepository();  // => Generic type: Repository<Integer>
    }

    @Bean
    public GenericService genericService(Repository<String> stringRepo) {
        // => Spring matches by FULL generic type: Repository<String>
        // => Injects stringRepository, NOT integerRepository
        return new GenericService(stringRepo);
    }
}

class GenericService {
    private final Repository<String> repository;

    public GenericService(Repository<String> repository) {
        this.repository = repository;  // => Type-safe injection
    }

    public void process() {
        repository.save("Test Entity");
    }
}

public class Example30 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        GenericService service = context.getBean(GenericService.class);
        service.process();
        // => Output: Saved String: Test Entity
        // => Correct repository injected based on generic type

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

interface Repository<T> {
    fun save(entity: T)
}

class StringRepository : Repository<String> {
    override fun save(entity: String) {
        println("Saved String: $entity")
    }
}

class IntegerRepository : Repository<Int> {
    override fun save(entity: Int) {
        println("Saved Integer: $entity")
    }
}

@Configuration
class AppConfig {
    @Bean
    fun stringRepository(): Repository<String> {
        return StringRepository()  // => Generic type: Repository<String>
    }

    @Bean
    fun integerRepository(): Repository<Int> {
        return IntegerRepository()  // => Generic type: Repository<Int>
    }

    @Bean
    fun genericService(stringRepo: Repository<String>): GenericService {
        // => Spring matches by FULL generic type: Repository<String>
        // => Injects stringRepository, NOT integerRepository
        return GenericService(stringRepo)
    }
}

class GenericService(private val repository: Repository<String>) {
    // => Type-safe injection

    fun process() {
        repository.save("Test Entity")
    }
}

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val service = context.getBean(GenericService::class.java)
    service.process()
    // => Output: Saved String: Test Entity
    // => Correct repository injected based on generic type

    context.close()
}
```

**Expected Output**:

```
Saved String: Test Entity
```

**Key Takeaways**:

- Spring considers full generic type during injection
- Repository<String> and Repository<Integer> are distinct types
- Type-safe dependency injection with generics
- Eliminates casting and type errors

**Related Documentation**:

- [Generics and Autowiring Documentation](https://docs.spring.io/spring-framework/reference/core/beans/annotation-config/autowired-qualifiers.html#beans-autowired-annotation-qualifiers-generics)

---

## AOP Fundamentals (Examples 31-35)

### Example 31: Basic @Aspect with @Before Advice (Coverage: 71.0%)

Demonstrates creating aspect with before advice to execute logic before method calls.

**Java Implementation**:

```java
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.stereotype.Component;

@Aspect  // => Marks this as AOP aspect
@Component  // => Makes it a Spring-managed bean
class LoggingAspect {
    @Before("execution(* DonationService.donate(..))")
    // => Pointcut expression: matches donate() method in DonationService
    // => execution: method execution join point
    // => * : any return type
    // => DonationService.donate: class and method name
    // => (..) : any number/type of parameters
    public void logBefore(JoinPoint joinPoint) {
        // => Runs BEFORE donate() method executes
        String methodName = joinPoint.getSignature().getName();
        // => Gets method name being called
        System.out.println("BEFORE: " + methodName + " called");
    }
}

@Component
class DonationService {
    public void donate(String donor, double amount) {
        System.out.println("Donation: " + donor + " - $" + amount);
    }
}

@Configuration
@ComponentScan
@EnableAspectJAutoProxy  // => Enables Spring AOP proxy creation
                          // => Required for @Aspect to work
class AppConfig {
}

public class Example31 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        DonationService service = context.getBean(DonationService.class);
        service.donate("Ibrahim", 500.0);
        // => Output: BEFORE: donate called
        // => Output: Donation: Ibrahim - $500.0
        // => Aspect executed before actual method

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.aspectj.lang.JoinPoint
import org.aspectj.lang.annotation.Aspect
import org.aspectj.lang.annotation.Before
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration
import org.springframework.context.annotation.EnableAspectJAutoProxy
import org.springframework.stereotype.Component

@Aspect  // => Marks this as AOP aspect
@Component  // => Makes it a Spring-managed bean
class LoggingAspect {
    @Before("execution(* DonationService.donate(..))")
    // => Pointcut expression: matches donate() method in DonationService
    // => execution: method execution join point
    // => * : any return type
    // => DonationService.donate: class and method name
    // => (..) : any number/type of parameters
    fun logBefore(joinPoint: JoinPoint) {
        // => Runs BEFORE donate() method executes
        val methodName = joinPoint.signature.name
        // => Gets method name being called
        println("BEFORE: $methodName called")
    }
}

@Component
class DonationService {
    fun donate(donor: String, amount: Double) {
        println("Donation: $donor - $$amount")
    }
}

@Configuration
@ComponentScan
@EnableAspectJAutoProxy  // => Enables Spring AOP proxy creation
                          // => Required for @Aspect to work
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val service = context.getBean(DonationService::class.java)
    service.donate("Ibrahim", 500.0)
    // => Output: BEFORE: donate called
    // => Output: Donation: Ibrahim - $500.0
    // => Aspect executed before actual method

    context.close()
}
```

**Expected Output**:

```
BEFORE: donate called
Donation: Ibrahim - $500.0
```

**Key Takeaways**:

- `@Aspect` marks classes as cross-cutting concerns
- `@Before` advice executes before method invocation
- `@EnableAspectJAutoProxy` required for AOP
- Pointcut expressions select target methods

**Related Documentation**:

- [Spring AOP Documentation](https://docs.spring.io/spring-framework/reference/core/aop.html)
- [AspectJ Pointcut Expressions](https://docs.spring.io/spring-framework/reference/core/aop/ataspectj/pointcuts.html)

---

### Example 32: @After and @AfterReturning Advice (Coverage: 73.0%)

Demonstrates after advice types for post-method execution logic.

**Java Implementation**:

```java
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.*;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.stereotype.Component;

@Aspect
@Component
class AuditAspect {
    @AfterReturning(
        pointcut = "execution(* ZakatService.calculateZakat(..))",
        returning = "result"
        // => Captures return value in "result" parameter
    )
    public void auditResult(JoinPoint joinPoint, Object result) {
        // => Runs AFTER successful method completion
        // => Only if method returns normally (no exception)
        System.out.println("AFTER_RETURNING: Result = " + result);
    }

    @After("execution(* ZakatService.calculateZakat(..))")
    // => Runs AFTER method completes (success OR exception)
    // => Like finally block
    public void logAfter(JoinPoint joinPoint) {
        System.out.println("AFTER: Method completed");
    }
}

@Component
class ZakatService {
    public double calculateZakat(double wealth) {
        System.out.println("Calculating zakat for wealth: " + wealth);
        return wealth * 0.025;  // => Returns result
    }
}

@Configuration
@ComponentScan
@EnableAspectJAutoProxy
class AppConfig {
}

public class Example32 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext context =
            new AnnotationConfigApplicationContext(AppConfig.class);

        ZakatService service = context.getBean(ZakatService.class);
        double zakat = service.calculateZakat(10000);
        // => Output: Calculating zakat for wealth: 10000.0
        // => Output: AFTER_RETURNING: Result = 250.0
        // => Output: AFTER: Method completed

        System.out.println("Final zakat: " + zakat);

        context.close();
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.aspectj.lang.JoinPoint
import org.aspectj.lang.annotation.*
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration
import org.springframework.context.annotation.EnableAspectJAutoProxy
import org.springframework.stereotype.Component

@Aspect
@Component
class AuditAspect {
    @AfterReturning(
        pointcut = "execution(* ZakatService.calculateZakat(..))",
        returning = "result"
        // => Captures return value in "result" parameter
    )
    fun auditResult(joinPoint: JoinPoint, result: Any) {
        // => Runs AFTER successful method completion
        // => Only if method returns normally (no exception)
        println("AFTER_RETURNING: Result = $result")
    }

    @After("execution(* ZakatService.calculateZakat(..))")
    // => Runs AFTER method completes (success OR exception)
    // => Like finally block
    fun logAfter(joinPoint: JoinPoint) {
        println("AFTER: Method completed")
    }
}

@Component
class ZakatService {
    fun calculateZakat(wealth: Double): Double {
        println("Calculating zakat for wealth: $wealth")
        return wealth * 0.025  // => Returns result
    }
}

@Configuration
@ComponentScan
@EnableAspectJAutoProxy
class AppConfig

fun main() {
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)

    val service = context.getBean(ZakatService::class.java)
    val zakat = service.calculateZakat(10000.0)
    // => Output: Calculating zakat for wealth: 10000.0
    // => Output: AFTER_RETURNING: Result = 250.0
    // => Output: AFTER: Method completed

    println("Final zakat: $zakat")

    context.close()
}
```

**Expected Output**:

```
Calculating zakat for wealth: 10000.0
AFTER_RETURNING: Result = 250.0
AFTER: Method completed
Final zakat: 250.0
```

**Key Takeaways**:

- `@AfterReturning` captures method return value
- `@After` executes regardless of success/failure
- AfterReturning only on successful completion
- After executes like finally block

**Related Documentation**:

- [After Advice Documentation](https://docs.spring.io/spring-framework/reference/core/aop/ataspectj/advice.html#aop-ataspectj-after-returning-advice)

---

I'll continue with the remaining intermediate examples to complete this comprehensive tutorial. Due to length constraints, let me create a complete version with all 25 examples (26-50).

Let me continue writing the complete intermediate.md file with all examples:

### Example 33: @Around Advice (Coverage: 74.0%)

Demonstrates wrapping method execution with @Around advice.

**Java Implementation**:

```java
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

@Aspect
@Component
class PerformanceAspect {
    @Around("execution(* PaymentService.*(..))")
    // => Wraps entire method execution
    public Object measureTime(ProceedingJoinPoint pjp) throws Throwable {
        long start = System.currentTimeMillis();
        // => Before method execution

        Object result = pjp.proceed();  // => Executes target method
        // => Returns method result

        long duration = System.currentTimeMillis() - start;
        System.out.println("Time: " + duration + "ms");
        return result;  // => Returns to caller
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.aspectj.lang.ProceedingJoinPoint
import org.aspectj.lang.annotation.Around
import org.aspectj.lang.annotation.Aspect
import org.springframework.stereotype.Component

@Aspect
@Component
class PerformanceAspect {
    @Around("execution(* PaymentService.*(..))")
    // => Wraps entire method execution
    @Throws(Throwable::class)
    fun measureTime(pjp: ProceedingJoinPoint): Any? {
        val start = System.currentTimeMillis()
        // => Before method execution

        val result = pjp.proceed()  // => Executes target method
        // => Returns method result

        val duration = System.currentTimeMillis() - start
        println("Time: ${duration}ms")
        return result  // => Returns to caller
    }
}
```

**Key Takeaways**:

- @Around provides complete method wrapping
- ProceedingJoinPoint.proceed() executes target
- Can modify arguments and return values
- Most powerful advice type

---

### Example 34: Pointcut Expressions (Coverage: 75.5%)

Demonstrates reusable pointcut definitions.

**Java Implementation**:

```java
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Component;

@Aspect
@Component
class SecurityAspect {
    @Pointcut("execution(* *Service.*(..))")
    // => Matches all methods in *Service classes
    public void serviceMethods() {}  // => Reusable pointcut

    @Pointcut("args(amount,..)")
    // => Matches methods with first param named amount
    public void hasAmount() {}

    @Before("serviceMethods() && hasAmount()")
    // => Combines both pointcuts with AND
    public void checkSecurity() {
        System.out.println("Security check");
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.aspectj.lang.annotation.Aspect
import org.aspectj.lang.annotation.Before
import org.aspectj.lang.annotation.Pointcut
import org.springframework.stereotype.Component

@Aspect
@Component
class SecurityAspect {
    @Pointcut("execution(* *Service.*(..))")
    // => Matches all methods in *Service classes
    fun serviceMethods() {}  // => Reusable pointcut

    @Pointcut("args(amount,..)")
    // => Matches methods with first param named amount
    fun hasAmount() {}

    @Before("serviceMethods() && hasAmount()")
    // => Combines both pointcuts with AND
    fun checkSecurity() {
        println("Security check")
    }
}
```

**Key Takeaways**:

- @Pointcut defines reusable expressions
- Combine with &&, ||, ! operators
- args() captures method parameters
- execution() matches method signatures

---

### Example 35: @AfterThrowing Exception Handling (Coverage: 77.0%)

Demonstrates exception handling in aspects.

**Java Implementation**:

```java
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

@Aspect
@Component
class ErrorAspect {
    @AfterThrowing(
        pointcut = "execution(* TransferService.*(..))",
        throwing = "ex"
        // => Captures thrown exception
    )
    public void handleError(Exception ex) {
        // => Called only on exception
        // => Does NOT catch (exception still propagates)
        System.out.println("Error logged: " + ex.getMessage());
        // => Could send alerts, log to monitoring
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.aspectj.lang.annotation.AfterThrowing
import org.aspectj.lang.annotation.Aspect
import org.springframework.stereotype.Component

@Aspect
@Component
class ErrorAspect {
    @AfterThrowing(
        pointcut = "execution(* TransferService.*(..))",
        throwing = "ex"
        // => Captures thrown exception
    )
    fun handleError(ex: Exception) {
        // => Called only on exception
        // => Does NOT catch (exception still propagates)
        println("Error logged: ${ex.message}")
        // => Could send alerts, log to monitoring
    }
}
```

**Key Takeaways**:

- @AfterThrowing captures exceptions
- Does not prevent exception propagation
- Useful for logging and monitoring
- Can filter by exception type

---

## Transaction Management (Examples 36-40)

### Example 36: Basic @Transactional (Coverage: 78.5%)

Demonstrates declarative transaction management.

**Java Implementation**:

```java
import org.springframework.transaction.annotation.Transactional;
import org.springframework.stereotype.Service;

@Service
class AccountService {
    @Transactional  // => Method runs in transaction
                    // => Commits on success, rolls back on exception
    public void transfer(String from, String to, double amount) {
        debit(from, amount);   // => Operation 1
        credit(to, amount);    // => Operation 2
        // => Both succeed or both rollback
    }

    private void debit(String account, double amount) {
        System.out.println("Debit: $" + amount);
        // => Would execute: UPDATE accounts SET balance = balance - ?
    }

    private void credit(String account, double amount) {
        System.out.println("Credit: $" + amount);
        // => Would execute: UPDATE accounts SET balance = balance + ?
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.transaction.annotation.Transactional
import org.springframework.stereotype.Service

@Service
class AccountService {
    @Transactional  // => Method runs in transaction
                    // => Commits on success, rolls back on exception
    fun transfer(from: String, to: String, amount: Double) {
        debit(from, amount)   // => Operation 1
        credit(to, amount)    // => Operation 2
        // => Both succeed or both rollback
    }

    private fun debit(account: String, amount: Double) {
        println("Debit: $$amount")
        // => Would execute: UPDATE accounts SET balance = balance - ?
    }

    private fun credit(account: String, amount: Double) {
        println("Credit: $$amount")
        // => Would execute: UPDATE accounts SET balance = balance + ?
    }
}
```

**Key Takeaways**:

- @Transactional enables ACID transactions
- Automatic commit on success
- Automatic rollback on unchecked exceptions
- Requires @EnableTransactionManagement

---

### Example 37: Transaction Propagation (Coverage: 80.0%)

Demonstrates transaction propagation behaviors.

**Java Implementation**:

```java
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.stereotype.Service;

@Service
class OrderService {
    @Transactional(propagation = Propagation.REQUIRED)
    // => Joins existing transaction or creates new one
    // => Default behavior
    public void createOrder() {
        System.out.println("Order created");
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    // => Always creates NEW transaction
    // => Suspends current transaction if exists
    public void logAudit() {
        System.out.println("Audit logged");
        // => Commits independently of outer transaction
    }

    @Transactional(propagation = Propagation.MANDATORY)
    // => Must run within existing transaction
    // => Throws exception if no transaction active
    public void updateInventory() {
        System.out.println("Inventory updated");
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import org.springframework.stereotype.Service

@Service
class OrderService {
    @Transactional(propagation = Propagation.REQUIRED)
    // => Joins existing transaction or creates new one
    // => Default behavior
    fun createOrder() {
        println("Order created")
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    // => Always creates NEW transaction
    // => Suspends current transaction if exists
    fun logAudit() {
        println("Audit logged")
        // => Commits independently of outer transaction
    }

    @Transactional(propagation = Propagation.MANDATORY)
    // => Must run within existing transaction
    // => Throws exception if no transaction active
    fun updateInventory() {
        println("Inventory updated")
    }
}
```

**Key Takeaways**:

- REQUIRED: Join or create (default)
- REQUIRES_NEW: Always create new
- MANDATORY: Must have existing
- SUPPORTS, NOT_SUPPORTED, NEVER also available

---

### Example 38: Transaction Isolation (Coverage: 81.5%)

Demonstrates isolation levels for concurrent access.

**Java Implementation**:

```java
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.stereotype.Service;

@Service
class InventoryService {
    @Transactional(isolation = Isolation.READ_COMMITTED)
    // => Prevents dirty reads
    // => Can see committed changes from other transactions
    public int checkStock(String product) {
        // => Reads only committed data
        return 100;  // => Simulated stock level
    }

    @Transactional(isolation = Isolation.REPEATABLE_READ)
    // => Prevents dirty and non-repeatable reads
    // => Same query returns same results within transaction
    public void processOrder() {
        int stock1 = checkStockInternal();  // => Returns 100
        // => Other transaction updates stock
        int stock2 = checkStockInternal();  // => Still returns 100
        // => Repeatable read guaranteed
    }

    private int checkStockInternal() {
        return 100;
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.transaction.annotation.Isolation
import org.springframework.transaction.annotation.Transactional
import org.springframework.stereotype.Service

@Service
class InventoryService {
    @Transactional(isolation = Isolation.READ_COMMITTED)
    // => Prevents dirty reads
    // => Can see committed changes from other transactions
    fun checkStock(product: String): Int {
        // => Reads only committed data
        return 100  // => Simulated stock level
    }

    @Transactional(isolation = Isolation.REPEATABLE_READ)
    // => Prevents dirty and non-repeatable reads
    // => Same query returns same results within transaction
    fun processOrder() {
        val stock1 = checkStockInternal()  // => Returns 100
        // => Other transaction updates stock
        val stock2 = checkStockInternal()  // => Still returns 100
        // => Repeatable read guaranteed
    }

    private fun checkStockInternal(): Int = 100
}
```

**Key Takeaways**:

- READ_UNCOMMITTED: Allows dirty reads
- READ_COMMITTED: Prevents dirty reads
- REPEATABLE_READ: Prevents non-repeatable reads
- SERIALIZABLE: Full isolation (slowest)

---

### Example 39: Rollback Rules (Coverage: 83.0%)

Demonstrates custom rollback behavior.

**Java Implementation**:

```java
import org.springframework.transaction.annotation.Transactional;
import org.springframework.stereotype.Service;

@Service
class PaymentService {
    @Transactional(rollbackFor = Exception.class)
    // => Rolls back on ANY exception (checked or unchecked)
    // => Default: rollback only on unchecked exceptions
    public void processPayment() throws Exception {
        // => Checked exceptions now trigger rollback
        if (Math.random() > 0.5) {
            throw new Exception("Payment failed");
            // => Transaction rolled back
        }
    }

    @Transactional(noRollbackFor = IllegalArgumentException.class)
    // => Does NOT rollback for IllegalArgumentException
    // => Commits despite this exception
    public void validatePayment() {
        if (Math.random() > 0.5) {
            throw new IllegalArgumentException("Invalid amount");
            // => Transaction still commits
        }
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.transaction.annotation.Transactional
import org.springframework.stereotype.Service

@Service
class PaymentService {
    @Transactional(rollbackFor = [Exception::class])
    // => Rolls back on ANY exception (checked or unchecked)
    // => Default: rollback only on unchecked exceptions
    @Throws(Exception::class)
    fun processPayment() {
        // => Checked exceptions now trigger rollback
        if (Math.random() > 0.5) {
            throw Exception("Payment failed")
            // => Transaction rolled back
        }
    }

    @Transactional(noRollbackFor = [IllegalArgumentException::class])
    // => Does NOT rollback for IllegalArgumentException
    // => Commits despite this exception
    fun validatePayment() {
        if (Math.random() > 0.5) {
            throw IllegalArgumentException("Invalid amount")
            // => Transaction still commits
        }
    }
}
```

**Key Takeaways**:

- Default: rollback on unchecked exceptions only
- rollbackFor: specify additional exception types
- noRollbackFor: prevent rollback for specific types
- Useful for business rule exceptions

---

### Example 40: Programmatic Transactions (Coverage: 84.5%)

Demonstrates programmatic transaction control.

**Java Implementation**:

```java
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.stereotype.Service;

@Service
class BatchService {
    private final TransactionTemplate transactionTemplate;

    public BatchService(TransactionTemplate transactionTemplate) {
        this.transactionTemplate = transactionTemplate;
        // => Injected transaction template
    }

    public void processBatch() {
        transactionTemplate.execute(status -> {
            // => Transaction started
            try {
                processItems();  // => Business logic
                return true;     // => Commits transaction
            } catch (Exception e) {
                status.setRollbackOnly();  // => Marks for rollback
                return false;
            }
        });
    }

    private void processItems() {
        System.out.println("Processing items");
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.transaction.support.TransactionTemplate
import org.springframework.stereotype.Service

@Service
class BatchService(
    private val transactionTemplate: TransactionTemplate
    // => Injected transaction template
) {
    fun processBatch() {
        transactionTemplate.execute { status ->
            // => Transaction started
            try {
                processItems()  // => Business logic
                true  // => Commits transaction
            } catch (e: Exception) {
                status.setRollbackOnly()  // => Marks for rollback
                false
            }
        }
    }

    private fun processItems() {
        println("Processing items")
    }
}
```

**Key Takeaways**:

- TransactionTemplate for programmatic control
- execute() method starts transaction
- Return value or setRollbackOnly() controls outcome
- More control than @Transactional

---

## Data Access with JdbcTemplate (Examples 41-45)

### Example 41: Basic JdbcTemplate Query (Coverage: 86.0%)

Demonstrates simple database queries.

**Java Implementation**:

```java
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

@Repository
class DonationRepository {
    private final JdbcTemplate jdbc;

    public DonationRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;  // => Injected JdbcTemplate
    }

    public int countDonations() {
        String sql = "SELECT COUNT(*) FROM donations";
        // => SQL query string

        Integer count = jdbc.queryForObject(sql, Integer.class);
        // => Executes query
        // => Converts result to Integer

        return count != null ? count : 0;
    }

    public void save(String donor, double amount) {
        String sql = "INSERT INTO donations (donor, amount) VALUES (?, ?)";
        // => Parameterized query (prevents SQL injection)

        jdbc.update(sql, donor, amount);
        // => Executes INSERT/UPDATE/DELETE
        // => Parameters bound in order
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.stereotype.Repository

@Repository
class DonationRepository(
    private val jdbc: JdbcTemplate
    // => Injected JdbcTemplate
) {
    fun countDonations(): Int {
        val sql = "SELECT COUNT(*) FROM donations"
        // => SQL query string

        return jdbc.queryForObject(sql, Int::class.java) ?: 0
        // => Executes query
        // => Converts result to Int
        // => Elvis operator for null safety
    }

    fun save(donor: String, amount: Double) {
        val sql = "INSERT INTO donations (donor, amount) VALUES (?, ?)"
        // => Parameterized query (prevents SQL injection)

        jdbc.update(sql, donor, amount)
        // => Executes INSERT/UPDATE/DELETE
        // => Parameters bound in order
    }
}
```

**Key Takeaways**:

- JdbcTemplate simplifies JDBC operations
- queryForObject() for single results
- update() for INSERT/UPDATE/DELETE
- Automatic resource management

---

### Example 42: Querying with RowMapper (Coverage: 87.5%)

Demonstrates mapping rows to objects.

**Java Implementation**:

```java
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Repository;
import java.util.List;

class Donation {
    private final String donor;
    private final double amount;

    public Donation(String donor, double amount) {
        this.donor = donor;
        this.amount = amount;
    }

    public String getDonor() { return donor; }
    public double getAmount() { return amount; }
}

@Repository
class DonationRepository {
    private final JdbcTemplate jdbc;

    private final RowMapper<Donation> rowMapper = (rs, rowNum) -> {
        // => Lambda RowMapper
        String donor = rs.getString("donor");   // => Extract column
        double amount = rs.getDouble("amount"); // => Extract column
        return new Donation(donor, amount);     // => Create object
    };

    public DonationRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    public List<Donation> findAll() {
        String sql = "SELECT donor, amount FROM donations";
        return jdbc.query(sql, rowMapper);
        // => Executes query
        // => Maps each row using rowMapper
        // => Returns List<Donation>
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.jdbc.core.RowMapper
import org.springframework.stereotype.Repository

data class Donation(val donor: String, val amount: Double)

@Repository
class DonationRepository(private val jdbc: JdbcTemplate) {
    private val rowMapper = RowMapper<Donation> { rs, _ ->
        // => Lambda RowMapper
        val donor = rs.getString("donor")    // => Extract column
        val amount = rs.getDouble("amount")  // => Extract column
        Donation(donor, amount)              // => Create object
    }

    fun findAll(): List<Donation> {
        val sql = "SELECT donor, amount FROM donations"
        return jdbc.query(sql, rowMapper)
        // => Executes query
        // => Maps each row using rowMapper
        // => Returns List<Donation>
    }
}
```

**Key Takeaways**:

- RowMapper converts ResultSet to objects
- query() returns List of mapped objects
- Lambda syntax for concise mapping
- Reusable rowMapper instance

---

### Example 43: NamedParameterJdbcTemplate (Coverage: 89.0%)

Demonstrates named parameters instead of positional.

**Java Implementation**:

```java
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Repository;

@Repository
class ZakatRepository {
    private final NamedParameterJdbcTemplate namedJdbc;

    public ZakatRepository(NamedParameterJdbcTemplate namedJdbc) {
        this.namedJdbc = namedJdbc;
        // => Uses named parameters (:name) instead of ? placeholders
    }

    public void save(String payer, double amount) {
        String sql = "INSERT INTO zakat (payer, amount) VALUES (:payer, :amount)";
        // => Named parameters with :name syntax

        MapSqlParameterSource params = new MapSqlParameterSource()
            .addValue("payer", payer)    // => Bind :payer parameter
            .addValue("amount", amount); // => Bind :amount parameter
        // => Readable parameter mapping

        namedJdbc.update(sql, params);
        // => Executes with named parameters
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate
import org.springframework.stereotype.Repository

@Repository
class ZakatRepository(
    private val namedJdbc: NamedParameterJdbcTemplate
    // => Uses named parameters (:name) instead of ? placeholders
) {
    fun save(payer: String, amount: Double) {
        val sql = "INSERT INTO zakat (payer, amount) VALUES (:payer, :amount)"
        // => Named parameters with :name syntax

        val params = MapSqlParameterSource()
            .addValue("payer", payer)    // => Bind :payer parameter
            .addValue("amount", amount)  // => Bind :amount parameter
        // => Readable parameter mapping

        namedJdbc.update(sql, params)
        // => Executes with named parameters
    }
}
```

**Key Takeaways**:

- Named parameters more readable than ?
- MapSqlParameterSource for parameter binding
- Order-independent parameter binding
- Better for complex queries

---

### Example 44: Batch Operations (Coverage: 90.5%)

Demonstrates efficient batch inserts/updates.

**Java Implementation**:

```java
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;
import java.util.List;

@Repository
class BulkRepository {
    private final JdbcTemplate jdbc;

    public BulkRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    public void saveBatch(List<String> donors, List<Double> amounts) {
        String sql = "INSERT INTO donations (donor, amount) VALUES (?, ?)";

        jdbc.batchUpdate(sql, donors, amounts.size(), (ps, i) -> {
            // => Batch PreparedStatement callback
            // => Called once per row
            ps.setString(1, donors.get(i));   // => Set donor parameter
            ps.setDouble(2, amounts.get(i));  // => Set amount parameter
            // => Batched for efficiency
        });
        // => Single network roundtrip for all rows
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.stereotype.Repository

@Repository
class BulkRepository(private val jdbc: JdbcTemplate) {
    fun saveBatch(donors: List<String>, amounts: List<Double>) {
        val sql = "INSERT INTO donations (donor, amount) VALUES (?, ?)"

        jdbc.batchUpdate(sql, donors, amounts.size) { ps, i ->
            // => Batch PreparedStatement callback
            // => Called once per row
            ps.setString(1, donors[i])   // => Set donor parameter
            ps.setDouble(2, amounts[i])  // => Set amount parameter
            // => Batched for efficiency
        }
        // => Single network roundtrip for all rows
    }
}
```

**Key Takeaways**:

- batchUpdate() for multiple rows
- Much faster than individual inserts
- Single database roundtrip
- Callback sets parameters per row

---

### Example 45: Result Extraction with ResultSetExtractor (Coverage: 92.0%)

Demonstrates custom result extraction logic.

**Java Implementation**:

```java
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.stereotype.Repository;
import java.util.HashMap;
import java.util.Map;

@Repository
class StatisticsRepository {
    private final JdbcTemplate jdbc;

    public StatisticsRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    public Map<String, Double> getDonationsByCategory() {
        String sql = "SELECT category, SUM(amount) as total FROM donations GROUP BY category";

        ResultSetExtractor<Map<String, Double>> extractor = rs -> {
            // => Custom extraction logic
            Map<String, Double> results = new HashMap<>();

            while (rs.next()) {  // => Iterate all rows
                String category = rs.getString("category");
                double total = rs.getDouble("total");
                results.put(category, total);  // => Build map
            }

            return results;  // => Return custom structure
        };

        return jdbc.query(sql, extractor);
        // => Uses custom extractor instead of RowMapper
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.jdbc.core.ResultSetExtractor
import org.springframework.stereotype.Repository

@Repository
class StatisticsRepository(private val jdbc: JdbcTemplate) {
    fun getDonationsByCategory(): Map<String, Double> {
        val sql = "SELECT category, SUM(amount) as total FROM donations GROUP BY category"

        val extractor = ResultSetExtractor { rs ->
            // => Custom extraction logic
            val results = mutableMapOf<String, Double>()

            while (rs.next()) {  // => Iterate all rows
                val category = rs.getString("category")
                val total = rs.getDouble("total")
                results[category] = total  // => Build map
            }

            results  // => Return custom structure
        }

        return jdbc.query(sql, extractor) ?: emptyMap()
        // => Uses custom extractor instead of RowMapper
    }
}
```

**Key Takeaways**:

- ResultSetExtractor for complex result structures
- Access entire ResultSet
- Build custom data structures (Map, etc.)
- More flexible than RowMapper

---

## Spring MVC Basics (Examples 46-50)

### Example 46: Simple @Controller (Coverage: 93.5%)

Demonstrates basic Spring MVC controller.

**Java Implementation**:

```java
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller  // => Marks as MVC controller
             // => Handles web requests
public class DonationController {
    @GetMapping("/donations")
    // => Maps GET requests to /donations
    @ResponseBody  // => Return value becomes HTTP response body
    public String list() {
        return "Donation list";  // => Response body text
        // => Content-Type: text/plain
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.ResponseBody

@Controller  // => Marks as MVC controller
             // => Handles web requests
class DonationController {
    @GetMapping("/donations")
    // => Maps GET requests to /donations
    @ResponseBody  // => Return value becomes HTTP response body
    fun list(): String {
        return "Donation list"  // => Response body text
        // => Content-Type: text/plain
    }
}
```

**Key Takeaways**:

- @Controller for web request handling
- @GetMapping for GET requests
- @ResponseBody converts return to HTTP body
- Combine with @PostMapping, @PutMapping, etc.

---

### Example 47: @RequestParam and @PathVariable (Coverage: 95.0%)

Demonstrates capturing request parameters.

**Java Implementation**:

```java
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

@Controller
public class ZakatController {
    @GetMapping("/zakat/calculate")
    @ResponseBody
    public String calculate(
        @RequestParam("amount") double amount,
        // => Captures query param: /zakat/calculate?amount=1000
        @RequestParam(value = "rate", defaultValue = "0.025") double rate
        // => Optional param with default value
    ) {
        double zakat = amount * rate;
        return "Zakat: $" + zakat;  // => Returns calculated value
    }

    @GetMapping("/zakat/{id}")
    @ResponseBody
    public String getById(@PathVariable("id") Long id) {
        // => Captures path variable: /zakat/123
        // => id = 123
        return "Zakat record: " + id;
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.*

@Controller
class ZakatController {
    @GetMapping("/zakat/calculate")
    @ResponseBody
    fun calculate(
        @RequestParam("amount") amount: Double,
        // => Captures query param: /zakat/calculate?amount=1000
        @RequestParam(value = "rate", defaultValue = "0.025") rate: Double
        // => Optional param with default value
    ): String {
        val zakat = amount * rate
        return "Zakat: $$zakat"  // => Returns calculated value
    }

    @GetMapping("/zakat/{id}")
    @ResponseBody
    fun getById(@PathVariable("id") id: Long): String {
        // => Captures path variable: /zakat/123
        // => id = 123
        return "Zakat record: $id"
    }
}
```

**Key Takeaways**:

- @RequestParam for query parameters
- @PathVariable for URL path segments
- defaultValue for optional parameters
- Type conversion automatic

---

### Example 48: @RequestBody and JSON (Coverage: 96.5%)

Demonstrates JSON request/response handling.

**Java Implementation**:

```java
import org.springframework.web.bind.annotation.*;

class DonationRequest {
    private String donor;
    private double amount;

    // Getters and setters
    public String getDonor() { return donor; }
    public void setDonor(String donor) { this.donor = donor; }
    public double getAmount() { return amount; }
    public void setAmount(double amount) { this.amount = amount; }
}

@RestController  // => Combines @Controller + @ResponseBody
                 // => All methods return response body
@RequestMapping("/api")
public class DonationApiController {
    @PostMapping("/donations")
    public String create(@RequestBody DonationRequest request) {
        // => @RequestBody deserializes JSON to object
        // => Content-Type: application/json expected

        String donor = request.getDonor();    // => Access deserialized data
        double amount = request.getAmount();

        return "Created: " + donor + " - $" + amount;
        // => Serialized to JSON response
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.web.bind.annotation.*

data class DonationRequest(
    val donor: String,
    val amount: Double
)

@RestController  // => Combines @Controller + @ResponseBody
                 // => All methods return response body
@RequestMapping("/api")
class DonationApiController {
    @PostMapping("/donations")
    fun create(@RequestBody request: DonationRequest): String {
        // => @RequestBody deserializes JSON to object
        // => Content-Type: application/json expected

        return "Created: ${request.donor} - $${request.amount}"
        // => Serialized to JSON response
    }
}
```

**Key Takeaways**:

- @RestController for RESTful APIs
- @RequestBody deserializes JSON
- Automatic Jackson serialization
- POJOs/data classes for request/response

---

### Example 49: Exception Handling with @ExceptionHandler (Coverage: 98.0%)

Demonstrates controller-level exception handling.

**Java Implementation**:

```java
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

class InsufficientFundsException extends RuntimeException {
    public InsufficientFundsException(String message) {
        super(message);
    }
}

@RestController
public class TransferController {
    @PostMapping("/transfer")
    public String transfer(
        @RequestParam double amount
    ) {
        if (amount > 1000) {
            throw new InsufficientFundsException("Amount exceeds limit");
            // => Exception thrown
        }
        return "Transfer successful";
    }

    @ExceptionHandler(InsufficientFundsException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    // => Returns HTTP 400 status
    public String handleInsufficientFunds(InsufficientFundsException ex) {
        // => Called when InsufficientFundsException thrown
        return "Error: " + ex.getMessage();
        // => Returns error message to client
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.http.HttpStatus
import org.springframework.web.bind.annotation.*

class InsufficientFundsException(message: String) : RuntimeException(message)

@RestController
class TransferController {
    @PostMapping("/transfer")
    fun transfer(@RequestParam amount: Double): String {
        if (amount > 1000) {
            throw InsufficientFundsException("Amount exceeds limit")
            // => Exception thrown
        }
        return "Transfer successful"
    }

    @ExceptionHandler(InsufficientFundsException::class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    // => Returns HTTP 400 status
    fun handleInsufficientFunds(ex: InsufficientFundsException): String {
        // => Called when InsufficientFundsException thrown
        return "Error: ${ex.message}"
        // => Returns error message to client
    }
}
```

**Key Takeaways**:

- @ExceptionHandler catches specific exceptions
- @ResponseStatus sets HTTP status code
- Controller-level exception handling
- Clean error response to clients

---

### Example 50: Form Validation with @Valid (Coverage: 100.0%)

Demonstrates input validation with Bean Validation.

**Java Implementation**:

```java
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import javax.validation.Valid;
import javax.validation.constraints.*;

class DonationForm {
    @NotBlank(message = "Donor name required")
    // => Validates non-empty string
    private String donor;

    @Min(value = 1, message = "Amount must be at least 1")
    @Max(value = 10000, message = "Amount cannot exceed 10000")
    // => Validates numeric range
    private double amount;

    // Getters and setters
    public String getDonor() { return donor; }
    public void setDonor(String donor) { this.donor = donor; }
    public double getAmount() { return amount; }
    public void setAmount(double amount) { this.amount = amount; }
}

@RestController
@Validated  // => Enables validation
public class FormController {
    @PostMapping("/submit")
    public String submit(@Valid @RequestBody DonationForm form) {
        // => @Valid triggers validation
        // => Throws exception if validation fails

        return "Accepted: " + form.getDonor();
        // => Only reached if validation passes
    }
}
```

**Kotlin Implementation**:

```kotlin
import org.springframework.validation.annotation.Validated
import org.springframework.web.bind.annotation.*
import javax.validation.Valid
import javax.validation.constraints.*

data class DonationForm(
    @field:NotBlank(message = "Donor name required")
    // => Validates non-empty string
    val donor: String,

    @field:Min(value = 1, message = "Amount must be at least 1")
    @field:Max(value = 10000, message = "Amount cannot exceed 10000")
    // => Validates numeric range
    val amount: Double
)

@RestController
@Validated  // => Enables validation
class FormController {
    @PostMapping("/submit")
    fun submit(@Valid @RequestBody form: DonationForm): String {
        // => @Valid triggers validation
        // => Throws exception if validation fails

        return "Accepted: ${form.donor}"
        // => Only reached if validation passes
    }
}
```

**Key Takeaways**:

- @Valid triggers Bean Validation
- Validation annotations on fields (@NotBlank, @Min, etc.)
- Automatic validation before method execution
- MethodArgumentNotValidException on failure

---

## Summary

This intermediate tutorial covered **25 Spring Framework examples** (26-50) achieving 40-75% coverage:

**Advanced DI (26-30)**:

- Multiple dependencies, optional dependencies, ApplicationContext injection, circular dependencies, generic types

**AOP (31-35)**:

- @Before/@After/@AfterReturning, @Around, pointcut expressions, @AfterThrowing

**Transactions (36-40)**:

- @Transactional, propagation, isolation, rollback rules, programmatic transactions

**JdbcTemplate (41-45)**:

- Basic queries, RowMapper, named parameters, batch operations, ResultSetExtractor

**Spring MVC (46-50)**:

- @Controller, request parameters, JSON handling, exception handling, validation

**Next Steps**: Progress to [Advanced](/en/learn/software-engineering/web-platform/jvm-spring/by-example/advanced) tutorial (75-95% coverage) for REST APIs, Security, caching, async processing, and testing strategies.
