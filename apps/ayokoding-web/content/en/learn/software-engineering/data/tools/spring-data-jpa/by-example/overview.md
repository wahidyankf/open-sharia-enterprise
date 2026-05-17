---
title: "Overview"
date: 2026-01-01T22:52:24+07:00
draft: false
weight: 10000000
description: "Learn Spring Data JPA through 85+ annotated code examples covering 95% of the framework - ideal for experienced developers building production data access layers"
tags: ["spring-data-jpa", "tutorial", "by-example", "examples", "code-first", "jpa", "hibernate", "orm"]
---

## What is Spring Data JPA By Example?

**Spring Data JPA By Example** is a code-first tutorial series teaching experienced Java developers how to build production-ready data access layers using Spring Data JPA. Through 85 heavily annotated, self-contained examples, you'll achieve 95% coverage of Spring Data JPA patterns—from basic CRUD operations to advanced Specifications, Criteria API, and performance optimization.

This tutorial assumes you're an experienced developer familiar with Java, Spring Framework basics, and relational databases. If you're new to Java or Spring, start with foundational tutorials first.

## Why By Example?

**Philosophy**: Show the code first, run it second, understand through direct interaction.

Traditional tutorials explain concepts then show code. By-example tutorials reverse this: every example is a working, runnable code snippet with inline annotations showing exactly what happens at each step—entity states, SQL queries executed, results returned, and common pitfalls.

**Target Audience**: Experienced developers who:

- Already know Java and Spring Framework fundamentals
- Understand relational databases and SQL
- Prefer learning through working code rather than narrative explanations
- Want comprehensive reference material covering 95% of production patterns

**Not For**: Developers new to Java, Spring, or databases. This tutorial moves quickly and assumes foundational knowledge.

## What Does 95% Coverage Mean?

**95% coverage** means depth and breadth of Spring Data JPA features needed for production work, not toy examples.

### Included in 95% Coverage

- **Repository Patterns**: JpaRepository hierarchy, CRUD operations, batch operations, custom repositories
- **Query Methods**: Derived query methods, @Query with JPQL and native SQL, named/positional parameters
- **Relationships**: All relationship types (@OneToOne, @OneToMany, @ManyToOne, @ManyToMany), cascading, fetch strategies
- **Pagination & Sorting**: Page vs Slice, multi-field sorting, cursor-based pagination
- **Transactions**: Propagation levels, isolation, rollback rules, read-only optimization
- **Locking**: Optimistic (@Version), pessimistic (PESSIMISTIC_READ/WRITE)
- **Dynamic Queries**: Specifications API, Criteria API, QueryDSL integration
- **Performance**: EntityGraph, JOIN FETCH, N+1 problem solutions, batch fetching, projections
- **Auditing**: @CreatedDate, @LastModifiedDate, @CreatedBy, @LastModifiedBy
- **Entity Lifecycle**: States (transient/managed/detached/removed), callbacks (@PrePersist, @PostUpdate)
- **Advanced Mapping**: Embedded types, composite keys, inheritance strategies
- **Production Patterns**: DTO projections, interface projections, query hints

### Excluded from 95% (the remaining 5%)

- **Framework Internals**: Hibernate SessionFactory implementation details, EntityManager proxy mechanics
- **Rare Edge Cases**: Obscure combination of features not used in typical production code
- **Legacy Features**: Deprecated APIs, XML-based mappings (focus on annotation-based)
- **Platform-Specific**: Database-specific optimizations beyond standard JPA
- **Advanced Hibernate**: Hibernate-specific features outside JPA specification (unless commonly used)

## Tutorial Structure

### 85 Examples Across Three Levels

**Sequential numbering**: Examples 1-85 (unified reference system)

**Distribution**:

- **Beginner** (Examples 1-30): 0-40% coverage - Repository basics, simple queries, relationships, entity fundamentals
- **Intermediate** (Examples 31-60): 40-75% coverage - @Query annotation, pagination, advanced relationships, transactions, locking
- **Advanced** (Examples 61-85): 75-95% coverage - Specifications, Criteria API, custom repositories, auditing, performance optimization

**Rationale**: 85 examples provide granular progression from CRUD operations to expert mastery without overwhelming maintenance burden.

## Five-Part Example Format

Every example follows a **mandatory five-part structure**:

### Part 1: Brief Explanation (2-3 sentences)

**Answers**:

- What is this concept/pattern?
- Why does it matter in production code?
- When should you use it?

**Example**:

> ### Example 12: @OneToMany Relationship with Cascade
>
> The @OneToMany relationship maps a single entity to multiple related entities, commonly used for parent-child relationships like User → Posts or Order → OrderItems. Cascade operations (PERSIST, MERGE, REMOVE) automatically propagate entity state changes from parent to children, reducing boilerplate code and preventing orphaned records in production databases.

### Part 2: Mermaid Diagram (when appropriate)

**Included when** (~40% of examples):

- Data flow between repository and database is non-obvious
- Entity relationships involve multiple tables
- Transaction boundaries or persistence context lifecycle needs visualization
- N+1 problems or lazy loading behavior requires illustration
- Query execution flow spans multiple layers

**Skipped when**:

- Simple CRUD operations with obvious flow
- Single-method examples with linear execution
- Concept is clearer from code alone

**All diagrams use color-blind friendly palette**: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161 (never red/green/yellow).

### Part 3: Heavily Annotated Code

**Core requirement**: Every significant line has inline comment with `// =>` notation.

**Annotations show**:

- **Entity states**: `// => Entity state: TRANSIENT/MANAGED/DETACHED/REMOVED`
- **SQL queries**: `// => SQL: INSERT INTO users (name, age) VALUES ('Alice', 25)`
- **Query results**: `// => Result: List<User>[User(id=1), User(id=2)]`
- **Lazy loading**: `// => Lazy initialization triggered: SELECT * FROM posts WHERE user_id=1`
- **Transaction boundaries**: `// => Transaction begins/commits/rolls back`
- **Performance issues**: `// => N+1 problem: 101 queries executed!`

**Example**:

```java
// Entity creation
User user = new User("Alice", 25);
// => Entity state: TRANSIENT (not yet tracked by persistence context)
// => No database interaction

// Persist entity
User saved = userRepository.save(user);
// => SQL: INSERT INTO users (name, age) VALUES ('Alice', 25)
// => Entity state: MANAGED (id=1, tracked by persistence context)
// => Result: User(id=1, name="Alice", age=25)

// Query by ID
Optional<User> found = userRepository.findById(1L);
// => SQL: SELECT * FROM users WHERE id = 1
// => Result: Optional[User(id=1, name="Alice", age=25)]
// => Entity state: MANAGED (same instance as 'saved' if in same transaction)
```

### Part 4: Key Takeaway (1-2 sentences)

**Highlights**:

- Most important pattern or concept
- When to apply in production
- Common pitfalls to avoid

**Example**:

> **Key Takeaway**: Use `CascadeType.ALL` only when child entities have no meaning outside the parent relationship (true composition). For looser associations, use specific cascade types (PERSIST, MERGE) to avoid accidental deletions in production.

### Part 5: Why It Matters (2-3 sentences, 50-100 words)

**Purpose**: Connect the concept to production relevance and real-world impact

**Must explain**:

- Why professionals care about this in real systems (sentence 1: production relevance)
- How it compares to alternatives or what problems it solves (sentence 2: comparative insight)
- Consequences for quality/performance/safety/scalability (sentence 3: practical impact)

**Example**:

> **Why It Matters**: Cascade operations prevent orphaned database records in production systems, automatically maintaining referential integrity when parent entities are deleted. Unlike manual cleanup with DELETE queries, cascades ensure atomic operations within a transaction, preventing partial failures that corrupt data. Proper cascade configuration reduces boilerplate code by 40-60% compared to manually managing entity lifecycles, while preventing memory leaks from forgotten cleanup operations.

## Self-Containment Rules

**Golden rule**: If you delete all other examples, this example should still compile and run.

### Beginner Level (Examples 1-30)

**Rule**: Each example is completely standalone.

**Requirements**:

- Full entity class definitions with all annotations
- Complete repository interfaces
- All necessary Spring configuration
- Runnable test class with setup and execution
- No references to other examples' code

**Allowed**: Conceptual references ("as we saw with relationships") but code must be self-contained.

### Intermediate Level (Examples 31-60)

**Rule**: Assumes beginner concepts but includes all necessary code.

**Allowed assumptions**:

- Reader knows basic entity mapping and CRUD operations
- Reader understands fundamental relationship types
- Reader can define simple repository interfaces

**Requirements**:

- Full runnable code with all entities and repositories
- Can reference beginner patterns conceptually without repeating explanations
- Must be runnable without looking at previous examples
- Include all configuration needed for advanced features

### Advanced Level (Examples 61-85)

**Rule**: Assumes beginner + intermediate knowledge but remains runnable.

**Allowed assumptions**:

- Reader knows entity mapping, relationships, and query methods
- Reader understands transactions and pagination
- Reader can navigate JPA documentation for context

**Requirements**:

- Full runnable code including entities, repositories, and configuration
- Can reference patterns by name ("using Specifications from earlier examples")
- Include all interfaces, implementations, and setup code
- Provide complete working example even when building on concepts

## Educational Comment Standards

### Entity State Annotations

```java
// Creation (transient)
User user = new User("Bob", 30);
// => Entity state: TRANSIENT (no ID, not tracked)

// Persistence (managed)
User saved = userRepository.save(user);
// => SQL: INSERT INTO users (name, age) VALUES ('Bob', 30)
// => Entity state: MANAGED (id=1, persistence context tracking changes)

// Detachment (detached)
entityManager.detach(saved);
// => Entity state: DETACHED (id=1, changes not tracked)

// Removal (removed)
userRepository.delete(saved);
// => SQL: DELETE FROM users WHERE id = 1
// => Entity state: REMOVED (scheduled for deletion)
```

### SQL Query Annotations

```java
// Single query
User user = userRepository.findById(1L).orElse(null);
// => SQL: SELECT * FROM users WHERE id = 1
// => Result: User(id=1, name="Alice", age=25)

// Multiple queries (N+1 problem)
List<User> users = userRepository.findAll();
// => 1 query: SELECT * FROM users (fetched 100 users)

users.forEach(u -> System.out.println(u.getPosts()));
// => 100 queries: SELECT * FROM posts WHERE user_id = ? (one per user!)
// => Total: 101 queries executed! (N+1 problem)
```

### Relationship Loading Annotations

```java
@Entity
public class User {
    @OneToMany(fetch = FetchType.LAZY, mappedBy = "user")
    private List<Post> posts = new ArrayList<>();
    // => Lazy: posts not loaded until accessed
}

// Query user
User user = userRepository.findById(1L).get();
// => SQL: SELECT * FROM users WHERE id = 1
// => posts collection: NOT loaded (proxy placeholder)

// Access lazy collection
List<Post> posts = user.getPosts();
// => Lazy initialization triggered
// => SQL: SELECT * FROM posts WHERE user_id = 1
// => posts collection: NOW loaded with actual data
```

### Transaction Boundary Annotations

```java
@Transactional
public void transferMoney(Long fromId, Long toId, BigDecimal amount) {
    // => Transaction begins (isolation level: READ_COMMITTED)

    Account from = accountRepository.findById(fromId).orElseThrow();
    // => SQL: SELECT * FROM accounts WHERE id = ? (with read lock if needed)

    Account to = accountRepository.findById(toId).orElseThrow();
    // => SQL: SELECT * FROM accounts WHERE id = ?

    from.debit(amount);
    to.credit(amount);
    // => Entity state changes tracked (dirty checking enabled)

    // => Transaction commits (flushes changes to database)
    // => SQL: UPDATE accounts SET balance = ? WHERE id = ? (two updates)
}
```

## Diagram Guidelines

### When to Include Diagrams

**INCLUDE diagram when**:

- Entity relationships span multiple tables (OneToMany, ManyToMany)
- Query execution flow involves repository → EntityManager → database
- Lazy loading or N+1 problems need visualization
- Transaction boundaries or propagation levels affect behavior
- Cascade operations propagate across entity graph
- Specification composition builds complex queries

**SKIP diagram when**:

- Simple CRUD operation (save, findById, delete)
- Single-field query derivation (findByName)
- Trivial method invocation with obvious result

### Diagram Frequency Target

**Guideline**: ~40% of examples (34 diagrams across 85 examples)

- **Beginner**: ~30% (12 diagrams in 30 examples) - Simpler concepts, fewer diagrams needed
- **Intermediate**: ~40% (12 diagrams in 30 examples) - Relationships and transactions benefit from visualization
- **Advanced**: ~40% (10 diagrams in 25 examples) - Complex patterns require visual aids

## Coverage Progression

### Beginner (0-40% coverage, Examples 1-30)

**Focus**: Repository basics, simple queries, relationships, entity fundamentals

**Topics**:

- JpaRepository hierarchy and CRUD operations
- Query derivation (findBy, comparison operators, string queries)
- Basic relationships (@OneToOne, @OneToMany, @ManyToOne)
- Cascade types and fetch strategies
- Entity lifecycle states
- ID generation strategies
- Column mapping and type conversions

**Example count**: 30 examples in 4 groups

### Intermediate (40-75% coverage, Examples 31-60)

**Focus**: Custom queries, pagination, advanced relationships, transactions

**Topics**:

- @Query annotation with JPQL and native SQL
- Named and positional parameters
- Pagination (Page vs Slice) and sorting
- @ManyToMany relationships and @JoinTable
- N+1 problem detection and solutions
- Transaction management (@Transactional, propagation, isolation)
- Optimistic and pessimistic locking

**Example count**: 30 examples in 5 groups

### Advanced (75-95% coverage, Examples 61-85)

**Focus**: Dynamic queries, custom repositories, performance optimization

**Topics**:

- Specifications API for dynamic queries
- Criteria API with CriteriaBuilder
- Custom repository implementations with EntityManager
- Auditing (@CreatedDate, @LastModifiedDate, @CreatedBy, @LastModifiedBy)
- Entity lifecycle callbacks (@PrePersist, @PostUpdate, @EntityListeners)
- DTO and interface projections
- Query hints and batch fetching
- QueryDSL integration basics

**Example count**: 25 examples in 5 groups

## How to Use This Tutorial

### Step 1: Choose Your Starting Level

- **New to Spring Data JPA?** Start at Example 1 (Beginner)
- **Know basic repositories?** Start at Example 31 (Intermediate)
- **Need advanced patterns?** Jump to Example 61 (Advanced)

### Step 2: Run Every Example

Each example is copy-paste-runnable. Follow this workflow:

1. **Copy the entity classes** (complete with annotations)
2. **Copy the repository interface** (or custom implementation)
3. **Copy the test class** (includes setup and execution)
4. **Run the test** and observe the output
5. **Modify values** and re-run to see different behavior

### Step 3: Read Annotations Carefully

The `// =>` annotations are the core learning material. They show:

- SQL queries executed (what actually hits the database)
- Entity states (transient/managed/detached/removed)
- Results returned (actual data structures)
- Performance issues (N+1 queries, unnecessary selects)
- Transaction behavior (begin/commit/rollback)

### Step 4: Experiment and Break Things

The best way to learn:

- Change fetch types (LAZY → EAGER) and observe query count
- Remove cascade types and see orphan records
- Modify transaction propagation and watch behavior change
- Trigger optimistic locking exceptions intentionally

### Step 5: Use as Reference Material

This tutorial is designed for:

- **Quick lookup**: "How do I do pessimistic locking again?"
- **Pattern reference**: "What's the right way to handle N+1 queries?"
- **Production guidance**: "Should I use Specifications or @Query?"

## Prerequisites

### Required Knowledge

- **Java**: Generics, lambdas, streams, annotations, Optional
- **Spring Framework**: Dependency injection, component scanning, configuration
- **SQL**: SELECT, INSERT, UPDATE, DELETE, JOINs, indexes
- **Databases**: Relational model, primary keys, foreign keys, constraints
- **Maven/Gradle**: Dependency management, build lifecycle

### Required Tools

- **JDK**: 17 or higher (examples use Java 17+ features)
- **Build Tool**: Maven 3.8+ or Gradle 7.0+
- **IDE**: IntelliJ IDEA, Eclipse, or VS Code with Java extensions
- **Database**: H2 (in-memory, used in examples) or PostgreSQL/MySQL
- **Spring Boot**: 3.0+ (examples use Spring Boot 3.x)

### Dependencies Setup

All examples assume this minimal `pom.xml` configuration:

```xml
<dependencies>
    <!-- Spring Data JPA -->
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-data-jpa</artifactId>
    </dependency>

    <!-- H2 Database (in-memory for testing) -->
    <dependency>
        <groupId>com.h2database</groupId>
        <artifactId>h2</artifactId>
        <scope>runtime</scope>
    </dependency>

    <!-- Testing -->
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-test</artifactId>
        <scope>test</scope>
    </dependency>
</dependencies>
```

## Common Patterns You'll Learn

### Pattern 1: Repository-Based Data Access

```java
// Define entity
@Entity
public class Product {
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;
    private BigDecimal price;
}

// Define repository (no implementation needed!)
public interface ProductRepository extends JpaRepository<Product, Long> {
    List<Product> findByPriceGreaterThan(BigDecimal price);
}

// Use in service
@Service
public class ProductService {
    @Autowired
    private ProductRepository repository;

    public List<Product> findExpensiveProducts(BigDecimal threshold) {
        return repository.findByPriceGreaterThan(threshold);
        // => SQL: SELECT * FROM product WHERE price > ?
    }
}
```

### Pattern 2: Query Derivation vs @Query

```java
// Query derivation (method name defines query)
List<User> findByNameAndAgeGreaterThan(String name, int age);
// => SQL: SELECT * FROM users WHERE name = ? AND age > ?

// @Query annotation (explicit JPQL)
@Query("SELECT u FROM User u WHERE u.name = :name AND u.age > :age")
List<User> findByNameAndMinAge(@Param("name") String name, @Param("age") int age);
// => Same SQL, but explicit and supports complex queries
```

### Pattern 3: Lazy Loading and N+1 Prevention

```java
// BAD: N+1 queries
List<User> users = userRepository.findAll();
// => 1 query: SELECT * FROM users
users.forEach(u -> System.out.println(u.getPosts()));
// => N queries: SELECT * FROM posts WHERE user_id = ? (one per user!)

// GOOD: JOIN FETCH
@Query("SELECT u FROM User u LEFT JOIN FETCH u.posts")
List<User> findAllWithPosts();
// => 1 query: SELECT * FROM users u LEFT JOIN posts p ON u.id = p.user_id
```

### Pattern 4: Dynamic Queries with Specifications

```java
// Build query dynamically based on criteria
Specification<User> spec = Specification.where(null);

if (name != null) {
    spec = spec.and((root, query, cb) -> cb.equal(root.get("name"), name));
}

if (minAge != null) {
    spec = spec.and((root, query, cb) -> cb.greaterThan(root.get("age"), minAge));
}

List<User> users = userRepository.findAll(spec);
// => SQL: SELECT * FROM users WHERE name = ? AND age > ? (only includes non-null criteria)
```

## What Makes This Tutorial Different?

### Code-First Philosophy

Traditional tutorials: Explain → Show code → Run
**By-example approach**: Show code → Run → Understand through annotation

Every example is designed to be:

1. **Copied** into your IDE
2. **Run** immediately (no missing dependencies or setup)
3. **Modified** to experiment with different values
4. **Referenced** when building production code

### Production-Ready Patterns

Examples aren't toy demonstrations—they're patterns used in real applications:

- Transaction boundaries that prevent data corruption
- Lazy loading strategies that avoid performance issues
- Locking mechanisms that handle concurrent access
- Projection techniques that reduce memory overhead

### Comprehensive Coverage

95% coverage means you won't hit "how do I do X?" roadblocks:

- All relationship types with cascade and fetch strategies
- All query methods (derivation, @Query, Specifications, Criteria API)
- All transaction behaviors (propagation, isolation, rollback)
- All performance optimizations (JOIN FETCH, EntityGraph, batch fetching)

### Self-Contained Examples

Every example includes:

- Complete entity class definitions
- Full repository interfaces or custom implementations
- Working test class with setup and assertions
- All necessary Spring configuration

No "assume you have this from Example 12" references—each example stands alone.

## Navigating the Tutorial

### Three Level Pages

1. **[Beginner](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner)** - Examples 1-30 (0-40% coverage)
   - Repository basics and CRUD operations
   - Simple query derivation
   - Basic relationships (@OneToOne, @OneToMany, @ManyToOne)
   - Entity fundamentals

2. **[Intermediate](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate)** - Examples 31-60 (40-75% coverage)
   - @Query annotation with JPQL and native SQL
   - Pagination and sorting
   - Advanced relationships (@ManyToMany, N+1 solutions)
   - Transactions and locking

3. **[Advanced](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced)** - Examples 61-85 (75-95% coverage)
   - Specifications API for dynamic queries
   - Criteria API with CriteriaBuilder
   - Custom repository implementations
   - Auditing, lifecycle callbacks, and performance optimization

### Quick Reference by Topic

**Need to find specific topics?**

- **CRUD Operations**: Examples 1-8 (Beginner)
- **Query Methods**: Examples 9-16 (Beginner)
- **Relationships**: Examples 17-24 (Beginner), 38-43 (Intermediate)
- **Custom Queries**: Examples 31-38 (Intermediate)
- **Pagination**: Examples 39-44 (Intermediate)
- **Transactions**: Examples 50-55 (Intermediate)
- **Locking**: Examples 56-60 (Intermediate)
- **Dynamic Queries**: Examples 61-68 (Advanced)
- **Performance**: Examples 78-85 (Advanced)

## Learning Path Recommendations

### Path 1: Complete Beginner (New to Spring Data JPA)

1. Read this Overview completely
2. Start at Example 1 and work sequentially through all 30 beginner examples
3. Run every example and experiment with modifications
4. Move to Intermediate only after mastering beginner patterns
5. Expected time: Allow yourself to learn at your own pace

### Path 2: Experienced Spring Developer (Know Spring, New to JPA)

1. Skim Overview for tutorial structure
2. Review Examples 1-8 (Repository basics) for JPA-specific patterns
3. Focus on Examples 17-30 (Relationships and entity fundamentals)
4. Jump to Intermediate for transaction and query patterns
5. Expected time: Learn at your own pace

### Path 3: Reference Lookup (Know JPA, Need Specific Pattern)

1. Use Quick Reference by Topic above to find relevant examples
2. Jump directly to specific examples
3. Copy example code and adapt to your use case
4. Cross-reference related examples for context

### Path 4: Performance Optimization (Fixing Production Issues)

1. Study Examples 38-43 (N+1 problem detection and solutions)
2. Review Examples 56-60 (Locking and concurrency)
3. Master Examples 78-85 (Projections, query hints, batch fetching)
4. Apply Specifications (Examples 61-68) for complex dynamic queries

## Tips for Success

### Tip 1: Run Every Example

Don't just read the code—copy it, run it, modify it. The `// =>` annotations show expected behavior, but running the code cements understanding.

### Tip 2: Enable SQL Logging

Add this to `application.properties` to see actual SQL queries:

```properties
spring.jpa.show-sql=true
spring.jpa.properties.hibernate.format_sql=true
logging.level.org.hibernate.SQL=DEBUG
logging.level.org.hibernate.type.descriptor.sql.BasicBinder=TRACE
```

Compare logged SQL with `// => SQL:` annotations in examples.

### Tip 3: Use a Real Database for Production Patterns

Examples use H2 (in-memory) for simplicity, but test production patterns with PostgreSQL or MySQL to observe:

- Transaction isolation differences
- Locking behavior variations
- Performance characteristics

### Tip 4: Experiment with Failures

Intentionally trigger errors to understand behavior:

- Remove `@Transactional` and watch cascade operations fail
- Access lazy collections outside transactions to trigger LazyInitializationException
- Cause optimistic locking exceptions by modifying same entity concurrently

### Tip 5: Build Mental Models

For each example, visualize:

- Entity states (transient → managed → detached → removed)
- Persistence context lifecycle (transaction boundaries)
- SQL query execution (when does database I/O occur?)
- Object graph loading (eager vs lazy, N+1 problems)

## Next Steps

**Ready to start?** Jump into the tutorial:

1. **[Beginner Examples (1-30)](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner)** - Start here if you're new to Spring Data JPA
2. **[Intermediate Examples (31-60)](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate)** - Continue here after mastering beginner patterns
3. **[Advanced Examples (61-85)](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced)** - Expert-level patterns for production optimization

**Have questions?** Remember: The code is the documentation. Every `// =>` annotation shows exactly what happens at runtime. Run the examples, read the annotations, experiment with changes.

**Happy learning through code!**

## Examples by Level

### Beginner (Examples 1–30)

- [Example 1: First JPA Repository](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-1-first-jpa-repository)
- [Example 2: Save and Persist Entities](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-2-save-and-persist-entities)
- [Example 3: Find by ID](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-3-find-by-id)
- [Example 4: Find All Entities](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-4-find-all-entities)
- [Example 5: Delete Operations](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-5-delete-operations)
- [Example 6: Count and Exists](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-6-count-and-exists)
- [Example 7: Save All Batch Operations](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-7-save-all-batch-operations)
- [Example 8: Flush and Transaction Management](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-8-flush-and-transaction-management)
- [Example 9: Find By Single Property](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-9-find-by-single-property)
- [Example 10: Find By Multiple Properties](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-10-find-by-multiple-properties)
- [Example 11: Comparison Operators](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-11-comparison-operators)
- [Example 12: String Matching](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-12-string-matching)
- [Example 13: Ordering Results](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-13-ordering-results)
- [Example 14: Limiting Results](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-14-limiting-results)
- [Example 15: Null Handling](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-15-null-handling)
- [Example 16: Case-Insensitive Queries](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-16-case-insensitive-queries)
- [Example 17: One-to-Many Relationship Basics](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-17-one-to-many-relationship-basics)
- [Example 18: Querying Through Relationships](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-18-querying-through-relationships)
- [Example 19: Many-to-One Relationship](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-19-many-to-one-relationship)
- [Example 20: Cascade Types](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-20-cascade-types)
- [Example 21: Lazy vs Eager Loading](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-21-lazy-vs-eager-loading)
- [Example 22: Bidirectional Relationship Synchronization](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-22-bidirectional-relationship-synchronization)
- [Example 23: Join Column Configuration](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-23-join-column-configuration)
- [Example 24: Collection Types](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-24-collection-types)
- [Example 25: Column Mapping and Constraints](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-25-column-mapping-and-constraints)
- [Example 26: Temporal Types and Dates](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-26-temporal-types-and-dates)
- [Example 27: Enumerated Types](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-27-enumerated-types)
- [Example 28: Table and Index Configuration](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-28-table-and-index-configuration)
- [Example 29: Transient and Computed Fields](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-29-transient-and-computed-fields)
- [Example 30: Entity Lifecycle Callbacks](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/beginner#example-30-entity-lifecycle-callbacks)

### Intermediate (Examples 31–60)

- [Example 31: @Query with JPQL](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-31-query-with-jpql)
- [Example 32: Native SQL Queries](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-32-native-sql-queries)
- [Example 33: @Modifying Queries](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-33-modifying-queries)
- [Example 34: Constructor Expressions (DTO Projections)](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-34-constructor-expressions-dto-projections)
- [Example 35: JOIN Queries](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-35-join-queries)
- [Example 36: Subqueries in JPQL](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-36-subqueries-in-jpql)
- [Example 37: Named Queries](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-37-named-queries)
- [Example 38: Dynamic Queries with Specifications](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-38-dynamic-queries-with-specifications)
- [Example 39: Basic Pagination with PageRequest](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-39-basic-pagination-with-pagerequest)
- [Example 40: Sorting with Pageable](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-40-sorting-with-pageable)
- [Example 41: Page vs Slice](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-41-page-vs-slice)
- [Example 42: Custom Sorting Directions](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-42-custom-sorting-directions)
- [Example 43: Pagination with Specifications](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-43-pagination-with-specifications)
- [Example 44: Infinite Scroll with Slice](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-44-infinite-scroll-with-slice)
- [Example 45: @ManyToMany Relationships](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-45-manytomany-relationships)
- [Example 46: @EntityGraph for Fetch Optimization](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-46-entitygraph-for-fetch-optimization)
- [Example 47: N+1 Query Problem Demonstration](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-47-n1-query-problem-demonstration)
- [Example 48: @Embeddable Composite Objects](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-48-embeddable-composite-objects)
- [Example 49: Cascading Operations](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-49-cascading-operations)
- [Example 50: Bidirectional Relationship Management](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-50-bidirectional-relationship-management)
- [Example 51: @Transactional Propagation](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-51-transactional-propagation)
- [Example 52: Transaction Isolation Levels](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-52-transaction-isolation-levels)
- [Example 53: Rollback Rules and Exceptions](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-53-rollback-rules-and-exceptions)
- [Example 54: Read-Only Transactions](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-54-read-only-transactions)
- [Example 55: Programmatic Transaction Management](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-55-programmatic-transaction-management)
- [Example 56: Nested Transactions with NESTED Propagation](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-56-nested-transactions-with-nested-propagation)
- [Example 57: Optimistic Locking with @Version](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-57-optimistic-locking-with-version)
- [Example 58: Pessimistic Locking (PESSIMISTIC_READ)](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-58-pessimistic-locking-pessimistic_read)
- [Example 59: Pessimistic Locking (PESSIMISTIC_WRITE)](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-59-pessimistic-locking-pessimistic_write)
- [Example 60: Handling OptimisticLockException](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/intermediate#example-60-handling-optimisticlockexception)

### Advanced (Examples 61–85)

- [Example 61: Basic Specification with Single Predicate](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-61-basic-specification-with-single-predicate)
- [Example 62: Combining Specifications with AND/OR](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-62-combining-specifications-with-andor)
- [Example 63: Dynamic Query Building with Null-Safe Specifications](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-63-dynamic-query-building-with-null-safe-specifications)
- [Example 64: Specifications with Joins](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-64-specifications-with-joins)
- [Example 65: Specifications with NOT and Complex Predicates](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-65-specifications-with-not-and-complex-predicates)
- [Example 66: Specifications with Sorting and Pagination](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-66-specifications-with-sorting-and-pagination)
- [Example 67: Specifications with Distinct and Group By](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-67-specifications-with-distinct-and-group-by)
- [Example 68: Specifications with Case-Insensitive and Null Checks](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-68-specifications-with-case-insensitive-and-null-checks)
- [Example 69: Basic Criteria API Query](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-69-basic-criteria-api-query)
- [Example 70: Criteria API with Joins and Path Expressions](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-70-criteria-api-with-joins-and-path-expressions)
- [Example 71: Criteria API with Subqueries](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-71-criteria-api-with-subqueries)
- [Example 72: Criteria API with Dynamic Predicates](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-72-criteria-api-with-dynamic-predicates)
- [Example 73: Criteria API with Projections and DTOs](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-73-criteria-api-with-projections-and-dtos)
- [Example 74: Custom Repository Implementation](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-74-custom-repository-implementation)
- [Example 75: Custom Repository with Batch Operations](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-75-custom-repository-with-batch-operations)
- [Example 76: Custom Repository with Native Queries](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-76-custom-repository-with-native-queries)
- [Example 77: Custom Repository with QueryDSL Integration](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-77-custom-repository-with-querydsl-integration)
- [Example 78: Custom Repository Fragment Composition](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-78-custom-repository-fragment-composition)
- [Example 79: JPA Auditing with Timestamps](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-79-jpa-auditing-with-timestamps)
- [Example 80: JPA Auditing with User Tracking](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-80-jpa-auditing-with-user-tracking)
- [Example 81: Entity Lifecycle Callbacks with @EntityListeners](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-81-entity-lifecycle-callbacks-with-entitylisteners)
- [Example 82: Combining Auditing with Custom Callbacks](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-82-combining-auditing-with-custom-callbacks)
- [Example 83: Interface-Based Projections for Performance](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-83-interface-based-projections-for-performance)
- [Example 84: DTO Projections with Constructor Expressions](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-84-dto-projections-with-constructor-expressions)
- [Example 85: Query Hints and Batch Fetching for Performance](/en/learn/software-engineering/data/tools/spring-data-jpa/by-example/advanced#example-85-query-hints-and-batch-fetching-for-performance)
