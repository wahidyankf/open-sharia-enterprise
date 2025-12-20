---
title: "How to Access Databases with Exposed"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 1000200
description: "Use Exposed ORM for type-safe database access in Kotlin"
tags: ["kotlin", "database", "exposed", "orm", "sql"]
categories: ["learn"]
---

## Problem

JDBC is verbose and error-prone with raw SQL strings. Traditional ORMs have complex configurations. Exposed is a lightweight Kotlin SQL library offering both DSL and DAO patterns with type safety.

This guide shows how to use Exposed for database operations.

## Basic Setup

### Project Configuration

Add Exposed dependencies.

```kotlin
// ✅ build.gradle.kts
dependencies {
  implementation("org.jetbrains.exposed:exposed-core:0.46.0")
  implementation("org.jetbrains.exposed:exposed-dao:0.46.0")
  implementation("org.jetbrains.exposed:exposed-jdbc:0.46.0")
  implementation("com.h2database:h2:2.2.224")  // Or your DB driver
  implementation("org.postgresql:postgresql:42.7.1")
}
```

### Database Connection

Connect to database.

```kotlin
import org.jetbrains.exposed.sql.Database

// ✅ H2 in-memory database
Database.connect("jdbc:h2:mem:test", driver = "org.h2.Driver")

// ✅ PostgreSQL
Database.connect(
  url = "jdbc:postgresql://localhost:5432/mydb",
  driver = "org.postgresql.Driver",
  user = "postgres",
  password = "password"
)

// ✅ With HikariCP connection pool
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource

val config = HikariConfig().apply {
  jdbcUrl = "jdbc:postgresql://localhost:5432/mydb"
  driverClassName = "org.postgresql.Driver"
  username = "postgres"
  password = "password"
  maximumPoolSize = 10
}

Database.connect(HikariDataSource(config))
```

## DSL API

### Define Tables

Create type-safe table schemas.

```kotlin
import org.jetbrains.exposed.sql.Table

// ✅ Define table
object Users : Table() {
  val id = varchar("id", 50).primaryKey()
  val name = varchar("name", 100)
  val email = varchar("email", 100).uniqueIndex()
  val age = integer("age")
  val createdAt = long("created_at")
}

// ✅ Table with auto-increment
object Posts : Table() {
  val id = integer("id").autoIncrement()
  val userId = varchar("user_id", 50) references Users.id
  val title = varchar("title", 200)
  val content = text("content")
  val publishedAt = long("published_at").nullable()

  override val primaryKey = PrimaryKey(id)
}
```

### CRUD Operations

Perform database operations with DSL.

```kotlin
import org.jetbrains.exposed.sql.*
import org.jetbrains.exposed.sql.transactions.transaction

// ✅ Create schema
transaction {
  SchemaUtils.create(Users, Posts)
}

// ✅ Insert
transaction {
  Users.insert {
    it[id] = "user1"
    it[name] = "Alice"
    it[email] = "alice@example.com"
    it[age] = 30
    it[createdAt] = System.currentTimeMillis()
  }
}

// ✅ Insert and get ID
val newUserId = transaction {
  Users.insert {
    it[id] = "user2"
    it[name] = "Bob"
    it[email] = "bob@example.com"
    it[age] = 25
    it[createdAt] = System.currentTimeMillis()
  } get Users.id
}

// ✅ Select all
val allUsers = transaction {
  Users.selectAll().map {
    User(
      id = it[Users.id],
      name = it[Users.name],
      email = it[Users.email],
      age = it[Users.age]
    )
  }
}

// ✅ Select with where
val alice = transaction {
  Users.select { Users.email eq "alice@example.com" }
    .map {
      User(
        id = it[Users.id],
        name = it[Users.name],
        email = it[Users.email],
        age = it[Users.age]
      )
    }
    .firstOrNull()
}

// ✅ Update
transaction {
  Users.update({ Users.id eq "user1" }) {
    it[age] = 31
  }
}

// ✅ Delete
transaction {
  Users.deleteWhere { Users.id eq "user1" }
}
```

### Queries with Joins

Join tables for complex queries.

```kotlin
// ✅ Inner join
val usersWithPosts = transaction {
  (Users innerJoin Posts)
    .select { Posts.publishedAt.isNotNull() }
    .map {
      UserPost(
        userId = it[Users.id],
        userName = it[Users.name],
        postTitle = it[Posts.title]
      )
    }
}

// ✅ Left join
val allUsersWithPosts = transaction {
  (Users leftJoin Posts)
    .selectAll()
    .groupBy { it[Users.id] }
    .map { (userId, rows) ->
      val user = rows.first()
      UserWithPosts(
        id = user[Users.id],
        name = user[Users.name],
        posts = rows.mapNotNull { row ->
          row.getOrNull(Posts.title)?.let { title ->
            Post(
              title = title,
              content = row[Posts.content]
            )
          }
        }
      )
    }
}
```

## DAO API

### Define Entities

Use object-oriented DAO pattern.

```kotlin
import org.jetbrains.exposed.dao.*
import org.jetbrains.exposed.dao.id.*

// ✅ Define table with IntIdTable
object UsersTable : IntIdTable("users") {
  val name = varchar("name", 100)
  val email = varchar("email", 100).uniqueIndex()
  val age = integer("age")
}

// ✅ Define entity class
class User(id: EntityID<Int>) : IntEntity(id) {
  companion object : IntEntityClass<User>(UsersTable)

  var name by UsersTable.name
  var email by UsersTable.email
  var age by UsersTable.age
}

// ✅ One-to-many relationship
object PostsTable : IntIdTable("posts") {
  val user = reference("user_id", UsersTable)
  val title = varchar("title", 200)
  val content = text("content")
}

class Post(id: EntityID<Int>) : IntEntity(id) {
  companion object : IntEntityClass<Post>(PostsTable)

  var user by User referencedOn PostsTable.user
  var title by PostsTable.title
  var content by PostsTable.content
}
```

### DAO Operations

Perform operations with entity objects.

```kotlin
// ✅ Create
val user = transaction {
  User.new {
    name = "Alice"
    email = "alice@example.com"
    age = 30
  }
}

// ✅ Find by ID
val found = transaction {
  User.findById(1)
}

// ✅ Find all
val allUsers = transaction {
  User.all().toList()
}

// ✅ Find with condition
val adults = transaction {
  User.find { UsersTable.age greaterEq 18 }.toList()
}

// ✅ Update
transaction {
  val user = User.findById(1)
  user?.age = 31
}

// ✅ Delete
transaction {
  val user = User.findById(1)
  user?.delete()
}

// ✅ Create related entities
transaction {
  val user = User.new {
    name = "Bob"
    email = "bob@example.com"
    age = 25
  }

  Post.new {
    this.user = user
    title = "My First Post"
    content = "Hello, world!"
  }
}

// ✅ Access relationships
transaction {
  val user = User.findById(1)
  val posts = Post.find { PostsTable.user eq user!!.id }
}
```

## Transactions

### Transaction Management

Control transaction boundaries.

```kotlin
// ✅ Basic transaction
transaction {
  Users.insert {
    it[id] = "user1"
    it[name] = "Alice"
  }
}

// ✅ Nested transaction
transaction {
  Users.insert { /* ... */ }

  transaction {
    // Nested transaction
    Posts.insert { /* ... */ }
  }
}

// ✅ Transaction with return value
val userId: String = transaction {
  Users.insert {
    it[id] = "user1"
    it[name] = "Alice"
  } get Users.id
}

// ✅ Rollback on exception
try {
  transaction {
    Users.insert { /* ... */ }
    throw Exception("Rollback!")
    Posts.insert { /* ... */ }  // Not executed
  }
} catch (e: Exception) {
  println("Transaction rolled back")
}
```

### Transaction with Coroutines

Use transactions with suspend functions.

```kotlin
import org.jetbrains.exposed.sql.transactions.experimental.newSuspendedTransaction
import kotlinx.coroutines.*

// ✅ Suspended transaction
suspend fun createUser(name: String, email: String): User {
  return newSuspendedTransaction(Dispatchers.IO) {
    User.new {
      this.name = name
      this.email = email
      this.age = 0
    }
  }
}

// ✅ Usage
suspend fun example() {
  val user = createUser("Alice", "alice@example.com")
  println("Created user: ${user.id}")
}
```

## Common Pitfalls

### Forgetting Transaction Block

```kotlin
// ❌ No transaction - throws exception
Users.selectAll()  // ❌ Must be in transaction

// ✅ Wrap in transaction
transaction {
  Users.selectAll()
}
```

### N+1 Query Problem

```kotlin
// ❌ N+1 queries
transaction {
  val users = User.all()
  users.forEach { user ->
    val posts = Post.find { PostsTable.user eq user.id }  // Separate query!
  }
}

// ✅ Use join to fetch in one query
transaction {
  (UsersTable innerJoin PostsTable)
    .selectAll()
    .groupBy { it[UsersTable.id] }
}
```

### Not Using Connection Pool

```kotlin
// ❌ No connection pooling
Database.connect("jdbc:postgresql://localhost/db", driver = "org.postgresql.Driver")

// ✅ Use HikariCP
val config = HikariConfig().apply {
  jdbcUrl = "jdbc:postgresql://localhost/db"
  maximumPoolSize = 10
}
Database.connect(HikariDataSource(config))
```

## Variations

### Batch Insert

Insert multiple records efficiently.

```kotlin
// ✅ Batch insert
transaction {
  Users.batchInsert(listOf(
    UserData("user1", "Alice", "alice@example.com"),
    UserData("user2", "Bob", "bob@example.com"),
    UserData("user3", "Charlie", "charlie@example.com")
  )) { userData ->
    this[Users.id] = userData.id
    this[Users.name] = userData.name
    this[Users.email] = userData.email
  }
}
```

### Custom SQL

Execute raw SQL when needed.

```kotlin
// ✅ Custom SQL query
transaction {
  exec("CREATE INDEX idx_users_email ON users(email)")
}

// ✅ Raw query
val result = transaction {
  exec("SELECT COUNT(*) FROM users") { rs ->
    rs.next()
    rs.getInt(1)
  }
}
```

## Related Patterns

**Learn more**:

- [Intermediate Tutorial - Database](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate#database-integration) - Database patterns
- [Coroutines Basics](/en/learn/swe/prog-lang/kotlin/how-to/handle-coroutines-and-async) - Async database access
- [REST APIs with Ktor](/en/learn/swe/prog-lang/kotlin/how-to/build-rest-apis-ktor) - Database in web apps

**Cookbook recipes**:

- [Database with Exposed](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#database-exposed) - Quick reference
- [JDBC Extensions](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#jdbc-extensions) - JDBC patterns
- [Transaction Management](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#transaction-management) - Transaction patterns
