---
title: "Organize Code Packages"
date: 2025-12-18T14:50:04+07:00
draft: false
weight: 1000015
description: "Best practices for organizing Kotlin code with packages, modules, and visibility modifiers"
tags: ["kotlin", "packages", "modules", "code-organization", "architecture"]
categories: ["learn"]
---

## Problem

As codebases grow, poor organization leads to tangled dependencies, circular references, and difficult maintenance. Proper package structure and module boundaries create maintainable, scalable applications. Kotlin's visibility modifiers help enforce encapsulation at package and module levels.

This guide shows how to organize Kotlin code effectively.

## Package Organization

### Package Naming Conventions

Follow standard Java package naming conventions.

```kotlin
// ✅ Reverse domain name + feature
package com.example.myapp.user
package com.example.myapp.payment
package com.example.myapp.notification

// ✅ Lowercase, no underscores or special characters
package com.example.myapp.dataprovider  // ✅ Correct
// package com.example.myapp.data_provider  // ❌ Avoid underscores
// package com.example.myapp.DataProvider   // ❌ Avoid uppercase

// ✅ Meaningful, not too deep
package com.example.myapp.user.repository  // ✅ Clear
// package com.example.myapp.user.data.repository.impl  // ❌ Too deep
```

**Rules**:

- All lowercase
- Reverse domain name as root
- Feature/module-based organization
- Avoid overly deep hierarchies (3-4 levels max)

### Feature-Based vs Layer-Based

Organize by feature (recommended) or by technical layer.

**Layer-based (traditional)**:

```kotlin
// ❌ Groups by technical role
com.example.myapp
├── controllers
│   ├── UserController.kt
│   ├── OrderController.kt
│   └── PaymentController.kt
├── services
│   ├── UserService.kt
│   ├── OrderService.kt
│   └── PaymentService.kt
└── repositories
    ├── UserRepository.kt
    ├── OrderRepository.kt
    └── PaymentRepository.kt
```

**Feature-based (recommended)**:

```kotlin
// ✅ Groups by business feature
com.example.myapp
├── user
│   ├── UserController.kt
│   ├── UserService.kt
│   ├── UserRepository.kt
│   └── User.kt
├── order
│   ├── OrderController.kt
│   ├── OrderService.kt
│   ├── OrderRepository.kt
│   └── Order.kt
└── payment
    ├── PaymentController.kt
    ├── PaymentService.kt
    ├── PaymentRepository.kt
    └── Payment.kt
```

**Benefits of feature-based**:

- Related code grouped together
- Easier to understand business logic
- Reduced coupling between features
- Easier to extract to microservices

### Package-by-Feature Structure

Detailed feature package organization.

```kotlin
// ✅ Complete feature package
com.example.myapp.user
├── User.kt                    // Domain model
├── UserDto.kt                 // Data transfer objects
├── UserController.kt          // API endpoints
├── UserService.kt             // Business logic
├── UserRepository.kt          // Data access interface
├── UserRepositoryImpl.kt      // Data access implementation
├── UserMapper.kt              // DTO ↔ Entity mapping
├── UserValidator.kt           // Validation logic
└── exceptions
    └── UserNotFoundException.kt

// ✅ Feature with subfeatures
com.example.myapp.order
├── domain
│   ├── Order.kt
│   ├── OrderItem.kt
│   └── OrderStatus.kt
├── api
│   ├── OrderController.kt
│   └── OrderDto.kt
├── service
│   ├── OrderService.kt
│   └── OrderServiceImpl.kt
└── repository
    ├── OrderRepository.kt
    └── OrderRepositoryImpl.kt
```

## Visibility Modifiers

### Public Visibility

Default visibility - accessible everywhere.

```kotlin
// ✅ Public by default (no modifier needed)
package com.example.myapp.user

class User(val id: String, val name: String)  // Public

fun createUser(name: String): User {  // Public
  return User(generateId(), name)
}

// Accessible from anywhere
import com.example.myapp.user.User
import com.example.myapp.user.createUser

val user = createUser("Alice")
```

**Use for**: Public APIs, domain models exposed to other packages.

### Internal Visibility

Visible within the same module only.

```kotlin
// ✅ Internal - module-scoped
package com.example.myapp.user

internal class UserRepositoryImpl : UserRepository {
  // Implementation details hidden from other modules
}

internal fun validateEmail(email: String): Boolean {
  return email.contains("@")
}

// ✅ Can be used within same module
// ❌ Cannot be used from other modules
```

**Use for**: Implementation details, internal utilities, module-private APIs.

### Private Visibility

Private to file or class.

```kotlin
// ✅ Private top-level (file-scoped)
package com.example.myapp.user

private fun generateId(): String {
  return java.util.UUID.randomUUID().toString()
}

class User(val id: String, val name: String) {
  // ✅ Private member (class-scoped)
  private var lastModified: Long = System.currentTimeMillis()

  private fun updateTimestamp() {
    lastModified = System.currentTimeMillis()
  }
}
```

**Use for**: File-local helpers, private class members.

### Protected Visibility

Visible in class and subclasses only.

```kotlin
// ✅ Protected - for inheritance
open class BaseRepository {
  protected fun log(message: String) {
    println("[${this::class.simpleName}] $message")
  }
}

class UserRepository : BaseRepository() {
  fun save(user: User) {
    log("Saving user: ${user.id}")  // ✅ Can access protected method
  }
}

// ❌ Cannot access from outside
// UserRepository().log("test")  // Compilation error
```

**Use for**: Methods meant for subclass extension.

## Module Organization

### What is a Module?

Module is compilation unit in Gradle/Maven project.

```kotlin
// ✅ Multi-module project structure
root-project/
├── settings.gradle.kts
├── build.gradle.kts
├── core/                      // Core domain module
│   ├── build.gradle.kts
│   └── src/main/kotlin/
│       └── com/example/core/
│           ├── User.kt
│           └── Order.kt
├── api/                       // REST API module
│   ├── build.gradle.kts
│   └── src/main/kotlin/
│       └── com/example/api/
│           └── UserController.kt
├── service/                   // Business logic module
│   ├── build.gradle.kts
│   └── src/main/kotlin/
│       └── com/example/service/
│           └── UserService.kt
└── infrastructure/            // Data access module
    ├── build.gradle.kts
    └── src/main/kotlin/
        └── com/example/infra/
            └── UserRepository.kt
```

### Module Dependencies

Define clear dependency directions.

```kotlin
// ✅ Good dependency hierarchy
api → service → core
     ↓
infrastructure → core

// build.gradle.kts in api module
dependencies {
  implementation(project(":service"))
  implementation(project(":core"))
}

// build.gradle.kts in service module
dependencies {
  implementation(project(":core"))
  implementation(project(":infrastructure"))
}

// ❌ Avoid circular dependencies
// service → api → service  // ❌ Circular
```

**Principles**:

- Core/domain has no dependencies (pure business logic)
- Infrastructure depends only on core
- Service depends on core and infrastructure
- API depends on service and core

### Internal Visibility Across Modules

`internal` prevents cross-module access.

```kotlin
// ✅ In module A
package com.example.moduleA

internal class InternalHelper {
  fun help() = "Internal"
}

public class PublicApi {
  internal fun internalMethod() = "Internal method"
}

// ✅ In module B (cannot access internal declarations from A)
import com.example.moduleA.PublicApi
// import com.example.moduleA.InternalHelper  // ❌ Compilation error

val api = PublicApi()
// api.internalMethod()  // ❌ Compilation error
```

## Common Organization Patterns

### Hexagonal Architecture (Ports and Adapters)

Organize code by architectural layers.

```kotlin
// ✅ Hexagonal architecture
com.example.myapp
├── domain                     // Core business logic
│   ├── User.kt
│   ├── UserService.kt
│   └── port                   // Interfaces (ports)
│       ├── UserRepository.kt
│       └── EmailSender.kt
├── application                // Use cases
│   ├── CreateUserUseCase.kt
│   └── UpdateUserUseCase.kt
└── adapter                    // External adapters
    ├── inbound
    │   └── rest
    │       └── UserController.kt
    └── outbound
        ├── database
        │   └── UserRepositoryImpl.kt
        └── email
            └── SmtpEmailSender.kt
```

**Key principle**: Domain is independent, adapters depend on domain.

### Clean Architecture

Layer-based with dependency inversion.

```kotlin
// ✅ Clean architecture layers
com.example.myapp
├── entities                   // Enterprise business rules
│   └── User.kt
├── usecases                   // Application business rules
│   ├── CreateUser.kt
│   └── GetUser.kt
├── interfaces                 // Interface adapters
│   ├── controllers
│   │   └── UserController.kt
│   ├── presenters
│   │   └── UserPresenter.kt
│   └── gateways
│       └── UserRepositoryGateway.kt
└── frameworks                 // Frameworks and drivers
    ├── database
    │   └── JpaUserRepository.kt
    └── web
        └── SpringConfiguration.kt
```

**Dependency rule**: Inner layers never depend on outer layers.

### Screaming Architecture

Package names reflect business domain, not technical layers.

```kotlin
// ✅ Screaming architecture - business first
com.example.onlinestore
├── catalog                    // Product catalog
│   ├── product
│   ├── category
│   └── search
├── checkout                   // Checkout process
│   ├── cart
│   ├── payment
│   └── shipping
├── customer                   // Customer management
│   ├── account
│   ├── profile
│   └── authentication
└── order                      // Order management
    ├── fulfillment
    ├── tracking
    └── returns
```

**Benefit**: Package names instantly communicate business capabilities.

## File Organization

### One Class Per File (Usually)

Standard practice: one public class per file.

```kotlin
// ✅ User.kt
package com.example.myapp.user

data class User(
  val id: String,
  val name: String,
  val email: String
)

// ✅ UserService.kt
package com.example.myapp.user

class UserService(
  private val repository: UserRepository
) {
  fun create(name: String, email: String): User {
    return repository.save(User(generateId(), name, email))
  }
}
```

### Grouping Related Classes

Small, tightly related classes can share files.

```kotlin
// ✅ Result.kt - sealed class hierarchy in one file
package com.example.myapp.common

sealed class Result<out T> {
  data class Success<T>(val data: T) : Result<T>()
  data class Error(val message: String) : Result<Nothing>()
  object Loading : Result<Nothing>()
}

// ✅ Extensions.kt - related extension functions
package com.example.myapp.extensions

fun String.isValidEmail(): Boolean = contains("@")
fun String.truncate(maxLength: Int): String {
  return if (length <= maxLength) this else substring(0, maxLength) + "..."
}
```

**When to group**:

- Sealed class hierarchies
- Related extension functions
- Small data classes with tight coupling
- Internal helper classes

### Constants and Configuration

Centralize constants in dedicated files or objects.

```kotlin
// ✅ Constants.kt
package com.example.myapp.common

object Constants {
  const val MAX_NAME_LENGTH = 100
  const val MIN_PASSWORD_LENGTH = 8
  const val DEFAULT_PAGE_SIZE = 20
}

object ApiEndpoints {
  const val BASE_URL = "https://api.example.com"
  const val USERS = "/users"
  const val ORDERS = "/orders"
}

// ✅ Usage
import com.example.myapp.common.Constants

fun validateName(name: String): Boolean {
  return name.length <= Constants.MAX_NAME_LENGTH
}
```

## Common Pitfalls

### God Package

```kotlin
// ❌ Everything in one package
com.example.myapp
├── User.kt
├── Order.kt
├── Product.kt
├── Payment.kt
├── UserController.kt
├── OrderController.kt
├── ProductController.kt
└── PaymentController.kt
// ... 50+ files

// ✅ Organize by feature
com.example.myapp
├── user/
├── order/
├── product/
└── payment/
```

**Why problematic**: No structure, hard to navigate, unclear boundaries.

### Over-Engineering Package Structure

```kotlin
// ❌ Too many layers
com.example.myapp.user.domain.model.entity.impl.UserImpl.kt

// ✅ Reasonable depth
com.example.myapp.user.User.kt
```

**Why problematic**: Excessive nesting reduces readability.

### Circular Package Dependencies

```kotlin
// ❌ Circular dependencies
package com.example.myapp.user
import com.example.myapp.order.Order  // User imports Order

package com.example.myapp.order
import com.example.myapp.user.User    // Order imports User

// ✅ Extract shared types to common package
package com.example.myapp.common
data class UserId(val value: String)
data class OrderId(val value: String)

package com.example.myapp.user
import com.example.myapp.common.OrderId  // Depends on common

package com.example.myapp.order
import com.example.myapp.common.UserId   // Depends on common
```

### Mixing Public and Internal Inappropriately

```kotlin
// ❌ Implementation leaking as public
package com.example.myapp.user

class UserService(val repository: UserRepositoryImpl) {  // ❌ Exposes impl
  // ...
}

// ✅ Depend on interface
package com.example.myapp.user

class UserService(private val repository: UserRepository) {  // ✅ Interface
  // ...
}

internal class UserRepositoryImpl : UserRepository {  // ✅ Hide implementation
  // ...
}
```

## Variations

### Shared Module Pattern

Extract common code to shared module.

```kotlin
// ✅ Shared module structure
shared/
├── domain/
│   ├── Result.kt
│   ├── Entity.kt
│   └── ValueObject.kt
├── extensions/
│   ├── StringExtensions.kt
│   └── CollectionExtensions.kt
└── utils/
    ├── DateUtils.kt
    └── ValidationUtils.kt

// Other modules depend on shared
dependencies {
  implementation(project(":shared"))
}
```

### Multi-Platform Module Organization

Organize code for Kotlin Multiplatform.

```kotlin
// ✅ Multiplatform structure
common/
├── commonMain/
│   └── kotlin/
│       └── com.example.myapp/
│           ├── User.kt
│           └── UserRepository.kt
├── androidMain/
│   └── kotlin/
│       └── com.example.myapp/
│           └── AndroidUserRepository.kt
└── iosMain/
    └── kotlin/
        └── com.example.myapp/
            └── IosUserRepository.kt
```

### Testing Package Organization

Mirror main source structure in tests.

```kotlin
// src/main/kotlin/com/example/myapp/user/UserService.kt
package com.example.myapp.user

class UserService { /*...*/ }

// src/test/kotlin/com/example/myapp/user/UserServiceTest.kt
package com.example.myapp.user

import org.junit.jupiter.api.Test

class UserServiceTest {
  @Test
  fun `should create user`() { /*...*/ }
}
```

## Related Patterns

**Learn more**:

- [Beginner Tutorial - Packages](/en/learn/software-engineering/programming-language/kotlin/tutorials/beginner#packages) - Package basics
- [Intermediate Tutorial - Modules](/en/learn/software-engineering/programming-language/kotlin/tutorials/intermediate#multi-module-projects) - Multi-module projects
- [Best Practices](/en/learn/software-engineering/programming-language/kotlin/explanation/best-practices) - Code organization principles
- [Visibility Modifiers](/en/learn/software-engineering/programming-language/kotlin/tutorials/beginner#visibility-modifiers) - Visibility fundamentals

**Cookbook recipes**:

- [Project Structure](/en/learn/software-engineering/programming-language/kotlin/how-to/cookbook#project-structure) - Quick reference
- [Module Organization](/en/learn/software-engineering/programming-language/kotlin/how-to/cookbook#modules) - Common patterns
- [Clean Architecture](/en/learn/software-engineering/programming-language/kotlin/how-to/cookbook#clean-architecture) - Layered organization
