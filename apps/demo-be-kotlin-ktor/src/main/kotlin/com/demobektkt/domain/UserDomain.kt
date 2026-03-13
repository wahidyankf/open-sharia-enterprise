package com.demobektkt.domain

private val EMAIL_REGEX = Regex("^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$")
private val SPECIAL_CHAR_REGEX = Regex("[^A-Za-z0-9]")
private val UPPERCASE_REGEX = Regex("[A-Z]")
private val LOWERCASE_REGEX = Regex("[a-z]")
private val DIGIT_REGEX = Regex("[0-9]")

fun validatePassword(password: String): Result<String> {
  if (password.isEmpty()) {
    return Result.failure(
      DomainException(DomainError.ValidationError("password", "Password must not be empty"))
    )
  }
  if (password.length < 12) {
    return Result.failure(
      DomainException(
        DomainError.ValidationError("password", "Password must be at least 12 characters")
      )
    )
  }
  if (!UPPERCASE_REGEX.containsMatchIn(password)) {
    return Result.failure(
      DomainException(
        DomainError.ValidationError(
          "password",
          "Password must contain at least one uppercase letter",
        )
      )
    )
  }
  if (!LOWERCASE_REGEX.containsMatchIn(password)) {
    return Result.failure(
      DomainException(
        DomainError.ValidationError(
          "password",
          "Password must contain at least one lowercase letter",
        )
      )
    )
  }
  if (!DIGIT_REGEX.containsMatchIn(password)) {
    return Result.failure(
      DomainException(
        DomainError.ValidationError("password", "Password must contain at least one digit")
      )
    )
  }
  if (!SPECIAL_CHAR_REGEX.containsMatchIn(password)) {
    return Result.failure(
      DomainException(
        DomainError.ValidationError(
          "password",
          "Password must contain at least one special character",
        )
      )
    )
  }
  return Result.success(password)
}

fun validateEmail(email: String): Result<String> {
  if (email.isEmpty()) {
    return Result.failure(
      DomainException(DomainError.ValidationError("email", "Email must not be empty"))
    )
  }
  if (!EMAIL_REGEX.matches(email)) {
    return Result.failure(
      DomainException(DomainError.ValidationError("email", "Invalid email format"))
    )
  }
  return Result.success(email)
}

fun validateUsername(username: String): Result<String> {
  if (username.isEmpty()) {
    return Result.failure(
      DomainException(DomainError.ValidationError("username", "Username must not be empty"))
    )
  }
  if (username.length < 3) {
    return Result.failure(
      DomainException(
        DomainError.ValidationError("username", "Username must be at least 3 characters")
      )
    )
  }
  return Result.success(username)
}

fun validateDisplayName(name: String): Result<String> {
  if (name.isEmpty()) {
    return Result.failure(
      DomainException(DomainError.ValidationError("display_name", "Display name must not be empty"))
    )
  }
  if (name.length > 100) {
    return Result.failure(
      DomainException(
        DomainError.ValidationError("display_name", "Display name must not exceed 100 characters")
      )
    )
  }
  return Result.success(name)
}
