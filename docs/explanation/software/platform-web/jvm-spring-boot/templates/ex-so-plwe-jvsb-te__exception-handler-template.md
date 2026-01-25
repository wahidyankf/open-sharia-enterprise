---
title: Exception Handler Template
description: Template for global exception handling with @RestControllerAdvice
category: explanation
subcategory: platform-web-templates
tags:
  - spring-boot
  - template
  - exception-handling
  - rest-controller-advice
last_updated: 2026-01-25
---

# Exception Handler Template

```java
package com.oseplatform.[domain].api.rest.advice;

import com.oseplatform.[domain].domain.exception.*;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.ConstraintViolationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ProblemDetail;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.context.request.WebRequest;

import java.net.URI;
import java.time.Instant;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Global exception handler for [Domain] REST controllers.
 *
 * <p>Converts exceptions to RFC 7807 Problem Details responses.
 */
@RestControllerAdvice(basePackages = "com.oseplatform.[domain].api.rest")
public class [Domain]ExceptionHandler {

    private static final Logger logger = LoggerFactory.getLogger([Domain]ExceptionHandler.class);

    /**
     * Handle domain-specific not found exceptions.
     */
    @ExceptionHandler([Resource]NotFoundException.class)
    public ResponseEntity<ProblemDetail> handleNotFound(
        [Resource]NotFoundException ex,
        WebRequest request
    ) {
        logger.warn("[Resource] not found: {}", ex.getMessage());

        ProblemDetail problem = ProblemDetail.forStatusAndDetail(
            HttpStatus.NOT_FOUND,
            ex.getMessage()
        );
        problem.setTitle("[Resource] Not Found");
        problem.setType(URI.create("https://api.oseplatform.com/problems/not-found"));
        problem.setProperty("timestamp", Instant.now());
        problem.setProperty("resourceId", ex.getResourceId());

        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(problem);
    }

    /**
     * Handle domain-specific validation exceptions.
     */
    @ExceptionHandler([Resource]ValidationException.class)
    public ResponseEntity<ProblemDetail> handleValidationException(
        [Resource]ValidationException ex,
        WebRequest request
    ) {
        logger.warn("[Resource] validation failed: {}", ex.getMessage());

        ProblemDetail problem = ProblemDetail.forStatusAndDetail(
            HttpStatus.BAD_REQUEST,
            "Validation failed"
        );
        problem.setTitle("Validation Error");
        problem.setType(URI.create("https://api.oseplatform.com/problems/validation-error"));
        problem.setProperty("timestamp", Instant.now());
        problem.setProperty("errors", ex.getErrors());

        return ResponseEntity.badRequest().body(problem);
    }

    /**
     * Handle business rule violations.
     */
    @ExceptionHandler([Resource]BusinessRuleException.class)
    public ResponseEntity<ProblemDetail> handleBusinessRuleViolation(
        [Resource]BusinessRuleException ex,
        WebRequest request
    ) {
        logger.warn("Business rule violation: {}", ex.getMessage());

        ProblemDetail problem = ProblemDetail.forStatusAndDetail(
            HttpStatus.UNPROCESSABLE_ENTITY,
            ex.getMessage()
        );
        problem.setTitle("Business Rule Violation");
        problem.setType(URI.create("https://api.oseplatform.com/problems/business-rule-violation"));
        problem.setProperty("timestamp", Instant.now());
        problem.setProperty("ruleCode", ex.getRuleCode());

        return ResponseEntity.unprocessableEntity().body(problem);
    }

    /**
     * Handle Bean Validation (@Valid) failures.
     */
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ProblemDetail> handleMethodArgumentNotValid(
        MethodArgumentNotValidException ex,
        WebRequest request
    ) {
        logger.warn("Request validation failed: {} errors", ex.getErrorCount());

        Map<String, String> errors = new HashMap<>();
        ex.getBindingResult().getAllErrors().forEach(error -> {
            String fieldName = ((FieldError) error).getField();
            String errorMessage = error.getDefaultMessage();
            errors.put(fieldName, errorMessage);
        });

        ProblemDetail problem = ProblemDetail.forStatusAndDetail(
            HttpStatus.BAD_REQUEST,
            "Request validation failed"
        );
        problem.setTitle("Invalid Request");
        problem.setType(URI.create("https://api.oseplatform.com/problems/invalid-request"));
        problem.setProperty("timestamp", Instant.now());
        problem.setProperty("errors", errors);

        return ResponseEntity.badRequest().body(problem);
    }

    /**
     * Handle constraint violations (e.g., @Validated on method parameters).
     */
    @ExceptionHandler(ConstraintViolationException.class)
    public ResponseEntity<ProblemDetail> handleConstraintViolation(
        ConstraintViolationException ex,
        WebRequest request
    ) {
        logger.warn("Constraint violation: {}", ex.getMessage());

        Map<String, String> errors = ex.getConstraintViolations().stream()
            .collect(Collectors.toMap(
                violation -> violation.getPropertyPath().toString(),
                ConstraintViolation::getMessage
            ));

        ProblemDetail problem = ProblemDetail.forStatusAndDetail(
            HttpStatus.BAD_REQUEST,
            "Constraint violation"
        );
        problem.setTitle("Constraint Violation");
        problem.setType(URI.create("https://api.oseplatform.com/problems/constraint-violation"));
        problem.setProperty("timestamp", Instant.now());
        problem.setProperty("errors", errors);

        return ResponseEntity.badRequest().body(problem);
    }

    /**
     * Handle illegal argument exceptions.
     */
    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<ProblemDetail> handleIllegalArgument(
        IllegalArgumentException ex,
        WebRequest request
    ) {
        logger.warn("Illegal argument: {}", ex.getMessage());

        ProblemDetail problem = ProblemDetail.forStatusAndDetail(
            HttpStatus.BAD_REQUEST,
            ex.getMessage()
        );
        problem.setTitle("Invalid Argument");
        problem.setType(URI.create("https://api.oseplatform.com/problems/invalid-argument"));
        problem.setProperty("timestamp", Instant.now());

        return ResponseEntity.badRequest().body(problem);
    }

    /**
     * Handle all other uncaught exceptions.
     */
    @ExceptionHandler(Exception.class)
    public ResponseEntity<ProblemDetail> handleGenericException(
        Exception ex,
        WebRequest request
    ) {
        logger.error("Unexpected error occurred", ex);

        ProblemDetail problem = ProblemDetail.forStatusAndDetail(
            HttpStatus.INTERNAL_SERVER_ERROR,
            "An unexpected error occurred"
        );
        problem.setTitle("Internal Server Error");
        problem.setType(URI.create("https://api.oseplatform.com/problems/internal-error"));
        problem.setProperty("timestamp", Instant.now());

        // Don't expose internal error details in production
        // problem.setProperty("message", ex.getMessage());

        return ResponseEntity.internalServerError().body(problem);
    }
}
```

**Custom Exception Examples**:

```java
// Domain-specific exceptions
package com.oseplatform.[domain].domain.exception;

public class [Resource]NotFoundException extends RuntimeException {
    private final String resourceId;

    public [Resource]NotFoundException(String resourceId) {
        super(String.format("[Resource] not found with id: %s", resourceId));
        this.resourceId = resourceId;
    }

    public String getResourceId() {
        return resourceId;
    }
}

public class [Resource]ValidationException extends RuntimeException {
    private final Map<String, String> errors;

    public [Resource]ValidationException(Map<String, String> errors) {
        super("Validation failed");
        this.errors = errors;
    }

    public Map<String, String> getErrors() {
        return errors;
    }
}

public class [Resource]BusinessRuleException extends RuntimeException {
    private final String ruleCode;

    public [Resource]BusinessRuleException(String ruleCode, String message) {
        super(message);
        this.ruleCode = ruleCode;
    }

    public String getRuleCode() {
        return ruleCode;
    }
}
```

**Example Error Response** (RFC 7807 Problem Details):

```json
{
  "type": "https://api.oseplatform.com/problems/validation-error",
  "title": "Validation Error",
  "status": 400,
  "detail": "Validation failed",
  "timestamp": "2026-01-25T10:30:00Z",
  "errors": {
    "amount": "must be greater than 0",
    "currency": "must not be blank"
  }
}
```

**Replace**:

- `[domain]` - Your bounded context (e.g., `zakat`, `murabaha`)
- `[Domain]` - Domain name PascalCase (e.g., `Zakat`, `Murabaha`)
- `[Resource]` - Resource name PascalCase (e.g., `Calculation`, `Contract`)
- `[resource]` - Resource name lowercase (e.g., `calculation`, `contract`)

**Best Practices**:

- Use RFC 7807 Problem Details format for consistent error responses
- Log errors at appropriate levels (warn for client errors, error for server errors)
- Don't expose sensitive internal details in production error responses
- Use specific exception handlers before generic ones
- Include correlation IDs for distributed tracing
- Provide actionable error messages for API consumers
- Use proper HTTP status codes (404, 400, 422, 500)

---

**Last Updated**: 2026-01-25
