---
title: "Error Handling in Production"
date: 2026-02-06T00:00:00+07:00
draft: false
weight: 1000033
description: "Manual exception handling → @ControllerAdvice and /error endpoint for consistent error responses"
tags: ["spring-boot", "in-the-field", "production", "error-handling", "exception"]
---

## Why Error Handling Matters

Spring Boot's @ControllerAdvice provides centralized exception handling across all controllers, eliminating duplicate try-catch blocks. In production APIs serving millions of requests, centralized error handling ensures consistent JSON error responses, proper HTTP status codes, and security (no stack trace leaks)—reducing error handling code from 50+ try-catch blocks per controller to single @ExceptionHandler methods.

**Problem**: Manual exception handling requires try-catch in every controller method with inconsistent error responses.

**Solution**: Spring Boot @ControllerAdvice centralizes exception handling with consistent error format and /error fallback.

## Manual Exception Handling

```java
@RestController
public class DonationController {

    @GetMapping("/donations/{id}")
    public ResponseEntity<?> getDonation(@PathVariable Long id) {
        try {
            DonationResponse response = donationService.findById(id);
            return ResponseEntity.ok(response);
        } catch (DonationNotFoundException e) {
            // => Manual error response
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                .body(Map.of("error", "Donation not found", "id", id));
        } catch (Exception e) {
            // => Generic error handling
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(Map.of("error", "Internal server error"));
        }
    }

    @PostMapping("/donations")
    public ResponseEntity<?> createDonation(@RequestBody DonationRequest request) {
        try {
            // => Duplicate try-catch in every method
            return ResponseEntity.ok(donationService.create(request));
        } catch (InvalidDonationException e) {
            return ResponseEntity.badRequest().body(Map.of("error", e.getMessage()));
        } catch (Exception e) {
            return ResponseEntity.status(500).body(Map.of("error", "Error creating donation"));
        }
    }
}
```

**Limitations**: Duplicate error handling, inconsistent error format, hard to maintain.

## Centralized Error Handling with @ControllerAdvice

```java
@RestControllerAdvice
// => @ControllerAdvice + @ResponseBody: handles exceptions from all @RestControllers
public class GlobalExceptionHandler {

    @ExceptionHandler(DonationNotFoundException.class)
    @ResponseStatus(HttpStatus.NOT_FOUND)
    // => Handles DonationNotFoundException from ANY controller
    // => Returns HTTP 404
    public ErrorResponse handleNotFound(DonationNotFoundException ex) {
        return new ErrorResponse(
            "DONATION_NOT_FOUND",
            ex.getMessage(),
            Instant.now()
        );
    }

    @ExceptionHandler(InvalidDonationException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    // => Returns HTTP 400
    public ErrorResponse handleInvalidDonation(InvalidDonationException ex) {
        return new ErrorResponse(
            "INVALID_DONATION",
            ex.getMessage(),
            Instant.now()
        );
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    // => Handles Bean Validation failures
    public ValidationErrorResponse handleValidation(
            MethodArgumentNotValidException ex) {

        Map<String, String> fieldErrors = new HashMap<>();
        ex.getBindingResult().getFieldErrors().forEach(error ->
            fieldErrors.put(error.getField(), error.getDefaultMessage())
        );

        return new ValidationErrorResponse(
            "VALIDATION_FAILED",
            "Request validation failed",
            fieldErrors,
            Instant.now()
        );
    }

    @ExceptionHandler(Exception.class)
    @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
    // => Catch-all for unexpected exceptions
    public ErrorResponse handleGeneric(Exception ex) {
        // => Log exception (don't expose to client)
        log.error("Unexpected error", ex);

        return new ErrorResponse(
            "INTERNAL_ERROR",
            "An unexpected error occurred",  // => Generic message (security)
            Instant.now()
        );
    }
}

// => Controllers without try-catch
@RestController
@RequestMapping("/api/donations")
public class DonationController {

    @GetMapping("/{id}")
    public DonationResponse getDonation(@PathVariable Long id) {
        // => No try-catch: @ControllerAdvice handles exceptions
        return donationService.findById(id);
        // => If DonationNotFoundException thrown, GlobalExceptionHandler catches it
    }

    @PostMapping
    public DonationResponse createDonation(@RequestBody @Valid DonationRequest request) {
        // => No try-catch needed
        return donationService.create(request);
    }
}
```

## Standardized Error Response

```java
public class ErrorResponse {
    private String code;  // => Machine-readable error code
    private String message;  // => Human-readable message
    private Instant timestamp;
    private String path;  // => Request path (optional)

    // => Constructors, getters, setters
}
```

**Example error response**:

```json
{
  "code": "DONATION_NOT_FOUND",
  "message": "Donation with ID 123 not found",
  "timestamp": "2026-02-06T10:30:00Z",
  "path": "/api/donations/123"
}
```

## HTTP Status Code Mapping

```java
@RestControllerAdvice
public class StatusCodeMapping {

    @ExceptionHandler(EntityNotFoundException.class)
    @ResponseStatus(HttpStatus.NOT_FOUND)  // => 404
    public ErrorResponse handleNotFound(EntityNotFoundException ex) { }

    @ExceptionHandler(IllegalArgumentException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)  // => 400
    public ErrorResponse handleBadRequest(IllegalArgumentException ex) { }

    @ExceptionHandler(AccessDeniedException.class)
    @ResponseStatus(HttpStatus.FORBIDDEN)  // => 403
    public ErrorResponse handleForbidden(AccessDeniedException ex) { }

    @ExceptionHandler(UnauthorizedException.class)
    @ResponseStatus(HttpStatus.UNAUTHORIZED)  // => 401
    public ErrorResponse handleUnauthorized(UnauthorizedException ex) { }

    @ExceptionHandler(ConflictException.class)
    @ResponseStatus(HttpStatus.CONFLICT)  // => 409
    public ErrorResponse handleConflict(ConflictException ex) { }
}
```

## Spring Boot /error Endpoint

Spring Boot provides default /error endpoint for exceptions not caught by @ControllerAdvice:

```yaml
# => application.yml
server:
  error:
    include-message: always # => Include exception message
    include-binding-errors: never # => Don't include binding errors (security)
    include-stacktrace: never # => NEVER expose stack trace in production
    include-exception: false # => Don't include exception class name
```

**Trade-offs**: @ControllerAdvice for application exceptions. /error endpoint for framework errors (404, 405, 500).

## Next Steps

- [Validation](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/validation) - Bean Validation integration
- [Security](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/security) - Authentication exceptions
