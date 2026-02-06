---
title: "Validation in Production"
date: 2026-02-06T00:00:00+07:00
draft: false
weight: 1000032
description: "Manual validator setup → auto-configured Hibernate Validator with @Valid integration in Spring Boot"
tags: ["spring-boot", "in-the-field", "production", "validation", "bean-validation"]
---

## Why Validation Matters

Spring Boot's auto-configured Bean Validation (Hibernate Validator) eliminates manual validation code through declarative annotations (@NotNull, @Size, @Email). In production APIs processing millions of requests with complex validation rules (email format, amount ranges, date constraints), annotation-based validation reduces 500+ lines of if/else checks to concise@Validated declarations—while providing standardized error responses.

**Problem**: Manual validation requires verbose if/else checks scattered across controllers and services.

**Solution**: Spring Boot auto-configures Hibernate Validator for declarative @Valid/@Validated validation.

## Manual Validation

```java
@RestController
public class DonationController {

    @PostMapping("/donations")
    public ResponseEntity<?> createDonation(@RequestBody DonationRequest request) {
        // => Manual validation: verbose, error-prone
        List<String> errors = new ArrayList<>();

        if (request.getDonorName() == null || request.getDonorName().trim().isEmpty()) {
            errors.add("Donor name is required");
        }

        if (request.getEmail() == null || !request.getEmail().matches("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,6}$")) {
            errors.add("Invalid email format");
        }

        if (request.getAmount() == null || request.getAmount().compareTo(BigDecimal.ZERO) <= 0) {
            errors.add("Amount must be positive");
        }

        if (!errors.isEmpty()) {
            return ResponseEntity.badRequest().body(Map.of("errors", errors));
        }

        // => Business logic
        return ResponseEntity.ok(donationService.create(request));
    }
}
```

**Limitations**: Boilerplate, inconsistent error messages, validation logic mixed with business logic.

## Bean Validation with Spring Boot

```java
// => Request DTO with validation annotations
public class DonationRequest {

    @NotBlank(message = "Donor name is required")
    // => @NotBlank: not null, not empty, not whitespace only
    @Size(min = 3, max = 100, message = "Name must be 3-100 characters")
    private String donorName;

    @NotNull(message = "Email is required")
    @Email(message = "Invalid email format")
    // => @Email: validates email pattern
    private String email;

    @NotNull(message = "Amount is required")
    @Positive(message = "Amount must be positive")
    // => @Positive: > 0
    @Digits(integer = 10, fraction = 2, message = "Invalid amount format")
    // => Max 10 digits before decimal, 2 after
    private BigDecimal amount;

    @NotNull(message = "Category is required")
    private ZakatCategory category;

    // => Getters/setters
}

@RestController
@RequestMapping("/api/donations")
public class DonationController {

    @PostMapping
    public ResponseEntity<DonationResponse> createDonation(
            @RequestBody @Valid DonationRequest request) {
        // => @Valid triggers validation before method execution
        // => If validation fails, MethodArgumentNotValidException thrown
        // => No manual validation code needed

        DonationResponse response = donationService.create(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
}
```

**Automatic error handling** (Spring Boot provides default):

```json
{
  "timestamp": "2026-02-06T10:30:00",
  "status": 400,
  "error": "Bad Request",
  "errors": [
    { "field": "donorName", "message": "Donor name is required" },
    { "field": "amount", "message": "Amount must be positive" }
  ]
}
```

## Custom Error Response

```java
@RestControllerAdvice
public class ValidationExceptionHandler {

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ErrorResponse> handleValidation(
            MethodArgumentNotValidException ex) {

        // => Extract validation errors from exception
        Map<String, String> errors = new HashMap<>();
        ex.getBindingResult().getFieldErrors().forEach(error ->
            errors.put(error.getField(), error.getDefaultMessage())
        );

        ErrorResponse response = new ErrorResponse(
            "Validation failed",
            errors
        );

        return ResponseEntity.badRequest().body(response);
    }
}
```

## Common Validation Annotations

```java
public class ComprehensiveValidationExample {

    @NotNull  // => Not null
    @NotEmpty  // => Not null, size > 0 (collections/strings)
    @NotBlank  // => Not null, not empty, not whitespace (strings only)
    private String field1;

    @Size(min = 5, max = 50)  // => Length constraints
    private String field2;

    @Min(18)  // => Minimum value
    @Max(100)  // => Maximum value
    private Integer age;

    @Positive  // => > 0
    @PositiveOrZero  // => >= 0
    private BigDecimal amount;

    @Email  // => Email format
    private String email;

    @Pattern(regexp = "^\\+?[0-9]{10,15}$")  // => Regex validation
    private String phoneNumber;

    @Past  // => Date in the past
    private LocalDate birthDate;

    @Future  // => Date in the future
    private LocalDate appointmentDate;

    @Valid  // => Nested validation
    private Address address;
}
```

## Custom Validator

```java
@Constraint(validatedBy = NisabValidator.class)
@Target({ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
public @interface ValidNisab {
    String message() default "Amount below nisab threshold";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}

public class NisabValidator implements ConstraintValidator<ValidNisab, BigDecimal> {

    private static final BigDecimal NISAB = new BigDecimal("85"); // grams of gold

    @Override
    public boolean isValid(BigDecimal value, ConstraintValidatorContext context) {
        if (value == null) return true;  // => @NotNull handles null
        return value.compareTo(NISAB) >= 0;  // => >= nisab threshold
    }
}

// => Usage
public class ZakatRequest {
    @NotNull
    @ValidNisab(message = "Wealth must meet nisab threshold (85g gold)")
    private BigDecimal wealth;
}
```

**Trade-offs**: Bean Validation covers 95% validation needs. Custom validators for business rules (nisab, hibr year).

## Next Steps

- [Error Handling](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/error-handling) - @ControllerAdvice patterns
- [REST API Development](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/rest-api-development) - Production API patterns
