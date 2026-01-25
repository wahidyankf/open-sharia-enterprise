---
title: Java Unit Test Template
description: Template for creating unit tests in Java using JUnit 5, AssertJ, and Mockito
category: template
tags:
  - java
  - testing
  - tdd
  - unit-test
  - junit
  - mockito
  - assertj
related:
  - ex-so-prla-ja__test-driven-development.md
  - ex-so-de-tedrdeve-te__unit-test-template.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
  - automation-over-manual
last_updated: 2026-01-21
---

# Java Unit Test Template

This template provides a standardized structure for writing unit tests in Java using JUnit 5, AssertJ for assertions, and Mockito for mocking dependencies.

## Template Structure

```java
package com.openshariaenterprise.{domain}.{subdomain};

import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.*;
import org.mockito.*;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Stream;

/**
 * Unit tests for {@link ClassUnderTest}.
 *
 * <p>Test Coverage:
 * <ul>
 *   <li>Business logic validation</li>
 *   <li>Edge cases and boundary conditions</li>
 *   <li>Error handling and exceptions</li>
 *   <li>State transitions</li>
 * </ul>
 *
 * @see ClassUnderTest
 */
@DisplayName("ClassUnderTest Unit Tests")
class ClassUnderTestTest {

    // System Under Test
    private ClassUnderTest sut;

    // Dependencies (Mocks)
    @Mock
    private DependencyInterface mockDependency;

    @Mock
    private AnotherDependency mockAnotherDependency;

    // Test Data
    private TestDataObject validTestData;
    private TestDataObject invalidTestData;

    @BeforeAll
    static void setUpClass() {
        // One-time setup for all tests
        // e.g., Initialize static resources, load configuration
    }

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);

        // Initialize system under test
        sut = new ClassUnderTest(mockDependency, mockAnotherDependency);

        // Prepare test data
        validTestData = createValidTestData();
        invalidTestData = createInvalidTestData();
    }

    @AfterEach
    void tearDown() {
        // Clean up after each test
        sut = null;
        validTestData = null;
        invalidTestData = null;
    }

    @AfterAll
    static void tearDownClass() {
        // One-time cleanup for all tests
    }

    // ========================================
    // Happy Path Tests
    // ========================================

    @Nested
    @DisplayName("Happy Path Scenarios")
    class HappyPathTests {

        @Test
        @DisplayName("Should perform operation successfully with valid input")
        void testSuccessfulOperation() {
            // Arrange
            var input = createValidInput();
            var expectedResult = createExpectedResult();

            when(mockDependency.someMethod(any()))
                .thenReturn(expectedDependencyResult());

            // Act
            var actualResult = sut.performOperation(input);

            // Assert
            assertThat(actualResult)
                .isNotNull()
                .isEqualTo(expectedResult);

            verify(mockDependency).someMethod(any());
            verifyNoMoreInteractions(mockDependency);
        }

        @Test
        @DisplayName("Should handle multiple valid inputs correctly")
        void testMultipleValidInputs() {
            // Arrange
            var inputs = List.of(
                createValidInput("value1"),
                createValidInput("value2"),
                createValidInput("value3")
            );

            // Act
            var results = inputs.stream()
                .map(sut::performOperation)
                .toList();

            // Assert
            assertThat(results)
                .hasSize(3)
                .allMatch(result -> result != null)
                .extracting("status")
                .containsOnly("SUCCESS");
        }
    }

    // ========================================
    // Edge Cases and Boundary Conditions
    // ========================================

    @Nested
    @DisplayName("Edge Cases and Boundaries")
    class EdgeCaseTests {

        @Test
        @DisplayName("Should handle empty input")
        void testEmptyInput() {
            // Arrange
            var emptyInput = createEmptyInput();

            // Act & Assert
            assertThatThrownBy(() -> sut.performOperation(emptyInput))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("Input cannot be empty");
        }

        @Test
        @DisplayName("Should handle null input")
        void testNullInput() {
            // Act & Assert
            assertThatThrownBy(() -> sut.performOperation(null))
                .isInstanceOf(NullPointerException.class)
                .hasMessageContaining("Input must not be null");
        }

        @Test
        @DisplayName("Should handle minimum boundary value")
        void testMinimumBoundary() {
            // Arrange
            var minValue = BigDecimal.ZERO;
            var input = createInputWithValue(minValue);

            // Act
            var result = sut.calculateAmount(input);

            // Assert
            assertThat(result)
                .isEqualByComparingTo(BigDecimal.ZERO);
        }

        @Test
        @DisplayName("Should handle maximum boundary value")
        void testMaximumBoundary() {
            // Arrange
            var maxValue = new BigDecimal("999999999.99");
            var input = createInputWithValue(maxValue);

            // Act
            var result = sut.calculateAmount(input);

            // Assert
            assertThat(result)
                .isGreaterThan(BigDecimal.ZERO)
                .isLessThanOrEqualTo(new BigDecimal("999999999.99"));
        }
    }

    // ========================================
    // Error Handling Tests
    // ========================================

    @Nested
    @DisplayName("Error Handling")
    class ErrorHandlingTests {

        @Test
        @DisplayName("Should throw exception when dependency fails")
        void testDependencyFailure() {
            // Arrange
            var input = createValidInput();
            when(mockDependency.someMethod(any()))
                .thenThrow(new RuntimeException("Dependency failed"));

            // Act & Assert
            assertThatThrownBy(() -> sut.performOperation(input))
                .isInstanceOf(RuntimeException.class)
                .hasMessageContaining("Dependency failed");
        }

        @Test
        @DisplayName("Should handle invalid state transition")
        void testInvalidStateTransition() {
            // Arrange
            sut.setState(State.COMPLETED);

            // Act & Assert
            assertThatThrownBy(() -> sut.transitionTo(State.PENDING))
                .isInstanceOf(IllegalStateException.class)
                .hasMessageContaining("Cannot transition from COMPLETED to PENDING");
        }

        @Test
        @DisplayName("Should validate business rules")
        void testBusinessRuleViolation() {
            // Arrange
            var invalidAmount = new BigDecimal("-100.00");
            var input = createInputWithAmount(invalidAmount);

            // Act & Assert
            assertThatThrownBy(() -> sut.processTransaction(input))
                .isInstanceOf(BusinessRuleViolationException.class)
                .hasMessageContaining("Amount must be positive");
        }
    }

    // ========================================
    // Parameterized Tests
    // ========================================

    @Nested
    @DisplayName("Parameterized Tests")
    class ParameterizedTests {

        @ParameterizedTest(name = "Input: {0}, Expected: {1}")
        @CsvSource({
            "100.00, 2.50",
            "5000.00, 125.00",
            "10000.00, 250.00",
            "100000.00, 2500.00"
        })
        @DisplayName("Should calculate correct percentage for various amounts")
        void testCalculationWithMultipleInputs(String inputAmount, String expectedResult) {
            // Arrange
            var amount = new BigDecimal(inputAmount);
            var input = createInputWithAmount(amount);

            // Act
            var result = sut.calculatePercentage(input);

            // Assert
            assertThat(result)
                .isEqualByComparingTo(new BigDecimal(expectedResult));
        }

        @ParameterizedTest
        @ValueSource(strings = {"PENDING", "APPROVED", "REJECTED", "COMPLETED"})
        @DisplayName("Should handle all valid states")
        void testAllValidStates(String stateName) {
            // Arrange
            var state = State.valueOf(stateName);

            // Act & Assert
            assertThatCode(() -> sut.setState(state))
                .doesNotThrowAnyException();

            assertThat(sut.getState()).isEqualTo(state);
        }

        @ParameterizedTest
        @MethodSource("provideInvalidInputs")
        @DisplayName("Should reject all invalid inputs")
        void testInvalidInputs(InvalidInputCase testCase) {
            // Act & Assert
            assertThatThrownBy(() -> sut.performOperation(testCase.input()))
                .isInstanceOf(testCase.expectedException())
                .hasMessageContaining(testCase.expectedMessage());
        }

        static Stream<InvalidInputCase> provideInvalidInputs() {
            return Stream.of(
                new InvalidInputCase(null, NullPointerException.class, "must not be null"),
                new InvalidInputCase(createEmptyInput(), IllegalArgumentException.class, "cannot be empty"),
                new InvalidInputCase(createInvalidInput(), ValidationException.class, "validation failed")
            );
        }
    }

    // ========================================
    // State Verification Tests
    // ========================================

    @Nested
    @DisplayName("State Verification")
    class StateVerificationTests {

        @Test
        @DisplayName("Should verify correct state after operation")
        void testStateAfterOperation() {
            // Arrange
            var input = createValidInput();
            assertThat(sut.getState()).isEqualTo(State.INITIAL);

            // Act
            sut.performOperation(input);

            // Assert
            assertThat(sut.getState()).isEqualTo(State.COMPLETED);
            assertThat(sut.isProcessed()).isTrue();
            assertThat(sut.getLastModified()).isNotNull();
        }

        @Test
        @DisplayName("Should maintain immutability")
        void testImmutability() {
            // Arrange
            var originalValue = sut.getValue();
            var originalState = sut.getState();

            // Act - Attempt to modify
            var returnedValue = sut.getValue();
            returnedValue = "modified"; // This should not affect original

            // Assert
            assertThat(sut.getValue()).isEqualTo(originalValue);
            assertThat(sut.getState()).isEqualTo(originalState);
        }
    }

    // ========================================
    // Mock Verification Tests
    // ========================================

    @Nested
    @DisplayName("Mock Interaction Verification")
    class MockVerificationTests {

        @Test
        @DisplayName("Should call dependency with correct arguments")
        void testDependencyCalledWithCorrectArgs() {
            // Arrange
            var input = createValidInput();
            var expectedArg = createExpectedArgument();

            // Act
            sut.performOperation(input);

            // Assert
            verify(mockDependency).someMethod(eq(expectedArg));
            verify(mockDependency, times(1)).someMethod(any());
            verifyNoMoreInteractions(mockDependency);
        }

        @Test
        @DisplayName("Should call dependencies in correct order")
        void testDependencyCallOrder() {
            // Arrange
            var input = createValidInput();
            InOrder inOrder = inOrder(mockDependency, mockAnotherDependency);

            // Act
            sut.performComplexOperation(input);

            // Assert
            inOrder.verify(mockDependency).firstMethod(any());
            inOrder.verify(mockAnotherDependency).secondMethod(any());
            inOrder.verify(mockDependency).finalMethod(any());
        }

        @Test
        @DisplayName("Should not call dependency when condition not met")
        void testConditionalDependencyCall() {
            // Arrange
            var input = createInputWithConditionFalse();

            // Act
            sut.performOperation(input);

            // Assert
            verifyNoInteractions(mockDependency);
        }
    }

    // ========================================
    // Helper Methods
    // ========================================

    private TestDataObject createValidTestData() {
        return TestDataObject.builder()
            .id(UUID.randomUUID())
            .amount(new BigDecimal("1000.00"))
            .currency("USD")
            .status(Status.ACTIVE)
            .createdAt(LocalDateTime.now())
            .build();
    }

    private TestDataObject createInvalidTestData() {
        return TestDataObject.builder()
            .id(null)
            .amount(new BigDecimal("-100.00"))
            .currency("INVALID")
            .status(null)
            .build();
    }

    private InputObject createValidInput() {
        return new InputObject(
            "valid-id",
            new BigDecimal("5000.00"),
            "USD",
            LocalDateTime.now()
        );
    }

    private InputObject createValidInput(String value) {
        return new InputObject(value, validAmount(), "USD", LocalDateTime.now());
    }

    private InputObject createEmptyInput() {
        return new InputObject("", BigDecimal.ZERO, "", null);
    }

    private InputObject createInvalidInput() {
        return new InputObject("invalid", new BigDecimal("-1"), "XXX", null);
    }

    private InputObject createInputWithValue(BigDecimal value) {
        return new InputObject("test-id", value, "USD", LocalDateTime.now());
    }

    private InputObject createInputWithAmount(BigDecimal amount) {
        return new InputObject("test-id", amount, "USD", LocalDateTime.now());
    }

    private InputObject createInputWithConditionFalse() {
        return new InputObject("test-id", BigDecimal.ZERO, "USD", LocalDateTime.now());
    }

    private ExpectedResult createExpectedResult() {
        return new ExpectedResult("success", new BigDecimal("125.00"));
    }

    private DependencyResult createExpectedDependencyResult() {
        return new DependencyResult(true, "processed");
    }

    private ArgumentObject createExpectedArgument() {
        return new ArgumentObject("expected-value");
    }

    private BigDecimal validAmount() {
        return new BigDecimal("1000.00");
    }

    // Test Data Classes
    record InvalidInputCase(
        InputObject input,
        Class<? extends Exception> expectedException,
        String expectedMessage
    ) {}
}
```

## Financial Domain Example: Zakat Calculator

```java
package com.openhariaenterprise.zakat.calculation;

import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.*;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Unit tests for {@link ZakatCalculator}.
 *
 * <p>Tests Zakat calculation logic including:
 * <ul>
 *   <li>2.5% calculation on wealth above nisab</li>
 *   <li>Haul (one lunar year) validation</li>
 *   <li>Multiple asset type aggregation</li>
 *   <li>Currency conversion</li>
 * </ul>
 */
@DisplayName("ZakatCalculator Unit Tests")
class ZakatCalculatorTest {

    private ZakatCalculator calculator;

    @Mock
    private NisabProvider mockNisabProvider;

    @Mock
    private CurrencyConverter mockCurrencyConverter;

    @Mock
    private HaulValidator mockHaulValidator;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        calculator = new ZakatCalculator(
            mockNisabProvider,
            mockCurrencyConverter,
            mockHaulValidator
        );
    }

    @Nested
    @DisplayName("Basic Zakat Calculation")
    class BasicCalculationTests {

        @Test
        @DisplayName("Should calculate 2.5% zakat for wealth above nisab")
        void testBasicZakatCalculation() {
            // Arrange
            var wealth = Money.of(new BigDecimal("100000.00"), "USD");
            var nisab = Money.of(new BigDecimal("5000.00"), "USD");
            var expectedZakat = Money.of(new BigDecimal("2500.00"), "USD");

            when(mockNisabProvider.getCurrentNisab("USD")).thenReturn(nisab);
            when(mockHaulValidator.hasCompletedHaul(any(), any())).thenReturn(true);

            // Act
            var actualZakat = calculator.calculateZakat(wealth);

            // Assert
            assertThat(actualZakat).isEqualTo(expectedZakat);
            verify(mockNisabProvider).getCurrentNisab("USD");
            verify(mockHaulValidator).hasCompletedHaul(any(), any());
        }

        @Test
        @DisplayName("Should return zero when wealth below nisab")
        void testNoZakatBelowNisab() {
            // Arrange
            var wealth = Money.of(new BigDecimal("4000.00"), "USD");
            var nisab = Money.of(new BigDecimal("5000.00"), "USD");

            when(mockNisabProvider.getCurrentNisab("USD")).thenReturn(nisab);

            // Act
            var zakat = calculator.calculateZakat(wealth);

            // Assert
            assertThat(zakat.getAmount())
                .isEqualByComparingTo(BigDecimal.ZERO);
        }

        @Test
        @DisplayName("Should return zero when haul not completed")
        void testNoZakatWithoutHaul() {
            // Arrange
            var wealth = Money.of(new BigDecimal("100000.00"), "USD");
            var nisab = Money.of(new BigDecimal("5000.00"), "USD");

            when(mockNisabProvider.getCurrentNisab("USD")).thenReturn(nisab);
            when(mockHaulValidator.hasCompletedHaul(any(), any())).thenReturn(false);

            // Act
            var zakat = calculator.calculateZakat(wealth);

            // Assert
            assertThat(zakat.getAmount())
                .isEqualByComparingTo(BigDecimal.ZERO);
        }
    }

    @Nested
    @DisplayName("Parameterized Calculations")
    class ParameterizedCalculationTests {

        @ParameterizedTest(name = "Wealth: {0} USD, Expected Zakat: {1} USD")
        @CsvSource({
            "100000.00, 2500.00",
            "50000.00, 1250.00",
            "10000.00, 250.00",
            "5000.00, 125.00"
        })
        @DisplayName("Should calculate correct zakat for various wealth amounts")
        void testZakatForVariousAmounts(String wealthAmount, String expectedZakatAmount) {
            // Arrange
            var wealth = Money.of(new BigDecimal(wealthAmount), "USD");
            var nisab = Money.of(new BigDecimal("5000.00"), "USD");
            var expectedZakat = Money.of(new BigDecimal(expectedZakatAmount), "USD");

            when(mockNisabProvider.getCurrentNisab("USD")).thenReturn(nisab);
            when(mockHaulValidator.hasCompletedHaul(any(), any())).thenReturn(true);

            // Act
            var actualZakat = calculator.calculateZakat(wealth);

            // Assert
            assertThat(actualZakat.getAmount())
                .isEqualByComparingTo(expectedZakat.getAmount());
        }
    }

    @Nested
    @DisplayName("Multi-Currency Support")
    class MultiCurrencyTests {

        @Test
        @DisplayName("Should convert to base currency before calculation")
        void testCurrencyConversion() {
            // Arrange
            var wealthInEUR = Money.of(new BigDecimal("10000.00"), "EUR");
            var wealthInUSD = Money.of(new BigDecimal("11000.00"), "USD");
            var nisab = Money.of(new BigDecimal("5000.00"), "USD");

            when(mockCurrencyConverter.convert(wealthInEUR, "USD"))
                .thenReturn(wealthInUSD);
            when(mockNisabProvider.getCurrentNisab("USD")).thenReturn(nisab);
            when(mockHaulValidator.hasCompletedHaul(any(), any())).thenReturn(true);

            // Act
            var zakat = calculator.calculateZakat(wealthInEUR);

            // Assert
            assertThat(zakat.getAmount())
                .isEqualByComparingTo(new BigDecimal("275.00"));
            verify(mockCurrencyConverter).convert(wealthInEUR, "USD");
        }
    }
}
```

## Usage Guidelines

1. **Naming**: Use descriptive test method names with `test` prefix or `@DisplayName`
2. **Structure**: Follow Arrange-Act-Assert (AAA) pattern consistently
3. **Isolation**: Each test should be independent and not rely on others
4. **Mocking**: Mock external dependencies, test only the unit in isolation
5. **Assertions**: Use AssertJ for fluent, readable assertions
6. **Coverage**: Test happy paths, edge cases, and error conditions
7. **Clarity**: Use `@Nested` classes to group related tests logically

## Related Templates

- [Integration Test Template](./ex-so-prla-ja-te__integration-test-template.md)
- [BDD Step Definition Template](./ex-so-prla-ja-te__bdd-step-definition-template.md)

## See Also

- [Test-Driven Development](../ex-so-prla-ja__test-driven-development.md)
- [Behaviour-Driven Development](../ex-so-prla-ja__behaviour-driven-development.md)

---

**Principles Applied**: Simplicity Over Complexity, Explicit Over Implicit, Automation Over Manual

---

**Last Updated**: 2025-01-23
**Java Version**: 17+
