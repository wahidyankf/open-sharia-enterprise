---
title: Java BDD Step Definition Template
description: Template for creating Cucumber step definitions in Java with Given-When-Then pattern and reusable steps
category: template
tags:
  - java
  - testing
  - bdd
  - cucumber
  - gherkin
  - step-definitions
related:
  - ex-so-stla-ja__behaviour-driven-development.md
  - ex-so-de-bdd-te__step-definition-template.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
last_updated: 2026-01-21
---

# Java BDD Step Definition Template

This template provides a standardized structure for creating Cucumber step definitions in Java using the Given-When-Then pattern for behavior-driven development.

## Template Structure

```java
package com.openshariaenterprise.{domain}.steps;

import io.cucumber.java.en.*;
import io.cucumber.datatable.DataTable;
import io.cucumber.spring.CucumberContextConfiguration;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

import static org.assertj.core.api.Assertions.*;

/**
 * Step definitions for {Feature Name} scenarios.
 *
 * <p>This class contains Cucumber step definitions that implement the Given-When-Then
 * steps for testing {feature description}.
 *
 * <p>Step Patterns:
 * <ul>
 *   <li>Given - Set up test preconditions</li>
 *   <li>When - Execute the action being tested</li>
 *   <li>Then - Verify expected outcomes</li>
 *   <li>And/But - Additional conditions or assertions</li>
 * </ul>
 *
 * @see FeatureUnderTest
 */
@CucumberContextConfiguration
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class FeatureStepDefinitions {

    @Autowired
    private TestRestTemplate restTemplate;

    @Autowired
    private Repository repository;

    // Scenario Context (Shared state between steps)
    private TestContext context;
    private ResponseEntity<EntityResponse> lastResponse;
    private EntityUnderTest entityUnderTest;
    private Exception lastException;

    @Before
    public void setUp() {
        context = new TestContext();
        lastResponse = null;
        entityUnderTest = null;
        lastException = null;

        // Clean database before each scenario
        repository.deleteAll();
    }

    @After
    public void tearDown() {
        // Clean up after each scenario
        repository.deleteAll();
        context = null;
    }

    // ========================================
    // Given Steps (Setup Preconditions)
    // ========================================

    @Given("a user with username {string} exists")
    public void aUserWithUsernameExists(String username) {
        var user = UserTestData.createUser(username);
        context.setUser(user);
        repository.save(user);
    }

    @Given("the following entities exist:")
    public void theFollowingEntitiesExist(DataTable dataTable) {
        var entities = dataTable.asMaps(String.class, String.class);

        for (var entityData : entities) {
            var entity = EntityUnderTest.builder()
                .name(entityData.get("name"))
                .amount(new BigDecimal(entityData.get("amount")))
                .status(entityData.get("status"))
                .build();

            repository.save(entity);
            context.addEntity(entity);
        }
    }

    @Given("an entity with ID {string} and amount {string}")
    public void anEntityWithIDAndAmount(String id, String amount) {
        entityUnderTest = EntityUnderTest.builder()
            .id(id)
            .amount(new BigDecimal(amount))
            .status("ACTIVE")
            .build();

        repository.save(entityUnderTest);
    }

    @Given("the entity has the following properties:")
    public void theEntityHasTheFollowingProperties(DataTable dataTable) {
        var properties = dataTable.asMap(String.class, String.class);

        entityUnderTest = EntityUnderTest.builder()
            .name(properties.get("name"))
            .amount(new BigDecimal(properties.get("amount")))
            .currency(properties.get("currency"))
            .status(properties.get("status"))
            .build();

        repository.save(entityUnderTest);
    }

    @Given("the system is in {string} state")
    public void theSystemIsInState(String state) {
        context.setSystemState(state);
        // Configure system for the specified state
        switch (state) {
            case "maintenance" -> enableMaintenanceMode();
            case "normal" -> disableMaintenanceMode();
            default -> throw new IllegalArgumentException("Unknown state: " + state);
        }
    }

    // ========================================
    // When Steps (Execute Actions)
    // ========================================

    @When("I create a new entity with name {string} and amount {string}")
    public void iCreateANewEntityWithNameAndAmount(String name, String amount) {
        var request = new EntityCreateRequest(name, new BigDecimal(amount));

        lastResponse = restTemplate.postForEntity(
            "/api/v1/entities",
            request,
            EntityResponse.class
        );
    }

    @When("I request entity with ID {string}")
    public void iRequestEntityWithID(String entityId) {
        lastResponse = restTemplate.getForEntity(
            "/api/v1/entities/" + entityId,
            EntityResponse.class
        );
    }

    @When("I update the entity with:")
    public void iUpdateTheEntityWith(DataTable dataTable) {
        var updates = dataTable.asMap(String.class, String.class);
        var request = new EntityUpdateRequest(
            updates.get("name"),
            new BigDecimal(updates.get("amount")),
            updates.get("status")
        );

        lastResponse = restTemplate.exchange(
            "/api/v1/entities/" + entityUnderTest.getId(),
            HttpMethod.PUT,
            new HttpEntity<>(request),
            EntityResponse.class
        );
    }

    @When("I delete the entity")
    public void iDeleteTheEntity() {
        restTemplate.delete("/api/v1/entities/" + entityUnderTest.getId());
    }

    @When("I perform {string} operation")
    public void iPerformOperation(String operation) {
        try {
            switch (operation) {
                case "calculation" -> performCalculation();
                case "validation" -> performValidation();
                case "transformation" -> performTransformation();
                default -> throw new IllegalArgumentException("Unknown operation: " + operation);
            }
        } catch (Exception e) {
            lastException = e;
        }
    }

    @When("the following operations are executed:")
    public void theFollowingOperationsAreExecuted(List<String> operations) {
        for (var operation : operations) {
            iPerformOperation(operation);
        }
    }

    // ========================================
    // Then Steps (Verify Outcomes)
    // ========================================

    @Then("the response status should be {int}")
    public void theResponseStatusShouldBe(int expectedStatus) {
        assertThat(lastResponse.getStatusCode().value())
            .isEqualTo(expectedStatus);
    }

    @Then("the response should contain:")
    public void theResponseShouldContain(DataTable dataTable) {
        var expectedValues = dataTable.asMap(String.class, String.class);
        var responseBody = lastResponse.getBody();

        assertThat(responseBody).isNotNull();

        for (var entry : expectedValues.entrySet()) {
            var field = entry.getKey();
            var expectedValue = entry.getValue();

            switch (field) {
                case "name" -> assertThat(responseBody.getName()).isEqualTo(expectedValue);
                case "amount" -> assertThat(responseBody.getAmount())
                    .isEqualByComparingTo(new BigDecimal(expectedValue));
                case "status" -> assertThat(responseBody.getStatus()).isEqualTo(expectedValue);
                default -> throw new IllegalArgumentException("Unknown field: " + field);
            }
        }
    }

    @Then("the entity should be stored in the database")
    public void theEntityShouldBeStoredInTheDatabase() {
        var responseBody = lastResponse.getBody();
        assertThat(responseBody).isNotNull();

        var storedEntity = repository.findById(responseBody.getId());
        assertThat(storedEntity).isPresent();
    }

    @Then("the entity should have {int} associated records")
    public void theEntityShouldHaveAssociatedRecords(int expectedCount) {
        var entity = repository.findById(entityUnderTest.getId()).orElseThrow();
        assertThat(entity.getAssociatedRecords()).hasSize(expectedCount);
    }

    @Then("the operation should fail with {string}")
    public void theOperationShouldFailWith(String expectedError) {
        assertThat(lastException)
            .isNotNull()
            .hasMessageContaining(expectedError);
    }

    @Then("the operation should succeed")
    public void theOperationShouldSucceed() {
        assertThat(lastException).isNull();
        assertThat(lastResponse.getStatusCode().is2xxSuccessful()).isTrue();
    }

    @Then("the following entities should exist:")
    public void theFollowingEntitiesShouldExist(DataTable dataTable) {
        var expectedEntities = dataTable.asMaps(String.class, String.class);
        var actualEntities = repository.findAll();

        assertThat(actualEntities).hasSize(expectedEntities.size());

        for (var expectedData : expectedEntities) {
            var matchingEntity = actualEntities.stream()
                .filter(e -> e.getName().equals(expectedData.get("name")))
                .findFirst()
                .orElseThrow(() -> new AssertionError("Entity not found: " + expectedData.get("name")));

            assertThat(matchingEntity.getAmount())
                .isEqualByComparingTo(new BigDecimal(expectedData.get("amount")));
            assertThat(matchingEntity.getStatus())
                .isEqualTo(expectedData.get("status"));
        }
    }

    // ========================================
    // And/But Steps (Additional Conditions)
    // ========================================

    @And("the entity is in {string} status")
    public void theEntityIsInStatus(String status) {
        entityUnderTest.setStatus(status);
        repository.save(entityUnderTest);
    }

    @And("I wait for {int} seconds")
    public void iWaitForSeconds(int seconds) throws InterruptedException {
        Thread.sleep(seconds * 1000L);
    }

    @But("the entity should not be deleted")
    public void theEntityShouldNotBeDeleted() {
        var entity = repository.findById(entityUnderTest.getId());
        assertThat(entity).isPresent();
    }

    @But("no entities should be created")
    public void noEntitiesShouldBeCreated() {
        var entities = repository.findAll();
        assertThat(entities).isEmpty();
    }

    // ========================================
    // Helper Methods
    // ========================================

    private void enableMaintenanceMode() {
        // Implementation
    }

    private void disableMaintenanceMode() {
        // Implementation
    }

    private void performCalculation() {
        // Implementation
    }

    private void performValidation() {
        // Implementation
    }

    private void performTransformation() {
        // Implementation
    }

    // ========================================
    // Test Context (Shared State)
    // ========================================

    private static class TestContext {
        private User user;
        private String systemState;
        private final List<EntityUnderTest> entities = new ArrayList<>();

        public void setUser(User user) {
            this.user = user;
        }

        public User getUser() {
            return user;
        }

        public void setSystemState(String state) {
            this.systemState = state;
        }

        public String getSystemState() {
            return systemState;
        }

        public void addEntity(EntityUnderTest entity) {
            entities.add(entity);
        }

        public List<EntityUnderTest> getEntities() {
            return entities;
        }
    }
}
```

## Financial Domain Example: Zakat Calculation Step Definitions

```java
package com.openshariaenterprise.zakat.steps;

import io.cucumber.java.en.*;
import io.cucumber.datatable.DataTable;
import io.cucumber.spring.CucumberContextConfiguration;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;

import static org.assertj.core.api.Assertions.*;

/**
 * Step definitions for Zakat calculation scenarios.
 *
 * <p>Implements Given-When-Then steps for testing Zakat calculation logic including:
 * <ul>
 *   <li>Wealth assessment above/below nisab</li>
 *   <li>Haul (one lunar year) validation</li>
 *   <li>Zakat rate calculation (2.5%)</li>
 *   <li>Payment recording and receipt generation</li>
 * </ul>
 */
@CucumberContextConfiguration
@SpringBootTest
public class ZakatCalculationStepDefinitions {

    @Autowired
    private ZakatCalculationService calculationService;

    @Autowired
    private ZakatAccountRepository accountRepository;

    @Autowired
    private ZakatPaymentRepository paymentRepository;

    private ZakatAccount account;
    private ZakatCalculation calculationResult;
    private ZakatPayment payment;
    private Money zakatAmount;
    private Exception lastException;

    @Before
    public void setUp() {
        accountRepository.deleteAll();
        paymentRepository.deleteAll();
        account = null;
        calculationResult = null;
        payment = null;
        zakatAmount = null;
        lastException = null;
    }

    // ========================================
    // Given Steps
    // ========================================

    @Given("a Zakat account with balance {string}")
    public void aZakatAccountWithBalance(String balance) {
        var amount = Money.parse(balance);
        account = ZakatAccount.open(
            ZakatAccountId.generate(),
            AccountHolderId.generate(),
            amount,
            Money.of(new BigDecimal("5000"), "USD"), // Default nisab
            UserId.system()
        );

        accountRepository.save(account);
    }

    @Given("the nisab threshold is {string}")
    public void theNisabThresholdIs(String nisab) {
        var nisabAmount = Money.parse(nisab);

        if (account != null) {
            account.updateNisabThreshold(nisabAmount, UserId.system());
            accountRepository.save(account);
        }
    }

    @Given("the account has been active for {int} days")
    public void theAccountHasBeenActiveFor(int days) {
        // Set haul start date in the past
        var haulStartDate = LocalDate.now().minusDays(days);
        account.setHaulStartDate(haulStartDate);
        accountRepository.save(account);
    }

    @Given("the account has the following transaction history:")
    public void theAccountHasTheFollowingTransactionHistory(DataTable dataTable) {
        var transactions = dataTable.asMaps(String.class, String.class);

        for (var txnData : transactions) {
            var date = LocalDate.parse(txnData.get("date"));
            var amount = Money.parse(txnData.get("amount"));
            var type = txnData.get("type");

            if ("deposit".equalsIgnoreCase(type)) {
                account.deposit(amount, UserId.system());
            } else if ("withdrawal".equalsIgnoreCase(type)) {
                account.withdraw(amount, UserId.system());
            }
        }

        accountRepository.save(account);
    }

    // ========================================
    // When Steps
    // ========================================

    @When("I calculate Zakat for the account")
    public void iCalculateZakatForTheAccount() {
        try {
            calculationResult = calculationService.calculateZakat(account.getId());
        } catch (Exception e) {
            lastException = e;
        }
    }

    @When("I process a Zakat payment of {string}")
    public void iProcessAZakatPaymentOf(String amount) {
        try {
            var paymentAmount = Money.parse(amount);
            payment = ZakatPayment.create(
                ZakatPaymentId.generate(),
                account.getId(),
                paymentAmount,
                PaymentMethod.BANK_TRANSFER,
                "Local Zakat Foundation"
            );

            account.recordPayment(payment, UserId.system());
            paymentRepository.save(payment);
            accountRepository.save(account);
        } catch (Exception e) {
            lastException = e;
        }
    }

    @When("I generate a Zakat receipt")
    public void iGenerateAZakatReceipt() {
        // Receipt generation logic
        assertThat(payment).isNotNull();
    }

    // ========================================
    // Then Steps
    // ========================================

    @Then("the Zakat amount should be {string}")
    public void theZakatAmountShouldBe(String expectedAmount) {
        assertThat(calculationResult).isNotNull();
        assertThat(calculationResult.isEligible()).isTrue();

        var expected = Money.parse(expectedAmount);
        assertThat(calculationResult.getZakatAmount())
            .isEqualTo(expected);
    }

    @Then("the account should be eligible for Zakat")
    public void theAccountShouldBeEligibleForZakat() {
        assertThat(calculationResult).isNotNull();
        assertThat(calculationResult.isEligible()).isTrue();
    }

    @Then("the account should not be eligible for Zakat")
    public void theAccountShouldNotBeEligibleForZakat() {
        assertThat(calculationResult).isNotNull();
        assertThat(calculationResult.isEligible()).isFalse();
    }

    @Then("the reason should be {string}")
    public void theReasonShouldBe(String expectedReason) {
        assertThat(calculationResult).isNotNull();
        assertThat(calculationResult.getIneligibilityReason())
            .contains(expectedReason);
    }

    @Then("the account balance should be {string}")
    public void theAccountBalanceShouldBe(String expectedBalance) {
        var expected = Money.parse(expectedBalance);
        var actual = accountRepository.findById(account.getId())
            .map(ZakatAccount::getBalance)
            .orElseThrow();

        assertThat(actual).isEqualTo(expected);
    }

    @Then("a payment record should exist with:")
    public void aPaymentRecordShouldExistWith(DataTable dataTable) {
        var expectedData = dataTable.asMap(String.class, String.class);

        assertThat(payment).isNotNull();
        assertThat(payment.getAmount()).isEqualTo(Money.parse(expectedData.get("amount")));
        assertThat(payment.getStatus().toString()).isEqualTo(expectedData.get("status"));
        assertThat(payment.getRecipient()).isEqualTo(expectedData.get("recipient"));
    }

    @Then("the calculation should show:")
    public void theCalculationShouldShow(DataTable dataTable) {
        var expectedData = dataTable.asMap(String.class, String.class);

        assertThat(calculationResult).isNotNull();
        assertThat(calculationResult.getWealthAmount())
            .isEqualTo(Money.parse(expectedData.get("wealth")));
        assertThat(calculationResult.getNisabThreshold())
            .isEqualTo(Money.parse(expectedData.get("nisab")));
        assertThat(calculationResult.getZakatAmount())
            .isEqualTo(Money.parse(expectedData.get("zakat")));
    }

    // ========================================
    // And/But Steps
    // ========================================

    @And("the haul period has been completed")
    public void theHaulPeriodHasBeenCompleted() {
        account.setHaulStartDate(LocalDate.now().minusDays(355));
        accountRepository.save(account);
    }

    @But("the payment should fail")
    public void thePaymentShouldFail() {
        assertThat(lastException).isNotNull();
        assertThat(payment).isNull();
    }
}
```

## Companion Feature File

```gherkin
# features/zakat_calculation.feature

Feature: Zakat Calculation
  As a Muslim account holder
  I want to calculate my Zakat obligation
  So that I can fulfill my religious duty

  Background:
    Given the nisab threshold is "5,000 USD"

  Scenario: Calculate Zakat for wealth above nisab after complete haul
    Given a Zakat account with balance "100,000 USD"
    And the account has been active for 355 days
    When I calculate Zakat for the account
    Then the account should be eligible for Zakat
    And the Zakat amount should be "2,500.00 USD"

  Scenario: No Zakat due when balance below nisab
    Given a Zakat account with balance "3,000 USD"
    When I calculate Zakat for the account
    Then the account should not be eligible for Zakat
    And the reason should be "Balance below nisab threshold"

  Scenario: Process Zakat payment
    Given a Zakat account with balance "100,000 USD"
    And the account has been active for 355 days
    When I calculate Zakat for the account
    And I process a Zakat payment of "2,500.00 USD"
    Then the account balance should be "97,500.00 USD"
    And a payment record should exist with:
      | amount    | 2,500.00 USD |
      | status    | COMPLETED    |
      | recipient | Local Zakat Foundation |
```

## Usage Guidelines

1. **One Feature Per Class**: Create separate step definition classes for each feature
2. **Shared State**: Use instance variables or context objects for sharing state between steps
3. **Reusability**: Write steps that can be reused across multiple scenarios
4. **Clear Naming**: Use descriptive step names that read like natural language
5. **Data Tables**: Use DataTables for complex data structures
6. **Cleanup**: Always clean up test data in @Before and @After hooks
7. **Assertions**: Use AssertJ for fluent, readable assertions
8. **Error Handling**: Capture exceptions and verify them in Then steps

## Related Templates

- [Unit Test Template](./ex-so-stla-ja-te__unit-test-template.md)
- [Integration Test Template](./ex-so-stla-ja-te__integration-test-template.md)

## See Also

- [Behaviour-Driven Development](../ex-so-stla-ja__behaviour-driven-development.md)
- [Cucumber Documentation](https://cucumber.io/docs/cucumber/)
- [Gherkin Reference](https://cucumber.io/docs/gherkin/reference/)

---

**Principles Applied**: Simplicity Over Complexity, Explicit Over Implicit
