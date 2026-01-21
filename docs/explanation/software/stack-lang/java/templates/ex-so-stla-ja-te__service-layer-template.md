---
title: Java Service Layer Template
description: Template for creating application service layer that orchestrates domain operations and coordinates transactions
category: template
tags:
  - java
  - service-layer
  - application-services
  - transactions
  - orchestration
related:
  - ex-so-stla-ja-te__aggregate-template.md
  - ex-so-stla-ja-te__repository-template.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
last_updated: 2026-01-21
---

# Java Service Layer Template

This template provides a standardized structure for creating application service layers in Java that orchestrate domain operations, manage transactions, and coordinate between aggregates.

## Template Structure

```java
package com.openshariaenterprise.{domain}.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Optional;

/**
 * Application service for {DomainConcept} operations.
 *
 * <p>Service Responsibilities:
 * <ul>
 *   <li>Orchestrate domain operations across multiple aggregates</li>
 *   <li>Manage transaction boundaries</li>
 *   <li>Coordinate repository access</li>
 *   <li>Publish domain events</li>
 *   <li>Handle cross-cutting concerns (logging, validation)</li>
 * </ul>
 *
 * <p>Design Principles:
 * <ul>
 *   <li>Thin layer - Business logic stays in domain model</li>
 *   <li>Transaction management - One transaction per service method</li>
 *   <li>Orchestration - Coordinates between aggregates</li>
 *   <li>No business logic - Delegates to domain model</li>
 * </ul>
 *
 * @see AggregateRoot
 * @see Repository
 */
@Service
@Transactional
public class DomainService {

    private static final Logger log = LoggerFactory.getLogger(DomainService.class);

    private final AggregateRepository aggregateRepository;
    private final RelatedRepository relatedRepository;
    private final DomainEventPublisher eventPublisher;

    public DomainService(
        AggregateRepository aggregateRepository,
        RelatedRepository relatedRepository,
        DomainEventPublisher eventPublisher
    ) {
        this.aggregateRepository = aggregateRepository;
        this.relatedRepository = relatedRepository;
        this.eventPublisher = eventPublisher;
    }

    // ========================================
    // Command Operations (Write)
    // ========================================

    /**
     * Creates a new aggregate with the given parameters.
     *
     * @param request creation request with parameters
     * @return created aggregate ID
     * @throws IllegalArgumentException if request is invalid
     * @throws BusinessRuleViolationException if business rules are violated
     */
    public AggregateId createAggregate(CreateAggregateRequest request) {
        log.info("Creating aggregate with params: {}", request);

        // Validate request
        validateCreateRequest(request);

        // Check business rules
        if (aggregateRepository.existsByBusinessId(request.getBusinessId())) {
            throw new BusinessRuleViolationException(
                "Aggregate already exists with business ID: " + request.getBusinessId()
            );
        }

        // Create aggregate using domain model
        var aggregate = AggregateRoot.create(
            AggregateId.generate(),
            request.getName(),
            request.getValueObject(),
            request.getCreatedBy()
        );

        // Persist
        aggregateRepository.save(aggregate);

        // Publish domain events
        publishEvents(aggregate);

        log.info("Created aggregate with ID: {}", aggregate.getId());

        return aggregate.getId();
    }

    /**
     * Updates an existing aggregate.
     *
     * @param aggregateId ID of aggregate to update
     * @param request update request with new values
     * @throws EntityNotFoundException if aggregate not found
     * @throws IllegalStateException if update not allowed in current state
     */
    public void updateAggregate(AggregateId aggregateId, UpdateAggregateRequest request) {
        log.info("Updating aggregate {}: {}", aggregateId, request);

        // Load aggregate
        var aggregate = aggregateRepository.findById(aggregateId)
            .orElseThrow(() -> new EntityNotFoundException(
                "Aggregate not found: " + aggregateId
            ));

        // Update through domain model
        aggregate.updateProperty(request.getNewValue(), request.getUpdatedBy());

        // Persist changes
        aggregateRepository.save(aggregate);

        // Publish domain events
        publishEvents(aggregate);

        log.info("Updated aggregate: {}", aggregateId);
    }

    /**
     * Performs complex operation across multiple aggregates.
     *
     * @param request operation request
     * @return operation result
     * @throws EntityNotFoundException if required entities not found
     * @throws BusinessRuleViolationException if operation violates business rules
     */
    public OperationResult performComplexOperation(ComplexOperationRequest request) {
        log.info("Performing complex operation: {}", request);

        // Load primary aggregate
        var aggregate = aggregateRepository.findById(request.getAggregateId())
            .orElseThrow(() -> new EntityNotFoundException(
                "Aggregate not found: " + request.getAggregateId()
            ));

        // Load related aggregate
        var related = relatedRepository.findById(request.getRelatedId())
            .orElseThrow(() -> new EntityNotFoundException(
                "Related entity not found: " + request.getRelatedId()
            ));

        // Coordinate operation between aggregates
        var operationParams = OperationParams.from(request);
        var result = aggregate.performComplexOperation(
            operationParams,
            request.getPerformedBy()
        );

        // Update related aggregate
        related.updateFromOperation(result);

        // Persist changes
        aggregateRepository.save(aggregate);
        relatedRepository.save(related);

        // Publish domain events from both aggregates
        publishEvents(aggregate);
        publishEvents(related);

        log.info("Complex operation completed with result: {}", result);

        return result;
    }

    /**
     * Deletes an aggregate.
     *
     * @param aggregateId ID of aggregate to delete
     * @param deletedBy user performing deletion
     * @throws EntityNotFoundException if aggregate not found
     * @throws IllegalStateException if deletion not allowed
     */
    public void deleteAggregate(AggregateId aggregateId, UserId deletedBy) {
        log.info("Deleting aggregate: {} by {}", aggregateId, deletedBy);

        var aggregate = aggregateRepository.findById(aggregateId)
            .orElseThrow(() -> new EntityNotFoundException(
                "Aggregate not found: " + aggregateId
            ));

        // Check if deletion is allowed
        if (!aggregate.canBeDeleted()) {
            throw new IllegalStateException(
                "Aggregate cannot be deleted in current state: " + aggregate.getStatus()
            );
        }

        // Perform soft delete (archive)
        aggregate.archive(deletedBy);
        aggregateRepository.save(aggregate);

        // Or hard delete
        // aggregateRepository.delete(aggregate);

        // Publish domain events
        publishEvents(aggregate);

        log.info("Deleted aggregate: {}", aggregateId);
    }

    // ========================================
    // Query Operations (Read)
    // ========================================

    /**
     * Finds aggregate by ID.
     *
     * @param aggregateId the aggregate identifier
     * @return optional aggregate
     */
    @Transactional(readOnly = true)
    public Optional<AggregateRoot> findById(AggregateId aggregateId) {
        log.debug("Finding aggregate by ID: {}", aggregateId);
        return aggregateRepository.findById(aggregateId);
    }

    /**
     * Finds aggregates matching criteria.
     *
     * @param criteria search criteria
     * @return list of matching aggregates
     */
    @Transactional(readOnly = true)
    public List<AggregateRoot> findByCriteria(SearchCriteria criteria) {
        log.debug("Finding aggregates by criteria: {}", criteria);
        return aggregateRepository.findByCriteria(criteria);
    }

    /**
     * Checks if aggregate exists.
     *
     * @param businessId business identifier
     * @return true if exists
     */
    @Transactional(readOnly = true)
    public boolean exists(String businessId) {
        return aggregateRepository.existsByBusinessId(businessId);
    }

    // ========================================
    // Helper Methods
    // ========================================

    private void validateCreateRequest(CreateAggregateRequest request) {
        if (request.getName() == null || request.getName().isBlank()) {
            throw new IllegalArgumentException("Name must not be blank");
        }

        if (request.getValueObject() == null) {
            throw new IllegalArgumentException("Value object must not be null");
        }
    }

    private void publishEvents(AggregateRoot aggregate) {
        var events = aggregate.getAndClearDomainEvents();
        events.forEach(eventPublisher::publish);
    }
}
```

## Financial Domain Example: Zakat Calculation Service

```java
package com.openshariaenterprise.zakat.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * Application service for Zakat calculation and payment operations.
 *
 * <p>Orchestrates Zakat-related operations including:
 * <ul>
 *   <li>Opening Zakat accounts</li>
 *   <li>Calculating Zakat obligations</li>
 *   <li>Processing Zakat payments</li>
 *   <li>Generating payment receipts</li>
 *   <li>Managing account transactions</li>
 * </ul>
 */
@Service
@Transactional
public class ZakatCalculationService {

    private static final Logger log = LoggerFactory.getLogger(ZakatCalculationService.class);
    private static final int HAUL_DAYS = 354; // One lunar year

    private final ZakatAccountRepository accountRepository;
    private final ZakatPaymentRepository paymentRepository;
    private final NisabProvider nisabProvider;
    private final DomainEventPublisher eventPublisher;
    private final ReceiptGenerator receiptGenerator;

    public ZakatCalculationService(
        ZakatAccountRepository accountRepository,
        ZakatPaymentRepository paymentRepository,
        NisabProvider nisabProvider,
        DomainEventPublisher eventPublisher,
        ReceiptGenerator receiptGenerator
    ) {
        this.accountRepository = accountRepository;
        this.paymentRepository = paymentRepository;
        this.nisabProvider = nisabProvider;
        this.eventPublisher = eventPublisher;
        this.receiptGenerator = receiptGenerator;
    }

    // ========================================
    // Account Operations
    // ========================================

    /**
     * Opens a new Zakat account for account holder.
     *
     * @param request account opening request
     * @return created account ID
     * @throws IllegalArgumentException if request is invalid
     * @throws BusinessRuleViolationException if holder already has active account
     */
    public ZakatAccountId openAccount(OpenZakatAccountRequest request) {
        log.info("Opening Zakat account for holder: {}", request.getHolderId());

        // Validate request
        validateOpenAccountRequest(request);

        // Business rule: One active account per holder
        if (accountRepository.existsByHolderIdAndStatus(
            request.getHolderId(),
            ZakatAccountStatus.ACTIVE
        )) {
            throw new BusinessRuleViolationException(
                "Holder already has an active Zakat account"
            );
        }

        // Get current nisab threshold
        var nisabThreshold = nisabProvider.getCurrentNisab(
            request.getInitialBalance().getCurrencyCode()
        );

        // Create account
        var account = ZakatAccount.open(
            ZakatAccountId.generate(),
            request.getHolderId(),
            request.getInitialBalance(),
            nisabThreshold,
            request.getOpenedBy()
        );

        // Persist
        accountRepository.save(account);

        // Publish events
        publishEvents(account);

        log.info("Opened Zakat account: {}", account.getId());

        return account.getId();
    }

    /**
     * Deposits funds into Zakat account.
     *
     * @param accountId account identifier
     * @param amount amount to deposit
     * @param depositedBy user making deposit
     * @throws EntityNotFoundException if account not found
     */
    public void deposit(ZakatAccountId accountId, Money amount, UserId depositedBy) {
        log.info("Depositing {} to account {}", amount, accountId);

        var account = findAccountOrThrow(accountId);

        // Delegate to domain model
        account.deposit(amount, depositedBy);

        accountRepository.save(account);
        publishEvents(account);

        log.info("Deposit successful. New balance: {}", account.getBalance());
    }

    /**
     * Withdraws funds from Zakat account.
     *
     * @param accountId account identifier
     * @param amount amount to withdraw
     * @param withdrawnBy user making withdrawal
     * @throws EntityNotFoundException if account not found
     * @throws IllegalArgumentException if insufficient balance
     */
    public void withdraw(ZakatAccountId accountId, Money amount, UserId withdrawnBy) {
        log.info("Withdrawing {} from account {}", amount, accountId);

        var account = findAccountOrThrow(accountId);

        account.withdraw(amount, withdrawnBy);

        accountRepository.save(account);
        publishEvents(account);

        log.info("Withdrawal successful. New balance: {}", account.getBalance());
    }

    // ========================================
    // Zakat Calculation Operations
    // ========================================

    /**
     * Calculates Zakat obligation for an account.
     *
     * @param accountId account identifier
     * @return calculation result
     * @throws EntityNotFoundException if account not found
     */
    @Transactional(readOnly = true)
    public ZakatCalculation calculateZakat(ZakatAccountId accountId) {
        log.info("Calculating Zakat for account: {}", accountId);

        var account = findAccountOrThrow(accountId);

        // Delegate to domain model
        var calculation = account.calculateZakat();

        log.info("Zakat calculation result: eligible={}, amount={}",
            calculation.isEligible(),
            calculation.isEligible() ? calculation.getZakatAmount() : "N/A"
        );

        return calculation;
    }

    /**
     * Finds all accounts eligible for Zakat calculation.
     *
     * @return list of eligible accounts
     */
    @Transactional(readOnly = true)
    public List<ZakatAccount> findEligibleAccounts() {
        log.info("Finding accounts eligible for Zakat");

        var nisabThreshold = nisabProvider.getCurrentNisab("USD");
        var haulCutoffDate = LocalDate.now().minusDays(HAUL_DAYS);

        var eligible = accountRepository.findEligibleForZakat(
            nisabThreshold.getAmount(),
            haulCutoffDate
        );

        log.info("Found {} eligible accounts", eligible.size());

        return eligible;
    }

    // ========================================
    // Payment Operations
    // ========================================

    /**
     * Processes a Zakat payment.
     *
     * @param request payment processing request
     * @return payment ID and receipt
     * @throws EntityNotFoundException if account not found
     * @throws IllegalStateException if payment cannot be processed
     */
    public ZakatPaymentResult processPayment(ProcessZakatPaymentRequest request) {
        log.info("Processing Zakat payment: {}", request);

        var account = findAccountOrThrow(request.getAccountId());

        // Validate eligibility
        var calculation = account.calculateZakat();
        if (!calculation.isEligible()) {
            throw new IllegalStateException(
                "Account not eligible for Zakat: " + calculation.getIneligibilityReason()
            );
        }

        // Create payment
        var payment = ZakatPayment.create(
            ZakatPaymentId.generate(),
            request.getAccountId(),
            request.getAmount(),
            request.getPaymentMethod(),
            request.getRecipient()
        );

        // Record payment in account
        account.recordPayment(payment, request.getProcessedBy());

        // Persist both
        paymentRepository.save(payment);
        accountRepository.save(account);

        // Publish events
        publishEvents(account);

        // Generate receipt
        var receipt = receiptGenerator.generateZakatReceipt(
            payment,
            account.getHolderId()
        );

        log.info("Zakat payment processed: {}", payment.getId());

        return ZakatPaymentResult.success(payment.getId(), receipt);
    }

    /**
     * Retrieves payment history for an account.
     *
     * @param accountId account identifier
     * @return list of payments
     * @throws EntityNotFoundException if account not found
     */
    @Transactional(readOnly = true)
    public List<ZakatPayment> getPaymentHistory(ZakatAccountId accountId) {
        log.debug("Retrieving payment history for account: {}", accountId);

        var account = findAccountOrThrow(accountId);

        return account.getPayments();
    }

    // ========================================
    // Helper Methods
    // ========================================

    private ZakatAccount findAccountOrThrow(ZakatAccountId accountId) {
        return accountRepository.findById(accountId)
            .orElseThrow(() -> new EntityNotFoundException(
                "Zakat account not found: " + accountId
            ));
    }

    private void validateOpenAccountRequest(OpenZakatAccountRequest request) {
        if (request.getHolderId() == null) {
            throw new IllegalArgumentException("Holder ID must not be null");
        }

        if (request.getInitialBalance() == null) {
            throw new IllegalArgumentException("Initial balance must not be null");
        }

        if (request.getInitialBalance().isNegative()) {
            throw new IllegalArgumentException("Initial balance must not be negative");
        }
    }

    private void publishEvents(ZakatAccount account) {
        var events = account.getAndClearDomainEvents();
        events.forEach(eventPublisher::publish);
    }
}

/**
 * Request object for opening Zakat account.
 */
record OpenZakatAccountRequest(
    AccountHolderId holderId,
    Money initialBalance,
    UserId openedBy
) {}

/**
 * Request object for processing Zakat payment.
 */
record ProcessZakatPaymentRequest(
    ZakatAccountId accountId,
    Money amount,
    PaymentMethod paymentMethod,
    String recipient,
    UserId processedBy
) {}

/**
 * Result of Zakat payment processing.
 */
record ZakatPaymentResult(
    ZakatPaymentId paymentId,
    Receipt receipt,
    boolean success,
    String message
) {
    public static ZakatPaymentResult success(ZakatPaymentId paymentId, Receipt receipt) {
        return new ZakatPaymentResult(paymentId, receipt, true, "Payment processed successfully");
    }
}
```

## Service Testing

```java
package com.openshariaenterprise.zakat.service;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Unit tests for {@link ZakatCalculationService}.
 */
@ExtendWith(MockitoExtension.class)
@DisplayName("ZakatCalculationService Tests")
class ZakatCalculationServiceTest {

    @Mock
    private ZakatAccountRepository accountRepository;

    @Mock
    private ZakatPaymentRepository paymentRepository;

    @Mock
    private NisabProvider nisabProvider;

    @Mock
    private DomainEventPublisher eventPublisher;

    @Mock
    private ReceiptGenerator receiptGenerator;

    @InjectMocks
    private ZakatCalculationService service;

    @Test
    @DisplayName("Should open new Zakat account successfully")
    void testOpenAccount() {
        // Arrange
        var request = new OpenZakatAccountRequest(
            AccountHolderId.generate(),
            Money.of(new BigDecimal("10000"), "USD"),
            UserId.system()
        );

        var nisabThreshold = Money.of(new BigDecimal("5000"), "USD");

        when(accountRepository.existsByHolderIdAndStatus(any(), any()))
            .thenReturn(false);
        when(nisabProvider.getCurrentNisab("USD"))
            .thenReturn(nisabThreshold);
        when(accountRepository.save(any()))
            .thenAnswer(i -> i.getArgument(0));

        // Act
        var accountId = service.openAccount(request);

        // Assert
        assertThat(accountId).isNotNull();
        verify(accountRepository).save(any(ZakatAccount.class));
        verify(eventPublisher, atLeastOnce()).publish(any());
    }

    @Test
    @DisplayName("Should throw exception when holder already has active account")
    void testOpenAccountDuplicateHolder() {
        // Arrange
        var request = new OpenZakatAccountRequest(
            AccountHolderId.generate(),
            Money.of(new BigDecimal("10000"), "USD"),
            UserId.system()
        );

        when(accountRepository.existsByHolderIdAndStatus(any(), any()))
            .thenReturn(true);

        // Act & Assert
        assertThatThrownBy(() -> service.openAccount(request))
            .isInstanceOf(BusinessRuleViolationException.class)
            .hasMessageContaining("already has an active Zakat account");

        verify(accountRepository, never()).save(any());
    }
}
```

## Usage Guidelines

1. **Thin Service Layer**: Keep business logic in domain model, not service
2. **Transaction Management**: One transaction per service method
3. **Orchestration Only**: Services coordinate, domain models execute
4. **Event Publishing**: Always publish domain events after persistence
5. **Exception Handling**: Let domain exceptions bubble up
6. **Read-Only Queries**: Mark query methods with `@Transactional(readOnly = true)`
7. **Logging**: Log service method entry, exit, and important decisions

## Related Templates

- [Aggregate Template](./ex-so-stla-ja-te__aggregate-template.md)
- [Repository Template](./ex-so-stla-ja-te__repository-template.md)
- [Domain Event Template](./ex-so-stla-ja-te__domain-event-template.md)
- [Unit Test Template](./ex-so-stla-ja-te__unit-test-template.md)

## See Also

- [Service Layer Pattern](https://martinfowler.com/eaaCatalog/serviceLayer.html)
- [Application Services in DDD](https://www.dddcommunity.org/library/vernon_2011/)

---

**Principles Applied**: Simplicity Over Complexity, Explicit Over Implicit
