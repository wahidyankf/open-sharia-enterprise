---
title: Go Service Layer Template
description: Template for creating application service layer in Go with orchestration, transaction management, event publishing, and dependency injection
category: template
tags:
  - golang
  - ddd
  - service-layer
  - application-services
  - orchestration
  - transactions
  - domain-events
  - dependency-injection
  - error-handling
  - go-1.18
  - go-1.21
  - go-1.22
  - go-1.23
  - go-1.24
  - go-1.25
related:
  - repository-template.md
  - domain-event-template.md
  - aggregate-template.md
  - ex-so-stla-go__domain-driven-design.md
  - ex-so-stla-go__best-practices.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
---

# Go Service Layer Template

This template provides a standardized structure for creating application service layers in Go. The service layer orchestrates domain logic, coordinates repositories, manages transactions, and publishes domain events to implement business use cases.

## Table of Contents

1. [Overview](#overview)
2. [Template Structure](#template-structure)
3. [Orchestration Layer](#orchestration-layer)
4. [Transaction Management](#transaction-management)
5. [Event Publishing](#event-publishing)
6. [Dependency Injection](#dependency-injection)
7. [Error Handling](#error-handling)
8. [Complete Example: Zakat Calculation Service](#complete-example-zakat-calculation-service)
9. [Before/After Comparison](#beforeafter-comparison)
10. [Usage Guidelines](#usage-guidelines)
11. [Testing Services](#testing-services)
12. [Related Documentation](#related-documentation)

## Overview

The service layer (also called **application services**) sits between the presentation layer and the domain layer. Services orchestrate business use cases by coordinating domain objects, repositories, and external services without containing business logic themselves.

**Key Characteristics**:

- **Orchestration focused**: Coordinates domain logic, doesn't contain it
- **Use case implementation**: One method per use case
- **Transaction boundaries**: Manages transaction lifecycle
- **Event publishing**: Publishes domain events after successful transactions
- **Thin layer**: Minimal logic, delegates to domain objects
- **Context propagation**: Uses `context.Context` for cancellation and tracing

**Go vs Java Differences**:

```go
// Go: Constructor function with explicit dependencies
type ZakatCalculationService struct {
 accountRepo     domain.ZakatAccountRepository
 calculationRepo domain.CalculationRepository
 eventPublisher  event.EventPublisher
 logger          Logger
}

// Constructor function (manual dependency injection)
func NewZakatCalculationService(
 accountRepo domain.ZakatAccountRepository,
 calculationRepo domain.CalculationRepository,
 eventPublisher event.EventPublisher,
 logger Logger,
) *ZakatCalculationService {
 return &ZakatCalculationService{
  accountRepo:     accountRepo,
  calculationRepo: calculationRepo,
  eventPublisher:  eventPublisher,
  logger:          logger,
 }
}

// Use case method with context.Context
func (s *ZakatCalculationService) CalculateZakat(
 ctx context.Context,
 accountID domain.AccountID,
 calculatedBy domain.UserID,
) (*domain.ZakatCalculation, error) {
 // Orchestration logic
 return nil, nil
}
```

```java
// Java: Class with @Service annotation and dependency injection
@Service
public class ZakatCalculationService {
    private final ZakatAccountRepository accountRepo;
    private final CalculationRepository calculationRepo;
    private final EventPublisher eventPublisher;
    private final Logger logger;

    // Constructor injection (Spring framework)
    @Autowired
    public ZakatCalculationService(
        ZakatAccountRepository accountRepo,
        CalculationRepository calculationRepo,
        EventPublisher eventPublisher,
        Logger logger
    ) {
        this.accountRepo = accountRepo;
        this.calculationRepo = calculationRepo;
        this.eventPublisher = eventPublisher;
        this.logger = logger;
    }

    // Use case method
    @Transactional
    public ZakatCalculation calculateZakat(
        AccountId accountId,
        UserId calculatedBy
    ) {
        // Orchestration logic
    }
}
```

**Critical Differences**:

- Go uses **constructor functions** instead of DI frameworks
- Go uses **`context.Context`** for cancellation and timeout propagation
- Go returns **explicit errors** instead of throwing exceptions
- Go has **no automatic transaction management** (`@Transactional` annotation)
- Go services manage **transaction context manually**
- Go uses **struct embedding** for composition instead of inheritance

## Template Structure

```go
package service

import (
 "context"
 "fmt"
 "time"

 "yourapp/domain"
 "yourapp/domain/event"
)

// ========================================
// Service Interface (Optional)
// ========================================

// ZakatServiceInterface defines use cases for zakat operations.
//
// Interface enables dependency inversion and facilitates testing.
// Not required but recommended for services with external dependencies.
//
// Example:
//
// var _ ZakatServiceInterface = (*ZakatCalculationService)(nil)
type ZakatServiceInterface interface {
 // CalculateZakat calculates zakat obligation for an account.
 CalculateZakat(
  ctx context.Context,
  accountID domain.AccountID,
  calculatedBy domain.UserID,
 ) (*domain.ZakatCalculation, error)

 // ValidateZakatEligibility checks if account meets nisab threshold.
 ValidateZakatEligibility(
  ctx context.Context,
  accountID domain.AccountID,
 ) (bool, error)

 // RecordZakatPayment processes a zakat payment.
 RecordZakatPayment(
  ctx context.Context,
  accountID domain.AccountID,
  paymentID domain.PaymentID,
  amount domain.Money,
  paidBy domain.UserID,
 ) (*domain.ZakatPayment, error)
}

// ========================================
// Service Implementation
// ========================================

// ZakatCalculationService orchestrates zakat calculation use cases.
//
// Responsibilities:
//   - Load domain aggregates from repositories
//   - Coordinate domain logic execution
//   - Manage transaction boundaries
//   - Publish domain events after successful operations
//   - Handle cross-aggregate coordination
//
// Does NOT contain business logic - delegates to domain objects.
//
// Example:
//
// service := NewZakatCalculationService(accountRepo, calcRepo, publisher, logger)
// calculation, err := service.CalculateZakat(ctx, accountID, userID)
type ZakatCalculationService struct {
 // ========================================
 // Dependencies (Injected via Constructor)
 // ========================================

 accountRepo     domain.ZakatAccountRepository  // Account persistence
 calculationRepo domain.CalculationRepository   // Calculation persistence
 eventPublisher  event.EventPublisher           // Event publishing
 logger          Logger                         // Logging
}

// ========================================
// Constructor
// ========================================

// NewZakatCalculationService creates a new zakat calculation service.
//
// All dependencies are required (no nil values allowed).
// Constructor validates dependencies and returns configured service.
//
// Parameters:
//   - accountRepo: Repository for zakat accounts
//   - calculationRepo: Repository for calculations
//   - eventPublisher: Publisher for domain events
//   - logger: Logger for operation tracking
//
// Returns configured service ready for use.
//
// Example:
//
// service := NewZakatCalculationService(
//     sqlAccountRepo,
//     sqlCalculationRepo,
//     kafkaPublisher,
//     zap.NewLogger(),
// )
func NewZakatCalculationService(
 accountRepo domain.ZakatAccountRepository,
 calculationRepo domain.CalculationRepository,
 eventPublisher event.EventPublisher,
 logger Logger,
) *ZakatCalculationService {
 // Validate dependencies (fail fast on misconfiguration)
 if accountRepo == nil {
  panic("accountRepo is required")
 }
 if calculationRepo == nil {
  panic("calculationRepo is required")
 }
 if eventPublisher == nil {
  panic("eventPublisher is required")
 }
 if logger == nil {
  panic("logger is required")
 }

 return &ZakatCalculationService{
  accountRepo:     accountRepo,
  calculationRepo: calculationRepo,
  eventPublisher:  eventPublisher,
  logger:          logger,
 }
}

// ========================================
// Use Case Methods
// ========================================

// CalculateZakat calculates zakat obligation for a zakat account.
//
// Orchestration Steps:
//  1. Load account aggregate from repository
//  2. Delegate calculation to domain logic
//  3. Persist calculation result
//  4. Publish domain event
//
// Parameters:
//   - ctx: Context for cancellation and timeout
//   - accountID: Account to calculate zakat for
//   - calculatedBy: User performing calculation
//
// Returns:
//   - ZakatCalculation domain object with results
//   - Error if operation fails
//
// Example:
//
// calculation, err := service.CalculateZakat(ctx, accountID, userID)
// if err != nil {
//     return fmt.Errorf("calculate zakat: %w", err)
// }
func (s *ZakatCalculationService) CalculateZakat(
 ctx context.Context,
 accountID domain.AccountID,
 calculatedBy domain.UserID,
) (*domain.ZakatCalculation, error) {
 // Step 1: Load account aggregate
 account, err := s.accountRepo.FindByID(ctx, accountID)
 if err != nil {
  return nil, fmt.Errorf("load account: %w", err)
 }

 // Step 2: Execute domain logic (business rules in aggregate)
 calculation, err := account.CalculateZakat(calculatedBy)
 if err != nil {
  return nil, fmt.Errorf("calculate zakat: %w", err)
 }

 // Step 3: Persist calculation result
 if err := s.calculationRepo.Save(ctx, calculation); err != nil {
  return nil, fmt.Errorf("save calculation: %w", err)
 }

 // Step 4: Publish domain event
 evt := event.ZakatCalculationCompleted{
  EventID:       event.NewEventID(),
  AggregateID:   accountID,
  CalculationID: calculation.ID(),
  ZakatAmount:   calculation.Amount(),
  CalculatedBy:  calculatedBy,
  OccurredAt:    time.Now(),
  Version:       1,
 }

 if err := s.eventPublisher.Publish(ctx, evt); err != nil {
  // Event publishing failure is non-fatal (can retry later)
  s.logger.Error("failed to publish event", "error", err, "event", evt)
 }

 return calculation, nil
}

// ValidateZakatEligibility checks if account meets nisab threshold.
//
// Read-only operation - no state mutation, no events published.
//
// Parameters:
//   - ctx: Context for cancellation
//   - accountID: Account to check eligibility
//
// Returns:
//   - true if account balance >= nisab threshold
//   - false if below threshold
//   - Error if operation fails
//
// Example:
//
// eligible, err := service.ValidateZakatEligibility(ctx, accountID)
// if err != nil {
//     return err
// }
// if !eligible {
//     return fmt.Errorf("account not eligible for zakat")
// }
func (s *ZakatCalculationService) ValidateZakatEligibility(
 ctx context.Context,
 accountID domain.AccountID,
) (bool, error) {
 // Load account
 account, err := s.accountRepo.FindByID(ctx, accountID)
 if err != nil {
  return false, fmt.Errorf("load account: %w", err)
 }

 // Delegate to domain logic (no event needed for query)
 return account.IsEligibleForZakat(), nil
}

// RecordZakatPayment processes a zakat payment.
//
// Transactional operation coordinating multiple aggregates.
//
// Orchestration Steps:
//  1. Start transaction
//  2. Load account aggregate
//  3. Record payment on aggregate
//  4. Save updated account
//  5. Persist payment record
//  6. Commit transaction
//  7. Publish domain event
//
// Parameters:
//   - ctx: Context with transaction propagation
//   - accountID: Account receiving payment
//   - paymentID: Unique payment identifier
//   - amount: Payment amount
//   - paidBy: User making payment
//
// Returns:
//   - ZakatPayment domain object
//   - Error if operation fails (transaction rolled back)
//
// Example:
//
// payment, err := service.RecordZakatPayment(ctx, accountID, paymentID, amount, userID)
// if err != nil {
//     return fmt.Errorf("record payment: %w", err)
// }
func (s *ZakatCalculationService) RecordZakatPayment(
 ctx context.Context,
 accountID domain.AccountID,
 paymentID domain.PaymentID,
 amount domain.Money,
 paidBy domain.UserID,
) (*domain.ZakatPayment, error) {
 var payment *domain.ZakatPayment
 var evt event.DomainEvent

 // Execute within transaction
 err := s.accountRepo.WithTransaction(ctx, func(txCtx context.Context) error {
  // Load account
  account, err := s.accountRepo.FindByID(txCtx, accountID)
  if err != nil {
   return fmt.Errorf("load account: %w", err)
  }

  // Execute domain logic
  payment, err = account.RecordPayment(paymentID, amount, paidBy)
  if err != nil {
   return fmt.Errorf("record payment: %w", err)
  }

  // Save updated account
  if err := s.accountRepo.Save(txCtx, account); err != nil {
   return fmt.Errorf("save account: %w", err)
  }

  // Prepare event (published after transaction commits)
  evt = event.ZakatPaymentRecorded{
   EventID:     event.NewEventID(),
   AggregateID: accountID,
   PaymentID:   paymentID,
   Amount:      amount,
   PaidBy:      paidBy,
   OccurredAt:  time.Now(),
   Version:     1,
  }

  return nil
 })

 if err != nil {
  return nil, err
 }

 // Publish event after successful transaction
 if err := s.eventPublisher.Publish(ctx, evt); err != nil {
  s.logger.Error("failed to publish payment event", "error", err)
 }

 return payment, nil
}

// ========================================
// Logger Interface (Example)
// ========================================

// Logger defines logging operations.
type Logger interface {
 Info(msg string, keysAndValues ...interface{})
 Error(msg string, keysAndValues ...interface{})
 Debug(msg string, keysAndValues ...interface{})
}
```

## Orchestration Layer

The service layer orchestrates use cases without containing business logic. All business rules reside in domain objects.

### ✅ Correct: Service as Orchestrator

```go
package service

import (
 "context"
 "fmt"

 "yourapp/domain"
 "yourapp/domain/event"
)

// Service orchestrates, domain executes
type DonationService struct {
 donationRepo   domain.DonationRepository
 campaignRepo   domain.CampaignRepository
 eventPublisher event.EventPublisher
}

// ProcessDonation orchestrates donation processing use case
func (s *DonationService) ProcessDonation(
 ctx context.Context,
 campaignID domain.CampaignID,
 donorID domain.DonorID,
 amount domain.Money,
) (*domain.Donation, error) {
 // Step 1: Load campaign aggregate
 campaign, err := s.campaignRepo.FindByID(ctx, campaignID)
 if err != nil {
  return nil, fmt.Errorf("load campaign: %w", err)
 }

 // Step 2: Delegate business logic to domain
 // (campaign validates business rules, creates donation)
 donation, err := campaign.AcceptDonation(donorID, amount)
 if err != nil {
  return nil, fmt.Errorf("accept donation: %w", err)
 }

 // Step 3: Save donation
 if err := s.donationRepo.Save(ctx, donation); err != nil {
  return nil, fmt.Errorf("save donation: %w", err)
 }

 // Step 4: Update campaign state
 if err := s.campaignRepo.Save(ctx, campaign); err != nil {
  return nil, fmt.Errorf("save campaign: %w", err)
 }

 // Step 5: Publish domain event
 evt := event.DonationReceived{
  EventID:     event.NewEventID(),
  AggregateID: donation.ID(),
  CampaignID:  campaignID,
  Amount:      amount,
  DonorID:     donorID,
  OccurredAt:  time.Now(),
 }
 s.eventPublisher.Publish(ctx, evt)

 return donation, nil
}
```

### ❌ Wrong: Business Logic in Service

```go
package service

// WRONG: Service contains business logic
type DonationService struct {
 donationRepo   domain.DonationRepository
 campaignRepo   domain.CampaignRepository
 eventPublisher event.EventPublisher
}

// ProcessDonation contains business logic (WRONG!)
func (s *DonationService) ProcessDonation(
 ctx context.Context,
 campaignID domain.CampaignID,
 donorID domain.DonorID,
 amount domain.Money,
) (*domain.Donation, error) {
 // Load campaign
 campaign, err := s.campaignRepo.FindByID(ctx, campaignID)
 if err != nil {
  return nil, err
 }

 // ❌ WRONG: Business rules in service!
 if campaign.Status() != "ACTIVE" {
  return nil, fmt.Errorf("campaign not active")
 }

 if amount.IsLessThan(campaign.MinimumDonation()) {
  return nil, fmt.Errorf("amount below minimum")
 }

 if campaign.CurrentAmount().Add(amount).IsGreaterThan(campaign.GoalAmount()) {
  return nil, fmt.Errorf("exceeds campaign goal")
 }

 // ❌ WRONG: Creating domain object in service
 donation := &domain.Donation{
  ID:         domain.NewDonationID(),
  CampaignID: campaignID,
  DonorID:    donorID,
  Amount:     amount,
  Status:     "PENDING",
 }

 // Save
 s.donationRepo.Save(ctx, donation)

 // PROBLEMS:
 // - Business rules scattered in service
 // - Domain model becomes anemic (just data)
 // - Hard to test business logic
 // - Duplicate rules across multiple services
 // - Cannot reuse validation logic

 return donation, nil
}
```

### Orchestration Patterns

```go
package service

// Pattern 1: Single Aggregate Coordination
func (s *Service) SingleAggregateUseCase(ctx context.Context, id domain.ID) error {
 // Load aggregate
 aggregate, err := s.repo.FindByID(ctx, id)
 if err != nil {
  return err
 }

 // Execute domain logic
 if err := aggregate.ExecuteBusinessLogic(); err != nil {
  return err
 }

 // Persist changes
 if err := s.repo.Save(ctx, aggregate); err != nil {
  return err
 }

 // Publish events
 s.publishEvents(ctx, aggregate.UncommittedEvents())

 return nil
}

// Pattern 2: Multi-Aggregate Coordination
func (s *Service) MultiAggregateUseCase(
 ctx context.Context,
 aggregate1ID domain.ID1,
 aggregate2ID domain.ID2,
) error {
 // Load both aggregates
 agg1, err := s.repo1.FindByID(ctx, aggregate1ID)
 if err != nil {
  return err
 }

 agg2, err := s.repo2.FindByID(ctx, aggregate2ID)
 if err != nil {
  return err
 }

 // Coordinate across aggregates (eventual consistency via events)
 if err := agg1.CoordinateWith(agg2); err != nil {
  return err
 }

 // Save both aggregates
 if err := s.repo1.Save(ctx, agg1); err != nil {
  return err
 }

 if err := s.repo2.Save(ctx, agg2); err != nil {
  return err
 }

 // Publish events
 s.publishEvents(ctx, agg1.UncommittedEvents())
 s.publishEvents(ctx, agg2.UncommittedEvents())

 return nil
}

// Pattern 3: Query Use Case (Read-Only)
func (s *Service) QueryUseCase(
 ctx context.Context,
 criteria domain.SearchCriteria,
) ([]domain.Aggregate, error) {
 // Query repository (no state mutation)
 results, err := s.repo.FindByCriteria(ctx, criteria)
 if err != nil {
  return nil, err
 }

 // Return query results (no events published)
 return results, nil
}

// Pattern 4: Saga Coordination (Eventual Consistency)
func (s *Service) SagaUseCase(ctx context.Context, command domain.Command) error {
 // Step 1: Execute first aggregate
 agg1, err := s.repo1.FindByID(ctx, command.Aggregate1ID)
 if err != nil {
  return err
 }

 if err := agg1.Execute(command); err != nil {
  return err
 }

 if err := s.repo1.Save(ctx, agg1); err != nil {
  return err
 }

 // Step 2: Publish event (triggers saga step 2 via event handler)
 evt := event.SagaStepCompleted{
  SagaID:  command.SagaID,
  StepID:  1,
  Payload: command.Payload,
 }
 s.eventPublisher.Publish(ctx, evt)

 // Saga continues via event handlers (eventual consistency)
 return nil
}
```

## Transaction Management

Services manage transaction boundaries using context-based transaction propagation.

### Transaction Context Pattern

```go
package service

import (
 "context"
 "fmt"

 "yourapp/domain"
)

// Service with transaction management
type OrderService struct {
 orderRepo   domain.OrderRepository
 paymentRepo domain.PaymentRepository
}

// PlaceOrder coordinates order and payment in single transaction
func (s *OrderService) PlaceOrder(
 ctx context.Context,
 orderID domain.OrderID,
 paymentDetails domain.PaymentDetails,
) (*domain.Order, error) {
 var order *domain.Order

 // Execute within transaction
 err := s.orderRepo.WithTransaction(ctx, func(txCtx context.Context) error {
  // Create order
  order = domain.NewOrder(orderID)

  // Save order (within transaction)
  if err := s.orderRepo.Save(txCtx, order); err != nil {
   return fmt.Errorf("save order: %w", err)
  }

  // Process payment (within same transaction)
  payment, err := order.ProcessPayment(paymentDetails)
  if err != nil {
   return fmt.Errorf("process payment: %w", err)
  }

  // Save payment (within transaction)
  if err := s.paymentRepo.Save(txCtx, payment); err != nil {
   return fmt.Errorf("save payment: %w", err)
  }

  // Both order and payment committed atomically
  return nil
 })

 if err != nil {
  return nil, err
 }

 return order, nil
}
```

### Rollback Handling

```go
package service

import (
 "context"
 "fmt"
)

// Service with explicit error handling and rollback
type TransferService struct {
 accountRepo domain.AccountRepository
}

// TransferFunds coordinates debit and credit in transaction
func (s *TransferService) TransferFunds(
 ctx context.Context,
 fromAccountID domain.AccountID,
 toAccountID domain.AccountID,
 amount domain.Money,
) error {
 return s.accountRepo.WithTransaction(ctx, func(txCtx context.Context) error {
  // Load sender account
  fromAccount, err := s.accountRepo.FindByID(txCtx, fromAccountID)
  if err != nil {
   return fmt.Errorf("load sender account: %w", err)
  }

  // Load receiver account
  toAccount, err := s.accountRepo.FindByID(txCtx, toAccountID)
  if err != nil {
   return fmt.Errorf("load receiver account: %w", err)
  }

  // Debit sender (domain validation)
  if err := fromAccount.Debit(amount); err != nil {
   // Business rule violation - rollback transaction
   return fmt.Errorf("debit sender: %w", err)
  }

  // Credit receiver
  if err := toAccount.Credit(amount); err != nil {
   // Rollback entire transaction (debit + credit)
   return fmt.Errorf("credit receiver: %w", err)
  }

  // Save sender
  if err := s.accountRepo.Save(txCtx, fromAccount); err != nil {
   return fmt.Errorf("save sender: %w", err)
  }

  // Save receiver
  if err := s.accountRepo.Save(txCtx, toAccount); err != nil {
   return fmt.Errorf("save receiver: %w", err)
  }

  // Both accounts updated atomically or both rollback
  return nil
 })
}
```

### Nested Transaction Pattern

```go
package service

import "context"

// Service coordinating multiple transactional operations
type ComplexService struct {
 orderRepo   domain.OrderRepository
 inventoryRepo domain.InventoryRepository
 shippingRepo  domain.ShippingRepository
}

// ProcessOrder coordinates multiple repositories in one transaction
func (s *ComplexService) ProcessOrder(
 ctx context.Context,
 orderID domain.OrderID,
) error {
 // Single transaction spans all operations
 return s.orderRepo.WithTransaction(ctx, func(txCtx context.Context) error {
  // Load order
  order, err := s.orderRepo.FindByID(txCtx, orderID)
  if err != nil {
   return err
  }

  // Reserve inventory (same transaction context)
  for _, item := range order.Items() {
   inventory, err := s.inventoryRepo.FindByProductID(txCtx, item.ProductID)
   if err != nil {
    return err
   }

   if err := inventory.Reserve(item.Quantity); err != nil {
    return err // Rollback all operations
   }

   if err := s.inventoryRepo.Save(txCtx, inventory); err != nil {
    return err
   }
  }

  // Create shipping record (same transaction)
  shipping, err := order.CreateShippingRecord()
  if err != nil {
   return err
  }

  if err := s.shippingRepo.Save(txCtx, shipping); err != nil {
   return err
  }

  // Mark order as processed
  order.MarkProcessed()

  if err := s.orderRepo.Save(txCtx, order); err != nil {
   return err
  }

  // All operations committed atomically
  return nil
 })
}
```

## Event Publishing

Services publish domain events after successful operations to enable eventual consistency and cross-aggregate coordination.

### Event Publishing Pattern

```go
package service

import (
 "context"
 "time"

 "yourapp/domain"
 "yourapp/domain/event"
)

// Service with event publishing
type AccountService struct {
 accountRepo    domain.AccountRepository
 eventPublisher event.EventPublisher
 logger         Logger
}

// OpenAccount creates account and publishes event
func (s *AccountService) OpenAccount(
 ctx context.Context,
 accountID domain.AccountID,
 ownerID domain.UserID,
) (*domain.Account, error) {
 // Create account
 account, err := domain.NewAccount(accountID, ownerID)
 if err != nil {
  return nil, err
 }

 // Save account
 if err := s.accountRepo.Save(ctx, account); err != nil {
  return nil, err
 }

 // Publish event after successful save
 evt := event.AccountOpened{
  EventID:     event.NewEventID(),
  AggregateID: accountID,
  OwnerID:     ownerID,
  OpenedAt:    time.Now(),
  Version:     1,
 }

 if err := s.eventPublisher.Publish(ctx, evt); err != nil {
  // Event publishing failure is non-fatal
  // (Can retry via background job or event outbox)
  s.logger.Error("failed to publish AccountOpened event", "error", err)
 }

 return account, nil
}
```

### Publish After Transaction Commit

```go
package service

import (
 "context"
 "time"

 "yourapp/domain"
 "yourapp/domain/event"
)

// Service publishing events after transaction commits
type PaymentService struct {
 paymentRepo    domain.PaymentRepository
 accountRepo    domain.AccountRepository
 eventPublisher event.EventPublisher
}

// ProcessPayment coordinates payment and account update, publishes event
func (s *PaymentService) ProcessPayment(
 ctx context.Context,
 paymentID domain.PaymentID,
 accountID domain.AccountID,
 amount domain.Money,
) (*domain.Payment, error) {
 var payment *domain.Payment
 var evt event.DomainEvent

 // Transaction: Payment and account update
 err := s.paymentRepo.WithTransaction(ctx, func(txCtx context.Context) error {
  // Load account
  account, err := s.accountRepo.FindByID(txCtx, accountID)
  if err != nil {
   return err
  }

  // Process payment
  payment, err = account.ProcessPayment(paymentID, amount)
  if err != nil {
   return err
  }

  // Save payment
  if err := s.paymentRepo.Save(txCtx, payment); err != nil {
   return err
  }

  // Save account
  if err := s.accountRepo.Save(txCtx, account); err != nil {
   return err
  }

  // Prepare event (not published yet - transaction not committed)
  evt = event.PaymentProcessed{
   EventID:     event.NewEventID(),
   AggregateID: paymentID,
   AccountID:   accountID,
   Amount:      amount,
   OccurredAt:  time.Now(),
   Version:     1,
  }

  return nil
 })

 if err != nil {
  return nil, err
 }

 // Publish event AFTER transaction commits
 // (Ensures event only published if transaction successful)
 if err := s.eventPublisher.Publish(ctx, evt); err != nil {
  s.logger.Error("failed to publish PaymentProcessed event", "error", err)
 }

 return payment, nil
}
```

### Batch Event Publishing

```go
package service

import (
 "context"
 "time"

 "yourapp/domain"
 "yourapp/domain/event"
)

// Service publishing multiple events
type BatchProcessingService struct {
 orderRepo      domain.OrderRepository
 eventPublisher event.EventPublisher
}

// ProcessOrders processes multiple orders and publishes batch events
func (s *BatchProcessingService) ProcessOrders(
 ctx context.Context,
 orderIDs []domain.OrderID,
) ([]domain.Order, error) {
 var orders []domain.Order
 var events []event.DomainEvent

 // Process all orders
 for _, orderID := range orderIDs {
  order, err := s.orderRepo.FindByID(ctx, orderID)
  if err != nil {
   return nil, err
  }

  if err := order.Process(); err != nil {
   return nil, err
  }

  if err := s.orderRepo.Save(ctx, order); err != nil {
   return nil, err
  }

  orders = append(orders, *order)

  // Collect event
  events = append(events, event.OrderProcessed{
   EventID:     event.NewEventID(),
   AggregateID: orderID,
   OccurredAt:  time.Now(),
   Version:     1,
  })
 }

 // Publish all events in single batch
 if err := s.eventPublisher.PublishBatch(ctx, events); err != nil {
  s.logger.Error("failed to publish batch events", "error", err)
 }

 return orders, nil
}
```

## Dependency Injection

Go services use **constructor functions** for manual dependency injection instead of DI frameworks.

### Constructor Pattern

```go
package service

import (
 "yourapp/domain"
 "yourapp/domain/event"
)

// Service with explicit dependencies
type DonationService struct {
 donationRepo   domain.DonationRepository
 campaignRepo   domain.CampaignRepository
 eventPublisher event.EventPublisher
 emailService   EmailService
 logger         Logger
}

// NewDonationService creates service with validated dependencies
func NewDonationService(
 donationRepo domain.DonationRepository,
 campaignRepo domain.CampaignRepository,
 eventPublisher event.EventPublisher,
 emailService EmailService,
 logger Logger,
) *DonationService {
 // Validate dependencies (fail fast)
 if donationRepo == nil {
  panic("donationRepo is required")
 }
 if campaignRepo == nil {
  panic("campaignRepo is required")
 }
 if eventPublisher == nil {
  panic("eventPublisher is required")
 }
 if emailService == nil {
  panic("emailService is required")
 }
 if logger == nil {
  panic("logger is required")
 }

 return &DonationService{
  donationRepo:   donationRepo,
  campaignRepo:   campaignRepo,
  eventPublisher: eventPublisher,
  emailService:   emailService,
  logger:         logger,
 }
}

// EmailService interface (dependency)
type EmailService interface {
 SendDonationReceipt(ctx context.Context, donorID domain.DonorID, amount domain.Money) error
}

// Logger interface (dependency)
type Logger interface {
 Info(msg string, keysAndValues ...interface{})
 Error(msg string, keysAndValues ...interface{})
}
```

### Interface Dependencies

```go
package service

import (
 "context"

 "yourapp/domain"
 "yourapp/domain/event"
)

// Service depends on interfaces (not concrete implementations)
type ZakatService struct {
 // Repositories (interfaces from domain layer)
 accountRepo     domain.ZakatAccountRepository
 calculationRepo domain.CalculationRepository

 // Event publisher (interface)
 eventPublisher event.EventPublisher

 // External services (interfaces)
 notificationService NotificationService
 auditLogger         AuditLogger
}

// NotificationService interface (application service)
type NotificationService interface {
 SendZakatCalculationNotification(ctx context.Context, accountID domain.AccountID) error
}

// AuditLogger interface (infrastructure)
type AuditLogger interface {
 LogCalculation(ctx context.Context, calculation *domain.ZakatCalculation) error
}

// Constructor with interface dependencies
func NewZakatService(
 accountRepo domain.ZakatAccountRepository,
 calculationRepo domain.CalculationRepository,
 eventPublisher event.EventPublisher,
 notificationService NotificationService,
 auditLogger AuditLogger,
) *ZakatService {
 return &ZakatService{
  accountRepo:         accountRepo,
  calculationRepo:     calculationRepo,
  eventPublisher:      eventPublisher,
  notificationService: notificationService,
  auditLogger:         auditLogger,
 }
}
```

### Service Factory Pattern

```go
package service

import (
 "database/sql"

 "yourapp/domain"
 "yourapp/domain/event"
 "yourapp/infrastructure"
)

// ServiceFactory creates configured services with dependencies
type ServiceFactory struct {
 db             *sql.DB
 eventPublisher event.EventPublisher
 logger         Logger
}

// NewServiceFactory creates service factory
func NewServiceFactory(
 db *sql.DB,
 eventPublisher event.EventPublisher,
 logger Logger,
) *ServiceFactory {
 return &ServiceFactory{
  db:             db,
  eventPublisher: eventPublisher,
  logger:         logger,
 }
}

// NewZakatService creates fully configured zakat service
func (f *ServiceFactory) NewZakatService() *ZakatService {
 // Create repositories
 accountRepo := infrastructure.NewSQLZakatAccountRepository(f.db)
 calculationRepo := infrastructure.NewSQLCalculationRepository(f.db)

 // Create service with dependencies
 return NewZakatService(
  accountRepo,
  calculationRepo,
  f.eventPublisher,
  f.logger,
 )
}

// NewDonationService creates fully configured donation service
func (f *ServiceFactory) NewDonationService() *DonationService {
 donationRepo := infrastructure.NewSQLDonationRepository(f.db)
 campaignRepo := infrastructure.NewSQLCampaignRepository(f.db)

 return NewDonationService(
  donationRepo,
  campaignRepo,
  f.eventPublisher,
  f.logger,
 )
}
```

## Error Handling

Services handle errors explicitly, wrapping domain and infrastructure errors with context.

### Error Wrapping Pattern

```go
package service

import (
 "context"
 "fmt"

 "yourapp/domain"
)

// Service with contextual error wrapping
type AccountService struct {
 accountRepo domain.AccountRepository
}

// OpenAccount wraps errors with operation context
func (s *AccountService) OpenAccount(
 ctx context.Context,
 accountID domain.AccountID,
 ownerID domain.UserID,
) (*domain.Account, error) {
 // Create account (domain logic)
 account, err := domain.NewAccount(accountID, ownerID)
 if err != nil {
  // Wrap domain error with service context
  return nil, fmt.Errorf("create account: %w", err)
 }

 // Save account (infrastructure)
 if err := s.accountRepo.Save(ctx, account); err != nil {
  // Wrap infrastructure error
  return nil, fmt.Errorf("save account: %w", err)
 }

 return account, nil
}
```

### Domain Error Handling

```go
package service

import (
 "context"
 "errors"
 "fmt"

 "yourapp/domain"
)

// Service handling specific domain errors
type TransferService struct {
 accountRepo domain.AccountRepository
}

// Transfer handles domain-specific errors
func (s *TransferService) Transfer(
 ctx context.Context,
 fromID domain.AccountID,
 toID domain.AccountID,
 amount domain.Money,
) error {
 // Load accounts
 fromAccount, err := s.accountRepo.FindByID(ctx, fromID)
 if err != nil {
  if errors.Is(err, domain.ErrNotFound) {
   return fmt.Errorf("sender account not found: %w", err)
  }
  return fmt.Errorf("load sender: %w", err)
 }

 toAccount, err := s.accountRepo.FindByID(ctx, toID)
 if err != nil {
  if errors.Is(err, domain.ErrNotFound) {
   return fmt.Errorf("receiver account not found: %w", err)
  }
  return fmt.Errorf("load receiver: %w", err)
 }

 // Execute transfer (domain logic)
 if err := fromAccount.Debit(amount); err != nil {
  // Handle domain errors specifically
  if errors.Is(err, domain.ErrInsufficientFunds) {
   return fmt.Errorf("insufficient funds in sender account: %w", err)
  }
  return fmt.Errorf("debit sender: %w", err)
 }

 if err := toAccount.Credit(amount); err != nil {
  return fmt.Errorf("credit receiver: %w", err)
 }

 // Save changes
 if err := s.accountRepo.Save(ctx, fromAccount); err != nil {
  return fmt.Errorf("save sender: %w", err)
 }

 if err := s.accountRepo.Save(ctx, toAccount); err != nil {
  return fmt.Errorf("save receiver: %w", err)
 }

 return nil
}
```

### Service-Level Errors

```go
package service

import (
 "errors"
 "fmt"
)

// Service-specific errors
var (
 ErrInvalidOperation     = errors.New("invalid operation")
 ErrOperationNotAllowed  = errors.New("operation not allowed")
 ErrConcurrentModification = errors.New("concurrent modification detected")
)

// Service using custom errors
type OrderService struct {
 orderRepo domain.OrderRepository
}

// CancelOrder returns service-specific errors
func (s *OrderService) CancelOrder(
 ctx context.Context,
 orderID domain.OrderID,
 canceledBy domain.UserID,
) error {
 // Load order
 order, err := s.orderRepo.FindByID(ctx, orderID)
 if err != nil {
  return err
 }

 // Check business rules at service level
 if order.Status() == domain.OrderShipped {
  // Service-level validation error
  return fmt.Errorf("%w: cannot cancel shipped order", ErrOperationNotAllowed)
 }

 // Execute domain logic
 if err := order.Cancel(canceledBy); err != nil {
  return fmt.Errorf("cancel order: %w", err)
 }

 // Save order
 if err := s.orderRepo.Save(ctx, order); err != nil {
  if errors.Is(err, domain.ErrOptimisticLockConflict) {
   return fmt.Errorf("%w: %w", ErrConcurrentModification, err)
  }
  return fmt.Errorf("save order: %w", err)
 }

 return nil
}
```

## Complete Example: Zakat Calculation Service

Complete working implementation of zakat calculation service with all patterns applied.

```go
package service

import (
 "context"
 "fmt"
 "time"

 "yourapp/domain"
 "yourapp/domain/event"
)

// ========================================
// Service Definition
// ========================================

// ZakatCalculationService orchestrates zakat calculation use cases.
type ZakatCalculationService struct {
 accountRepo     domain.ZakatAccountRepository
 calculationRepo domain.CalculationRepository
 paymentRepo     domain.PaymentRepository
 eventPublisher  event.EventPublisher
 logger          Logger
}

// NewZakatCalculationService creates zakat calculation service.
func NewZakatCalculationService(
 accountRepo domain.ZakatAccountRepository,
 calculationRepo domain.CalculationRepository,
 paymentRepo domain.PaymentRepository,
 eventPublisher event.EventPublisher,
 logger Logger,
) *ZakatCalculationService {
 if accountRepo == nil {
  panic("accountRepo is required")
 }
 if calculationRepo == nil {
  panic("calculationRepo is required")
 }
 if paymentRepo == nil {
  panic("paymentRepo is required")
 }
 if eventPublisher == nil {
  panic("eventPublisher is required")
 }
 if logger == nil {
  panic("logger is required")
 }

 return &ZakatCalculationService{
  accountRepo:     accountRepo,
  calculationRepo: calculationRepo,
  paymentRepo:     paymentRepo,
  eventPublisher:  eventPublisher,
  logger:          logger,
 }
}

// ========================================
// Use Case: Calculate Zakat
// ========================================

// CalculateZakat calculates zakat obligation for account.
func (s *ZakatCalculationService) CalculateZakat(
 ctx context.Context,
 accountID domain.AccountID,
 nisabThreshold domain.Money,
 calculatedBy domain.UserID,
) (*domain.ZakatCalculation, error) {
 s.logger.Info("calculating zakat",
  "accountID", accountID,
  "calculatedBy", calculatedBy,
 )

 // Step 1: Load account aggregate
 account, err := s.accountRepo.FindByID(ctx, accountID)
 if err != nil {
  return nil, fmt.Errorf("load zakat account: %w", err)
 }

 // Step 2: Delegate calculation to domain logic
 calculation, err := account.CalculateZakat(nisabThreshold, calculatedBy)
 if err != nil {
  return nil, fmt.Errorf("calculate zakat: %w", err)
 }

 // Step 3: Persist calculation result
 if err := s.calculationRepo.Save(ctx, calculation); err != nil {
  return nil, fmt.Errorf("save calculation: %w", err)
 }

 // Step 4: Publish domain event
 evt := event.ZakatCalculationCompleted{
  EventID:        event.NewEventID(),
  AggregateID:    accountID,
  CalculationID:  calculation.ID(),
  ZakatAmount:    calculation.Amount(),
  AccountBalance: account.Balance(),
  NisabThreshold: nisabThreshold,
  IsEligible:     calculation.IsEligible(),
  CalculatedBy:   calculatedBy,
  OccurredAt:     time.Now(),
  Version:        1,
 }

 if err := s.eventPublisher.Publish(ctx, evt); err != nil {
  s.logger.Error("failed to publish calculation event", "error", err)
 }

 s.logger.Info("zakat calculation completed",
  "calculationID", calculation.ID(),
  "zakatAmount", calculation.Amount(),
  "isEligible", calculation.IsEligible(),
 )

 return calculation, nil
}

// ========================================
// Use Case: Validate Eligibility
// ========================================

// ValidateZakatEligibility checks if account meets nisab threshold.
func (s *ZakatCalculationService) ValidateZakatEligibility(
 ctx context.Context,
 accountID domain.AccountID,
 nisabThreshold domain.Money,
) (bool, error) {
 s.logger.Debug("validating zakat eligibility",
  "accountID", accountID,
  "nisabThreshold", nisabThreshold,
 )

 // Load account
 account, err := s.accountRepo.FindByID(ctx, accountID)
 if err != nil {
  return false, fmt.Errorf("load account: %w", err)
 }

 // Delegate to domain logic (read-only)
 eligible := account.IsEligibleForZakat(nisabThreshold)

 s.logger.Debug("eligibility validated",
  "accountID", accountID,
  "eligible", eligible,
 )

 return eligible, nil
}

// ========================================
// Use Case: Record Payment
// ========================================

// RecordZakatPayment processes zakat payment transaction.
func (s *ZakatCalculationService) RecordZakatPayment(
 ctx context.Context,
 accountID domain.AccountID,
 paymentID domain.PaymentID,
 amount domain.Money,
 paymentMethod domain.PaymentMethod,
 paidBy domain.UserID,
) (*domain.ZakatPayment, error) {
 s.logger.Info("recording zakat payment",
  "accountID", accountID,
  "paymentID", paymentID,
  "amount", amount,
  "method", paymentMethod,
 )

 var payment *domain.ZakatPayment
 var evt event.DomainEvent

 // Execute within transaction
 err := s.accountRepo.WithTransaction(ctx, func(txCtx context.Context) error {
  // Load account
  account, err := s.accountRepo.FindByID(txCtx, accountID)
  if err != nil {
   return fmt.Errorf("load account: %w", err)
  }

  // Execute domain logic (create payment)
  payment, err = account.RecordPayment(paymentID, amount, paymentMethod, paidBy)
  if err != nil {
   return fmt.Errorf("record payment: %w", err)
  }

  // Save updated account
  if err := s.accountRepo.Save(txCtx, account); err != nil {
   return fmt.Errorf("save account: %w", err)
  }

  // Save payment record
  if err := s.paymentRepo.Save(txCtx, payment); err != nil {
   return fmt.Errorf("save payment: %w", err)
  }

  // Prepare event (published after commit)
  evt = event.ZakatPaymentRecorded{
   EventID:       event.NewEventID(),
   AggregateID:   accountID,
   PaymentID:     paymentID,
   Amount:        amount,
   PaymentMethod: paymentMethod,
   PaidBy:        paidBy,
   OccurredAt:    time.Now(),
   Version:       1,
  }

  return nil
 })

 if err != nil {
  s.logger.Error("failed to record payment", "error", err)
  return nil, err
 }

 // Publish event after successful transaction
 if err := s.eventPublisher.Publish(ctx, evt); err != nil {
  s.logger.Error("failed to publish payment event", "error", err)
 }

 s.logger.Info("zakat payment recorded",
  "paymentID", paymentID,
  "amount", amount,
 )

 return payment, nil
}

// ========================================
// Use Case: Get Payment History
// ========================================

// GetPaymentHistory retrieves payment history for account.
func (s *ZakatCalculationService) GetPaymentHistory(
 ctx context.Context,
 accountID domain.AccountID,
) ([]*domain.ZakatPayment, error) {
 s.logger.Debug("retrieving payment history", "accountID", accountID)

 // Query repository (read-only)
 payments, err := s.paymentRepo.FindByAccountID(ctx, accountID)
 if err != nil {
  return nil, fmt.Errorf("find payments: %w", err)
 }

 return payments, nil
}

// ========================================
// Use Case: Get Recent Calculations
// ========================================

// GetRecentCalculations retrieves recent calculations for account.
func (s *ZakatCalculationService) GetRecentCalculations(
 ctx context.Context,
 accountID domain.AccountID,
 limit int,
) ([]*domain.ZakatCalculation, error) {
 s.logger.Debug("retrieving recent calculations",
  "accountID", accountID,
  "limit", limit,
 )

 // Query repository (read-only)
 calculations, err := s.calculationRepo.FindRecentByAccountID(ctx, accountID, limit)
 if err != nil {
  return nil, fmt.Errorf("find calculations: %w", err)
 }

 return calculations, nil
}

// ========================================
// Logger Interface
// ========================================

// Logger defines logging operations for service.
type Logger interface {
 Info(msg string, keysAndValues ...interface{})
 Error(msg string, keysAndValues ...interface{})
 Debug(msg string, keysAndValues ...interface{})
}
```

## Before/After Comparison

### ❌ Before: Anemic Service with Business Logic

```go
package service

import (
 "context"
 "database/sql"
 "fmt"
 "time"
)

// PROBLEM: Service contains business logic, direct database access
type ZakatService struct {
 db *sql.DB
}

// CalculateZakat has business rules mixed with infrastructure
func (s *ZakatService) CalculateZakat(
 ctx context.Context,
 accountID string,
 userID string,
) (float64, error) {
 // Direct SQL query (no domain model)
 var balance float64
 var nisab float64
 err := s.db.QueryRowContext(ctx, `
  SELECT balance, nisab_threshold
  FROM zakat_accounts
  WHERE id = $1
 `, accountID).Scan(&balance, &nisab)

 if err != nil {
  return 0, err
 }

 // ❌ Business logic in service!
 var zakatAmount float64
 if balance >= nisab {
  zakatAmount = balance * 0.025 // 2.5% zakat rate
 }

 // Direct SQL insert (no aggregate)
 _, err = s.db.ExecContext(ctx, `
  INSERT INTO zakat_calculations (account_id, amount, calculated_by, calculated_at)
  VALUES ($1, $2, $3, $4)
 `, accountID, zakatAmount, userID, time.Now())

 if err != nil {
  return 0, err
 }

 // ❌ No events, no audit trail, no domain model
 return zakatAmount, nil
}

// PROBLEMS:
// - Business rules scattered in service (2.5% rate)
// - Direct database access (no repository abstraction)
// - No domain model (primitive obsession)
// - No transaction management
// - No domain events
// - Hard to test
// - Cannot reuse business logic
```

### ✅ After: Rich Domain with Orchestrating Service

```go
package service

import (
 "context"
 "fmt"
 "time"

 "yourapp/domain"
 "yourapp/domain/event"
)

// Service orchestrates, domain contains business logic
type ZakatCalculationService struct {
 accountRepo     domain.ZakatAccountRepository
 calculationRepo domain.CalculationRepository
 eventPublisher  event.EventPublisher
}

// CalculateZakat orchestrates use case
func (s *ZakatCalculationService) CalculateZakat(
 ctx context.Context,
 accountID domain.AccountID,
 nisabThreshold domain.Money,
 calculatedBy domain.UserID,
) (*domain.ZakatCalculation, error) {
 // Load aggregate (repository abstraction)
 account, err := s.accountRepo.FindByID(ctx, accountID)
 if err != nil {
  return nil, fmt.Errorf("load account: %w", err)
 }

 // ✅ Domain logic in aggregate (business rules encapsulated)
 calculation, err := account.CalculateZakat(nisabThreshold, calculatedBy)
 if err != nil {
  return nil, fmt.Errorf("calculate zakat: %w", err)
 }

 // Persist result
 if err := s.calculationRepo.Save(ctx, calculation); err != nil {
  return nil, fmt.Errorf("save calculation: %w", err)
 }

 // ✅ Publish domain event (audit trail + integration)
 evt := event.ZakatCalculationCompleted{
  EventID:        event.NewEventID(),
  AggregateID:    accountID,
  CalculationID:  calculation.ID(),
  ZakatAmount:    calculation.Amount(),
  CalculatedBy:   calculatedBy,
  OccurredAt:     time.Now(),
  Version:        1,
 }
 s.eventPublisher.Publish(ctx, evt)

 return calculation, nil
}

// BENEFITS:
// - Service only orchestrates (thin layer)
// - Business logic in domain model (testable, reusable)
// - Repository abstraction (easy to swap implementations)
// - Domain events (audit trail, integration)
// - Rich domain model (type-safe value objects)
// - Easy to test (mock repositories)
```

### Key Improvements

| Aspect              | Before (Anemic Service)       | After (Rich Domain)              |
| ------------------- | ----------------------------- | -------------------------------- |
| **Business Logic**  | Scattered in service          | Encapsulated in domain           |
| **Database Access** | Direct SQL queries            | Repository abstraction           |
| **Domain Model**    | Primitive obsession (strings) | Rich value objects (AccountID)   |
| **Testability**     | Requires database             | Mock repositories                |
| **Reusability**     | Business rules duplicated     | Centralized in aggregates        |
| **Events**          | No audit trail                | Domain events published          |
| **Transactions**    | Manual                        | Context-based propagation        |
| **Error Handling**  | Generic errors                | Domain-specific errors wrapped   |
| **Type Safety**     | float64, string               | Money, AccountID (value objects) |

## Usage Guidelines

### When to Create Application Services

**✅ Create services for:**

- **Use case orchestration**: Coordinating domain objects to implement business scenarios
- **Transaction boundaries**: Managing atomic operations across multiple aggregates
- **Event publishing**: Publishing domain events after successful operations
- **Cross-aggregate coordination**: Coordinating multiple aggregates
- **Infrastructure integration**: Integrating domain with external systems

**❌ Don't create services for:**

- **Business logic**: Business rules belong in domain objects
- **Simple CRUD**: Direct repository access sufficient
- **Data transformation**: Use DTOs or mappers
- **Technical utilities**: Use libraries or helper functions

### Service Naming Conventions

**Pattern**: `<DomainConcept>Service`

**Examples:**

- `ZakatCalculationService` - Zakat calculation use cases
- `DonationProcessingService` - Donation processing orchestration
- `LoanApplicationService` - Loan application workflows
- `PaymentService` - Payment processing coordination

**Method naming:**

- Use case methods: `CalculateZakat`, `ProcessDonation`, `ApproveLoan`
- Query methods: `GetPaymentHistory`, `FindEligibleAccounts`
- Validation methods: `ValidateZakatEligibility`, `CheckLoanCriteria`

### Service Responsibilities

**Services SHOULD:**

- Load aggregates from repositories
- Coordinate domain object interactions
- Manage transaction boundaries
- Publish domain events
- Handle infrastructure concerns (logging, tracing)
- Validate preconditions
- Wrap errors with context

**Services SHOULD NOT:**

- Contain business logic (delegate to domain)
- Manipulate aggregate internals (use aggregate methods)
- Perform complex calculations (domain responsibility)
- Make business decisions (domain responsibility)

### Performance Considerations

```go
// ✅ Batch operations for efficiency
func (s *Service) ProcessBatch(ctx context.Context, ids []domain.ID) error {
 // Load all aggregates in one query (if repository supports)
 aggregates, err := s.repo.FindByIDs(ctx, ids)
 if err != nil {
  return err
 }

 // Process each
 for _, agg := range aggregates {
  agg.Process()
 }

 // Batch save
 return s.repo.SaveBatch(ctx, aggregates)
}

// ✅ Use read-only queries for reporting
func (s *Service) GenerateReport(ctx context.Context) (*Report, error) {
 // Use specialized query (no aggregate loading)
 data, err := s.repo.GetReportData(ctx)
 if err != nil {
  return nil, err
 }

 return createReport(data), nil
}

// ✅ Cache frequently accessed data
func (s *Service) GetConfiguration(ctx context.Context) (*Config, error) {
 // Check cache first
 if cached := s.cache.Get("config"); cached != nil {
  return cached.(*Config), nil
 }

 // Load from repository
 config, err := s.configRepo.Load(ctx)
 if err != nil {
  return nil, err
 }

 // Cache result
 s.cache.Set("config", config, 5*time.Minute)

 return config, nil
}
```

## Testing Services

### Unit Testing with Mocks

```go
package service_test

import (
 "context"
 "errors"
 "testing"

 "github.com/stretchr/testify/assert"
 "github.com/stretchr/testify/mock"
 "github.com/stretchr/testify/require"

 "yourapp/domain"
 "yourapp/domain/event"
 "yourapp/service"
)

// ========================================
// Mock Repository
// ========================================

type MockAccountRepository struct {
 mock.Mock
}

func (m *MockAccountRepository) FindByID(ctx context.Context, id domain.AccountID) (*domain.ZakatAccount, error) {
 args := m.Called(ctx, id)
 if args.Get(0) == nil {
  return nil, args.Error(1)
 }
 return args.Get(0).(*domain.ZakatAccount), args.Error(1)
}

func (m *MockAccountRepository) Save(ctx context.Context, account *domain.ZakatAccount) error {
 args := m.Called(ctx, account)
 return args.Error(0)
}

func (m *MockAccountRepository) WithTransaction(ctx context.Context, fn func(context.Context) error) error {
 args := m.Called(ctx, fn)
 return args.Error(0)
}

// ========================================
// Mock Event Publisher
// ========================================

type MockEventPublisher struct {
 mock.Mock
}

func (m *MockEventPublisher) Publish(ctx context.Context, evt event.DomainEvent) error {
 args := m.Called(ctx, evt)
 return args.Error(0)
}

func (m *MockEventPublisher) PublishBatch(ctx context.Context, events []event.DomainEvent) error {
 args := m.Called(ctx, events)
 return args.Error(0)
}

// ========================================
// Mock Logger
// ========================================

type MockLogger struct {
 mock.Mock
}

func (m *MockLogger) Info(msg string, keysAndValues ...interface{}) {
 m.Called(msg, keysAndValues)
}

func (m *MockLogger) Error(msg string, keysAndValues ...interface{}) {
 m.Called(msg, keysAndValues)
}

func (m *MockLogger) Debug(msg string, keysAndValues ...interface{}) {
 m.Called(msg, keysAndValues)
}

// ========================================
// Test Cases
// ========================================

func TestZakatCalculationService_CalculateZakat_Success(t *testing.T) {
 // Arrange
 ctx := context.Background()
 accountID := domain.NewAccountID("acc-001")
 nisab := domain.NewMoney(1000, 0, "USD")
 userID := domain.NewUserID("user-001")

 mockAccountRepo := new(MockAccountRepository)
 mockCalcRepo := new(MockCalculationRepository)
 mockPublisher := new(MockEventPublisher)
 mockLogger := new(MockLogger)

 service := service.NewZakatCalculationService(
  mockAccountRepo,
  mockCalcRepo,
  mockPublisher,
  mockLogger,
 )

 // Create test account
 account := domain.NewZakatAccount(accountID, domain.NewHolderID("holder-001"))
 account.Deposit(domain.NewMoney(5000, 0, "USD"))

 // Mock expectations
 mockAccountRepo.On("FindByID", ctx, accountID).Return(account, nil)
 mockCalcRepo.On("Save", ctx, mock.AnythingOfType("*domain.ZakatCalculation")).Return(nil)
 mockPublisher.On("Publish", ctx, mock.AnythingOfType("event.ZakatCalculationCompleted")).Return(nil)
 mockLogger.On("Info", mock.Anything, mock.Anything).Return()

 // Act
 calculation, err := service.CalculateZakat(ctx, accountID, nisab, userID)

 // Assert
 require.NoError(t, err)
 require.NotNil(t, calculation)
 assert.True(t, calculation.IsEligible())
 assert.Equal(t, domain.NewMoney(125, 0, "USD"), calculation.Amount()) // 2.5% of 5000

 mockAccountRepo.AssertExpectations(t)
 mockCalcRepo.AssertExpectations(t)
 mockPublisher.AssertExpectations(t)
}

func TestZakatCalculationService_CalculateZakat_AccountNotFound(t *testing.T) {
 // Arrange
 ctx := context.Background()
 accountID := domain.NewAccountID("acc-999")
 nisab := domain.NewMoney(1000, 0, "USD")
 userID := domain.NewUserID("user-001")

 mockAccountRepo := new(MockAccountRepository)
 mockCalcRepo := new(MockCalculationRepository)
 mockPublisher := new(MockEventPublisher)
 mockLogger := new(MockLogger)

 service := service.NewZakatCalculationService(
  mockAccountRepo,
  mockCalcRepo,
  mockPublisher,
  mockLogger,
 )

 // Mock expectations
 mockAccountRepo.On("FindByID", ctx, accountID).Return(nil, domain.ErrNotFound)

 // Act
 calculation, err := service.CalculateZakat(ctx, accountID, nisab, userID)

 // Assert
 require.Error(t, err)
 assert.Nil(t, calculation)
 assert.Contains(t, err.Error(), "load zakat account")
 assert.ErrorIs(t, err, domain.ErrNotFound)

 mockAccountRepo.AssertExpectations(t)
 mockCalcRepo.AssertNotCalled(t, "Save")
 mockPublisher.AssertNotCalled(t, "Publish")
}

func TestZakatCalculationService_RecordPayment_TransactionRollback(t *testing.T) {
 // Arrange
 ctx := context.Background()
 accountID := domain.NewAccountID("acc-001")
 paymentID := domain.NewPaymentID("pay-001")
 amount := domain.NewMoney(100, 0, "USD")
 userID := domain.NewUserID("user-001")

 mockAccountRepo := new(MockAccountRepository)
 mockPaymentRepo := new(MockPaymentRepository)
 mockPublisher := new(MockEventPublisher)
 mockLogger := new(MockLogger)

 service := service.NewZakatCalculationService(
  mockAccountRepo,
  mockPaymentRepo,
  mockPublisher,
  mockLogger,
 )

 // Mock transaction that fails on save
 mockAccountRepo.On("WithTransaction", ctx, mock.AnythingOfType("func(context.Context) error")).
  Run(func(args mock.Arguments) {
   fn := args.Get(1).(func(context.Context) error)
   txCtx := context.Background()

   account := domain.NewZakatAccount(accountID, domain.NewHolderID("holder-001"))
   mockAccountRepo.On("FindByID", txCtx, accountID).Return(account, nil).Once()
   mockAccountRepo.On("Save", txCtx, account).Return(errors.New("database error")).Once()

   fn(txCtx) // Execute transaction function
  }).
  Return(errors.New("database error"))

 mockLogger.On("Info", mock.Anything, mock.Anything).Return()
 mockLogger.On("Error", mock.Anything, mock.Anything).Return()

 // Act
 payment, err := service.RecordZakatPayment(ctx, accountID, paymentID, amount, domain.CreditCard, userID)

 // Assert
 require.Error(t, err)
 assert.Nil(t, payment)
 assert.Contains(t, err.Error(), "database error")

 // Event should NOT be published (transaction failed)
 mockPublisher.AssertNotCalled(t, "Publish")
}
```

### Integration Testing

```go
package service_test

import (
 "context"
 "database/sql"
 "testing"

 _ "github.com/lib/pq"
 "github.com/stretchr/testify/assert"
 "github.com/stretchr/testify/require"

 "yourapp/domain"
 "yourapp/infrastructure"
 "yourapp/service"
)

func setupTestDB(t *testing.T) *sql.DB {
 db, err := sql.Open("postgres", "postgres://localhost/test?sslmode=disable")
 require.NoError(t, err)

 // Run migrations
 err = runMigrations(db)
 require.NoError(t, err)

 return db
}

func TestZakatCalculationService_Integration(t *testing.T) {
 // Setup database
 db := setupTestDB(t)
 defer db.Close()

 // Create real repositories
 accountRepo := infrastructure.NewSQLZakatAccountRepository(db)
 calculationRepo := infrastructure.NewSQLCalculationRepository(db)
 paymentRepo := infrastructure.NewSQLPaymentRepository(db)

 // Create event publisher (in-memory for testing)
 eventBus := event.NewEventBus()
 publisher := event.NewInMemoryPublisher(eventBus)

 // Create logger
 logger := newTestLogger()

 // Create service with real dependencies
 service := service.NewZakatCalculationService(
  accountRepo,
  calculationRepo,
  paymentRepo,
  publisher,
  logger,
 )

 ctx := context.Background()

 // Create test account
 accountID := domain.NewAccountID("acc-integration-001")
 holderID := domain.NewHolderID("holder-001")
 account := domain.NewZakatAccount(accountID, holderID)
 account.Deposit(domain.NewMoney(10000, 0, "USD"))

 err := accountRepo.Save(ctx, account)
 require.NoError(t, err)

 // Test: Calculate zakat
 nisab := domain.NewMoney(1000, 0, "USD")
 userID := domain.NewUserID("user-001")

 calculation, err := service.CalculateZakat(ctx, accountID, nisab, userID)

 // Assert
 require.NoError(t, err)
 require.NotNil(t, calculation)
 assert.True(t, calculation.IsEligible())
 assert.Equal(t, domain.NewMoney(250, 0, "USD"), calculation.Amount())

 // Verify calculation persisted
 savedCalc, err := calculationRepo.FindByID(ctx, calculation.ID())
 require.NoError(t, err)
 assert.Equal(t, calculation.Amount(), savedCalc.Amount())
}
```

## Related Documentation

### DDD Templates

- [Repository Template](./repository-template.md) - Repository persistence abstraction
- [Domain Event Template](./domain-event-template.md) - Domain events and event handling
- [Aggregate Template](./aggregate-template.md) - Aggregate root pattern
- [Entity Template](./entity-template.md) - Entity with identity
- [Value Object Template](./value-object-template.md) - Immutable value objects

### Golang Best Practices

- [Go Best Practices](../ex-so-stla-go__best-practices.md) - Go coding standards
- [Go Idioms](../ex-so-stla-go__idioms.md) - Idiomatic Go patterns
- [Go Error Handling](../ex-so-stla-go__error-handling.md) - Error handling strategies
- [Go Interfaces](../ex-so-stla-go__interfaces-and-composition.md) - Interface design
- [Go Concurrency and Parallelism](../ex-so-stla-go__concurrency-and-parallelism.md) - Goroutines and channels

### Domain-Driven Design

- [DDD in Go](../ex-so-stla-go__domain-driven-design.md) - Complete DDD patterns in Go

### Conventions & Principles

- [Simplicity Over Complexity](../../../../../../governance/principles/general/simplicity-over-complexity.md)
- [Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)

---

**Template Version**: 1.0
**Go Compatibility**: 1.18+, 1.21+, 1.22+, 1.23+, 1.24+, 1.25+
**Last Updated**: 2025-01-23
