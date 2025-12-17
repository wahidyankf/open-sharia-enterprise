---
title: "How to Refactor God Classes"
date: 2025-12-17T07:06:48+07:00
draft: false
weight: 504
description: "Step-by-step guide to breaking down god classes into maintainable components"
tags:
  ["java", "refactoring", "god-class", "clean-code", "single-responsibility"]
categories: ["learn"]
---

## Problem

A god class is a class that knows too much or does too much, violating the Single Responsibility Principle. These classes are difficult to understand, test, and maintain.

**Symptoms**:

- Class has hundreds or thousands of lines
- Class has dozens of methods and fields
- Class name is vague (Manager, Handler, Util, Service)
- Multiple developers frequently edit the same class
- Class imports dozens of dependencies
- Tests require extensive mocking
- Changes in one area unexpectedly break others

This guide shows how to systematically refactor god classes into focused, maintainable components.

## Step 1: Analyze the God Class

Before refactoring, understand what the class actually does.

### Identify Responsibilities

List all responsibilities by grouping related methods and fields.

**Example god class**:

```java
public class OrderManager {
  // Database dependencies
  private DatabaseConnection db;
  private OrderRepository orderRepo;
  private CustomerRepository customerRepo;
  private ProductRepository productRepo;

  // External service dependencies
  private EmailService emailService;
  private PaymentGateway paymentGateway;
  private ShippingService shippingService;
  private InventorySystem inventorySystem;
  private TaxCalculator taxCalculator;
  private DiscountEngine discountEngine;

  // Order validation (Responsibility 1)
  public boolean validateOrder(Order order) { /* ... */ }
  private boolean checkProductAvailability(Order order) { /* ... */ }
  private boolean validateCustomer(Customer customer) { /* ... */ }

  // Payment processing (Responsibility 2)
  public PaymentResult processPayment(Order order) { /* ... */ }
  private String getPaymentToken(Order order) { /* ... */ }
  private void recordPaymentTransaction(PaymentResult result) { /* ... */ }

  // Inventory management (Responsibility 3)
  public void updateInventory(Order order) { /* ... */ }
  private void reserveStock(Order order) { /* ... */ }
  private void releaseStock(Order order) { /* ... */ }

  // Shipping (Responsibility 4)
  public void arrangeShipping(Order order) { /* ... */ }
  private ShippingMethod selectShippingMethod(Order order) { /* ... */ }
  private void generateShippingLabel(Order order) { /* ... */ }

  // Pricing (Responsibility 5)
  public BigDecimal calculateTotal(Order order) { /* ... */ }
  public BigDecimal calculateTax(Order order) { /* ... */ }
  public BigDecimal applyDiscounts(Order order) { /* ... */ }

  // Email notifications (Responsibility 6)
  public void sendOrderConfirmation(Order order) { /* ... */ }
  public void sendShippingNotification(Order order) { /* ... */ }
  private String buildEmailTemplate(Order order) { /* ... */ }

  // Database operations (Responsibility 7)
  public void saveOrder(Order order) { /* ... */ }
  public Order loadOrder(String id) { /* ... */ }
  public List<Order> findOrdersByCustomer(String customerId) { /* ... */ }

  // Reporting (Responsibility 8)
  public Report generateSalesReport() { /* ... */ }
  public Report generateInventoryReport() { /* ... */ }
}
```

**Identified responsibilities**:

1. Order validation
2. Payment processing
3. Inventory management
4. Shipping coordination
5. Price calculation
6. Email notifications
7. Database operations
8. Reporting

### Map Dependencies

Create a diagram showing which dependencies each responsibility uses.

```
Order validation → CustomerRepository, ProductRepository
Payment processing → PaymentGateway, Database
Inventory management → InventorySystem
Shipping → ShippingService
Price calculation → TaxCalculator, DiscountEngine
Email → EmailService
Database ops → All repositories
Reporting → Database, multiple repositories
```

## Step 2: Create Extract Strategy

Plan the extraction order based on dependencies.

### Prioritize Extraction

**Extract in this order**:

1. **Leaf dependencies first**: Classes with no dependencies on other responsibilities
2. **Supporting services**: Utilities used by multiple responsibilities
3. **Core business logic**: Domain logic that orchestrates services
4. **Orchestration layer**: High-level coordination

**For our example**:

```
Phase 1 (Leaf services):
  - Email notifications → EmailNotifier
  - Price calculation → PricingService
  - Inventory management → InventoryManager

Phase 2 (Mid-level services):
  - Payment processing → PaymentProcessor
  - Shipping coordination → ShippingCoordinator
  - Order validation → OrderValidator

Phase 3 (Data layer):
  - Database operations → OrderRepository (already exists, consolidate)

Phase 4 (Orchestration):
  - Order processing → OrderService (coordinates all above)
```

## Step 3: Extract Leaf Services

Start with responsibilities that have no dependencies on other responsibilities.

### Example: Extract Email Notifications

**Before**:

```java
public class OrderManager {
  private EmailService emailService;

  public void sendOrderConfirmation(Order order) {
    String subject = "Order Confirmation #" + order.getId();
    String body = buildEmailTemplate(order);
    emailService.send(order.getCustomer().getEmail(), subject, body);
  }

  public void sendShippingNotification(Order order) {
    String subject = "Your order has shipped #" + order.getId();
    String body = "Tracking: " + order.getTrackingNumber();
    emailService.send(order.getCustomer().getEmail(), subject, body);
  }

  private String buildEmailTemplate(Order order) {
    StringBuilder sb = new StringBuilder();
    sb.append("Thank you for your order!\n\n");
    sb.append("Order ID: ").append(order.getId()).append("\n");
    sb.append("Total: ").append(order.getTotal()).append("\n");
    return sb.toString();
  }
}
```

**After**:

```java
// New focused class
public class OrderNotifier {
  private final EmailService emailService;

  public OrderNotifier(EmailService emailService) {
    this.emailService = Objects.requireNonNull(emailService);
  }

  public void sendOrderConfirmation(Order order) {
    String subject = "Order Confirmation #" + order.getId();
    String body = buildConfirmationEmail(order);
    emailService.send(order.getCustomer().getEmail(), subject, body);
  }

  public void sendShippingNotification(Order order) {
    String subject = "Your order has shipped #" + order.getId();
    String body = buildShippingEmail(order);
    emailService.send(order.getCustomer().getEmail(), subject, body);
  }

  private String buildConfirmationEmail(Order order) {
    StringBuilder sb = new StringBuilder();
    sb.append("Thank you for your order!\n\n");
    sb.append("Order ID: ").append(order.getId()).append("\n");
    sb.append("Total: ").append(order.getTotal()).append("\n");
    return sb.toString();
  }

  private String buildShippingEmail(Order order) {
    StringBuilder sb = new StringBuilder();
    sb.append("Your order has shipped!\n\n");
    sb.append("Tracking: ").append(order.getTrackingNumber()).append("\n");
    return sb.toString();
  }
}

// Updated OrderManager
public class OrderManager {
  private final OrderNotifier notifier;

  public OrderManager(EmailService emailService) {
    this.notifier = new OrderNotifier(emailService);
  }

  // Delegate to extracted class
  public void sendOrderConfirmation(Order order) {
    notifier.sendOrderConfirmation(order);
  }
}
```

**Benefits**:

- `OrderNotifier` is testable in isolation
- Email logic grouped in one place
- Can evolve email functionality independently
- Reduced `OrderManager` size

### Example: Extract Pricing Logic

```java
// New focused class
public class PricingService {
  private final TaxCalculator taxCalculator;
  private final DiscountEngine discountEngine;

  public PricingService(TaxCalculator taxCalculator, DiscountEngine discountEngine) {
    this.taxCalculator = Objects.requireNonNull(taxCalculator);
    this.discountEngine = Objects.requireNonNull(discountEngine);
  }

  public BigDecimal calculateTotal(Order order) {
    BigDecimal subtotal = calculateSubtotal(order);
    BigDecimal discount = discountEngine.calculateDiscount(order, subtotal);
    BigDecimal afterDiscount = subtotal.subtract(discount);
    BigDecimal tax = taxCalculator.calculateTax(order, afterDiscount);
    return afterDiscount.add(tax);
  }

  private BigDecimal calculateSubtotal(Order order) {
    return order.getItems().stream()
      .map(item -> item.getPrice().multiply(new BigDecimal(item.getQuantity())))
      .reduce(BigDecimal.ZERO, BigDecimal::add);
  }

  public BigDecimal calculateTax(Order order) {
    BigDecimal subtotal = calculateSubtotal(order);
    return taxCalculator.calculateTax(order, subtotal);
  }
}
```

## Step 4: Extract Mid-Level Services

Extract responsibilities that depend on leaf services.

### Example: Extract Order Validation

```java
public class OrderValidator {
  private final ProductRepository productRepo;
  private final CustomerRepository customerRepo;

  public OrderValidator(
    ProductRepository productRepo,
    CustomerRepository customerRepo
  ) {
    this.productRepo = Objects.requireNonNull(productRepo);
    this.customerRepo = Objects.requireNonNull(customerRepo);
  }

  public ValidationResult validate(Order order) {
    List<String> errors = new ArrayList<>();

    if (order.getItems().isEmpty()) {
      errors.add("Order must contain at least one item");
    }

    if (!validateCustomer(order.getCustomer())) {
      errors.add("Invalid customer");
    }

    if (!checkProductAvailability(order)) {
      errors.add("Some products are not available");
    }

    return errors.isEmpty()
      ? ValidationResult.success()
      : ValidationResult.failure(errors);
  }

  private boolean validateCustomer(Customer customer) {
    return customerRepo.exists(customer.getId()) && customer.isActive();
  }

  private boolean checkProductAvailability(Order order) {
    return order.getItems().stream()
      .allMatch(item -> {
        Optional<Product> productOpt = productRepo.findById(item.getProductId());
        return productOpt.isPresent() && productOpt.get().getStock() >= item.getQuantity();
      });
  }
}
```

## Step 5: Create Orchestration Layer

After extracting specialized services, create a thin orchestration layer that coordinates them.

```java
public class OrderService {
  private final OrderValidator validator;
  private final PricingService pricingService;
  private final PaymentProcessor paymentProcessor;
  private final InventoryManager inventoryManager;
  private final ShippingCoordinator shippingCoordinator;
  private final OrderRepository orderRepository;
  private final OrderNotifier notifier;

  public OrderService(
    OrderValidator validator,
    PricingService pricingService,
    PaymentProcessor paymentProcessor,
    InventoryManager inventoryManager,
    ShippingCoordinator shippingCoordinator,
    OrderRepository orderRepository,
    OrderNotifier notifier
  ) {
    this.validator = Objects.requireNonNull(validator);
    this.pricingService = Objects.requireNonNull(pricingService);
    this.paymentProcessor = Objects.requireNonNull(paymentProcessor);
    this.inventoryManager = Objects.requireNonNull(inventoryManager);
    this.shippingCoordinator = Objects.requireNonNull(shippingCoordinator);
    this.orderRepository = Objects.requireNonNull(orderRepository);
    this.notifier = Objects.requireNonNull(notifier);
  }

  public OrderResult processOrder(Order order) {
    // Validate
    ValidationResult validation = validator.validate(order);
    if (!validation.isValid()) {
      return OrderResult.validationFailed(validation.getErrors());
    }

    // Calculate pricing
    BigDecimal total = pricingService.calculateTotal(order);
    order.setTotal(total);

    // Process payment
    PaymentResult payment = paymentProcessor.process(order);
    if (!payment.isSuccessful()) {
      return OrderResult.paymentFailed(payment.getError());
    }

    // Reserve inventory
    inventoryManager.reserve(order);

    // Save order
    orderRepository.save(order);

    // Arrange shipping
    shippingCoordinator.arrange(order);

    // Send confirmation
    notifier.sendOrderConfirmation(order);

    return OrderResult.success(order);
  }
}
```

**Key characteristics of orchestration layer**:

- Thin (mostly method calls to services)
- No business logic (delegated to services)
- Coordinates workflow
- Easy to test (mock all dependencies)
- Single Responsibility: Order processing workflow

## Step 6: Update Tests

After refactoring, update tests to reflect new structure.

### Before: God Class Test

```java
@Test
void shouldProcessOrder() {
  // Must mock everything
  DatabaseConnection db = mock(DatabaseConnection.class);
  OrderRepository orderRepo = mock(OrderRepository.class);
  CustomerRepository customerRepo = mock(CustomerRepository.class);
  ProductRepository productRepo = mock(ProductRepository.class);
  EmailService emailService = mock(EmailService.class);
  PaymentGateway paymentGateway = mock(PaymentGateway.class);
  ShippingService shippingService = mock(ShippingService.class);
  InventorySystem inventorySystem = mock(InventorySystem.class);
  TaxCalculator taxCalculator = mock(TaxCalculator.class);
  DiscountEngine discountEngine = mock(DiscountEngine.class);

  OrderManager manager = new OrderManager(
    db, orderRepo, customerRepo, productRepo,
    emailService, paymentGateway, shippingService,
    inventorySystem, taxCalculator, discountEngine
  );

  // Test one tiny piece of functionality
  manager.processOrder(testOrder);

  // Difficult to verify what happened
}
```

### After: Focused Tests

```java
// Test pricing in isolation
@Test
void shouldCalculateTotal() {
  TaxCalculator taxCalc = mock(TaxCalculator.class);
  DiscountEngine discountEngine = mock(DiscountEngine.class);

  when(discountEngine.calculateDiscount(any(), any()))
    .thenReturn(new BigDecimal("10"));
  when(taxCalc.calculateTax(any(), any()))
    .thenReturn(new BigDecimal("5"));

  PricingService pricingService = new PricingService(taxCalc, discountEngine);

  BigDecimal total = pricingService.calculateTotal(testOrder);

  assertEquals(new BigDecimal("95"), total); // 100 - 10 + 5
}

// Test validation in isolation
@Test
void shouldRejectEmptyOrder() {
  ProductRepository productRepo = mock(ProductRepository.class);
  CustomerRepository customerRepo = mock(CustomerRepository.class);

  OrderValidator validator = new OrderValidator(productRepo, customerRepo);

  Order emptyOrder = new Order();
  ValidationResult result = validator.validate(emptyOrder);

  assertFalse(result.isValid());
  assertTrue(result.getErrors().contains("Order must contain at least one item"));
}

// Test orchestration
@Test
void shouldProcessOrder() {
  OrderValidator validator = mock(OrderValidator.class);
  PricingService pricingService = mock(PricingService.class);
  PaymentProcessor paymentProcessor = mock(PaymentProcessor.class);
  InventoryManager inventoryManager = mock(InventoryManager.class);
  ShippingCoordinator shippingCoordinator = mock(ShippingCoordinator.class);
  OrderRepository orderRepository = mock(OrderRepository.class);
  OrderNotifier notifier = mock(OrderNotifier.class);

  when(validator.validate(any())).thenReturn(ValidationResult.success());
  when(pricingService.calculateTotal(any())).thenReturn(new BigDecimal("100"));
  when(paymentProcessor.process(any())).thenReturn(PaymentResult.success());

  OrderService service = new OrderService(
    validator, pricingService, paymentProcessor,
    inventoryManager, shippingCoordinator, orderRepository, notifier
  );

  OrderResult result = service.processOrder(testOrder);

  assertTrue(result.isSuccessful());
  verify(inventoryManager).reserve(testOrder);
  verify(orderRepository).save(testOrder);
  verify(notifier).sendOrderConfirmation(testOrder);
}
```

## Step 7: Update Client Code

Update code that uses the god class to use new services.

### Before

```java
public class OrderController {
  private OrderManager orderManager;

  @PostMapping("/orders")
  public ResponseEntity<?> createOrder(@RequestBody OrderRequest request) {
    Order order = request.toOrder();
    orderManager.processOrder(order);
    return ResponseEntity.ok(order);
  }
}
```

### After

```java
public class OrderController {
  private final OrderService orderService;

  public OrderController(OrderService orderService) {
    this.orderService = orderService;
  }

  @PostMapping("/orders")
  public ResponseEntity<?> createOrder(@RequestBody OrderRequest request) {
    Order order = request.toOrder();
    OrderResult result = orderService.processOrder(order);

    if (!result.isSuccessful()) {
      return ResponseEntity.badRequest().body(result.getErrors());
    }

    return ResponseEntity.ok(result.getOrder());
  }
}
```

## Common Pitfalls

### Extracting Too Much Too Fast

**Problem**: Trying to refactor entire god class in one step.

**Solution**: Incremental extraction. Extract one responsibility, test, deploy, then continue.

### Creating New God Classes

**Problem**: Extracting methods but putting them in another large class.

**Solution**: Follow Single Responsibility Principle. Each extracted class should have one clear purpose.

### Breaking Backwards Compatibility

**Problem**: Removing public methods that external code depends on.

**Solution**: Keep delegation methods in original class during transition:

```java
// Keep old method temporarily
@Deprecated
public void sendOrderConfirmation(Order order) {
  notifier.sendOrderConfirmation(order);
}
```

### Over-Engineering

**Problem**: Creating too many tiny classes.

**Solution**: Balance. Group related functionality. Aim for classes between 100-300 lines.

## Planning Your Refactoring Journey

Breaking down a god class requires patience and systematic execution. Start by taking inventory - identify every responsibility hiding in that massive class. Look for groups of methods that work with the same data or serve related purposes. These clusters reveal the natural seams where you can split the class apart.

Once you understand what the class does, map out how these responsibilities depend on each other. Some responsibilities are self-contained leaves that nothing else depends on. Others sit in the middle, depending on leaf services but supporting higher-level operations. Identifying these layers tells you the order of extraction that minimizes disruption.

With your map in hand, plan to extract from the bottom up. Start with leaf responsibilities that have no internal dependencies. Extract them first so you can use them as building blocks for mid-level services. Work your way up through the layers until you've extracted everything except the orchestration logic. This approach ensures each extraction has its dependencies already in place.

Execute the extractions one at a time. Pull out a single responsibility, write focused tests for the new class, and update any existing tests that touched that functionality. Before moving to the next extraction, keep delegation methods in the original god class for backwards compatibility. This lets existing client code continue working while you refactor.

Update client code incrementally as you go. You don't need to update everything at once - let old code call through delegation methods while new code uses the extracted classes directly. After each extraction, deploy and verify the system still works. Only after all clients have migrated away from a delegation method should you remove it.

This gradual approach spreads risk across multiple small changes instead of one massive refactoring. Each step is independently deployable and verifiable, giving you confidence as you dismantle the god class piece by piece.

## Summary

Refactoring god classes transforms monolithic tangles into well-organized systems through a systematic approach that builds confidence at each step. The journey begins with analysis - understanding exactly what that massive class does by identifying its distinct responsibilities and mapping how they depend on each other. This analysis reveals the natural structure hidden beneath the complexity.

Planning comes next. Use your dependency map to choose an extraction order that works from the bottom up. Leaf services with no internal dependencies come first, followed by mid-level services that orchestrate them, and finally the top-level coordination layer. This order ensures each extraction has its building blocks already in place.

The extraction phase requires discipline and patience. Pull out one responsibility at a time, creating a focused class that does one thing well. Write targeted tests for each extracted class - these tests are simpler and more maintainable than the tangled tests that covered the god class. Update existing tests to reflect the new structure, ensuring nothing breaks in the transition.

Client code updates happen incrementally and carefully. Keep delegation methods in the original class during the transition so existing code continues working. Migrate clients gradually to use the new extracted classes directly. After each extraction step, deploy the changes and monitor production to verify everything works as expected.

The final result is worth the effort - a system of focused classes where each one has a single, clear purpose. These classes are easier to understand because they're small. They're easier to test because they have fewer dependencies. They're easier to modify because changes stay local instead of rippling unpredictably. Most importantly, they're easier to work with as a team because different developers can modify different classes without constantly stepping on each other's toes.

## Related Content

- [Common Java Anti-Patterns](/en/learn/swe/prog-lang/java/explanation/anti-patterns)
- [Java Best Practices and Design Principles](/en/learn/swe/prog-lang/java/explanation/best-practices)
- [How to Implement Proper Exception Handling](/en/learn/swe/prog-lang/java/how-to/exception-handling)
- [How to Use Java Collections Effectively](/en/learn/swe/prog-lang/java/how-to/use-collections-effectively)
