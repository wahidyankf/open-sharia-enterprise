---
title: Spring Framework Observability
description: Monitoring covering Micrometer integration, custom metrics, logging, distributed tracing, application events, JMX, health indicators, and performance monitoring
category: explanation
subcategory: platform-web
tags:
  - spring-framework
  - observability
  - monitoring
  - metrics
  - java
  - kotlin
principles:
  - automation-over-manual
created: 2026-01-29
updated: 2026-01-29
---

# Spring Framework Observability

**Understanding-oriented documentation** for monitoring Spring applications.

## Overview

Observability enables understanding system behavior through metrics, logging, and tracing. Spring Framework integrates with Micrometer for comprehensive monitoring of Islamic finance applications.

**Version**: Spring Framework 6.1+ (Java 17+, Kotlin 1.9+)

## Quick Reference

**Jump to:**

- [Micrometer Integration](#micrometer-integration)
- [Custom Metrics](#custom-metrics)
- [Logging Best Practices](#logging-best-practices)
- [Application Events](#application-events)
- [Health Indicators](#health-indicators)

## Micrometer Integration

### Enable Micrometer

**Java Example**:

```java
@Configuration
public class MetricsConfig {

  @Bean
  public MeterRegistry meterRegistry() {
    return new SimpleMeterRegistry();
  }
}
```

### Using Metrics

**Java Example** (Donation Metrics):

```java
@Service
public class DonationService {
  private final DonationRepository repository;
  private final MeterRegistry meterRegistry;
  private final Counter donationCounter;
  private final Timer donationTimer;

  public DonationService(DonationRepository repository, MeterRegistry meterRegistry) {
    this.repository = repository;
    this.meterRegistry = meterRegistry;

    this.donationCounter = Counter.builder("donations.created")
      .description("Total donations created")
      .tag("category", "all")
      .register(meterRegistry);

    this.donationTimer = Timer.builder("donations.processing.time")
      .description("Donation processing time")
      .register(meterRegistry);
  }

  @Transactional
  public DonationResponse processDonation(CreateDonationRequest request) {
    return donationTimer.record(() -> {
      Donation donation = Donation.create(request);
      repository.save(donation);

      donationCounter.increment();

      meterRegistry.gauge("donations.amount.total",
        Tags.of("category", donation.getCategory().name()),
        donation,
        d -> d.getAmount().getAmount().doubleValue()
      );

      return toResponse(donation);
    });
  }
}
```

**Kotlin Example**:

```kotlin
@Service
class DonationService(
  private val repository: DonationRepository,
  private val meterRegistry: MeterRegistry
) {
  private val donationCounter: Counter = Counter.builder("donations.created")
    .description("Total donations created")
    .tag("category", "all")
    .register(meterRegistry)

  private val donationTimer: Timer = Timer.builder("donations.processing.time")
    .description("Donation processing time")
    .register(meterRegistry)

  @Transactional
  fun processDonation(request: CreateDonationRequest): DonationResponse {
    return donationTimer.record {
      val donation = Donation.create(request)
      repository.save(donation)

      donationCounter.increment()

      meterRegistry.gauge("donations.amount.total",
        Tags.of("category", donation.category.name),
        donation
      ) { it.amount.amount.toDouble() }

      donation.toResponse()
    }
  }
}
```

## Custom Metrics

### Business Metrics

**Java Example** (Zakat Metrics):

```java
@Service
public class ZakatMetricsService {
  private final MeterRegistry meterRegistry;

  public ZakatMetricsService(MeterRegistry meterRegistry) {
    this.meterRegistry = meterRegistry;
  }

  public void recordZakatCalculation(ZakatCalculation calculation) {
    Counter.builder("zakat.calculations.total")
      .tag("eligible", String.valueOf(calculation.isEligible()))
      .register(meterRegistry)
      .increment();

    if (calculation.isEligible()) {
      meterRegistry.summary("zakat.amount.distribution")
        .record(calculation.getZakatAmount().getAmount().doubleValue());
    }
  }

  public void recordNisabCheck(String currency, boolean eligible) {
    Counter.builder("zakat.nisab.checks")
      .tag("currency", currency)
      .tag("eligible", String.valueOf(eligible))
      .register(meterRegistry)
      .increment();
  }
}
```

## Logging Best Practices

**Java Example**:

```java
@Service
public class MurabahaContractService {
  private static final Logger logger = LoggerFactory.getLogger(MurabahaContractService.class);

  @Transactional
  public MurabahaContractResponse createContract(CreateContractRequest request) {
    logger.info("Creating Murabaha contract for asset cost: {}", request.assetCost());

    try {
      MurabahaContract contract = MurabahaContract.create(request);
      repository.save(contract);

      logger.info("Murabaha contract created successfully: contractId={}", contract.getId());
      return toResponse(contract);
    } catch (ContractValidationException e) {
      logger.error("Contract validation failed: {}", e.getMessage());
      throw e;
    } catch (Exception e) {
      logger.error("Unexpected error creating contract: assetCost={}", request.assetCost(), e);
      throw new ContractCreationException("Failed to create contract", e);
    }
  }
}
```

**Kotlin Example**:

```kotlin
@Service
class MurabahaContractService(private val repository: MurabahaContractRepository) {
  companion object {
    private val logger = LoggerFactory.getLogger(MurabahaContractService::class.java)
  }

  @Transactional
  fun createContract(request: CreateContractRequest): MurabahaContractResponse {
    logger.info("Creating Murabaha contract for asset cost: {}", request.assetCost)

    return try {
      val contract = MurabahaContract.create(request)
      repository.save(contract)

      logger.info("Murabaha contract created successfully: contractId={}", contract.id)
      contract.toResponse()
    } catch (e: ContractValidationException) {
      logger.error("Contract validation failed: {}", e.message)
      throw e
    } catch (e: Exception) {
      logger.error("Unexpected error creating contract: assetCost={}", request.assetCost, e)
      throw ContractCreationException("Failed to create contract", e)
    }
  }
}
```

## Application Events

**Java Example** (Donation Events):

```java
public class DonationCreatedEvent extends ApplicationEvent {
  private final DonationId donationId;
  private final Money amount;
  private final DonationCategory category;

  public DonationCreatedEvent(Object source, DonationId donationId, Money amount, DonationCategory category) {
    super(source);
    this.donationId = donationId;
    this.amount = amount;
    this.category = category;
  }

  // Getters
}

@Service
public class DonationService {
  private final ApplicationEventPublisher eventPublisher;

  @Transactional
  public DonationResponse processDonation(CreateDonationRequest request) {
    Donation donation = Donation.create(request);
    repository.save(donation);

    eventPublisher.publishEvent(new DonationCreatedEvent(
      this,
      donation.getId(),
      donation.getAmount(),
      donation.getCategory()
    ));

    return toResponse(donation);
  }
}

@Component
class DonationEventListener {
  private static final Logger logger = LoggerFactory.getLogger(DonationEventListener.class);
  private final MeterRegistry meterRegistry;

  public DonationEventListener(MeterRegistry meterRegistry) {
    this.meterRegistry = meterRegistry;
  }

  @EventListener
  public void handleDonationCreated(DonationCreatedEvent event) {
    logger.info("Donation created event: donationId={}, amount={}",
      event.getDonationId(), event.getAmount());

    Counter.builder("donations.events.created")
      .tag("category", event.getCategory().name())
      .register(meterRegistry)
      .increment();
  }
}
```

## Health Indicators

**Java Example** (Custom Health Check):

```java
@Component
public class DatabaseHealthIndicator implements HealthIndicator {
  private final JdbcTemplate jdbcTemplate;

  public DatabaseHealthIndicator(JdbcTemplate jdbcTemplate) {
    this.jdbcTemplate = jdbcTemplate;
  }

  @Override
  public Health health() {
    try {
      jdbcTemplate.queryForObject("SELECT 1", Integer.class);
      return Health.up()
        .withDetail("database", "PostgreSQL")
        .withDetail("status", "Connected")
        .build();
    } catch (Exception e) {
      return Health.down()
        .withDetail("database", "PostgreSQL")
        .withDetail("status", "Disconnected")
        .withException(e)
        .build();
    }
  }
}
```

**Kotlin Example**:

```kotlin
@Component
class DatabaseHealthIndicator(
  private val jdbcTemplate: JdbcTemplate
) : HealthIndicator {

  override fun health(): Health {
    return try {
      jdbcTemplate.queryForObject("SELECT 1", Int::class.java)
      Health.up()
        .withDetail("database", "PostgreSQL")
        .withDetail("status", "Connected")
        .build()
    } catch (e: Exception) {
      Health.down()
        .withDetail("database", "PostgreSQL")
        .withDetail("status", "Disconnected")
        .withException(e)
        .build()
    }
  }
}
```

## Related Documentation

### Core Spring Framework Documentation

- **[Spring Framework README](./README.md)** - Framework overview
- **[Performance](./ex-so-plwe-jvsp__performance.md)** - Optimization techniques

---

**Last Updated**: 2026-01-29
**Spring Framework Version**: 6.1+ (Java 17+, Kotlin 1.9+)
**Maintainers**: Platform Documentation Team
