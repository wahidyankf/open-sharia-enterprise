---
title: "Transaction Management in Production"
date: 2026-02-06T00:00:00+07:00
draft: false
weight: 1000043
description: "Manual PlatformTransactionManager → auto-configured @Transactional with Spring Boot"
tags: ["spring-boot", "in-the-field", "production", "transactions", "jpa"]
---

## Why Transaction Management Matters

Spring Boot's auto-configured @Transactional eliminates 50+ lines of TransactionTemplate boilerplate per service method. In production systems processing financial transactions (zakat donations, distributions), declarative transactions ensure ACID properties without manual commit/rollback—reducing transaction code from try-catch-finally blocks to single annotations while maintaining data integrity.

**Solution**: Spring Boot @Transactional with auto-configured PlatformTransactionManager.

```java
@Service
@Transactional  // => All methods transactional by default
public class DonationService {

    @Autowired
    private DonationRepository donationRepository;

    @Autowired
    private ReceiptService receiptService;

    public DonationResponse processDonation(DonationRequest request) {
        // => Transaction starts automatically
        // => Commits if method completes successfully
        // => Rolls back if exception thrown

        ZakatDonation donation = new ZakatDonation();
        donation.setAmount(request.getAmount());
        donation.setDonorName(request.getDonorName());
        donation = donationRepository.save(donation);  // => INSERT

        receiptService.generateReceipt(donation);  // => Also transactional

        return toResponse(donation);
        // => Automatic COMMIT (no manual transaction.commit())
    }

    @Transactional(readOnly = true)  // => Optimization for queries
    public DonationResponse findById(Long id) {
        return donationRepository.findById(id)
            .map(this::toResponse)
            .orElseThrow(() -> new DonationNotFoundException(id));
    }
}
```

**Propagation** (how transactions interact):

```java
@Transactional(propagation = Propagation.REQUIRED)  // => Join existing or create new (default)
@Transactional(propagation = Propagation.REQUIRES_NEW)  // => Always create new (suspend existing)
@Transactional(propagation = Propagation.SUPPORTS)  // => Join if exists, non-transactional otherwise
```

**Isolation levels**:

```java
@Transactional(isolation = Isolation.READ_COMMITTED)  // => Default PostgreSQL
@Transactional(isolation = Isolation.REPEATABLE_READ)  // => MySQL default
```

## Next Steps

- [Spring Data JPA](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/spring-data-jpa) - Repository patterns
