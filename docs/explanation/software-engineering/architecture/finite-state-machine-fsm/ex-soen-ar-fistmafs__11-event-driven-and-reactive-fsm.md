# Event-Driven and Reactive Finite State Machines

## Overview

Event-driven and reactive approaches represent modern paradigms for implementing Finite State Machines (FSMs) that excel in asynchronous, distributed, and high-concurrency environments. Unlike traditional synchronous FSMs where state transitions occur through direct method calls, event-driven FSMs react to events published to event buses or streams, enabling loose coupling, scalability, and resilience.

## Purpose

This explanation provides:

- Understanding of event-driven vs. traditional FSM architectures
- Reactive programming principles applied to FSMs
- Event sourcing patterns for state machine persistence
- Integration with message brokers and event buses
- Handling asynchronous transitions and side effects
- Patterns for distributed state machines
- Error handling and compensation in event-driven systems

## Target Audience

- Software architects designing event-driven systems
- Backend engineers building reactive applications
- Platform engineers implementing workflow engines
- Teams adopting microservices architectures
- Developers working with message-driven systems

## Prerequisites

- Understanding of FSM fundamentals (see `ex-soen-ar-fsm__01-fundamentals-and-theory.md`)
- Familiarity with implementation patterns (see `ex-soen-ar-fsm__04-implementation-patterns-approaches.md`)
- Basic knowledge of event-driven architecture
- Understanding of asynchronous programming concepts
- Familiarity with message queues or event streams

## Traditional vs. Event-Driven FSMs

### Traditional Synchronous FSM

In traditional FSMs, transitions are triggered by direct method calls:

```java
// Traditional synchronous FSM
public class ZakatApplicationService {
    private final ZakatApplicationFSM fsm;

    public void submitApplication(ZakatApplicationId id) {
        ZakatApplication application = repository.findById(id);

        // Direct, synchronous state transition
        fsm.transition(application, ZakatApplicationEvent.SUBMIT);

        // All side effects happen synchronously
        notificationService.notifyReviewers(application);
        auditService.log("Application submitted", application);

        repository.save(application);
    }
}
```

**Characteristics:**

- Synchronous execution
- Tight coupling between caller and FSM
- Blocking operations
- Hard to scale horizontally
- Difficult to add new listeners
- Single point of failure

### Event-Driven FSM

Event-driven FSMs react to events published to an event bus:

```java
// Event-driven FSM
@Component
public class ZakatApplicationFSM {
    private final EventPublisher eventPublisher;

    @EventHandler
    public void handle(ZakatApplicationSubmitted event) {
        ZakatApplication application = repository.findById(event.getApplicationId());

        // Validate transition is allowed
        if (application.getState() != ZakatApplicationState.DRAFT) {
            throw new IllegalStateTransitionException(
                "Cannot submit application in state: " + application.getState()
            );
        }

        // Perform state transition
        application.setState(ZakatApplicationState.PENDING_REVIEW);
        repository.save(application);

        // Publish domain event - observers react asynchronously
        eventPublisher.publish(new ZakatApplicationMovedToReview(
            event.getApplicationId(),
            application.getState(),
            Instant.now()
        ));
    }
}

// Separate listener for notifications
@Component
public class ZakatNotificationService {
    @EventHandler
    public void onApplicationMovedToReview(ZakatApplicationMovedToReview event) {
        ZakatApplication application = repository.findById(event.getApplicationId());
        notificationService.notifyReviewers(application);
    }
}

// Separate listener for auditing
@Component
public class ZakatAuditService {
    @EventHandler
    public void onApplicationMovedToReview(ZakatApplicationMovedToReview event) {
        auditLog.record(
            "Application moved to review",
            event.getApplicationId(),
            event.getTimestamp()
        );
    }
}
```

**Characteristics:**

- Asynchronous execution
- Loose coupling between components
- Non-blocking operations
- Horizontal scalability
- Easy to add new event listeners
- Fault isolation

### Comparison Matrix

| Aspect             | Traditional Synchronous      | Event-Driven                      |
| ------------------ | ---------------------------- | --------------------------------- |
| **Coupling**       | Tight coupling               | Loose coupling                    |
| **Execution**      | Synchronous, blocking        | Asynchronous, non-blocking        |
| **Scalability**    | Vertical scaling             | Horizontal scaling                |
| **Extensibility**  | Modify FSM to add behavior   | Add new event listeners           |
| **Error Handling** | Single error path            | Distributed error handling        |
| **Testing**        | Test entire flow             | Test components in isolation      |
| **Observability**  | Limited visibility           | Rich event streams for monitoring |
| **Recovery**       | Restart from last checkpoint | Replay events from event store    |

## Reactive Programming Principles

### Principle 1: Responsive

Systems respond in a timely manner.

**Example - Responsive Zakat Calculation:**

```java
@Component
public class ReactiveZakatCalculationFSM {
    private final ReactiveStateRepository stateRepository;

    public Mono<ZakatCalculationResponse> calculate(ZakatCalculationRequest request) {
        return stateRepository.findById(request.getCalculationId())
            .flatMap(calculation -> {
                // Non-blocking state transition
                return Mono.just(calculation)
                    .filter(c -> c.getState() == GATHERING_DATA)
                    .switchIfEmpty(Mono.error(
                        new IllegalStateException("Calculation not in GATHERING_DATA state")
                    ))
                    .flatMap(c -> {
                        c.setState(CALCULATING);
                        return stateRepository.save(c);
                    })
                    .flatMap(c -> performCalculation(c))
                    .flatMap(c -> {
                        c.setState(COMPLETED);
                        return stateRepository.save(c);
                    })
                    .map(this::toResponse);
            })
            .timeout(Duration.ofSeconds(5))
            .onErrorResume(error -> {
                // Graceful degradation
                return Mono.just(ZakatCalculationResponse.error(error.getMessage()));
            });
    }
}
```

**Benefits:**

- Fast response times
- Timeout protection
- Graceful degradation on errors

### Principle 2: Resilient

Systems remain responsive in the face of failure.

**Example - Resilient Contract Approval:**

```java
@Component
public class ResilientContractApprovalFSM {
    private final EventPublisher eventPublisher;
    private final CircuitBreaker circuitBreaker;

    @EventHandler
    public void handle(ContractSubmittedForReview event) {
        ContractId contractId = event.getContractId();

        Try.of(() -> {
            // Attempt to transition state
            Contract contract = repository.findById(contractId)
                .orElseThrow(() -> new ContractNotFoundException(contractId));

            contract.moveToReview();
            repository.save(contract);

            return contract;
        })
        .recover(ContractNotFoundException.class, error -> {
            // Contract not found - log and skip
            logger.warn("Contract not found: {}", contractId);
            return null;
        })
        .recover(OptimisticLockException.class, error -> {
            // Concurrent modification - retry
            logger.info("Retrying due to concurrent modification: {}", contractId);
            throw new RetryableException(error);
        })
        .onSuccess(contract -> {
            if (contract != null) {
                eventPublisher.publish(new ContractMovedToReview(contractId));
            }
        })
        .onFailure(RetryableException.class, error -> {
            // Will be retried by framework
            logger.info("Scheduling retry for contract: {}", contractId);
        })
        .onFailure(error -> {
            // Non-retryable error - move to error state
            logger.error("Failed to move contract to review: {}", contractId, error);
            eventPublisher.publish(new ContractReviewFailed(contractId, error.getMessage()));
        });
    }
}
```

**Benefits:**

- Graceful failure handling
- Automatic retries for transient errors
- Circuit breaker prevents cascading failures
- System continues functioning despite individual failures

### Principle 3: Elastic

Systems scale up and down based on load.

**Example - Elastic Loan Processing:**

```java
@Component
public class ElasticLoanProcessingFSM {
    private final EventStream<LoanApplicationEvent> eventStream;

    @PostConstruct
    public void initialize() {
        // Process events with backpressure and parallelism
        eventStream
            .buffer(Duration.ofMillis(100), 50)  // Batch events
            .parallel(8)  // Process in parallel
            .runOn(Schedulers.parallel())
            .flatMap(batch -> processLoanApplicationBatch(batch))
            .sequential()
            .subscribe(
                result -> logger.info("Processed loan application: {}", result),
                error -> logger.error("Error processing loan applications", error)
            );
    }

    private Flux<LoanApplicationResult> processLoanApplicationBatch(
        List<LoanApplicationEvent> events
    ) {
        return Flux.fromIterable(events)
            .flatMap(event -> {
                // Process each event, transitioning state
                return processLoanApplication(event)
                    .timeout(Duration.ofSeconds(10))
                    .onErrorResume(error -> {
                        // Continue processing other events
                        return Mono.just(LoanApplicationResult.error(
                            event.getApplicationId(),
                            error.getMessage()
                        ));
                    });
            });
    }
}
```

**Benefits:**

- Automatic batching under load
- Parallel processing
- Backpressure handling
- Scales with available resources

### Principle 4: Message-Driven

Systems rely on asynchronous message passing.

**Example - Message-Driven Zakat Campaign:**

```java
@Component
public class ZakatCampaignFSM {
    private final MessageChannel campaignChannel;

    @StreamListener(CampaignChannels.DONATION_RECEIVED)
    public void onDonationReceived(DonationReceived event) {
        Campaign campaign = repository.findById(event.getCampaignId())
            .orElseThrow(() -> new CampaignNotFoundException(event.getCampaignId()));

        // Update campaign total
        campaign.addDonation(event.getAmount());

        // Check for state transitions
        if (campaign.getState() == CampaignState.FUNDRAISING
            && campaign.getTotalDonations().compareTo(campaign.getGoal()) >= 0) {

            // Transition to goal reached
            campaign.setState(CampaignState.GOAL_REACHED);

            // Publish state change event
            campaignChannel.send(MessageBuilder
                .withPayload(new CampaignGoalReached(
                    campaign.getId(),
                    campaign.getTotalDonations(),
                    campaign.getGoal()
                ))
                .build());
        }

        repository.save(campaign);
    }

    @StreamListener(CampaignChannels.DISTRIBUTION_APPROVED)
    public void onDistributionApproved(DistributionApproved event) {
        Campaign campaign = repository.findById(event.getCampaignId())
            .orElseThrow(() -> new CampaignNotFoundException(event.getCampaignId()));

        if (campaign.getState() == CampaignState.GOAL_REACHED) {
            campaign.setState(CampaignState.DISTRIBUTING);

            campaignChannel.send(MessageBuilder
                .withPayload(new CampaignDistributionStarted(
                    campaign.getId(),
                    event.getRecipients(),
                    Instant.now()
                ))
                .build());

            repository.save(campaign);
        }
    }
}
```

**Benefits:**

- Location transparency
- Temporal decoupling
- Asynchronous communication
- Message durability

## Event Sourcing for State Machines

Event sourcing stores all state changes as a sequence of events, enabling complete audit trails and time-travel capabilities.

### Event Store Pattern

**Example - Loan Application Event Store:**

```java
// Domain events
public sealed interface LoanApplicationEvent permits
    LoanApplicationCreated,
    LoanApplicationSubmitted,
    LoanApplicationMovedToUnderwriting,
    LoanApplicationApproved,
    LoanApplicationRejected {

    LoanApplicationId getAggregateId();
    Instant getTimestamp();
    int getVersion();
}

public record LoanApplicationCreated(
    LoanApplicationId aggregateId,
    BigDecimal requestedAmount,
    Duration term,
    Instant timestamp,
    int version
) implements LoanApplicationEvent {
    @Override
    public LoanApplicationId getAggregateId() { return aggregateId; }
    @Override
    public Instant getTimestamp() { return timestamp; }
    @Override
    public int getVersion() { return version; }
}

public record LoanApplicationSubmitted(
    LoanApplicationId aggregateId,
    Instant timestamp,
    int version
) implements LoanApplicationEvent {
    @Override
    public LoanApplicationId getAggregateId() { return aggregateId; }
    @Override
    public Instant getTimestamp() { return timestamp; }
    @Override
    public int getVersion() { return version; }
}

// Event-sourced aggregate
public class LoanApplication {
    private LoanApplicationId id;
    private LoanApplicationState state;
    private BigDecimal requestedAmount;
    private Duration term;
    private List<LoanApplicationEvent> changes = new ArrayList<>();
    private int version = 0;

    // Reconstruct state from events
    public static LoanApplication fromEvents(List<LoanApplicationEvent> events) {
        LoanApplication application = new LoanApplication();
        events.forEach(application::apply);
        return application;
    }

    // Apply event to current state
    private void apply(LoanApplicationEvent event) {
        switch (event) {
            case LoanApplicationCreated created -> {
                this.id = created.aggregateId();
                this.state = LoanApplicationState.DRAFT;
                this.requestedAmount = created.requestedAmount();
                this.term = created.term();
                this.version = created.version();
            }
            case LoanApplicationSubmitted submitted -> {
                if (this.state != LoanApplicationState.DRAFT) {
                    throw new IllegalStateTransitionException(
                        "Cannot submit application in state: " + this.state
                    );
                }
                this.state = LoanApplicationState.SUBMITTED;
                this.version = submitted.version();
            }
            case LoanApplicationMovedToUnderwriting underwriting -> {
                if (this.state != LoanApplicationState.SUBMITTED) {
                    throw new IllegalStateTransitionException(
                        "Cannot move to underwriting from state: " + this.state
                    );
                }
                this.state = LoanApplicationState.UNDERWRITING;
                this.version = underwriting.version();
            }
            case LoanApplicationApproved approved -> {
                if (this.state != LoanApplicationState.UNDERWRITING) {
                    throw new IllegalStateTransitionException(
                        "Cannot approve application in state: " + this.state
                    );
                }
                this.state = LoanApplicationState.APPROVED;
                this.version = approved.version();
            }
            case LoanApplicationRejected rejected -> {
                if (this.state != LoanApplicationState.UNDERWRITING) {
                    throw new IllegalStateTransitionException(
                        "Cannot reject application in state: " + this.state
                    );
                }
                this.state = LoanApplicationState.REJECTED;
                this.version = rejected.version();
            }
        }
    }

    // Command methods - create events
    public void submit() {
        LoanApplicationSubmitted event = new LoanApplicationSubmitted(
            this.id,
            Instant.now(),
            this.version + 1
        );
        apply(event);
        changes.add(event);
    }

    public void moveToUnderwriting() {
        LoanApplicationMovedToUnderwriting event = new LoanApplicationMovedToUnderwriting(
            this.id,
            Instant.now(),
            this.version + 1
        );
        apply(event);
        changes.add(event);
    }

    public List<LoanApplicationEvent> getUncommittedChanges() {
        return Collections.unmodifiableList(changes);
    }

    public void markChangesAsCommitted() {
        changes.clear();
    }
}

// Event store repository
@Repository
public class LoanApplicationEventStore {
    private final JdbcTemplate jdbcTemplate;
    private final ObjectMapper objectMapper;

    public void save(LoanApplication application) {
        List<LoanApplicationEvent> events = application.getUncommittedChanges();

        for (LoanApplicationEvent event : events) {
            String eventData = objectMapper.writeValueAsString(event);
            String eventType = event.getClass().getSimpleName();

            jdbcTemplate.update(
                """
                INSERT INTO loan_application_events
                (aggregate_id, event_type, event_data, version, timestamp)
                VALUES (?, ?, ?::jsonb, ?, ?)
                """,
                event.getAggregateId().value(),
                eventType,
                eventData,
                event.getVersion(),
                event.getTimestamp()
            );
        }

        application.markChangesAsCommitted();
    }

    public LoanApplication findById(LoanApplicationId id) {
        List<LoanApplicationEvent> events = jdbcTemplate.query(
            """
            SELECT event_type, event_data
            FROM loan_application_events
            WHERE aggregate_id = ?
            ORDER BY version ASC
            """,
            (rs, rowNum) -> {
                String eventType = rs.getString("event_type");
                String eventData = rs.getString("event_data");
                Class<? extends LoanApplicationEvent> eventClass =
                    getEventClass(eventType);
                return objectMapper.readValue(eventData, eventClass);
            },
            id.value()
        );

        if (events.isEmpty()) {
            throw new LoanApplicationNotFoundException(id);
        }

        return LoanApplication.fromEvents(events);
    }
}
```

**Benefits:**

- Complete audit trail
- Time-travel queries (reconstruct state at any point)
- Event replay for debugging
- Easy to add projections
- Natural fit for event-driven systems

### Snapshots for Performance

For aggregates with many events, snapshots improve reconstruction performance:

```java
@Repository
public class LoanApplicationEventStoreWithSnapshots {
    private static final int SNAPSHOT_FREQUENCY = 50;

    public void save(LoanApplication application) {
        // Save new events
        List<LoanApplicationEvent> events = application.getUncommittedChanges();
        saveEvents(events);

        // Create snapshot if threshold reached
        if (application.getVersion() % SNAPSHOT_FREQUENCY == 0) {
            saveSnapshot(application);
        }

        application.markChangesAsCommitted();
    }

    public LoanApplication findById(LoanApplicationId id) {
        // Try to load from latest snapshot
        Optional<LoanApplicationSnapshot> snapshot = loadLatestSnapshot(id);

        List<LoanApplicationEvent> events;
        if (snapshot.isPresent()) {
            // Load only events after snapshot
            events = loadEventsSinceVersion(id, snapshot.get().getVersion());
            LoanApplication application = snapshot.get().toAggregate();
            events.forEach(application::apply);
            return application;
        } else {
            // No snapshot - load all events
            events = loadAllEvents(id);
            return LoanApplication.fromEvents(events);
        }
    }

    private void saveSnapshot(LoanApplication application) {
        String snapshotData = objectMapper.writeValueAsString(
            LoanApplicationSnapshot.from(application)
        );

        jdbcTemplate.update(
            """
            INSERT INTO loan_application_snapshots
            (aggregate_id, snapshot_data, version, timestamp)
            VALUES (?, ?::jsonb, ?, ?)
            ON CONFLICT (aggregate_id) DO UPDATE
            SET snapshot_data = EXCLUDED.snapshot_data,
                version = EXCLUDED.version,
                timestamp = EXCLUDED.timestamp
            WHERE loan_application_snapshots.version < EXCLUDED.version
            """,
            application.getId().value(),
            snapshotData,
            application.getVersion(),
            Instant.now()
        );
    }
}
```

**Benefits:**

- Fast aggregate reconstruction
- Reduced memory usage
- Bounded replay time
- Backwards compatible (snapshots are optimization, not requirement)

## Integration with Message Brokers

### Kafka Integration

Apache Kafka provides distributed, fault-tolerant event streaming:

```java
@Configuration
public class KafkaFSMConfiguration {

    @Bean
    public NewTopic zakatApplicationTopic() {
        return TopicBuilder.name("zakat-applications")
            .partitions(10)
            .replicas(3)
            .config(TopicConfig.RETENTION_MS_CONFIG, String.valueOf(Duration.ofDays(30).toMillis()))
            .build();
    }

    @Bean
    public ProducerFactory<String, ZakatApplicationEvent> producerFactory() {
        Map<String, Object> config = new HashMap<>();
        config.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092");
        config.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class);
        config.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, JsonSerializer.class);
        config.put(ProducerConfig.ACKS_CONFIG, "all");
        config.put(ProducerConfig.ENABLE_IDEMPOTENCE_CONFIG, true);
        config.put(ProducerConfig.MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION, 1);

        return new DefaultKafkaProducerFactory<>(config);
    }
}

@Component
public class KafkaZakatApplicationFSM {
    private final KafkaTemplate<String, ZakatApplicationEvent> kafkaTemplate;

    public void submitApplication(ZakatApplicationId applicationId) {
        ZakatApplication application = repository.findById(applicationId)
            .orElseThrow(() -> new ZakatApplicationNotFoundException(applicationId));

        // Validate state transition
        if (application.getState() != ZakatApplicationState.DRAFT) {
            throw new IllegalStateTransitionException(
                "Cannot submit application in state: " + application.getState()
            );
        }

        // Perform transition
        application.setState(ZakatApplicationState.PENDING_REVIEW);
        repository.save(application);

        // Publish event to Kafka
        ZakatApplicationSubmitted event = new ZakatApplicationSubmitted(
            applicationId,
            Instant.now()
        );

        kafkaTemplate.send(
            "zakat-applications",
            applicationId.value(),  // Partition by application ID
            event
        ).addCallback(
            success -> logger.info("Published event: {}", event),
            failure -> logger.error("Failed to publish event: {}", event, failure)
        );
    }

    @KafkaListener(
        topics = "zakat-applications",
        groupId = "zakat-fsm-processor",
        concurrency = "3"
    )
    public void handleZakatApplicationEvent(
        @Payload ZakatApplicationEvent event,
        @Header(KafkaHeaders.RECEIVED_KEY) String key,
        @Header(KafkaHeaders.RECEIVED_PARTITION) int partition,
        @Header(KafkaHeaders.OFFSET) long offset
    ) {
        logger.info(
            "Processing event: {} (partition={}, offset={})",
            event.getClass().getSimpleName(),
            partition,
            offset
        );

        switch (event) {
            case ZakatApplicationSubmitted submitted ->
                handleSubmitted(submitted);
            case ZakatApplicationApproved approved ->
                handleApproved(approved);
            case ZakatApplicationRejected rejected ->
                handleRejected(rejected);
            default ->
                logger.warn("Unknown event type: {}", event.getClass());
        }
    }
}
```

**Benefits:**

- Distributed processing
- Fault tolerance
- Scalable consumers
- Event replay capability
- Exactly-once semantics

### RabbitMQ Integration

RabbitMQ provides flexible routing and reliable messaging:

```java
@Configuration
public class RabbitMQFSMConfiguration {

    // Topic exchange for state transition events
    @Bean
    public TopicExchange contractExchange() {
        return new TopicExchange("contract-events", true, false);
    }

    // Queues for different processing stages
    @Bean
    public Queue legalReviewQueue() {
        return QueueBuilder.durable("contract.legal-review")
            .withArgument("x-message-ttl", 3600000)  // 1 hour TTL
            .withArgument("x-dead-letter-exchange", "contract-dlx")
            .build();
    }

    @Bean
    public Queue shariaReviewQueue() {
        return QueueBuilder.durable("contract.sharia-review")
            .withArgument("x-message-ttl", 3600000)
            .withArgument("x-dead-letter-exchange", "contract-dlx")
            .build();
    }

    @Bean
    public Queue approvedQueue() {
        return QueueBuilder.durable("contract.approved")
            .build();
    }

    // Bindings
    @Bean
    public Binding legalReviewBinding(
        Queue legalReviewQueue,
        TopicExchange contractExchange
    ) {
        return BindingBuilder
            .bind(legalReviewQueue)
            .to(contractExchange)
            .with("contract.submitted.#");
    }

    @Bean
    public Binding shariaReviewBinding(
        Queue shariaReviewQueue,
        TopicExchange contractExchange
    ) {
        return BindingBuilder
            .bind(shariaReviewQueue)
            .to(contractExchange)
            .with("contract.legal-approved.#");
    }
}

@Component
public class RabbitMQContractFSM {
    private final RabbitTemplate rabbitTemplate;

    public void submitContract(ContractId contractId) {
        Contract contract = repository.findById(contractId)
            .orElseThrow(() -> new ContractNotFoundException(contractId));

        contract.submit();
        repository.save(contract);

        // Publish to topic exchange with routing key
        ContractSubmitted event = new ContractSubmitted(contractId, Instant.now());
        rabbitTemplate.convertAndSend(
            "contract-events",
            "contract.submitted." + contractId.value(),
            event,
            message -> {
                message.getMessageProperties().setDeliveryMode(MessageDeliveryMode.PERSISTENT);
                message.getMessageProperties().setContentType("application/json");
                return message;
            }
        );
    }

    @RabbitListener(queues = "contract.legal-review")
    public void handleLegalReview(ContractSubmitted event) {
        logger.info("Processing contract for legal review: {}", event.getContractId());

        Contract contract = repository.findById(event.getContractId())
            .orElseThrow(() -> new ContractNotFoundException(event.getContractId()));

        // Perform legal review (simulated)
        boolean approved = legalReviewService.review(contract);

        if (approved) {
            contract.approveLegal();
            repository.save(contract);

            // Route to Sharia review
            ContractLegalApproved approvedEvent = new ContractLegalApproved(
                event.getContractId(),
                Instant.now()
            );
            rabbitTemplate.convertAndSend(
                "contract-events",
                "contract.legal-approved." + event.getContractId().value(),
                approvedEvent
            );
        } else {
            contract.reject("Legal review failed");
            repository.save(contract);
        }
    }

    @RabbitListener(queues = "contract.sharia-review")
    public void handleShariaReview(ContractLegalApproved event) {
        logger.info("Processing contract for Sharia review: {}", event.getContractId());

        Contract contract = repository.findById(event.getContractId())
            .orElseThrow(() -> new ContractNotFoundException(event.getContractId()));

        boolean compliant = shariaReviewService.review(contract);

        if (compliant) {
            contract.approveSharia();
            repository.save(contract);

            // Final approval
            ContractApproved approvedEvent = new ContractApproved(
                event.getContractId(),
                Instant.now()
            );
            rabbitTemplate.convertAndSend(
                "contract-events",
                "contract.approved." + event.getContractId().value(),
                approvedEvent
            );
        } else {
            contract.reject("Sharia compliance check failed");
            repository.save(contract);
        }
    }
}
```

**Benefits:**

- Flexible routing patterns
- Dead letter queues for error handling
- Message TTL and expiration
- Priority queues
- Built-in retry mechanisms

## Asynchronous Transition Handling

### CompletableFuture Pattern

Handle asynchronous state transitions with `CompletableFuture`:

```java
@Service
public class AsyncZakatCalculationFSM {
    private final ExecutorService executorService = Executors.newFixedThreadPool(10);

    public CompletableFuture<ZakatCalculation> calculate(ZakatCalculationRequest request) {
        return CompletableFuture
            .supplyAsync(() -> {
                // Load current state
                ZakatCalculation calculation = repository.findById(request.getCalculationId())
                    .orElseThrow(() -> new ZakatCalculationNotFoundException(
                        request.getCalculationId()
                    ));

                // Validate transition
                if (calculation.getState() != ZakatCalculationState.GATHERING_DATA) {
                    throw new IllegalStateTransitionException(
                        "Cannot calculate in state: " + calculation.getState()
                    );
                }

                return calculation;
            }, executorService)
            .thenApply(calculation -> {
                // Transition to calculating
                calculation.setState(ZakatCalculationState.CALCULATING);
                repository.save(calculation);
                return calculation;
            })
            .thenCompose(calculation -> {
                // Perform expensive calculation asynchronously
                return zakatCalculationService.calculateAsync(calculation);
            })
            .thenApply(calculation -> {
                // Check if review required
                if (calculation.isComplexCase()) {
                    calculation.setState(ZakatCalculationState.REVIEW_REQUIRED);
                    eventPublisher.publish(new ZakatCalculationReviewRequired(
                        calculation.getId()
                    ));
                } else {
                    calculation.setState(ZakatCalculationState.COMPLETED);
                    eventPublisher.publish(new ZakatCalculationCompleted(
                        calculation.getId()
                    ));
                }
                return calculation;
            })
            .thenApply(repository::save)
            .exceptionally(error -> {
                logger.error("Error calculating Zakat", error);
                ZakatCalculation calculation = repository.findById(request.getCalculationId())
                    .orElse(null);
                if (calculation != null) {
                    calculation.setState(ZakatCalculationState.ERROR);
                    calculation.setError(error.getMessage());
                    repository.save(calculation);
                }
                throw new ZakatCalculationException("Calculation failed", error);
            });
    }
}
```

**Benefits:**

- Non-blocking execution
- Composable async operations
- Error propagation
- Timeout support

### Project Reactor Pattern

Use reactive streams for backpressure-aware processing:

```java
@Service
public class ReactiveCampaignFSM {
    private final ReactiveCampaignRepository repository;

    public Mono<Campaign> processDonation(DonationReceived donation) {
        return repository.findById(donation.getCampaignId())
            .switchIfEmpty(Mono.error(
                new CampaignNotFoundException(donation.getCampaignId())
            ))
            .flatMap(campaign -> {
                // Add donation
                campaign.addDonation(donation.getAmount());

                // Check for state transition
                if (campaign.getState() == CampaignState.FUNDRAISING
                    && campaign.getTotalDonations().compareTo(campaign.getGoal()) >= 0) {

                    return transitionToGoalReached(campaign);
                } else {
                    return repository.save(campaign);
                }
            })
            .doOnSuccess(campaign ->
                logger.info("Processed donation for campaign: {}", campaign.getId())
            )
            .doOnError(error ->
                logger.error("Error processing donation", error)
            );
    }

    private Mono<Campaign> transitionToGoalReached(Campaign campaign) {
        campaign.setState(CampaignState.GOAL_REACHED);

        return repository.save(campaign)
            .flatMap(saved -> {
                // Publish event reactively
                return eventPublisher.publishReactive(
                    new CampaignGoalReached(saved.getId())
                ).thenReturn(saved);
            });
    }

    public Flux<Campaign> monitorCampaigns() {
        // Stream campaign state changes
        return campaignEventStream
            .filter(event -> event instanceof CampaignStateChanged)
            .cast(CampaignStateChanged.class)
            .flatMap(event -> repository.findById(event.getCampaignId()))
            .doOnNext(campaign ->
                logger.info("Campaign {} changed to state: {}",
                    campaign.getId(),
                    campaign.getState()
                )
            );
    }
}
```

**Benefits:**

- Backpressure handling
- Stream-based processing
- Composable operators
- Efficient resource usage

## Distributed State Machines

### Saga Pattern

Sagas coordinate distributed transactions across multiple services:

```java
// Saga orchestrator
@Component
public class LoanDisbursementSaga {
    private final EventPublisher eventPublisher;

    @SagaEventHandler(associationProperty = "loanId")
    public void on(LoanApproved event) {
        // Start saga - disburse loan across multiple services
        SagaLifecycle.associateWith("loanId", event.getLoanId().value());

        // Step 1: Create loan account
        commandGateway.send(new CreateLoanAccount(
            event.getLoanId(),
            event.getApprovedAmount()
        ));
    }

    @SagaEventHandler(associationProperty = "loanId")
    public void on(LoanAccountCreated event) {
        // Step 2: Transfer funds
        commandGateway.send(new TransferFunds(
            event.getLoanId(),
            event.getAccountNumber(),
            event.getAmount()
        ));
    }

    @SagaEventHandler(associationProperty = "loanId")
    public void on(FundsTransferred event) {
        // Step 3: Notify applicant
        commandGateway.send(new NotifyLoanDisbursed(
            event.getLoanId(),
            event.getTransactionId()
        ));
    }

    @SagaEventHandler(associationProperty = "loanId")
    public void on(LoanDisbursementNotificationSent event) {
        // Saga completed successfully
        eventPublisher.publish(new LoanDisbursementCompleted(
            event.getLoanId()
        ));

        SagaLifecycle.end();
    }

    // Compensation handlers for failures
    @SagaEventHandler(associationProperty = "loanId")
    public void on(FundsTransferFailed event) {
        // Compensate: Delete loan account
        commandGateway.send(new DeleteLoanAccount(
            event.getLoanId(),
            event.getAccountNumber()
        ));
    }

    @SagaEventHandler(associationProperty = "loanId")
    public void on(LoanAccountDeleted event) {
        // Compensate: Mark loan as disbursement failed
        commandGateway.send(new MarkLoanDisbursementFailed(
            event.getLoanId(),
            "Funds transfer failed"
        ));

        SagaLifecycle.end();
    }
}
```

**Benefits:**

- Distributed transaction coordination
- Automatic compensation
- Failure recovery
- Loose coupling between services

### Process Manager Pattern

Process managers maintain state across long-running workflows:

```java
@Component
public class ContractApprovalProcessManager {
    private ContractId contractId;
    private ContractApprovalState state;
    private boolean legalApproved = false;
    private boolean shariaApproved = false;

    @EventHandler
    public void on(ContractSubmitted event) {
        this.contractId = event.getContractId();
        this.state = ContractApprovalState.LEGAL_REVIEW;

        // Initiate legal review
        commandGateway.send(new StartLegalReview(contractId));
    }

    @EventHandler
    public void on(LegalReviewCompleted event) {
        if (event.isApproved()) {
            this.legalApproved = true;
            this.state = ContractApprovalState.SHARIA_REVIEW;

            // Initiate Sharia review
            commandGateway.send(new StartShariaReview(contractId));
        } else {
            this.state = ContractApprovalState.REJECTED;
            eventPublisher.publish(new ContractRejected(
                contractId,
                "Legal review failed"
            ));
        }
    }

    @EventHandler
    public void on(ShariaReviewCompleted event) {
        if (event.isApproved()) {
            this.shariaApproved = true;

            // Both reviews approved - finalize
            if (legalApproved && shariaApproved) {
                this.state = ContractApprovalState.APPROVED;
                eventPublisher.publish(new ContractApproved(contractId));
            }
        } else {
            this.state = ContractApprovalState.REJECTED;
            eventPublisher.publish(new ContractRejected(
                contractId,
                "Sharia compliance check failed"
            ));
        }
    }
}
```

**Benefits:**

- Stateful coordination
- Complex workflow management
- Clear separation of concerns
- Testable business logic

## Error Handling Strategies

### Retry with Exponential Backoff

```java
@Component
public class ResilientZakatApplicationFSM {
    private final RetryTemplate retryTemplate;

    public ResilientZakatApplicationFSM() {
        this.retryTemplate = RetryTemplate.builder()
            .maxAttempts(5)
            .exponentialBackoff(100, 2, 10000)
            .retryOn(TransientException.class)
            .retryOn(OptimisticLockException.class)
            .traversingCauses()
            .build();
    }

    public void submitApplication(ZakatApplicationId applicationId) {
        retryTemplate.execute(context -> {
            ZakatApplication application = repository.findById(applicationId)
                .orElseThrow(() -> new ZakatApplicationNotFoundException(applicationId));

            application.submit();
            repository.save(application);

            eventPublisher.publish(new ZakatApplicationSubmitted(applicationId));

            return null;
        });
    }
}
```

### Dead Letter Queue

```java
@Component
public class DeadLetterQueueHandler {

    @RabbitListener(queues = "contract-dlq")
    public void handleFailedMessage(
        Message message,
        @Header("x-exception-message") String errorMessage,
        @Header("x-exception-stacktrace") String stackTrace
    ) {
        logger.error(
            "Message moved to DLQ. Error: {}. StackTrace: {}",
            errorMessage,
            stackTrace
        );

        // Store for manual investigation
        failedMessageRepository.save(new FailedMessage(
            message.getMessageProperties().getMessageId(),
            new String(message.getBody(), StandardCharsets.UTF_8),
            errorMessage,
            stackTrace,
            Instant.now()
        ));

        // Alert operations team
        alertService.sendAlert(
            "Message processing failed",
            "Message ID: " + message.getMessageProperties().getMessageId()
        );
    }
}
```

### Circuit Breaker

```java
@Component
public class CircuitBreakerFSM {
    private final CircuitBreakerRegistry circuitBreakerRegistry;

    public void processLoanApplication(LoanApplicationId applicationId) {
        CircuitBreaker circuitBreaker = circuitBreakerRegistry
            .circuitBreaker("loan-processing");

        Try.ofSupplier(
            CircuitBreaker.decorateSupplier(circuitBreaker, () -> {
                LoanApplication application = repository.findById(applicationId)
                    .orElseThrow(() -> new LoanApplicationNotFoundException(applicationId));

                application.process();
                repository.save(application);

                return application;
            })
        ).onFailure(error -> {
            logger.error("Circuit breaker opened for loan processing", error);
            eventPublisher.publish(new LoanProcessingFailed(applicationId, error.getMessage()));
        });
    }
}
```

## Best Practices

### Practice 1: Idempotent Event Handlers

Ensure event handlers can be safely retried:

```java
@Component
public class IdempotentEventHandler {
    private final ProcessedEventRepository processedEventRepository;

    @EventHandler
    public void handle(ZakatApplicationSubmitted event) {
        // Check if already processed
        if (processedEventRepository.existsByEventId(event.getEventId())) {
            logger.info("Event already processed: {}", event.getEventId());
            return;
        }

        try {
            // Process event
            ZakatApplication application = repository.findById(event.getApplicationId())
                .orElseThrow(() -> new ZakatApplicationNotFoundException(
                    event.getApplicationId()
                ));

            application.moveToReview();
            repository.save(application);

            // Mark as processed
            processedEventRepository.save(new ProcessedEvent(
                event.getEventId(),
                event.getClass().getSimpleName(),
                Instant.now()
            ));
        } catch (Exception e) {
            logger.error("Error processing event: {}", event.getEventId(), e);
            throw e;
        }
    }
}
```

### Practice 2: Event Versioning

Support evolving event schemas:

```java
// Version 1
public record LoanApplicationSubmittedV1(
    LoanApplicationId applicationId,
    Instant timestamp
) implements LoanApplicationEvent {
    @Override
    public int getSchemaVersion() { return 1; }
}

// Version 2 - added requester information
public record LoanApplicationSubmittedV2(
    LoanApplicationId applicationId,
    UserId requesterId,
    Instant timestamp
) implements LoanApplicationEvent {
    @Override
    public int getSchemaVersion() { return 2; }
}

// Event upcaster
@Component
public class LoanApplicationEventUpcaster {
    public LoanApplicationEvent upcast(LoanApplicationEvent event) {
        if (event instanceof LoanApplicationSubmittedV1 v1) {
            // Upcast V1 to V2
            return new LoanApplicationSubmittedV2(
                v1.applicationId(),
                UserId.system(),  // Use system user for legacy events
                v1.timestamp()
            );
        }
        return event;
    }
}
```

### Practice 3: Event Ordering

Maintain event ordering when necessary:

```java
@Configuration
public class OrderedEventProcessing {

    @Bean
    public ConcurrentKafkaListenerContainerFactory<String, LoanApplicationEvent>
        kafkaListenerContainerFactory() {

        ConcurrentKafkaListenerContainerFactory<String, LoanApplicationEvent> factory =
            new ConcurrentKafkaListenerContainerFactory<>();

        // Single consumer per partition maintains order
        factory.setConcurrency(1);

        // Process in order
        factory.getContainerProperties()
            .setAckMode(ContainerProperties.AckMode.MANUAL);

        return factory;
    }
}
```

## Related Documentation

- **FSM Fundamentals**: `ex-soen-ar-fsm__01-fundamentals-and-theory.md`
- **Implementation Patterns**: `ex-soen-ar-fsm__04-implementation-patterns-approaches.md`
- **Testing FSMs**: `ex-soen-ar-fsm__12-testing-fsm-implementations.md`
- **Framework Comparison**: `ex-soen-ar-fsm__14-framework-statecharts-temporal-cadence.md`
- **DDD Integration**: `ex-soen-ar-fsm__19-integration-with-ddd-and-architecture.md`

## Summary

Event-driven and reactive FSMs provide powerful patterns for building scalable, resilient systems:

- **Loose Coupling**: Components communicate via events, not direct calls
- **Asynchronous Processing**: Non-blocking operations improve scalability
- **Resilience**: Failure isolation and automatic recovery
- **Audit Trail**: Event sourcing provides complete history
- **Scalability**: Horizontal scaling through event partitioning
- **Flexibility**: Easy to add new event listeners

Key considerations:

1. **Message Ordering**: Determine when ordering is critical
2. **Idempotency**: Ensure handlers can be safely retried
3. **Error Handling**: Plan for failures and compensation
4. **Performance**: Balance consistency with throughput
5. **Monitoring**: Instrument event flows for observability

Event-driven FSMs excel in distributed systems, microservices architectures, and scenarios requiring high throughput and fault tolerance.

## Principles Applied

- **Simplicity Over Complexity**: Event-driven patterns simplify distributed coordination
- **Explicit Over Implicit**: Events make state transitions explicit and auditable
- **Automation Over Manual**: Frameworks handle message delivery and retry logic
- **Robustness and Reliability**: Resilience patterns ensure system availability
