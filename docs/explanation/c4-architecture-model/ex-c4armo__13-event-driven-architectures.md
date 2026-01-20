# C4 for Event-Driven Architectures

Comprehensive guide for applying C4 model to event-driven systems, including event sourcing, CQRS, saga patterns, and event streaming.

## Overview

Event-driven architectures (EDA) present unique challenges for C4 diagrams. Unlike request-response systems with clear synchronous flows, event-driven systems communicate asynchronously through events, making temporal relationships and data flow less obvious.

**Why Event-Driven Patterns Matter**:

- **Scalability**: Services can process events independently at their own pace
- **Decoupling**: Event publishers don't need to know about event consumers
- **Resilience**: Services can continue processing events even if other services are temporarily down
- **Flexibility**: New event consumers can be added without modifying publishers
- **Audit Trail**: Events naturally provide history of state changes

**Challenges for C4 Diagrams**:

- **Temporal Complexity**: Event order and timing crucial but hard to show in static diagrams
- **Indirect Relationships**: Services communicate through events, not direct calls
- **Message Broker Visibility**: Should message broker be a container or infrastructure?
- **Event vs. Command**: Different semantic meanings requiring distinct visualization
- **Eventual Consistency**: State synchronization across services not immediately obvious

This guide shows how to effectively represent event-driven patterns using C4 model diagrams.

## Event-Driven Patterns in C4 Levels

Event-driven architecture manifests differently at each C4 level. Understanding how to represent events at each zoom level ensures clear, actionable documentation.

**Level 1: System Context** - Events as System Interactions

At Context level, event-driven systems typically show external event sources and sinks:

- **External Event Publishers**: Systems that send events into your system (IoT devices, third-party webhooks, legacy systems)
- **External Event Consumers**: Systems that consume events from your system (analytics platforms, audit logs, downstream systems)
- **Event-Driven Integrations**: Asynchronous communication with external systems

**Example**: IoT platform receives device events, processes them, and publishes alerts to external monitoring systems.

**Level 2: Container** - Events as Inter-Container Communication

At Container level, show:

- **Message Broker**: Explicit container (Kafka, RabbitMQ, AWS SQS/SNS)
- **Event-Driven Services**: Services that publish and consume events
- **Event Stores**: Databases storing events (if event sourcing)
- **Read Models**: Separate databases for queries (if CQRS)

**Key Decision**: Show event broker as infrastructure or as explicit container? Recommend explicit container if broker is self-hosted (Kafka cluster), infrastructure if managed service (AWS EventBridge).

**Level 3: Component** - Events as Inter-Component Messages

At Component level, show:

- **Event Publishers**: Components that emit events
- **Event Consumers**: Components that handle events
- **Event Handlers**: Specific handler functions/classes for event types
- **Saga Orchestrators/Coordinators**: Components managing multi-step event-driven workflows

**Level 4: Code** - Events as Domain Objects

At Code level, show:

- **Event Classes**: Class diagrams showing event structure
- **Event Handlers**: Handler classes and their relationships
- **Aggregate Roots**: Domain objects that emit events (DDD pattern)

## Pattern 1: Event Sourcing

**Concept**:

Event sourcing stores state changes as a sequence of events rather than storing current state directly. Current state is derived by replaying all events.

**Key Characteristics**:

- Events are immutable - never deleted or modified
- Events stored in append-only event store
- Current state reconstructed by replaying events
- Complete audit trail of all state changes
- Time travel possible (replay events to any point)

**C4 Representation**:

### Container Diagram: Event Sourcing Architecture

```mermaid
graph TB
    User[User<br/>Person]

    subgraph "Event-Sourced System"
        API[API Service<br/>Container: Node.js/Express<br/>Command Handler]
        EventStore[(Event Store<br/>Container: EventStoreDB<br/>Append-Only Log)]
        Projector[Projection Service<br/>Container: Node.js<br/>Read Model Builder]
        ReadDB[(Read Database<br/>Container: PostgreSQL<br/>Denormalized Views)]
        QueryAPI[Query Service<br/>Container: Node.js/Express<br/>Read-Only Queries]
    end

    User -->|Submits commands<br/>HTTP POST /api/orders| API
    API -->|Appends events<br/>OrderCreated, OrderPaid<br/>Event Stream| EventStore
    EventStore -->|Streams events<br/>Real-time subscription| Projector
    Projector -->|Updates read models<br/>SQL INSERT/UPDATE| ReadDB
    User -->|Queries data<br/>HTTP GET /api/orders/:id| QueryAPI
    QueryAPI -->|Reads denormalized views<br/>SQL SELECT| ReadDB

    style User fill:#029E73,stroke:#000000,color:#ffffff
    style API fill:#0173B2,stroke:#000000,color:#ffffff
    style EventStore fill:#DE8F05,stroke:#000000,color:#ffffff
    style Projector fill:#0173B2,stroke:#000000,color:#ffffff
    style ReadDB fill:#DE8F05,stroke:#000000,color:#ffffff
    style QueryAPI fill:#0173B2,stroke:#000000,color:#ffffff
```

**Diagram Explanation**:

- **API Service**: Receives commands (create order, pay order), validates, appends events to Event Store
- **Event Store**: Append-only log of all events (OrderCreated, OrderPaid, OrderShipped), source of truth
- **Projection Service**: Consumes events from Event Store, builds denormalized read models
- **Read Database**: Optimized for queries (e.g., order history view, customer orders view)
- **Query Service**: Handles all read requests, queries Read Database only

**Write Path**: User → API → Event Store
**Read Path**: User → Query Service → Read Database

### Component Diagram: Event Sourcing Command Handler

```mermaid
graph TB
    Request[HTTP Request<br/>External]

    subgraph "API Service Container"
        Controller[Order Controller<br/>Component: Express Router]
        CommandHandler[Create Order Handler<br/>Component: Command Handler]
        Validator[Order Validator<br/>Component: Business Logic]
        AggregateRoot[Order Aggregate<br/>Component: Domain Model]
        EventPublisher[Event Publisher<br/>Component: Event Store Client]
    end

    EventStore[(Event Store<br/>External Container)]

    Request -->|POST /orders| Controller
    Controller --> CommandHandler
    CommandHandler --> Validator
    Validator --> AggregateRoot
    AggregateRoot -->|Emits OrderCreated event| EventPublisher
    EventPublisher -->|Appends event| EventStore

    style Request fill:#CC78BC,stroke:#000000,color:#ffffff
    style Controller fill:#0173B2,stroke:#000000,color:#ffffff
    style CommandHandler fill:#0173B2,stroke:#000000,color:#ffffff
    style Validator fill:#0173B2,stroke:#000000,color:#ffffff
    style AggregateRoot fill:#0173B2,stroke:#000000,color:#ffffff
    style EventPublisher fill:#0173B2,stroke:#000000,color:#ffffff
    style EventStore fill:#CC78BC,stroke:#000000,color:#ffffff
```

**Key Lessons for Event Sourcing**:

- Show Event Store as explicit container (critical infrastructure)
- Separate write path (commands → events) from read path (queries → read models)
- Label event types explicitly (OrderCreated, OrderPaid) on relationships
- Show projection/denormalization process clearly
- Indicate append-only nature of Event Store in description

## Pattern 2: CQRS (Command Query Responsibility Segregation)

**Concept**:

CQRS separates read and write models. Write side handles commands and enforces business rules. Read side provides optimized query models.

**Key Characteristics**:

- Write model optimized for business logic (normalized, transactional)
- Read model optimized for queries (denormalized, fast reads)
- Eventually consistent (write and read models synchronized asynchronously)
- Often combined with event sourcing (but can be used independently)

**C4 Representation**:

### Container Diagram: CQRS Architecture

```mermaid
graph TB
    User[User<br/>Person]

    subgraph "CQRS System"
        CommandAPI[Command API<br/>Container: Spring Boot<br/>Write Operations]
        CommandDB[(Command Database<br/>Container: PostgreSQL<br/>Normalized Schema)]

        MessageBus[Message Bus<br/>Container: Apache Kafka<br/>Topic: order-events]

        QueryAPI[Query API<br/>Container: Node.js<br/>Read Operations]
        QueryDB[(Query Database<br/>Container: MongoDB<br/>Denormalized Views)]

        Sync[Synchronizer Service<br/>Container: Node.js<br/>Read Model Updater]
    end

    User -->|Creates/updates orders<br/>HTTP POST, PUT, DELETE| CommandAPI
    CommandAPI -->|Writes normalized data<br/>SQL INSERT/UPDATE| CommandDB
    CommandAPI -->|Publishes domain events<br/>OrderCreated, OrderUpdated| MessageBus

    MessageBus -->|Consumes events<br/>Kafka Consumer Group| Sync
    Sync -->|Updates denormalized views<br/>MongoDB upsert| QueryDB

    User -->|Queries orders<br/>HTTP GET /orders| QueryAPI
    QueryAPI -->|Reads denormalized data<br/>MongoDB find| QueryDB

    style User fill:#029E73,stroke:#000000,color:#ffffff
    style CommandAPI fill:#0173B2,stroke:#000000,color:#ffffff
    style CommandDB fill:#DE8F05,stroke:#000000,color:#ffffff
    style MessageBus fill:#DE8F05,stroke:#000000,color:#ffffff
    style QueryAPI fill:#0173B2,stroke:#000000,color:#ffffff
    style QueryDB fill:#DE8F05,stroke:#000000,color:#ffffff
    style Sync fill:#0173B2,stroke:#000000,color:#ffffff
```

**Diagram Explanation**:

- **Command API**: Handles write operations (create, update, delete), enforces business rules
- **Command Database**: Normalized schema optimized for transactional writes
- **Query API**: Handles read operations (list, search, filter), no business logic
- **Query Database**: Denormalized schema optimized for fast reads
- **Synchronizer**: Keeps read model in sync with write model via events
- **Message Bus**: Decouples write side from read side, enables eventual consistency

**Write Path**: User → Command API → Command DB → Message Bus
**Read Path**: User → Query API → Query DB
**Sync Path**: Message Bus → Synchronizer → Query DB

**Key Lessons for CQRS**:

- Show write and read sides as separate containers
- Make message bus explicit (critical for synchronization)
- Label database purposes (normalized vs. denormalized)
- Indicate eventual consistency with asynchronous message flow
- Show synchronizer as dedicated service (not just read side responsibility)

## Pattern 3: Saga Pattern

**Concept**:

Saga pattern coordinates long-running transactions across multiple microservices. Since distributed transactions (2PC) are problematic, sagas use a series of local transactions with compensating actions for rollback.

**Two Approaches**:

1. **Choreography**: Services react to events, no central coordinator
2. **Orchestration**: Central orchestrator directs workflow

**C4 Representation**:

### Container Diagram: Saga Choreography (Event-Driven Coordination)

```mermaid
graph TB
    User[User<br/>Person]

    subgraph "Saga Pattern - Choreography"
        API[API Gateway<br/>Container: Kong]

        OrderSvc[Order Service<br/>Container: Node.js]
        OrderDB[(Order DB<br/>PostgreSQL)]

        PaymentSvc[Payment Service<br/>Container: Python]
        PaymentDB[(Payment DB<br/>PostgreSQL)]

        InventorySvc[Inventory Service<br/>Container: Go]
        InventoryDB[(Inventory DB<br/>MySQL)]

        MessageBus[Message Bus<br/>Container: Kafka<br/>Topics: orders, payments, inventory]
    end

    User -->|Creates order<br/>POST /orders| API
    API --> OrderSvc

    OrderSvc -->|Saves order #40;status: PENDING#41;<br/>SQL INSERT| OrderDB
    OrderSvc -->|Publishes OrderCreated event<br/>Kafka topic: orders| MessageBus

    MessageBus -->|Consumes OrderCreated<br/>Event-driven trigger| PaymentSvc
    PaymentSvc -->|Processes payment<br/>SQL INSERT| PaymentDB
    PaymentSvc -->|Publishes PaymentCompleted event<br/>Kafka topic: payments| MessageBus

    MessageBus -->|Consumes PaymentCompleted<br/>Event-driven trigger| InventorySvc
    InventorySvc -->|Reserves inventory<br/>SQL UPDATE| InventoryDB
    InventorySvc -->|Publishes InventoryReserved event<br/>Kafka topic: inventory| MessageBus

    MessageBus -->|Consumes InventoryReserved<br/>Event-driven trigger| OrderSvc
    OrderSvc -->|Updates order #40;status: CONFIRMED#41;<br/>SQL UPDATE| OrderDB

    style User fill:#029E73,stroke:#000000,color:#ffffff
    style API fill:#CC78BC,stroke:#000000,color:#ffffff
    style OrderSvc fill:#0173B2,stroke:#000000,color:#ffffff
    style OrderDB fill:#DE8F05,stroke:#000000,color:#ffffff
    style PaymentSvc fill:#0173B2,stroke:#000000,color:#ffffff
    style PaymentDB fill:#DE8F05,stroke:#000000,color:#ffffff
    style InventorySvc fill:#0173B2,stroke:#000000,color:#ffffff
    style InventoryDB fill:#DE8F05,stroke:#000000,color:#ffffff
    style MessageBus fill:#DE8F05,stroke:#000000,color:#ffffff
```

**Choreography Explanation**:

- No central coordinator - each service reacts to events
- **Flow**: OrderCreated → PaymentCompleted → InventoryReserved → Order Confirmed
- **Compensation**: If payment fails, Payment Service publishes PaymentFailed event, Order Service marks order as cancelled
- **Pros**: Loose coupling, no single point of failure
- **Cons**: Hard to understand flow, complex error handling, no global view of saga state

### Container Diagram: Saga Orchestration (Centralized Coordination)

```mermaid
graph TB
    User[User<br/>Person]

    subgraph "Saga Pattern - Orchestration"
        API[API Gateway<br/>Container: Kong]

        SagaOrchestrator[Saga Orchestrator<br/>Container: Java/Spring<br/>State Machine]
        SagaDB[(Saga State Store<br/>PostgreSQL)]

        OrderSvc[Order Service<br/>Container: Node.js]
        OrderDB[(Order DB<br/>PostgreSQL)]

        PaymentSvc[Payment Service<br/>Container: Python]
        InventorySvc[Inventory Service<br/>Container: Go]
    end

    User -->|Creates order<br/>POST /orders| API
    API -->|Starts saga| SagaOrchestrator

    SagaOrchestrator -->|Saves saga state<br/>SQL INSERT| SagaDB
    SagaOrchestrator -->|Step 1: Create order<br/>HTTP POST /orders| OrderSvc
    OrderSvc -->|SQL INSERT| OrderDB
    OrderSvc -->|Returns order ID| SagaOrchestrator

    SagaOrchestrator -->|Updates saga state<br/>SQL UPDATE| SagaDB
    SagaOrchestrator -->|Step 2: Process payment<br/>HTTP POST /payments| PaymentSvc
    PaymentSvc -->|Returns payment result| SagaOrchestrator

    SagaOrchestrator -->|Updates saga state<br/>SQL UPDATE| SagaDB
    SagaOrchestrator -->|Step 3: Reserve inventory<br/>HTTP POST /inventory/reserve| InventorySvc
    InventorySvc -->|Returns reservation result| SagaOrchestrator

    SagaOrchestrator -->|Updates saga state #40;COMPLETED#41;<br/>SQL UPDATE| SagaDB
    SagaOrchestrator -->|Returns saga result| API
    API -->|Returns order confirmation| User

    style User fill:#029E73,stroke:#000000,color:#ffffff
    style API fill:#CC78BC,stroke:#000000,color:#ffffff
    style SagaOrchestrator fill:#DE8F05,stroke:#000000,color:#ffffff
    style SagaDB fill:#DE8F05,stroke:#000000,color:#ffffff
    style OrderSvc fill:#0173B2,stroke:#000000,color:#ffffff
    style OrderDB fill:#DE8F05,stroke:#000000,color:#ffffff
    style PaymentSvc fill:#0173B2,stroke:#000000,color:#ffffff
    style InventorySvc fill:#0173B2,stroke:#000000,color:#ffffff
```

**Orchestration Explanation**:

- Central orchestrator controls workflow
- **Flow**: Orchestrator → Create Order → Process Payment → Reserve Inventory → Complete
- **Compensation**: If inventory fails, orchestrator calls compensation APIs (refund payment, cancel order)
- **Pros**: Clear workflow, easier to understand, centralized error handling
- **Cons**: Orchestrator is single point of failure, tight coupling

**Key Lessons for Saga Patterns**:

- **Choreography**: Show message bus as central hub, no single orchestrator
- **Orchestration**: Show orchestrator as explicit container with state store
- Label saga steps numerically or sequentially
- Show compensation paths (use dashed lines or different colors)
- Indicate saga state storage (persistent state crucial for recovery)

## Pattern 4: Event Streaming

**Concept**:

Event streaming processes continuous streams of events in real-time. Unlike traditional message queues (consume once), event streams are durable logs that multiple consumers can read from different positions.

**Key Characteristics**:

- Events stored durably in log (Kafka topics, AWS Kinesis)
- Multiple consumers can read same stream independently
- Event replay possible (reprocess historical events)
- Stream processing (windowing, aggregations, joins)

**C4 Representation**:

### Container Diagram: Event Streaming Architecture

```mermaid
graph TB
    IoTDevices[IoT Devices<br/>1000s of sensors]

    subgraph "Event Streaming System"
        Ingest[Ingestion Service<br/>Container: Go<br/>HTTP → Kafka]

        Kafka[Kafka Cluster<br/>Container: Apache Kafka<br/>Topics: device-telemetry, alerts]

        StreamProcessor[Stream Processor<br/>Container: Kafka Streams/Java<br/>Real-Time Analytics]

        AlertService[Alert Service<br/>Container: Python<br/>Anomaly Detection]
        AlertDB[(Alert Database<br/>PostgreSQL)]

        Dashboard[Dashboard Service<br/>Container: React + Express<br/>Real-Time UI]
        DashboardDB[(Dashboard DB<br/>TimescaleDB<br/>Time-Series Data)]
    end

    IoTDevices -->|Sends telemetry<br/>HTTP POST /events| Ingest
    Ingest -->|Publishes to Kafka<br/>Topic: device-telemetry| Kafka

    Kafka -->|Consumes stream<br/>Kafka Streams API| StreamProcessor
    StreamProcessor -->|Aggregates metrics<br/>Windowed aggregation| StreamProcessor
    StreamProcessor -->|Publishes aggregations<br/>Topic: device-metrics| Kafka

    Kafka -->|Consumes alerts<br/>Kafka Consumer| AlertService
    AlertService -->|Detects anomalies| AlertService
    AlertService -->|Stores alerts<br/>SQL INSERT| AlertDB

    Kafka -->|Consumes telemetry<br/>Kafka Consumer| Dashboard
    Dashboard -->|Stores time-series<br/>SQL INSERT| DashboardDB

    style IoTDevices fill:#029E73,stroke:#000000,color:#ffffff
    style Ingest fill:#0173B2,stroke:#000000,color:#ffffff
    style Kafka fill:#DE8F05,stroke:#000000,color:#ffffff
    style StreamProcessor fill:#0173B2,stroke:#000000,color:#ffffff
    style AlertService fill:#0173B2,stroke:#000000,color:#ffffff
    style AlertDB fill:#DE8F05,stroke:#000000,color:#ffffff
    style Dashboard fill:#0173B2,stroke:#000000,color:#ffffff
    style DashboardDB fill:#DE8F05,stroke:#000000,color:#ffffff
```

**Streaming Explanation**:

- **Ingestion Service**: Receives IoT telemetry, publishes to Kafka
- **Kafka Cluster**: Durable event log, multiple topics for different event types
- **Stream Processor**: Real-time aggregations (Kafka Streams), outputs to new Kafka topic
- **Alert Service**: Consumes stream, detects anomalies, stores alerts
- **Dashboard Service**: Consumes stream, stores time-series for visualization

**Key Streaming Concepts**:

- Multiple consumers read same stream independently
- Stream processor outputs to new stream (Kafka topic chaining)
- Time-series database for historical queries

**Key Lessons for Event Streaming**:

- Show Kafka/Kinesis as central hub with multiple topics
- Indicate stream processing (aggregations, windowing) as separate container
- Show multiple independent consumers (parallel processing)
- Label topics explicitly (device-telemetry, device-metrics, alerts)
- Use time-series database for historical queries (TimescaleDB, InfluxDB)

## Pattern 5: Event Notification vs. Event-Carried State Transfer

**Concept**:

Two fundamentally different approaches to event-driven communication:

1. **Event Notification**: Event contains minimal data (ID, event type), consumers fetch details from origin service
2. **Event-Carried State Transfer**: Event contains complete state, consumers have all data needed without additional queries

**Comparison**:

| Aspect                    | Event Notification                                                        | Event-Carried State Transfer                                                      |
| ------------------------- | ------------------------------------------------------------------------- | --------------------------------------------------------------------------------- |
| **Event Size**            | Small (IDs, event type)                                                   | Large (complete entity state)                                                     |
| **Consumer Dependencies** | High - consumers query origin service                                     | Low - consumers self-sufficient                                                   |
| **Origin Service Load**   | High - many queries from consumers                                        | Low - consumers don't query origin                                                |
| **Data Consistency**      | Always fresh (consumer fetches current state)                             | Eventually consistent (event may be stale)                                        |
| **Network Calls**         | Many (event + query per consumer)                                         | One (event only)                                                                  |
| **Origin Availability**   | Required (consumers need to query)                                        | Not required (consumers have all data)                                            |
| **Coupling**              | Tight (consumers depend on origin API)                                    | Loose (consumers independent)                                                     |
| **Use Case**              | Low-volume events, fresh data critical, origin service highly available   | High-volume events, origin service availability issues, consumer autonomy desired |
| **Example**               | `{type: "OrderCreated", orderId: "123"}` → consumer fetches order details | `{type: "OrderCreated", orderId: "123", customerId: "456", total: 99.99}`         |

**C4 Representation**:

### Event Notification Pattern

```mermaid
graph TB
    subgraph "Event Notification Pattern"
        OrderSvc[Order Service<br/>Container: Node.js]
        OrderDB[(Order DB)]

        Kafka[Message Bus<br/>Kafka]

        EmailSvc[Email Service<br/>Container: Python]
        AnalyticsSvc[Analytics Service<br/>Container: Java]
    end

    OrderSvc -->|Publishes small event<br/>#123;type: OrderCreated, orderId: 123#125;| Kafka
    OrderSvc -->|Writes full order data| OrderDB

    Kafka -->|Consumes event| EmailSvc
    EmailSvc -->|Queries order details<br/>HTTP GET /orders/123| OrderSvc
    OrderSvc -->|Returns full order| EmailSvc
    EmailSvc -->|Sends email with order details| EmailSvc

    Kafka -->|Consumes event| AnalyticsSvc
    AnalyticsSvc -->|Queries order details<br/>HTTP GET /orders/123| OrderSvc
    OrderSvc -->|Returns full order| AnalyticsSvc
    AnalyticsSvc -->|Records analytics| AnalyticsSvc

    style OrderSvc fill:#0173B2,stroke:#000000,color:#ffffff
    style OrderDB fill:#DE8F05,stroke:#000000,color:#ffffff
    style Kafka fill:#DE8F05,stroke:#000000,color:#ffffff
    style EmailSvc fill:#0173B2,stroke:#000000,color:#ffffff
    style AnalyticsSvc fill:#0173B2,stroke:#000000,color:#ffffff
```

### Event-Carried State Transfer Pattern

```mermaid
graph TB
    subgraph "Event-Carried State Transfer Pattern"
        OrderSvc2[Order Service<br/>Container: Node.js]
        OrderDB2[(Order DB)]

        Kafka2[Message Bus<br/>Kafka]

        EmailSvc2[Email Service<br/>Container: Python]
        AnalyticsSvc2[Analytics Service<br/>Container: Java]
    end

    OrderSvc2 -->|Publishes full state event<br/>#123;type: OrderCreated,<br/>orderId: 123, customerId: 456,<br/>total: 99.99, items: #91;...#93;#125;| Kafka2
    OrderSvc2 -->|Writes full order data| OrderDB2

    Kafka2 -->|Consumes event<br/>with complete data| EmailSvc2
    EmailSvc2 -->|Sends email<br/>No additional query needed| EmailSvc2

    Kafka2 -->|Consumes event<br/>with complete data| AnalyticsSvc2
    AnalyticsSvc2 -->|Records analytics<br/>No additional query needed| AnalyticsSvc2

    style OrderSvc2 fill:#0173B2,stroke:#000000,color:#ffffff
    style OrderDB2 fill:#DE8F05,stroke:#000000,color:#ffffff
    style Kafka2 fill:#DE8F05,stroke:#000000,color:#ffffff
    style EmailSvc2 fill:#0173B2,stroke:#000000,color:#ffffff
    style AnalyticsSvc2 fill:#0173B2,stroke:#000000,color:#ffffff
```

**Key Lessons**:

- **Event Notification**: Show query-back arrows from consumers to origin service
- **Event-Carried State Transfer**: No query-back arrows, consumers self-sufficient
- Label event payload explicitly to show difference
- Indicate trade-offs in diagram notes (tight coupling vs. stale data)

## Labeling Conventions for Event-Driven Diagrams

Effective labeling is critical for event-driven diagrams since relationships are less intuitive than synchronous request-response.

**Event Relationship Labels**:

**Format**: `[Publishes/Consumes] [Event Type] [Additional Context]`

**Examples**:

- `Publishes OrderCreated event via Kafka topic: orders`
- `Consumes PaymentCompleted event via Kafka Consumer Group: order-processors`
- `Streams device telemetry events via Kafka topic: device-data`
- `Subscribes to InventoryUpdated events via RabbitMQ exchange: inventory`

**Best Practices**:

1. **Use Specific Event Names**: Avoid "sends message" or "publishes event" - use actual event type (OrderCreated, PaymentFailed)
2. **Include Topic/Queue Name**: Show Kafka topic, RabbitMQ exchange, AWS SNS topic name
3. **Indicate Direction**: "Publishes" (producer), "Consumes" (consumer), "Subscribes" (active subscription)
4. **Show Consumer Groups**: For Kafka, indicate consumer group if multiple consumers compete (vs. broadcast)
5. **Distinguish Event Types**:
   - **Commands**: Imperative (CreateOrder, ProcessPayment)
   - **Events**: Past tense (OrderCreated, PaymentProcessed)
   - **Queries**: Question form (GetOrderStatus)

**Color Coding** (optional but recommended):

- **Command Messages**: Orange (`#DE8F05`)
- **Event Messages**: Teal (`#029E73`)
- **Query Messages**: Purple (`#CC78BC`)

**Example Labels**:

```text
Good Labels:
✓ "Publishes OrderCreated event via Kafka topic: orders"
✓ "Consumes PaymentFailed events via Consumer Group: order-handlers"
✓ "Streams device-telemetry events via Kafka Streams API"
✓ "Subscribes to InventoryReserved events via RabbitMQ queue: notifications"

Avoid:
✗ "sends data"
✗ "publishes event"
✗ "uses Kafka"
✗ "message"
```

## Common Mistakes (Event-Driven Specific Anti-Patterns)

### Anti-Pattern 1: Showing Message Broker as Infrastructure Only

**Problem**: Hiding message broker in infrastructure layer makes event flow invisible.

**Solution**: Show message broker as explicit container (Kafka, RabbitMQ, AWS SQS).

### Anti-Pattern 2: Not Showing Event Types

**Problem**: Generic "publishes event" relationships don't communicate what events exist.

**Solution**: Label relationships with specific event types (OrderCreated, PaymentFailed).

### Anti-Pattern 3: Mixing Synchronous and Asynchronous Without Distinction

**Problem**: HTTP calls and event publishing look identical, causing confusion.

**Solution**: Use different line styles (solid for sync, dashed for async) or explicit labels ("HTTP POST" vs. "Publishes event").

### Anti-Pattern 4: Ignoring Eventual Consistency

**Problem**: Event-driven diagrams imply immediate consistency when reality is eventual.

**Solution**: Add notes indicating "eventually consistent" or show explicit synchronization delays.

### Anti-Pattern 5: Not Showing Compensating Actions

**Problem**: Saga diagrams show happy path only, ignoring failure scenarios.

**Solution**: Show compensating actions with dashed lines or separate error flow diagram.

### Anti-Pattern 6: Overloading Single Diagram with All Event Flows

**Problem**: Showing every event type in one Container diagram creates visual clutter.

**Solution**: Create multiple focused diagrams:

- One Container diagram showing services and message broker
- Separate Dynamic diagrams for specific event flows (order creation saga, payment failure compensation)

### Anti-Pattern 7: Not Distinguishing Event Notification vs. Event-Carried State Transfer

**Problem**: Both patterns show "publishes event" but have different coupling implications.

**Solution**: Explicitly label event payload size or show query-back arrows for Event Notification pattern.

## Summary

**Key Principles for Event-Driven C4 Diagrams**:

1. **Make Message Broker Explicit**: Show Kafka, RabbitMQ, etc. as containers, not just infrastructure
2. **Label Event Types**: Use specific event names (OrderCreated, PaymentFailed), not generic "event"
3. **Show Eventual Consistency**: Indicate asynchronous flows and potential delays
4. **Separate Concerns**: Create focused diagrams (Container for structure, Dynamic for specific flows)
5. **Distinguish Patterns**: Event Sourcing, CQRS, Saga, and Streaming each have distinct visual patterns
6. **Include Compensation**: Show error handling and compensating actions in Saga patterns
7. **Use Consistent Labeling**: "Publishes [EventType] via [Topic/Queue]" format

**Progressive Disclosure for Event-Driven Systems**:

- **Context**: Show external event sources/sinks, indicate "event-driven integration"
- **Container**: Show message broker, event-driven services, event stores
- **Component**: Show event publishers, consumers, handlers, saga orchestrators
- **Dynamic**: Show specific event flows with sequence diagrams

**When to Create Multiple Diagrams**:

- **One Container Diagram**: Overall event-driven architecture (all services + message broker)
- **Multiple Dynamic Diagrams**: One per critical event flow (order creation saga, payment failure, etc.)
- **Deployment Diagram**: Show message broker infrastructure (Kafka cluster, Zookeeper, etc.)

Event-driven architectures are complex, but clear C4 diagrams make them understandable and maintainable.
