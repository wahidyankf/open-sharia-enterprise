---
title: "Questions"
date: 2025-03-16T17:43:00+07:00
draft: false
weight: 503
---

I've reorganized these 50 system design questions into a progressive learning path, starting with foundational concepts and gradually advancing to more complex specialized domains. This sequence allows you to build knowledge systematically while tackling increasingly challenging design problems, with the most specialized domains (AI/ML and blockchain) at the end.

## Level 1: Foundation - Core Concepts

These questions focus on fundamental distributed systems concepts that serve as building blocks for more complex designs.

### 1. Design a Real-Time Gaming Leaderboard

**Why It Matters**: This design introduces data structures for sorted data, high-write systems, and caching strategies without the full complexity of distributed systems.

**Clarifying Questions**:

- What is the scale in terms of concurrent users and updates?
- What is the leaderboard update frequency requirement?
- Are there multiple leaderboards (global, regional, friend-based)?
- What additional metadata needs to be displayed with rankings?
- What are the consistency requirements for score updates?

**Solution Approach**:

1. Design appropriate data structures (sorted sets in Redis or similar)
2. Implement a caching strategy for different leaderboard views
3. Design a scoring submission and validation system
4. Create an efficient pagination mechanism for leaderboard retrieval
5. Implement a strategy for handling hotkeys and write amplification

**Key Considerations**:

- **Best Practices**: Implement probabilistic data structures for high-cardinality counters; use materialized views for different leaderboard aggregations; cache pre-computed leaderboard segments
- **Anti-Patterns**: Using database queries with ORDER BY for real-time rankings; storing complete leaderboards in application memory; updating global leaderboards synchronously with every score change; neglecting race conditions in score updates

### 2. Design a Unified Observability Platform

**Why It Matters**: Understanding observability is essential for working with distributed systems. This design provides insight into how systems are monitored and debugged.

**Clarifying Questions**:

- What observability data types need to be collected (metrics, logs, traces)?
- What is the data volume for each type?
- What retention periods are required?
- What alerting capabilities are needed?
- What are the latency requirements for data ingestion and querying?

**Solution Approach**:

1. Design data collection pipelines for metrics, logs, and traces
2. Implement storage systems optimized for each data type
3. Create correlation mechanisms across different observability signals
4. Design query and visualization interfaces
5. Implement SLO monitoring and alerting systems

**Key Considerations**:

- **Best Practices**: Implement context propagation through distributed tracing; use appropriate storage systems for different signal types; design for data downsample and compression; implement intelligent sampling for high-volume data
- **Anti-Patterns**: Collecting everything without sampling strategies; implementing a single storage backend for all signal types; neglecting data correlation capabilities; building dashboards without clear use cases; creating noisy alerting systems

### 3. Design an API Gateway for 100k RPS Microservices

**Why It Matters**: API gateways introduce concepts of request routing, rate limiting, and authentication that are relevant across many distributed systems.

**Clarifying Questions**:

- What specific API gateway features are required (authentication, rate limiting, protocol translation, request routing)?
- What is the average request size and response latency expectation?
- What is the geographic distribution of clients and backend services?
- What authentication mechanisms need to be supported?
- What are regulatory requirements for request logging and monitoring?

**Solution Approach**:

1. Design a multi-tier gateway architecture with separate tiers for routing, authentication, and rate limiting
2. Implement a stateless design that can be horizontally scaled with load balancers
3. Create a caching layer for frequently accessed data and authentication tokens
4. Design circuit breaker patterns to prevent cascading failures
5. Establish monitoring and observability systems with custom metrics

**Key Considerations**:

- **Best Practices**: Implement proper circuit breaking; use token buckets for rate limiting; deploy across multiple availability zones; use caching strategically
- **Anti-Patterns**: Creating a stateful gateway that can't scale horizontally; implementing excessive request transformation logic that adds latency; using synchronous calls for all operations; treating the gateway as a business logic layer

### 4. Design a Cost-Optimized Startup Architecture

**Why It Matters**: This design teaches the fundamentals of cloud architecture with practical constraints, helping understand resource optimization and service selection.

**Clarifying Questions**:

- What is the current and projected user base?
- What are the core features and their technical requirements?
- What is the budget constraint and burn rate considerations?
- What is the expected growth trajectory?
- What technical areas are most critical to the business?

**Solution Approach**:

1. Design a serverless-first architecture for variable workloads
2. Implement cost-aware autoscaling and resource allocation
3. Create a multi-tier storage strategy based on access patterns
4. Design for managed services over self-hosted when appropriate
5. Implement comprehensive cost monitoring and optimization

**Key Considerations**:

- **Best Practices**: Use consumption-based pricing models for variable workloads; implement proper autoscaling with predictive scaling when possible; design storage tiering for cost efficiency; leverage spot instances for non-critical workloads
- **Anti-Patterns**: Over-provisioning resources for future growth; implementing complex self-managed solutions when managed services are available; designing without cost monitoring; choosing technologies primarily for resume-building rather than business needs

### 5. Design a Global Session Management System

**Why It Matters**: Session management introduces distributed state, authentication, and data locality considerations that are fundamental to web application architecture.

**Clarifying Questions**:

- What is the expected number of concurrent sessions?
- What session attributes need to be stored?
- What are the security requirements for session tokens?
- What is the geographic distribution of users?
- What is the expected session lifetime and renewal policy?

**Solution Approach**:

1. Design secure token generation and validation mechanisms
2. Implement a distributed session storage system with appropriate consistency
3. Create mechanisms for session expiration and renewal
4. Design session revocation and force-logout capabilities
5. Implement data locality optimizations for session retrieval

**Key Considerations**:

- **Best Practices**: Use JWT with appropriate signing for stateless sessions; implement proper token rotation; design for session affinity with fallback; cache session data near users
- **Anti-Patterns**: Storing sensitive data in client-side tokens; implementing long-lived sessions without refresh mechanisms; using a single centralized session store; neglecting token revocation capabilities; storing entire user profiles in session data

## Level 2: Intermediate - Scalable Data Systems

These questions focus on data storage, caching, and retrieval at scale, which are fundamental to most distributed systems.

### 6. Design a Distributed Caching System with Consistent Hashing

**Why It Matters**: Caching is critical for performance, and consistent hashing introduces key concepts in distributed data management and node membership.

**Clarifying Questions**:

- What is the expected cache size and item count?
- What are the access patterns (read/write ratio, key distribution)?
- What consistency requirements exist between cache nodes?
- What eviction policies are needed?
- What are the latency requirements for cache operations?

**Solution Approach**:

1. Design the consistent hashing ring implementation
2. Implement data partitioning and replication strategy
3. Create cache node discovery and health checking
4. Design eviction and expiration policies
5. Implement efficient client-side hashing and routing

**Key Considerations**:

- **Best Practices**: Use virtual nodes to improve distribution; implement appropriate replication factors; design for graceful handling of node additions/removals; implement circuit breakers for degraded nodes
- **Anti-Patterns**: Using naive hash functions for distribution; implementing simplistic key distribution without virtual nodes; designing without considering hot key problems; neglecting proper failure handling

### 7. Design a Real-Time Metrics System

**Why It Matters**: Time-series data systems introduce specialized storage formats, efficient aggregation, and downsampling concepts that are widely applicable.

**Clarifying Questions**:

- What is the expected metrics volume and cardinality?
- What types of queries need to be supported (aggregation, filtering, etc.)?
- What is the required data retention period?
- What alerting capabilities are needed?
- What is the acceptable query latency for different time ranges?

**Solution Approach**:

1. Design an efficient time-series data model with appropriate encoding
2. Implement scalable ingest with buffering and batching
3. Create a storage engine optimized for time-series data
4. Design query processing with rollups and aggregation
5. Implement alerting with appropriate evaluation strategies

**Key Considerations**:

- **Best Practices**: Implement downsampling for long-term storage; use efficient encoding for timestamp data; design for high-cardinality dimensions; create appropriate pre-aggregations for common queries
- **Anti-Patterns**: Using general-purpose databases without time-series optimizations; storing high-resolution data indefinitely; implementing alerting that doesn't scale with metrics volume; neglecting cardinality explosion issues

### 8. Design a Distributed Search Index

**Why It Matters**: Search functionality introduces concepts of inverted indexes, text analysis, and relevance ranking that are fundamental to information retrieval at scale.

**Clarifying Questions**:

- What is the data volume and document count?
- What search features are required (full-text, faceting, geospatial)?
- What are the latency requirements for search queries?
- What is the update frequency for the indexed documents?
- What are the language and internationalization requirements?

**Solution Approach**:

1. Design document ingestion and indexing pipelines
2. Implement appropriate text analysis and tokenization strategies
3. Design a distributed inverted index with sharding
4. Create a query parsing and execution engine
5. Implement relevance scoring and ranking algorithms

**Key Considerations**:

- **Best Practices**: Separate indexing from querying concerns; implement appropriate index replication; use bulk operations for efficiency; design for incremental indexing
- **Anti-Patterns**: Building a monolithic search service; implementing naive term frequency scoring; neglecting index refresh strategies; using synchronous indexing for all document updates; failing to consider the trade-off between indexing depth and storage requirements

### 9. Design a Globally Consistent Key-Value Store

**Why It Matters**: This design introduces consistency models, consensus algorithms, and replication strategies that are fundamental to distributed data systems.

**Clarifying Questions**:

- What consistency level is required (eventual, strong, causal)?
- What is the read/write ratio expected?
- What are the latency requirements for reads and writes?
- What is the geographic distribution of reads and writes?
- What is the average key-value size and total data volume?

**Solution Approach**:

1. Choose an appropriate consensus algorithm (Paxos, Raft) based on consistency requirements
2. Design the data replication strategy with quorum-based writes
3. Implement a partitioning scheme to distribute data across nodes
4. Create a membership protocol for node discovery and failure detection
5. Design read/write paths with appropriate consistency controls

**Key Considerations**:

- **Best Practices**: Understand CAP theorem trade-offs; implement proper conflict resolution; use logical clocks for causality tracking; design for failure as the normal case
- **Anti-Patterns**: Choosing synchronous replication when not required; over-partitioning data leading to excessive cross-partition operations; neglecting network partition handling; implementing custom consensus protocols without careful validation

### 10. Design a Hybrid Cloud Storage Solution

**Why It Matters**: Hybrid architectures introduce important concepts of data tiering, synchronization, and cost optimization across different environments.

**Clarifying Questions**:

- What data types need to be stored (structured, unstructured, etc.)?
- What are the access patterns and performance requirements?
- What compliance and data sovereignty requirements exist?
- What is the expected data volume and growth rate?
- What are the cost constraints and objectives?

**Solution Approach**:

1. Design data classification and tiering policies
2. Implement data movement and synchronization mechanisms
3. Create caching strategies for frequently accessed data
4. Design security controls for data in transit and at rest
5. Implement optimization for cross-environment network costs

**Key Considerations**:

- **Best Practices**: Implement automated tiering based on access patterns; use appropriate caching for frequently accessed data; design for data lifecycle management; implement proper encryption for both environments
- **Anti-Patterns**: Moving all data to cloud without analyzing access patterns; implementing complex replication without considering egress costs; neglecting latency implications of hybrid architectures; designing without considering data sovereignty requirements

## Level 3: Advanced - System Resilience & Scale

These questions focus on handling greater scale, ensuring reliability, and managing complex distributed systems.

### 11. Design a Sharded Database for 10B+ Records

**Why It Matters**: Database sharding presents complex challenges around data distribution, cross-shard operations, and partition management at extreme scale.

**Clarifying Questions**:

- What is the data model and schema?
- What are the primary access patterns and query types?
- What is the expected growth rate of the data?
- What are the transactions that span multiple shards?
- What are the consistency requirements for cross-shard operations?

**Solution Approach**:

1. Select appropriate sharding keys based on access patterns
2. Design a mechanism for mapping keys to shards (hash-based or range-based)
3. Implement a strategy for handling cross-shard queries and transactions
4. Create a rebalancing mechanism for handling uneven data growth
5. Design backup and recovery processes for individual shards

**Key Considerations**:

- **Best Practices**: Choose sharding keys that minimize cross-shard operations; implement proper shard management and discovery service; design for incremental scaling; use consistent hashing for rebalancing
- **Anti-Patterns**: Using auto-incrementing IDs as sharding keys; implementing complex distributed transactions across many shards; designing without a rebalancing strategy; neglecting monitoring for hot shards

### 12. Design a Multi-Region Kubernetes Deployment

**Why It Matters**: Multi-region deployments introduce complex networking, service discovery, and data consistency challenges across geographical boundaries.

**Clarifying Questions**:

- How many regions need to be supported?
- What are the data residency requirements?
- What is the failover strategy between regions?
- What are the networking requirements between regions?
- What deployment model is required (active-active or active-passive)?

**Solution Approach**:

1. Design a global control plane architecture
2. Implement cross-region networking and service discovery
3. Create a multi-region data synchronization strategy
4. Design deployment pipelines for coordinated releases
5. Implement health monitoring and automated failover

**Key Considerations**:

- **Best Practices**: Use a service mesh for cross-cluster communication; implement cluster federation; design for regional independence during network partitions; use GitOps for declarative multi-cluster management
- **Anti-Patterns**: Tight coupling between regional clusters; implementing a single global etcd cluster; neglecting latency between control planes; synchronous cross-region calls in critical paths; assuming consistent network performance between regions

### 13. Design a Collaborative Document Editing System

**Why It Matters**: Real-time collaboration introduces complex concurrency challenges, conflict resolution strategies, and eventual consistency models.

**Clarifying Questions**:

- What is the maximum number of concurrent editors per document?
- What are the latency requirements for update propagation?
- What conflict resolution approach is preferred?
- What is the expected document size and complexity?
- What offline editing capabilities are required?

**Solution Approach**:

1. Design a data model using operational transforms or CRDTs
2. Implement a real-time synchronization protocol
3. Create a version control and history tracking system
4. Design conflict detection and resolution mechanisms
5. Implement permissions and access control

**Key Considerations**:

- **Best Practices**: Use operational transforms or CRDTs for conflict-free merging; implement optimistic concurrency control; design for offline-first operation with eventual consistency; use websockets for real-time updates
- **Anti-Patterns**: Relying on locks for concurrency control; implementing last-write-wins without conflict detection; sending entire documents with each update; neglecting idempotency in update operations

### 14. Design a Multi-Cloud Disaster Recovery Solution

**Why It Matters**: Disaster recovery across clouds combines challenges of data replication, networking, and automated failover with cross-provider compatibility concerns.

**Clarifying Questions**:

- What are the Recovery Time Objective (RTO) and Recovery Point Objective (RPO) requirements?
- Which cloud providers need to be supported?
- What is the size and nature of the data that needs to be replicated?
- What is the failover testing strategy?
- What are the cost constraints for the DR solution?

**Solution Approach**:

1. Design a cloud-agnostic infrastructure layer with appropriate abstraction
2. Implement data replication strategies based on RPO requirements
3. Create automated failover and failback procedures
4. Design networking for cross-cloud connectivity
5. Implement comprehensive DR testing and validation

**Key Considerations**:

- **Best Practices**: Use infrastructure as code for all environments; implement automated testing of DR procedures; design for loose coupling from provider-specific services when appropriate; consider cost-efficiency through tiered recovery strategies
- **Anti-Patterns**: Manual failover processes; implementing untested recovery plans; tight coupling to provider-specific services; designing without considering data sovereignty requirements; neglecting regular validation of recovery procedures

### 15. Design a Columnar Data Warehouse

**Why It Matters**: Analytical data warehouses introduce specialized storage formats, query optimization, and data organization principles for complex analytical workloads.

**Clarifying Questions**:

- What is the expected data volume and growth rate?
- What are the typical query patterns and their complexity?
- What are the data loading frequency requirements?
- What are the latency expectations for queries?
- What are the data retention and archiving requirements?

**Solution Approach**:

1. Design the columnar storage format and compression strategies
2. Implement parallel processing architecture for query execution
3. Create optimized indexing strategies for common access patterns
4. Design the data loading and transformation pipeline
5. Implement query optimization and execution planning

**Key Considerations**:

- **Best Practices**: Use appropriate compression techniques for different data types; implement zone maps for data skipping; design storage for predicate pushdown; implement partitioning strategies aligned with query patterns
- **Anti-Patterns**: Using row-oriented storage for analytical workloads; implementing OLTP-style normalized schemas; designing without considering column pruning; neglecting statistics for query optimization

### 16. Design a Chaos Engineering Framework

**Why It Matters**: Chaos engineering demonstrates advanced resilience thinking, proactive failure testing, and controlled experiment design for improving system reliability.

**Clarifying Questions**:

- What types of failures need to be simulated?
- What is the infrastructure environment (cloud, on-prem, hybrid)?
- What safety mechanisms are required to prevent customer impact?
- What metrics need to be collected during experiments?
- What is the approval process for experiment execution?

**Solution Approach**:

1. Design a fault injection mechanism for different failure types
2. Implement experiment configuration and safety guardrails
3. Create monitoring and observability for experiment impact
4. Design rollback mechanisms for experiment termination
5. Implement a workflow for experiment proposal and approval

**Key Considerations**:

- **Best Practices**: Start with small blast radius experiments; implement proper abort conditions; design for clear hypothesis testing; establish baseline metrics before experiments; create proper isolation between production and experiment management
- **Anti-Patterns**: Running experiments without sufficient monitoring; implementing without automatic rollback capabilities; designing overly complex initial experiments; conducting chaos tests on systems without basic resilience

### 17. Design a Monolith-to-Microservices Migration

**Why It Matters**: This migration pattern introduces challenges of incremental decomposition, data ownership transitions, and maintaining business continuity during architectural evolution.

**Clarifying Questions**:

- What is the size and complexity of the existing monolith?
- What are the primary pain points with the current architecture?
- What is the timeline for migration?
- What is the team structure and ownership model?
- What dependencies exist within the monolith?

**Solution Approach**:

1. Analyze the monolith to identify bounded contexts and service boundaries
2. Implement the strangler pattern with appropriate façade layer
3. Design a data migration strategy with dual-write patterns when needed
4. Create a CI/CD pipeline supporting both monolith and microservices
5. Implement monitoring and observability for the transitional architecture

**Key Considerations**:

- **Best Practices**: Extract services along bounded contexts; implement a façade pattern for gradual migration; use feature toggles to control migration; migrate data carefully with appropriate consistency mechanisms; start with less critical components
- **Anti-Patterns**: Big-bang rewrites; creating distributed monoliths with tight coupling; implementing synchronous chains of service calls; neglecting operational readiness for microservices management; migrating without clear ownership models

### 18. Design a Serverless IoT Workflow Engine

**Why It Matters**: IoT with serverless combines event-driven architecture, state management in stateless environments, and handling massive scale with cost efficiency.

**Clarifying Questions**:

- What is the expected event volume and pattern?
- What types of workflows need to be supported?
- What is the maximum workflow execution duration?
- What state persistence requirements exist?
- What are the integration points with external systems?

**Solution Approach**:

1. Design an event ingestion system with appropriate buffering
2. Implement a workflow definition language and parser
3. Create a state management system for long-running processes
4. Design workflow execution and monitoring components
5. Implement strategies for handling cold starts and scaling

**Key Considerations**:

- **Best Practices**: Use event sourcing for workflow state; implement checkpointing for long-running workflows; design for idempotent execution; use appropriate event buffers for traffic shaping
- **Anti-Patterns**: Relying on in-memory state for workflow execution; neglecting retry and error handling strategies; implementing synchronous external calls in workflow steps; designing without considering cold start latency

## Level 4: Financial Systems

These questions focus on financial technology systems that have extreme reliability, consistency, and regulatory requirements.

### 19. Design a Core Banking System with 24/7 Availability

**Why It Matters**: Core banking introduces the extreme reliability, consistency, and regulatory requirements unique to financial systems.

**Clarifying Questions**:

- What banking products need to be supported (accounts, loans, cards)?
- What transaction volume and peak load are expected?
- What regulatory requirements must be met?
- What integration points exist with external systems?
- What are the batch processing requirements?

**Solution Approach**:

1. Design a modular architecture with bounded contexts for different banking functions
2. Implement a transaction processing engine with appropriate isolation levels
3. Create a dual active-active deployment for continuous availability
4. Design batch processing that can run alongside online transactions
5. Implement comprehensive audit logging and reconciliation

**Key Considerations**:

- **Best Practices**: Design for double-entry accounting; implement proper transaction isolation; use event sourcing for audit trails; design failover mechanisms without data loss
- **Anti-Patterns**: Implementing maintenance windows in a 24/7 system; using single-region deployment; relying on database locks for concurrency; implementing tight coupling between modules; designing without considering regulatory reporting requirements

### 20. Design a Real-Time Payment Processing System (Zelle/Venmo)

**Why It Matters**: Payment systems combine immediate user experience requirements with eventual settlement guarantees and fraud detection challenges.

**Clarifying Questions**:

- What is the expected payment volume and transaction size distribution?
- What payment methods need to be supported?
- What fraud detection requirements exist?
- What are the compliance and regulatory requirements?
- What are the SLAs for payment processing and settlement?

**Solution Approach**:

1. Design an idempotent transaction processing pipeline
2. Implement a multi-stage payment lifecycle with appropriate state transitions
3. Create a real-time fraud detection system with configurable rules
4. Design a reconciliation and settlement engine
5. Implement comprehensive transaction monitoring and alerting

**Key Considerations**:

- **Best Practices**: Implement transaction idempotency with deduplication; design for optimistic user experience with risk management; use appropriate data partitioning for scalability; implement regulatory reporting capabilities
- **Anti-Patterns**: Processing payments synchronously end-to-end; implementing simplistic fraud detection; neglecting compliance requirements; using a monolithic architecture without separation of concerns

### 21. Design a Fraud Detection System for a Payment Processor

**Why It Matters**: Fraud detection introduces real-time pattern analysis, machine learning integration, and balancing false positives against business impact.

**Clarifying Questions**:

- What types of fraud need to be detected?
- What is the transaction volume that needs to be analyzed?
- What is the acceptable false positive rate?
- What is the latency requirement for fraud decisions?
- What manual review capabilities are needed for flagged transactions?

**Solution Approach**:

1. Design real-time transaction analysis pipelines
2. Implement rule-based and machine learning detection models
3. Create a risk scoring system with appropriate thresholds
4. Design a case management system for manual review
5. Implement feedback loops for continuous improvement

**Key Considerations**:

- **Best Practices**: Use a multi-layer approach combining rules and ML; implement appropriate feature engineering; design for model monitoring and drift detection; create clear escalation paths for high-risk transactions
- **Anti-Patterns**: Relying solely on static rules; implementing models without explainability; designing with excessive friction for legitimate transactions; neglecting feedback loops from manual reviews

### 22. Design a Digital Banking Platform with Microservices

**Why It Matters**: Digital banking platforms must deliver seamless customer experiences while integrating with legacy systems and addressing security concerns.

**Clarifying Questions**:

- What banking features need to be supported?
- What is the integration approach with the core banking system?
- What channels need to be supported (web, mobile, API)?
- What are the performance and scalability requirements?
- What security and compliance requirements exist?

**Solution Approach**:

1. Design domain-driven microservices architecture
2. Implement an integration layer with the core banking system
3. Create a secure authentication and authorization system
4. Design mobile-first API interfaces with appropriate versioning
5. Implement real-time notification and messaging capabilities

**Key Considerations**:

- **Best Practices**: Design clear domain boundaries between services; implement proper API contracts; use event-driven architecture for real-time features; implement circuit breakers for core banking integration
- **Anti-Patterns**: Creating a distributed monolith with tight coupling; implementing synchronous chains of service calls; neglecting performance for mobile experiences; designing without considering offline capabilities

### 23. Design a Financial Data Warehouse

**Why It Matters**: Financial data warehouses combine analytical performance requirements with strict regulatory compliance and reconciliation needs.

**Clarifying Questions**:

- What financial data domains need to be supported?
- What are the regulatory reporting requirements?
- What is the expected data volume and retention period?
- What types of analytics need to be performed?
- What are the data quality and reconciliation requirements?

**Solution Approach**:

1. Design the data model with appropriate financial domains
2. Implement data ingestion with validation and enrichment
3. Create a reconciliation framework for data integrity
4. Design regulatory reporting and compliance capabilities
5. Implement historical analysis with appropriate partitioning

**Key Considerations**:

- **Best Practices**: Implement proper data lineage tracking; design for point-in-time analysis; create appropriate data quality checks; implement SOX-compliant controls and audit trails
- **Anti-Patterns**: Using simplistic data models without considering financial complexities; implementing without data governance; designing without considering historical analysis requirements; neglecting regulatory reporting needs

### 24. Design a High-Frequency Trading Platform

**Why It Matters**: HFT systems represent the extreme end of low-latency architecture, where microseconds matter and reliability must be achieved without sacrificing performance.

**Clarifying Questions**:

- What asset classes need to be traded?
- What is the expected trading frequency and volume?
- What market data feeds need to be processed?
- What are the latency requirements for the trading path?
- What risk controls need to be implemented?

**Solution Approach**:

1. Design an ultra-low latency market data processing pipeline
2. Implement order management with minimal latency
3. Create pre-trade risk controls that don't impact critical path
4. Design specialized hardware integration (FPGA, custom network)
5. Implement comprehensive monitoring and circuit breakers

**Key Considerations**:

- **Best Practices**: Use kernel bypass networking; implement lock-free algorithms; design for predictable performance; use appropriate hardware acceleration; separate critical and non-critical paths
- **Anti-Patterns**: Using garbage-collected languages in the critical path; implementing synchronous logging; neglecting proper pre-trade risk checks; designing without considering market circuit breakers and volatility interruptions

## Level 5: Real-World Case Studies

These questions focus on analyzing and designing real-world large-scale systems with complex requirements.

### 25. Design Twitter's Timeline Service

**Why It Matters**: Twitter's timeline must combine real-time content delivery with personalization at massive scale, solving the complex "fan-out" problem.

**Clarifying Questions**:

- What is the scale in terms of users and tweets?
- What is the expected write to read ratio?
- What timeline types need to be supported (home, profile, search)?
- What is the required timeline freshness?
- What personalization features are needed?

**Solution Approach**:

1. Design data models for users, tweets, and relationships
2. Implement both push and pull models for timeline generation
3. Create caching strategies for different timeline types
4. Design real-time update mechanisms for live content
5. Implement ranking and personalization algorithms

**Key Considerations**:

- **Best Practices**: Use hybrid fan-out approaches for different user types; implement appropriate caching strategies; design for real-time updates; create efficient storage for timeline precomputation
- **Anti-Patterns**: Using pure push models for users with millions of followers; implementing timeline generation without caching; designing without considering the "thundering herd" problem; neglecting eventual consistency in distributed timelines

### 26. Design Uber's Ride Matching System

**Why It Matters**: Uber's matching system combines geospatial challenges, real-time constraints, and sophisticated pricing models to efficiently connect riders and drivers.

**Clarifying Questions**:

- What is the geographic scope and density of the service?
- What is the expected request volume during peak times?
- What matching factors need to be considered (distance, ETA, driver rating)?
- What is the latency requirement for matching?
- What surge pricing requirements exist?

**Solution Approach**:

1. Design geospatial indexing for efficient nearby driver search
2. Implement the matching algorithm with appropriate constraints
3. Create the surge pricing model based on supply and demand
4. Design real-time ETA calculation and route optimization
5. Implement driver and rider state management

**Key Considerations**:

- **Best Practices**: Use appropriate spatial indexing (quadtrees, S2 cells); implement efficient driver location updating; design for fare estimation accuracy; create proper supply-demand balancing mechanisms
- **Anti-Patterns**: Using simplistic distance-based matching without ETA consideration; implementing centralized dispatch for global operations; designing without considering driver cancellation behavior; neglecting operational differences between markets

### 27. Design Netflix's Streaming Architecture

**Why It Matters**: Netflix represents the pinnacle of video streaming at scale, with unique approaches to content delivery, encoding, and personalization.

**Clarifying Questions**:

- What is the expected scale in terms of concurrent viewers?
- What video quality levels need to be supported?
- What are the content protection requirements?
- What is the geographic distribution of users?
- What devices and platforms need to be supported?

**Solution Approach**:

1. Design the content preparation and encoding pipeline
2. Implement a global content delivery strategy with CDN
3. Create adaptive bitrate streaming with client-side logic
4. Design DRM implementation for content protection
5. Implement client device playback optimization

**Key Considerations**:

- **Best Practices**: Implement multi-CDN strategy with intelligent routing; use appropriate encoding ladders for different content types; design for device-specific optimizations; create robust client-side error recovery
- **Anti-Patterns**: Using a single encoding approach for all content; implementing a single CDN provider; designing without considering ISP peering relationships; neglecting offline viewing capabilities

### 28. Design Instagram's Photo Sharing System

**Why It Matters**: Instagram must handle massive media ingestion, processing, and delivery while maintaining a real-time social graph and engagement features.

**Clarifying Questions**:

- What is the expected scale for uploads and views?
- What media types need to be supported (photos, videos, stories)?
- What image processing requirements exist?
- What is the expected media storage growth?
- What is the social graph scale and connectedness?

**Solution Approach**:

1. Design media upload and processing pipelines
2. Implement storage and CDN strategy for efficient delivery
3. Create the social graph data model and access patterns
4. Design feed generation and notification systems
5. Implement content discovery algorithms

**Key Considerations**:

- **Best Practices**: Generate multiple resolutions for different use cases; implement appropriate image compression techniques; design for efficient feed generation; create proper caching strategies for popular content
- **Anti-Patterns**: Processing images synchronously during upload; implementing a single storage tier for all media; designing feeds without pagination; neglecting privacy controls in content distribution

### 29. Design a Stock Trading Platform

**Why It Matters**: Trading platforms must combine low latency with reliability while meeting strict regulatory requirements and preventing market manipulation.

**Clarifying Questions**:

- What types of securities need to be supported?
- What order types need to be implemented?
- What is the expected trading volume and frequency?
- What regulatory requirements must be met?
- What risk management controls are needed?

**Solution Approach**:

1. Design the order management system with appropriate validation
2. Implement the matching engine with price-time priority
3. Create market data distribution with minimal latency
4. Design risk controls and circuit breakers
5. Implement compliance reporting and audit trails

**Key Considerations**:

- **Best Practices**: Use memory-mapped files for persistence; implement proper order queue management; design for comprehensive audit logging; create proper risk controls and circuit breakers
- **Anti-Patterns**: Using databases with high latency for order storage; implementing simplistic matching without considering edge cases; designing without considering regulatory reporting; neglecting monitoring for market manipulation

### 30. Design an IoT Platform (1M events/sec)

**Why It Matters**: IoT platforms must handle massive volumes of telemetry data from heterogeneous devices while providing real-time insights and managing complex device lifecycles.

**Clarifying Questions**:

- What types of devices need to be supported?
- What is the expected data volume and velocity?
- What protocol support is required (MQTT, CoAP, HTTP)?
- What are the data processing and analytics requirements?
- What device management capabilities are needed?

**Solution Approach**:

1. Design the device registry and identity management system
2. Implement telemetry ingestion with appropriate protocol support
3. Create data processing pipelines for different use cases
4. Design the device management and provisioning system
5. Implement analytics and visualization capabilities

**Key Considerations**:

- **Best Practices**: Use appropriate protocols for different device types; implement efficient telemetry ingestion with batching; design for device heterogeneity; create proper device security and authentication
- **Anti-Patterns**: Using a single communication protocol for all devices; implementing synchronous processing for telemetry; designing without considering edge processing capabilities; neglecting device lifecycle management

### 31. Design a Global CDN for 4K Video

**Why It Matters**: 4K video delivery at global scale presents extreme bandwidth challenges that require sophisticated edge caching, request routing, and content preparation.

**Clarifying Questions**:

- What is the expected content volume and user base?
- What video formats and protocols need to be supported?
- What is the geographic distribution of users?
- What are the requirements for live vs. on-demand content?
- What edge computing capabilities are needed?

**Solution Approach**:

1. Design the global edge network architecture
2. Implement content ingestion and distribution pipelines
3. Create intelligent request routing with performance optimization
4. Design caching strategies for different content types
5. Implement edge computing for personalization and processing

**Key Considerations**:

- **Best Practices**: Implement content prepositioning for popular assets; use appropriate cache hierarchies; design for efficient cache invalidation; create proper origin shielding mechanisms
- **Anti-Patterns**: Using a single caching strategy for all content types; implementing request routing without considering network conditions; designing without considering ISP relationships; neglecting recovery mechanisms for cache misses

### 32. Design a Distributed Tracing System

**Why It Matters**: Distributed tracing enables observability across complex microservice architectures, requiring sophisticated sampling, correlation, and visualization capabilities.

**Clarifying Questions**:

- How many services and tiers need to be traced?
- What is the expected trace volume?
- What sampling strategy is appropriate?
- What visualization and analysis requirements exist?
- What integration requirements exist with other observability signals?

**Solution Approach**:

1. Design the trace data model with span relationships
2. Implement context propagation across service boundaries
3. Create sampling strategies for high-volume services
4. Design storage and indexing for efficient trace retrieval
5. Implement visualization and analysis interfaces

**Key Considerations**:

- **Best Practices**: Use standard instrumentation libraries when possible; implement appropriate sampling strategies; design for context propagation across different protocols; create correlation with logs and metrics
- **Anti-Patterns**: Sampling without considering critical paths; implementing proprietary context propagation; designing storage without considering cardinality explosion; collecting everything without clear use cases

## Level 6: AI/ML Systems

These questions focus on the unique challenges of building systems that incorporate artificial intelligence and machine learning.

### 33. Design a Feature Store for 1000+ Features

**Why It Matters**: Feature stores solve critical ML pipeline challenges around feature sharing, consistency, and computation efficiency.

**Clarifying Questions**:

- What types of features need to be supported (real-time, batch, etc.)?
- What are the sources of feature data?
- What is the feature update frequency and access patterns?
- What are the consistency requirements between training and serving?
- What transformation capabilities are needed in the feature pipeline?

**Solution Approach**:

1. Design the feature data model and storage architecture
2. Implement batch and real-time feature computation pipelines
3. Create a metadata management system for feature discovery
4. Design point-in-time correct retrieval for training datasets
5. Implement feature serving with appropriate caching

**Key Considerations**:

- **Best Practices**: Separate offline and online storage with sync mechanisms; implement proper feature versioning; design for point-in-time correctness; create appropriate feature groups based on domain knowledge
- **Anti-Patterns**: Rebuilding features for every model; implementing without feature discovery capabilities; neglecting consistency between training and serving; designing without considering feature lineage and provenance

### 34. Design an ML Monitoring System with Drift Detection

**Why It Matters**: ML models degrade over time as data distributions change, requiring sophisticated monitoring to detect performance issues.

**Clarifying Questions**:

- What types of models need to be monitored?
- What input and output features need to be tracked?
- What statistical methods are appropriate for drift detection?
- What is the expected prediction volume?
- What alerting and retraining triggers are required?

**Solution Approach**:

1. Design data collection for model inputs, outputs, and ground truth
2. Implement statistical tests for data drift and concept drift
3. Create performance monitoring for model metrics
4. Design visualization and dashboarding for monitoring insights
5. Implement automated alerting and retraining triggers

**Key Considerations**:

- **Best Practices**: Monitor both model inputs and outputs; implement appropriate statistical distance measures; design for incremental model updates; create clear thresholds for alerts and retraining
- **Anti-Patterns**: Monitoring only aggregate performance; implementing simplistic threshold-based detection; neglecting ground truth collection; designing without considering model versioning and rollback capabilities

### 35. Design a Recommendation System

**Why It Matters**: Recommendation engines directly drive business outcomes through personalization, requiring sophisticated approaches to feature engineering and ranking.

**Clarifying Questions**:

- What types of items are being recommended?
- What is the user base size and activity level?
- What data signals are available for personalization?
- What is the required freshness of recommendations?
- What business metrics should the recommendations optimize for?

**Solution Approach**:

1. Design feature engineering pipelines for users and items
2. Implement candidate generation with appropriate approaches
3. Create ranking models with multi-objective optimization
4. Design online experiment infrastructure for A/B testing
5. Implement feedback loops for continuous improvement

**Key Considerations**:

- **Best Practices**: Use a multi-stage architecture (retrieval, ranking, re-ranking); implement appropriate diversity measures; design for both explicit and implicit feedback; create proper cold-start strategies for new users and items
- **Anti-Patterns**: Using a single model for both retrieval and ranking; implementing without considering popularity bias; neglecting diversity in recommendations; designing without clear business metrics alignment

### 36. Design an AI Content Moderation System

**Why It Matters**: Content moderation at scale requires sophisticated AI capabilities while addressing evolving harmful content.

**Clarifying Questions**:

- What content types need to be moderated (text, images, video, etc.)?
- What categories of prohibited content need to be detected?
- What is the required moderation latency?
- What is the acceptable false positive/negative rate?
- What human review capabilities are needed?

**Solution Approach**:

1. Design content ingestion and preprocessing for different types
2. Implement multi-stage filtering with increasing sophistication
3. Create confidence scoring and human review routing
4. Design feedback loops from human reviewers to models
5. Implement continuous monitoring for emerging content patterns

**Key Considerations**:

- **Best Practices**: Use a tiered approach with multiple models; implement appropriate sampling for human review; design for regional and cultural variations; create clear policy implementation guidelines
- **Anti-Patterns**: Relying solely on automated systems without human review; implementing binary classification without confidence scoring; neglecting emerging content trends; designing without considering explainability for moderation decisions

### 37. Design an LLM Serving Infrastructure

**Why It Matters**: Large Language Models require specialized infrastructure for efficient inference at scale, with unique challenges around latency and resource utilization.

**Clarifying Questions**:

- What model sizes need to be served?
- What is the expected request volume and pattern?
- What are the latency requirements for different use cases?
- What is the token budget per request and response?
- What hardware is available for inference (GPUs, TPUs, etc.)?

**Solution Approach**:

1. Design efficient model loading and quantization strategies
2. Implement model parallelism and tensor parallelism as needed
3. Create batching mechanisms with dynamic batch sizing
4. Design caching for common prompts and responses
5. Implement request routing and load balancing across inference nodes

**Key Considerations**:

- **Best Practices**: Use model quantization appropriately; implement KV cache for efficient inference; design request queue management with appropriate priorities; implement speculative execution when applicable
- **Anti-Patterns**: Loading full precision models unnecessarily; serving all models on the same infrastructure regardless of size; implementing synchronous chained calls for complex workflows; neglecting memory management for concurrent requests

### 38. Design an Enterprise RAG System

**Why It Matters**: Retrieval-Augmented Generation systems enable LLMs to access enterprise knowledge, requiring careful consideration of document processing and retrieval relevance.

**Clarifying Questions**:

- What document types and sources need to be supported?
- What is the expected document volume and update frequency?
- What security and access control requirements exist?
- What is the expected query volume and complexity?
- What relevance and accuracy requirements exist?

**Solution Approach**:

1. Design document ingestion and preprocessing pipelines
2. Implement chunk extraction and embedding generation
3. Create vector storage with appropriate indexing for search
4. Design retrieval logic with relevance ranking
5. Implement prompt engineering for effective LLM augmentation

**Key Considerations**:

- **Best Practices**: Use appropriate chunking strategies for different document types; implement proper metadata filtering; design for attribution and citation; create feedback mechanisms for relevance improvement
- **Anti-Patterns**: Using fixed chunk sizes for all document types; implementing simplistic similarity measures; neglecting security filtering in retrieval; designing without considering retrieval diversity

## Level 7: Blockchain Systems

These questions focus on the specialized challenges of distributed ledger technologies and cryptocurrency systems.

### 39. Design a Cryptocurrency Exchange

**Why It Matters**: Crypto exchanges combine traditional trading systems with blockchain-specific security challenges and extreme market volatility.

**Clarifying Questions**:

- What cryptocurrencies need to be supported?
- What is the expected trading volume and user base?
- What security requirements exist for wallet management?
- What regulatory compliance requirements exist?
- What types of orders need to be supported?

**Solution Approach**:

1. Design secure wallet architecture with hot/warm/cold storage
2. Implement an order matching engine with priority queue
3. Create blockchain integration for deposits and withdrawals
4. Design a market data system for price discovery
5. Implement security controls for user and admin actions

**Key Considerations**:

- **Best Practices**: Implement multi-signature wallets; design for deterministic order matching; use appropriate segregation of duties; implement comprehensive transaction monitoring
- **Anti-Patterns**: Storing a majority of funds in hot wallets; implementing a single wallet per currency; designing without considering market manipulation detection; neglecting security for administrative functions

### 40. Design a Blockchain-based Trade Finance Platform

**Why It Matters**: Trade finance on blockchain combines complex business workflows with distributed ledger technology, requiring careful consideration of privacy, consensus, and integration.

**Clarifying Questions**:

- What trade finance instruments need to be supported?
- Who are the participants and what are their roles?
- What blockchain technology will be used (public, private, permissioned)?
- What are the privacy requirements between participants?
- What integration requirements exist with traditional systems?

**Solution Approach**:

1. Design the data model for trade finance instruments
2. Implement smart contracts for business logic automation
3. Create privacy mechanisms for sensitive transaction data
4. Design a consensus mechanism appropriate for the use case
5. Implement integration with traditional banking systems

**Key Considerations**:

- **Best Practices**: Use privacy-preserving techniques for sensitive data; implement appropriate consensus for business requirements; design for regulatory compliance and auditability; create clear participant onboarding processes
- **Anti-Patterns**: Putting sensitive data directly on-chain; implementing complex business logic in smart contracts; neglecting off-chain storage for documents; designing without considering legal enforceability of smart contracts

## How to Use This Learning Path

This progressive system design question sequence is designed to build your knowledge systematically. Here's how to make the most of it:

1. **Master the fundamentals first**: The early questions establish core concepts that appear repeatedly in more complex designs. Ensure you thoroughly understand topics like caching, data partitioning, and consistency models before advancing.

2. **Focus on patterns across questions**: Notice how similar patterns (like sharding, replication, caching) appear across different domains but with unique adaptations. Understanding these patterns makes tackling new problems much easier.

3. **Document your learning**: For each question, create a reference document with:
   - Key architectural diagrams
   - Trade-offs you considered
   - Best practices specific to that domain
   - How you would approach the clarifying questions

4. **Practice estimation**: Most system design questions require quantitative reasoning. Practice calculating required storage, throughput, network capacity, and instance counts.

5. **Build depth in common domains first**: Focus on mastering the core distributed systems concepts and financial systems before moving to specialized domains like AI/ML and blockchain.

6. **Understand tech lifecycles**: The progression from fundamental concepts to specialized domains also roughly maps to technology adoption cycles. Core concepts are more stable, while specialized domains evolve more rapidly.

7. **Connect theory to practice**: For each design concept, try to identify real-world systems that implement similar approaches. This helps anchor theoretical knowledge in practical understanding.

By following this structured learning path, you'll develop both the breadth and depth needed to excel in system design interviews across various domains. The sequence also helps ensure you have a solid foundation before tackling more specialized and complex systems.
