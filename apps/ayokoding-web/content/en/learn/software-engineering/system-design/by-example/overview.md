---
title: "Overview"
weight: 10000000
date: 2026-03-20T00:00:00+07:00
draft: false
description: "Learn system design through 85+ annotated examples covering 95% of essential concepts - ideal for experienced developers"
tags: ["system-design", "tutorial", "by-example", "code-first", "scalability"]
---

This section teaches system design through heavily annotated examples in **Go and Python**.
Each example is self-contained and includes annotations explaining the reasoning behind
every design decision.

## What You Will Learn

The by-example path covers 95% of essential system design concepts through 85+
concrete examples organized by difficulty level.

## How to Use This Section

Work through the examples in order or jump to topics relevant to your current
challenges. Each example stands alone and can be studied independently.

- [Beginner](/en/learn/software-engineering/system-design/by-example/beginner) -
  Foundational concepts with simple, approachable examples
- [Intermediate](/en/learn/software-engineering/system-design/by-example/intermediate) -
  Moderate complexity covering common real-world patterns
- [Advanced](/en/learn/software-engineering/system-design/by-example/advanced) -
  Complex distributed systems and large-scale architectures

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation** — what the design addresses and why it matters (2-3 sentences)
2. **Mermaid Diagram** — visual representation of system components, data flow, or architecture (when appropriate)
3. **Heavily Annotated Code** — implementations in Go and Python with `// =>` comments documenting decisions and trade-offs
4. **Key Takeaway** — the core insight to retain from the example (1-2 sentences)
5. **Why It Matters** — production relevance and real-world impact (50-100 words)

## Examples by Level

### Beginner (Examples 1–28)

- [Example 1: The Client-Server Model](/en/learn/software-engineering/system-design/by-example/beginner#example-1-the-client-server-model)
- [Example 2: HTTP Methods and Status Codes](/en/learn/software-engineering/system-design/by-example/beginner#example-2-http-methods-and-status-codes)
- [Example 3: REST API Design Basics](/en/learn/software-engineering/system-design/by-example/beginner#example-3-rest-api-design-basics)
- [Example 4: DNS Resolution — How Names Become Addresses](/en/learn/software-engineering/system-design/by-example/beginner#example-4-dns-resolution--how-names-become-addresses)
- [Example 5: Round-Robin Load Balancing](/en/learn/software-engineering/system-design/by-example/beginner#example-5-round-robin-load-balancing)
- [Example 6: Least-Connections Load Balancing](/en/learn/software-engineering/system-design/by-example/beginner#example-6-least-connections-load-balancing)
- [Example 7: In-Memory Caching with TTL](/en/learn/software-engineering/system-design/by-example/beginner#example-7-in-memory-caching-with-ttl)
- [Example 8: Cache Invalidation Strategies](/en/learn/software-engineering/system-design/by-example/beginner#example-8-cache-invalidation-strategies)
- [Example 9: SQL vs NoSQL — When to Use Each](/en/learn/software-engineering/system-design/by-example/beginner#example-9-sql-vs-nosql--when-to-use-each)
- [Example 10: Database Indexing Basics](/en/learn/software-engineering/system-design/by-example/beginner#example-10-database-indexing-basics)
- [Example 11: Vertical Scaling — Scale Up](/en/learn/software-engineering/system-design/by-example/beginner#example-11-vertical-scaling--scale-up)
- [Example 12: Horizontal Scaling — Scale Out](/en/learn/software-engineering/system-design/by-example/beginner#example-12-horizontal-scaling--scale-out)
- [Example 13: Stateless vs Stateful Services](/en/learn/software-engineering/system-design/by-example/beginner#example-13-stateless-vs-stateful-services)
- [Example 14: Forward Proxy — Client-Side Intermediary](/en/learn/software-engineering/system-design/by-example/beginner#example-14-forward-proxy--client-side-intermediary)
- [Example 15: Reverse Proxy — Server-Side Intermediary](/en/learn/software-engineering/system-design/by-example/beginner#example-15-reverse-proxy--server-side-intermediary)
- [Example 16: Measuring Latency and Throughput](/en/learn/software-engineering/system-design/by-example/beginner#example-16-measuring-latency-and-throughput)
- [Example 17: The P50/P95/P99 Latency Percentiles](/en/learn/software-engineering/system-design/by-example/beginner#example-17-the-p50p95p99-latency-percentiles)
- [Example 18: Block Storage vs File Storage vs Object Storage](/en/learn/software-engineering/system-design/by-example/beginner#example-18-block-storage-vs-file-storage-vs-object-storage)
- [Example 19: CDN — Content Delivery Network Basics](/en/learn/software-engineering/system-design/by-example/beginner#example-19-cdn--content-delivery-network-basics)
- [Example 20: API Rate Limiting — Token Bucket Algorithm](/en/learn/software-engineering/system-design/by-example/beginner#example-20-api-rate-limiting--token-bucket-algorithm)
- [Example 21: Pagination — Offset vs Cursor](/en/learn/software-engineering/system-design/by-example/beginner#example-21-pagination--offset-vs-cursor)
- [Example 22: Idempotency Keys — Safe Retries](/en/learn/software-engineering/system-design/by-example/beginner#example-22-idempotency-keys--safe-retries)
- [Example 23: Health Checks — Liveness and Readiness](/en/learn/software-engineering/system-design/by-example/beginner#example-23-health-checks--liveness-and-readiness)
- [Example 24: Message Queue Basics — Decoupling Services](/en/learn/software-engineering/system-design/by-example/beginner#example-24-message-queue-basics--decoupling-services)
- [Example 25: Service Discovery — How Services Find Each Other](/en/learn/software-engineering/system-design/by-example/beginner#example-25-service-discovery--how-services-find-each-other)
- [Example 26: Circuit Breaker Pattern](/en/learn/software-engineering/system-design/by-example/beginner#example-26-circuit-breaker-pattern)
- [Example 27: Consistent Hashing — Distributing Data Across Nodes](/en/learn/software-engineering/system-design/by-example/beginner#example-27-consistent-hashing--distributing-data-across-nodes)
- [Example 28: Availability, Reliability, and SLAs](/en/learn/software-engineering/system-design/by-example/beginner#example-28-availability-reliability-and-slas)

### Intermediate (Examples 29–57)

- [Example 29: Publish-Subscribe Pattern with Message Queue](/en/learn/software-engineering/system-design/by-example/intermediate#example-29-publish-subscribe-pattern-with-message-queue)
- [Example 30: Point-to-Point Queue (Work Queue)](/en/learn/software-engineering/system-design/by-example/intermediate#example-30-point-to-point-queue-work-queue)
- [Example 31: Master-Slave Replication](/en/learn/software-engineering/system-design/by-example/intermediate#example-31-master-slave-replication)
- [Example 32: Multi-Master Replication and Conflict Resolution](/en/learn/software-engineering/system-design/by-example/intermediate#example-32-multi-master-replication-and-conflict-resolution)
- [Example 33: Range-Based Sharding](/en/learn/software-engineering/system-design/by-example/intermediate#example-33-range-based-sharding)
- [Example 34: Hash-Based Sharding](/en/learn/software-engineering/system-design/by-example/intermediate#example-34-hash-based-sharding)
- [Example 35: Consistent Hashing Ring](/en/learn/software-engineering/system-design/by-example/intermediate#example-35-consistent-hashing-ring)
- [Example 36: Token Bucket Rate Limiter](/en/learn/software-engineering/system-design/by-example/intermediate#example-36-token-bucket-rate-limiter)
- [Example 37: Sliding Window Rate Limiter](/en/learn/software-engineering/system-design/by-example/intermediate#example-37-sliding-window-rate-limiter)
- [Example 38: Circuit Breaker with Three States](/en/learn/software-engineering/system-design/by-example/intermediate#example-38-circuit-breaker-with-three-states)
- [Example 39: API Gateway with Request Routing and Authentication](/en/learn/software-engineering/system-design/by-example/intermediate#example-39-api-gateway-with-request-routing-and-authentication)
- [Example 40: Service Registry and Client-Side Discovery](/en/learn/software-engineering/system-design/by-example/intermediate#example-40-service-registry-and-client-side-discovery)
- [Example 41: Health Check Endpoints with Dependency Probing](/en/learn/software-engineering/system-design/by-example/intermediate#example-41-health-check-endpoints-with-dependency-probing)
- [Example 42: Synchronous vs Asynchronous Communication](/en/learn/software-engineering/system-design/by-example/intermediate#example-42-synchronous-vs-asynchronous-communication)
- [Example 43: JSON Serialization and Schema Validation](/en/learn/software-engineering/system-design/by-example/intermediate#example-43-json-serialization-and-schema-validation)
- [Example 44: Protocol Buffers Serialization](/en/learn/software-engineering/system-design/by-example/intermediate#example-44-protocol-buffers-serialization)
- [Example 45: B-Tree Index vs Sequential Scan](/en/learn/software-engineering/system-design/by-example/intermediate#example-45-b-tree-index-vs-sequential-scan)
- [Example 46: Database Connection Pool](/en/learn/software-engineering/system-design/by-example/intermediate#example-46-database-connection-pool)
- [Example 47: Distributed Session with Token-Based Authentication](/en/learn/software-engineering/system-design/by-example/intermediate#example-47-distributed-session-with-token-based-authentication)
- [Example 48: Multi-Channel Notification Dispatcher](/en/learn/software-engineering/system-design/by-example/intermediate#example-48-multi-channel-notification-dispatcher)
- [Example 49: Inverted Index for Full-Text Search](/en/learn/software-engineering/system-design/by-example/intermediate#example-49-inverted-index-for-full-text-search)
- [Example 50: Object Storage with Metadata Indexing](/en/learn/software-engineering/system-design/by-example/intermediate#example-50-object-storage-with-metadata-indexing)
- [Example 51: Structured Logging with Correlation IDs](/en/learn/software-engineering/system-design/by-example/intermediate#example-51-structured-logging-with-correlation-ids)
- [Example 52: Metrics Collection and Alerting](/en/learn/software-engineering/system-design/by-example/intermediate#example-52-metrics-collection-and-alerting)
- [Example 53: Distributed Tracing with Span Propagation](/en/learn/software-engineering/system-design/by-example/intermediate#example-53-distributed-tracing-with-span-propagation)
- [Example 54: Key-Value Cache with TTL and LRU Eviction](/en/learn/software-engineering/system-design/by-example/intermediate#example-54-key-value-cache-with-ttl-and-lru-eviction)
- [Example 55: Write-Through and Write-Back Cache Patterns](/en/learn/software-engineering/system-design/by-example/intermediate#example-55-write-through-and-write-back-cache-patterns)
- [Example 56: Load Balancer Algorithms — Round Robin, Least Connections, and Weighted](/en/learn/software-engineering/system-design/by-example/intermediate#example-56-load-balancer-algorithms--round-robin-least-connections-and-weighted)
- [Example 57: Idempotency Keys for Safe Retries](/en/learn/software-engineering/system-design/by-example/intermediate#example-57-idempotency-keys-for-safe-retries)

### Advanced (Examples 58–85)

- [Example 58: CAP Theorem — Partition Tolerance Forces a Choice](/en/learn/software-engineering/system-design/by-example/advanced#example-58-cap-theorem--partition-tolerance-forces-a-choice)
- [Example 59: PACELC — Extending CAP with Latency](/en/learn/software-engineering/system-design/by-example/advanced#example-59-pacelc--extending-cap-with-latency)
- [Example 60: Raft Consensus — Leader Election Basics](/en/learn/software-engineering/system-design/by-example/advanced#example-60-raft-consensus--leader-election-basics)
- [Example 61: Paxos Concepts — Two-Phase Agreement](/en/learn/software-engineering/system-design/by-example/advanced#example-61-paxos-concepts--two-phase-agreement)
- [Example 62: Event Sourcing — State as Append-Only Event Log](/en/learn/software-engineering/system-design/by-example/advanced#example-62-event-sourcing--state-as-append-only-event-log)
- [Example 63: CQRS — Separating Read and Write Models](/en/learn/software-engineering/system-design/by-example/advanced#example-63-cqrs--separating-read-and-write-models)
- [Example 64: Saga Pattern — Long-Running Transactions Without 2PC](/en/learn/software-engineering/system-design/by-example/advanced#example-64-saga-pattern--long-running-transactions-without-2pc)
- [Example 65: Two-Phase Commit — Atomic Distributed Transactions](/en/learn/software-engineering/system-design/by-example/advanced#example-65-two-phase-commit--atomic-distributed-transactions)
- [Example 66: Vector Clocks — Causality Tracking Without Synchronized Clocks](/en/learn/software-engineering/system-design/by-example/advanced#example-66-vector-clocks--causality-tracking-without-synchronized-clocks)
- [Example 67: Gossip Protocol — Epidemic Information Dissemination](/en/learn/software-engineering/system-design/by-example/advanced#example-67-gossip-protocol--epidemic-information-dissemination)
- [Example 68: Bloom Filter — Space-Efficient Probabilistic Set Membership](/en/learn/software-engineering/system-design/by-example/advanced#example-68-bloom-filter--space-efficient-probabilistic-set-membership)
- [Example 69: Distributed Caching Strategies — Cache-Aside, Write-Through, Write-Behind](/en/learn/software-engineering/system-design/by-example/advanced#example-69-distributed-caching-strategies--cache-aside-write-through-write-behind)
- [Example 70: Leader Election — Distributed Lock with Expiring Leases](/en/learn/software-engineering/system-design/by-example/advanced#example-70-leader-election--distributed-lock-with-expiring-leases)
- [Example 71: Geo-Replication — Multi-Region Active-Active vs Active-Passive](/en/learn/software-engineering/system-design/by-example/advanced#example-71-geo-replication--multi-region-active-active-vs-active-passive)
- [Example 72: Kafka Concepts — Topics, Partitions, Consumer Groups](/en/learn/software-engineering/system-design/by-example/advanced#example-72-kafka-concepts--topics-partitions-consumer-groups)
- [Example 73: Service Mesh — Sidecar Proxy Pattern](/en/learn/software-engineering/system-design/by-example/advanced#example-73-service-mesh--sidecar-proxy-pattern)
- [Example 74: Chaos Engineering — Fault Injection Testing](/en/learn/software-engineering/system-design/by-example/advanced#example-74-chaos-engineering--fault-injection-testing)
- [Example 75: Blue-Green and Canary Deployments](/en/learn/software-engineering/system-design/by-example/advanced#example-75-blue-green-and-canary-deployments)
- [Example 76: Feature Flags — Progressive Feature Rollout](/en/learn/software-engineering/system-design/by-example/advanced#example-76-feature-flags--progressive-feature-rollout)
- [Example 77: Observability — Metrics, Logs, and Traces (The Three Pillars)](/en/learn/software-engineering/system-design/by-example/advanced#example-77-observability--metrics-logs-and-traces-the-three-pillars)
- [Example 78: SLA, SLO, SLI — Reliability Target Hierarchy](/en/learn/software-engineering/system-design/by-example/advanced#example-78-sla-slo-sli--reliability-target-hierarchy)
- [Example 79: Back-of-Envelope Estimation — Storage and Throughput](/en/learn/software-engineering/system-design/by-example/advanced#example-79-back-of-envelope-estimation--storage-and-throughput)
- [Example 80: Capacity Planning — Database Sizing and Sharding Decision](/en/learn/software-engineering/system-design/by-example/advanced#example-80-capacity-planning--database-sizing-and-sharding-decision)
- [Example 81: Data Lake Architecture — Ingestion, Storage, and Query Layers](/en/learn/software-engineering/system-design/by-example/advanced#example-81-data-lake-architecture--ingestion-storage-and-query-layers)
- [Example 82: Stream Processing — Tumbling Window Aggregations](/en/learn/software-engineering/system-design/by-example/advanced#example-82-stream-processing--tumbling-window-aggregations)
- [Example 83: Incident Management — Runbooks and Blameless Post-Mortems](/en/learn/software-engineering/system-design/by-example/advanced#example-83-incident-management--runbooks-and-blameless-post-mortems)
- [Example 84: Multi-Region Strategy — Data Residency and Routing](/en/learn/software-engineering/system-design/by-example/advanced#example-84-multi-region-strategy--data-residency-and-routing)
- [Example 85: URL Shortener at Scale — Applying Advanced Patterns](/en/learn/software-engineering/system-design/by-example/advanced#example-85-url-shortener-at-scale--applying-advanced-patterns)
