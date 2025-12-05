---
title: "Key Concepts"
date: 2025-03-16T07:20:00+07:00
draft: false
---

## **Messaging Protocols**

Messaging protocols define the rules and standards for exchanging messages between applications. Message brokers use different messaging protocols to transmit messages between applications. Some of the popular messaging protocols are:

- **AMQP (Advanced Message Queuing Protocol)**: It is an open standard messaging protocol that supports multiple messaging patterns like point-to-point, publish-subscribe, and request-response. AMQP is designed to be interoperable across different platforms and programming languages.
- **MQTT (Message Queuing Telemetry Transport)**: It is a lightweight messaging protocol that is ideal for IoT devices and mobile applications. MQTT is designed to be efficient in terms of bandwidth and power consumption.
- **STOMP (Simple Text Oriented Messaging Protocol)**: It is a text-based messaging protocol that is easy to implement and supports multiple messaging patterns. STOMP is designed to be simple and flexible.

Message brokers can support multiple messaging protocols to cater to different use cases and requirements.

## **Message Queues**

Message queues are a fundamental concept in message brokers. A message queue is a data structure that holds messages in a first-in, first-out (FIFO) order. The sender puts messages into the queue, and the receiver takes messages out of the queue. Message queues provide a reliable and scalable way to transmit messages between applications.

Message queues can be implemented using different storage technologies like in-memory, disk-based, or cloud-based storage. Message brokers can also support different message queues like durable, non-durable, or priority queues.

## **Topics and Subscriptions**

Message brokers use topics and subscriptions to implement publish-subscribe messaging patterns. A topic is a named entity representing a specific subject or category of messages. A subscription is a named entity representing a filter on the messages published to a topic. Subscribers can subscribe to a topic and receive messages that match their subscriptions.

Topics and subscriptions provide a flexible and scalable way to transmit messages between applications. Message brokers can support different types of topics and subscriptions like durable topics, non-durable topics, or wildcard subscriptions.

## **Message Routing**

Message routing is the process of delivering messages to the appropriate receiver. Message brokers use message routing to ensure that messages are delivered to the correct destination. Message routing can be based on criteria like message content, message header, or message destination. Message brokers can also route messages to multiple receivers based on the messaging pattern.

Message routing provides a reliable and flexible way to transmit messages between applications. Message brokers can support different types of message routing, like direct routing, topic-based routing, or content-based routing.

## **Message Transformation**

Message transformation is the process of converting messages from one format to another format to enable communication between applications that use different message formats. Message brokers can transform messages from one format to another format to ensure that messages are compatible with the receiving application. Message transformation can be done using different techniques like message mapping, message enrichment, or message filtering.

Message transformation provides a flexible and interoperable way to transmit messages between applications. Message brokers can support different types of message transformation like data mapping, data enrichment, or data filtering.

## **Further Readings**

- [Apache Kafka](https://kafka.apache.org/)
- [RabbitMQ](https://www.rabbitmq.com/)
- [ActiveMQ](https://activemq.apache.org/)
- [AWS SQS](https://aws.amazon.com/sqs/)
- [Azure Service Bus](https://azure.microsoft.com/en-us/services/service-bus/)
- [Google Cloud Pub/Sub](https://cloud.google.com/pubsub)
