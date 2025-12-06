---
title: "Profile"
date: 2025-03-16T07:20:00+07:00
draft: false
---

## **Introduction**

A relational database is a type of database that stores and organizes data in tables and columns. It is based on the relational model, which uses a set of tables to represent both data and the relationships among those data.

## **Key Characteristics**

- Structured data
- ACID compliance (Atomicity, Consistency, Isolation, Durability)
- Joins
- Transactions
- Strong consistency

## **CAP Theorem**

### **General CAP theorem handling**

The CAP theorem states that a distributed system cannot simultaneously provide all three guarantees: consistency, availability, and partition tolerance.

### **Guarantee of consistency**

Relational databases provide strong consistency, meaning that all nodes in the system see the same data simultaneously.

### **Guarantee of availability**

Relational databases can provide high availability through techniques such as replication and failover.

### **Guarantee of partition tolerance**

Relational databases can handle partition tolerance through techniques such as sharding and distributed transactions.

## **Usage**

### **Best usage**

Relational databases are well-suited for applications that require strong consistency and complex queries, such as financial systems and e-commerce applications.

### **Neutral usage**

Relational databases, such as content management systems, can also be used for applications that require less complex queries and do not require strong consistency.

### **Worst usage**

Relational databases may not be well-suited for applications that require high write throughput or have large amounts of unstructured data.

### **System Design Role**

Relational databases are well-suited for systems that require strong consistency and complex queries and can be used in various distributed system architectures.

## **Data Model**

Relational databases use a tabular data model, which organizes data into tables with rows and columns. It is a structured data model that enforces data integrity through constraints.

## **Query Language**

Relational databases use SQL (Structured Query Language) as their query language. SQL is a declarative language that allows users to specify what data they want to retrieve rather than how to retrieve it.

## **Scalability**

### **How to make it performant**

Relational databases can be made performant through techniques such as indexing and query optimization.

### **High traffic handling**

Relational databases can handle high traffic through techniques such as replication and sharding. They are well-suited for high-read workloads but may not be as performant for high-write workloads.

### **How to scale it**

Relational databases can be scaled through vertical scaling (adding more resources to a single node) or sharding (distributing data across multiple nodes).

### **Usage in distributed systems**

Relational databases can be used in a distributed system architecture, but care must be taken to ensure consistency and availability.

### **Replication**

Relational databases can handle replication through master-slave and multi-master replication techniques. Best practices for replication include ensuring consistency and minimizing latency.

## In Practice

### **Best Practices**

- Use indexes to improve query performance
- Normalize data to reduce redundancy and improve data integrity
- Use transactions to ensure data consistency

### Common Pitfalls

- Over-indexing can lead to decreased write performance
- Denormalizing data can lead to data integrity issues
- Failing to use transactions can lead to data inconsistencies

### Examples

Examples of relational databases include MySQL, PostgreSQL, and Oracle.

## Further Readings

- "Database Systems: The Complete Book" by Hector Garcia-Molina, Jeffrey D. Ullman, and Jennifer Widom
- "SQL Performance Explained" by Markus Winand
- "High Performance MySQL" by Baron Schwartz, Peter Zaitsev, and Vadim Tkachenko
- [MySQL](../../../../tools/data/mysql/)
- [PostgreSQL](../../../../tools/data/postgresql/)
