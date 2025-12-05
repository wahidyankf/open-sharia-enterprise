---
title: "Key Concepts"
date: 2025-03-16T07:20:00+07:00
draft: false
---

A graph database is a NoSQL database that uses graph theory to store, map, and query relationships between data. In a graph database, data is represented as nodes (or vertices) and edges, which connect the nodes. Nodes represent entities, such as people, places, or things, while edges represent the relationships between those entities.

## **Nodes**

Nodes are the fundamental building blocks of a graph database. Each node represents an entity, such as a person, place, or thing, and can have one or more properties associated with it. For example, a node representing a person might have properties such as name, age, and occupation.

## **Edges**

Edges are the connections between nodes in a graph database. Each edge represents a relationship between two nodes and can have one or more properties associated with it. For example, an edge representing a friendship between two people might have a property indicating how long they have been friends.

## **Properties**

Properties are key-value pairs associated with nodes and edges in a graph database. They provide additional information about the entities and relationships represented by the nodes and edges. For example, a property associated with a node representing a person might be their date of birth.

## **Labels**

Labels group nodes and edges together based on their type or function. For example, all nodes representing people might be labeled as "Person", while all edges representing friendships might be labeled as "Friendship".

## **Traversals**

Traversals are used to navigate the graph database and retrieve data based on specific criteria. Traversals can start at any node in the graph and follow edges to other nodes based on certain conditions. For example, a traversal might be used to find all nodes representing people who are friends with a particular person.

## **Examples**

Some examples of graph databases include:

- Neo4j is a popular open-source graph database used by companies like eBay, Walmart, and Cisco.
- Amazon Neptune: a fully-managed graph database service that is part of the Amazon Web Services (AWS) platform.
- Microsoft Azure Cosmos DB: a globally-distributed, multi-model database service that supports graph databases.

## **Further Readings**

- [Neo4j Documentation](https://neo4j.com/docs/)
- [Amazon Neptune Documentation](https://docs.aws.amazon.com/neptune/latest/userguide/intro.html)
- [Microsoft Azure Cosmos DB Documentation](https://docs.microsoft.com/en-us/azure/cosmos-db/graph-introduction)
