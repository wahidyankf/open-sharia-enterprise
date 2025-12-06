---
title: "Key Concepts"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Document-oriented databases are a type of NoSQL database that store data in a document format, typically using JSON or BSON. Unlike traditional relational databases, document-oriented databases do not require a fixed schema, allowing for more flexibility in data modeling. Here are some key concepts to understand when working with document-oriented databases:

## **Documents**

Documents are the basic unit of data in a document-oriented database. They are similar to rows in a relational database but with a few key differences. Documents are typically stored in a format such as JSON or BSON, which allows for nested data structures and more flexible data modeling. Unlike rows in a relational database with fixed columns, documents can also have different fields.

For example, consider a document representing a user in a social media application. This document might include fields such as the user's name, age, address, and friends list. The friends' field could contain an array of documents, each representing a friend and containing their name, age, and other information.

```json
{
	"name": "John Smith",
	"age": 30,
	"address": {
		"street": "123 Main St",
		"city": "Anytown",
		"state": "CA",
		"zip": "12345"
	},
	"friends": [
			{
				"name": "Jane Doe",
				"age": 28,
				"address": {
				"street": "456 Elm St",
				"city": "Anytown",
				"state": "CA",
				"zip": "12345"
			},
			{
				"name": "Bob Johnson",
				"age": 32,
				"address": {
				"street": "789 Oak St",
				"city": "Anytown",
				"state": "CA",
				"zip": "12345"
			}
	]
}
```

### **Collections**

Collections are similar to tables in a relational database. They are a grouping of related documents and can have different fields and data types. Collections can also have indexes to improve query performance.

For example, consider a collection of users in a social media application. This collection might include documents representing each user and indexes on fields such as the user's name or age to improve query performance.

```jsx
db.createCollection("users");
db.users.createIndex({ name: 1 });
db.users.createIndex({ age: 1 });
```

### **Queries**

Queries in document-oriented databases are typically done using a query language such as MongoDB's query language. Queries can be used to filter documents based on specific criteria, such as field values or nested data structures.

For example, consider a query to find all users in a social media application who are over the age of 25.

```jsx
db.users.find({ age: { $gt: 25 } });
```

This query would return all documents in the user's collection with an age field greater than 25.

### **Aggregation**

Aggregation is the process of performing calculations on a set of documents in a collection. This can include grouping documents by a specific field, calculating averages or sums, and more.

For example, consider an aggregation to count the number of users in a social media application by state.

```jsx
db.users.aggregate([
  {
    $group: {
      _id: "$address.state",
      count: {
        $sum: 1,
      },
    },
  },
]);
```

This aggregation would group all documents in the users’ collection by the state field in the address subdocument and then count the number of documents in each group.

## **Further Readings**

If you're interested in learning more about document-oriented databases, here are some resources to check out:

- [MongoDB Documentation](https://docs.mongodb.com/)
- [Couchbase Documentation](https://docs.couchbase.com/)
- [Amazon DynamoDB Documentation](https://docs.aws.amazon.com/dynamodb/)
