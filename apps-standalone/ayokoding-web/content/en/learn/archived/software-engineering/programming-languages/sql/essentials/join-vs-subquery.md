---
title: "JOIN vs. Subquery"
date: 2025-03-16T07:20:00+07:00
draft: false
---

When working with relational databases, it is common to need to combine data from multiple tables. SQL provides two main ways to achieve this: using JOINs or subqueries. Both methods have advantages and disadvantages, and their choice depends on the specific use case.

## SQL JOIN

A JOIN combines rows from two or more tables based on a related column between them. There are several types of JOINs, including INNER JOIN, LEFT JOIN, RIGHT JOIN, and FULL OUTER JOIN. The most commonly used JOIN is INNER JOIN, which returns only the rows with matching values in both tables.

Here is an example of an INNER JOIN:

```sql
SELECT orders.order_id, customers.customer_name
FROM orders
INNER JOIN customers
ON orders.customer_id = customers.customer_id;
```

This query returns the order ID and customer name for all orders that have a matching customer ID in the customer's table.

JOINs are generally faster than subqueries for large datasets, allowing the database to perform the join operation in a single pass. However, when dealing with multiple tables and conditions, JOINs can become complex and challenging to read.

## Subquery

A subquery is a query that is nested inside another query. It is used to retrieve data used in the main query as a condition or to perform calculations. Subqueries can be used in the SELECT, FROM, WHERE, and HAVING clauses.

Here is an example of a subquery:

```sql
SELECT customer_name, order_count
FROM customers
WHERE order_count = (
  SELECT COUNT(*)
  FROM orders
  WHERE orders.customer_id = customers.customer_id
);
```

This query returns the customer name and the number of orders they have placed, but only for customers who have placed specific orders (in this case, the same number of orders as the customer with the most orders).

Subqueries can be more flexible than JOINs, allowing for more complex conditions and calculations. However, they can also be slower than JOINs for large datasets, as they require the database to perform multiple passes.

## Conclusion

In general, JOINs are best for simple queries that involve only a few tables, while subqueries are better for more complex queries that involve multiple conditions and calculations. However, the choice between JOINs and subqueries ultimately depends on the specific use case and the performance requirements of the query.

## Further Readings

- [SQL JOINs](https://www.w3schools.com/sql/sql_join.asp)
