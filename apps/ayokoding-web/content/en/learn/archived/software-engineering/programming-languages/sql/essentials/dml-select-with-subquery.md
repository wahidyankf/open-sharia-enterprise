---
title: "DML: SELECT with Subquery"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

A subquery is a query that is nested inside another query. It can be used to retrieve data that will be used in the main query as a condition to restrict the data further to be retrieved.

## Basic Syntax

```sql
SELECT column1, column2, ...
FROM table_name
WHERE column_name operator (SELECT column_name FROM table_name WHERE condition);
```

- **column1, column2, ...**: The column(s) to retrieve from the table.
- **table_name**: The table to retrieve data from.
- **column_name**: The column(s) to use in the subquery.
- **operator**: The operator to use in the subquery. Common operators include `=`, `>`, `<`, `>=`, `<=`, and `<>`.
- **condition**: The condition to use in the subquery.

## Non-Correlated Subquery

A non-correlated subquery is a subquery that can be executed independently of the outer query. It is executed only once and the result is used as a condition in the outer query.

### Performance Characteristics

Non-correlated subqueries are generally faster than correlated subqueries because they are executed only once, and the result is cached in memory. However, if the subquery returns a large number of rows, it can slow down the performance of the query.

### Example

For the "customer" table:

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |
| 4           | Sally   | UK      |

If we want to retrieve all customers from the "customer" table who are from the same country as Sally, we can use the following SQL:

```sql
SELECT name, country
FROM customer
WHERE country = (SELECT country FROM customer WHERE name = 'Sally');
```

This will retrieve the following data:

| name  | country |
| ----- | ------- |
| Sarah | UK      |

## Correlated Subquery

A correlated subquery is a subquery that is executed for each row of the outer query. It is dependent on the outer query and cannot be executed independently.

### Performance Characteristics

Correlated subqueries are generally slower than non-correlated subqueries because they are executed for each row of the outer query. If the subquery returns a large number of rows, it can significantly slow down the performance of the query.

### Example

For the "order" table:

| order_id | customer_id | `order_date`          | amount |
| -------- | ----------- | --------------------- | ------ |
| 1        | 1           | `2021-01-01 07:35:22` | 100    |
| 2        | 1           | `2021-02-01 14:20:10` | 200    |
| 3        | 2           | `2021-01-15 19:45:01` | 150    |
| 4        | 3           | `2021-02-15 09:10:55` | 300    |

If we want to retrieve the total amount of orders for each customer, we can use the following SQL:

```sql
SELECT name, (SELECT SUM(amount) FROM order WHERE customer_id = customer.customer_id) AS total_amount
FROM customer;
```

This will retrieve the following data:

| name    | total_amount |
| ------- | ------------ |
| John    | 300          |
| Michael | 150          |
| Sarah   | 300          |
| Sally   | NULL         |

Note that Sally has no orders, so the `total_amount` column is `NULL`.

## Further Readings

1. [The SQL Subquery](https://www.sqltutorial.org/sql-subquery/) - SQL Tutorial
2. [Subqueries in MySQL](https://www.mysqltutorial.org/mysql-subquery/) - MySQL Tutorial
3. [SQL Server Subquery](https://www.sqlservertutorial.net/sql-server-basics/sql-server-subquery/) - SQL Server Tutorial
