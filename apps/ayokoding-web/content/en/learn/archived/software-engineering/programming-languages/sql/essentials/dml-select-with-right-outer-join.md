---
title: "DML: SELECT with RIGHT (OUTER) JOIN"
date: 2025-03-16T07:20:00+07:00
draft: false
---

The `RIGHT JOIN` or `RIGHT OUTER JOIN` in SQL is a concept in relational database management systems (RDBMS) that allows you to combine rows from two or more tables based on a related column between them.

The `RIGHT JOIN` keyword returns all records from the right table (table2), and the matched records from the left table (table1). The result is NULL on the left side when there is no match.

## Basic Syntax

```sql
SELECT column_name(s)
FROM table1
RIGHT JOIN table2
ON table1.column_name = table2.column_name;
```

- **table1**: The left table.
- **column_name(s)**: The column(s) you want to select.
- **table2**: The right table.
- **table1.column_name = table2.column_name**: The condition that connects the two tables.

## Example 1

Let's start with a basic example:

**Table "order"**

| order_id | customer_id | order_date            |
| -------- | ----------- | --------------------- |
| 1        | 3           | `2020-07-04 12:34:56` |
| 2        | 1           | `2020-09-13 00:00:01` |
| 3        | 2           | `2020-10-09 11:11:11` |
| 4        | 5           | `2020-12-02 23:59:59` |

**Table "customer"**

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |
| 4           | Sally   | UK      |

We can find all customers, whether they have a matching order or not, using the following query:

```sql
SELECT order.order_id, customer.name, customer.country
FROM order
RIGHT JOIN customer
ON order.customer_id = customer.customer_id;
```

The result-set will look like this:

| order_id | name    | country |
| -------- | ------- | ------- |
| 1        | Sarah   | France  |
| 2        | John    | USA     |
| 3        | Michael | Germany |
| NULL     | Sally   | UK      |

We see that the `customer Sally` has no matching `order` in the order table, so it returns NULL for `order_id`.

## Example 2 - Joining More than Two Tables

Let's consider a new table called "product":

**Table "product"**

| product_id | product_name | price |
| ---------- | ------------ | ----- |
| 1          | Apple        | 1.00  |
| 2          | Banana       | 0.50  |
| 3          | Cherry       | 2.00  |
| 4          | Dates        | 3.00  |

To get all products, the order they are included in, and the customer who made the order, we can use a `RIGHT JOIN` twice:

```sql
SELECT order.order_id, customer.name, product.product_name
FROM order
RIGHT JOIN customer ON order.customer_id = customer.customer_id
RIGHT JOIN product ON order.product_id = product.product_id;
```

The result-set will look like this:

| order_id | name    | product_name |
| -------- | ------- | ------------ |
| 1        | Sarah   | Apple        |
| 2        | John    | Banana       |
| 3        | Michael | Cherry       |
| NULL     | NULL    | Dates        |

Here, the product "Dates" doesn't match with any order or customer, so it returns NULL in those fields.

## Example 3 - Using RIGHT JOIN with WHERE Clause

You can also use the `RIGHT JOIN` clause with the `WHERE` clause to filter the records.

```sql
SELECT order.order_id, customer.name, customer.country
FROM order
RIGHT JOIN customer
ON order.customer_id = customer.customer_id
WHERE customer.country = 'USA';
```

Result-set:

| order_id | name | country |
| -------- | ---- | ------- |
| 2        | John | USA     |

The result only includes customers from the USA. Other customers, or customers with no matching order, are not included in the result set.

## Example 4 - Using RIGHT JOIN with Aggregate Functions

`RIGHT JOIN` can also be used with aggregate functions like `COUNT()`, `SUM()`, `AVG()`, etc.

Let's say we want to count the number of orders each customer has made:

```sql
SELECT customer.name, COUNT(order.order_id) as number_of_orders
FROM order
RIGHT JOIN customer
ON order.customer_id = customer.customer_id
GROUP BY customer.name;
```

Result-set:

| name    | number_of_orders |
| ------- | ---------------- |
| John    | 1                |
| Michael | 1                |
| Sarah   | 1                |
| Sally   | 0                |

This query groups the orders by customer names and counts the number of orders each customer has made. Sally has not made any orders, so the NumberOfOrders is 0.

1. [SQL RIGHT JOIN Keyword](https://www.w3schools.com/sql/sql_join_right.asp) - W3Schools
2. [RIGHT OUTER JOIN in SQL Server](https://learn.microsoft.com/en-us/sql/t-sql/queries/from-transact-sql?view=sql-server-ver15#right-outer-join) - Microsoft Docs
3. [SQL Aggregate Functions](https://www.w3schools.com/sql/sql_count_avg_sum.asp) - W3Schools
4. [The GROUP BY Statement in SQL](https://www.sqltutorial.org/sql-group-by/) - SQL Tutorial
5. [Using JOINs in SQL Server](https://learn.microsoft.com/en-us/sql/t-sql/queries/select-transact-sql?view=sql-server-ver15#using-joins) - Microsoft Docs
6. [Filtering Data with WHERE Clause in SQL Server](https://www.sqlservertutorial.net/sql-server-basics/sql-server-where/) - SQL Server Tutorial
