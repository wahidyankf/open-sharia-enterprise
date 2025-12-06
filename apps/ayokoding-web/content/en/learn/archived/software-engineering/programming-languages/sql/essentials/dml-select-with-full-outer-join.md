---
title: "DML: SELECT with FULL (OUTER) JOIN"
date: 2025-03-16T07:20:00+07:00
draft: false
---

The `FULL JOIN` or `FULL OUTER JOIN` in SQL is a concept in relational database management systems (RDBMS) that allows you to combine rows from two or more tables based on a related column between them.

The `FULL JOIN` keyword returns all records when there is a match in either the left (table1) or the right (table2) table records. The result is NULL on either side when there is no match.

## Basic Syntax

```sql
SELECT column_name(s)
FROM table1
FULL JOIN table2
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
| 1        | 3           | `2020-07-04 10:30:00` |
| 2        | 1           | `2020-09-13 15:45:00` |
| 3        | 2           | `2020-10-09 08:20:00` |
| 4        | 5           | `2020-12-02 20:00:00` |

**Table "customer"**

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |
| 4           | Sally   | UK      |

We can find all orders and customers, whether they have a matching record or not, using the following query:

```sql
SELECT order.order_id, customer.name, customer.country
FROM order
FULL JOIN customer
ON order.customer_id = customer.customer_id;
```

The result-set will look like this:

| order_id | name    | country |
| -------- | ------- | ------- |
| 1        | Sarah   | France  |
| 2        | John    | USA     |
| 3        | Michael | Germany |
| 4        | NULL    | NULL    |
| NULL     | Sally   | UK      |

We see that the `order_id 4` has no matching `customer_id` in the Customers table, so it returns NULL. Similarly, the customer Sally does not have a corresponding order in the Orders table, so it also returns NULL for `order_id`.

## Example 2 - Joining More than Two Tables

Let's consider a new table called "product":

**Table "product"**

| product_id | product_name | price |
| ---------- | ------------ | ----- |
| 1          | Apple        | 1.00  |
| 2          | Banana       | 0.50  |
| 3          | Cherry       | 2.00  |
| 4          | Dates        | 3.00  |

To get all orders, customers, and products, whether they have a matching record or not, we can use a `FULL JOIN` twice:

```sql
SELECT order.order_id, customer.name, product.product_name
FROM ((order
FULL JOIN customer ON order.customer_id = customer.customer_id)
FULL JOIN product ON order.product_id = product.product_id);
```

The result-set will look like this:

| order_id | name  | product_name |
| -------- | ----- | ------------ |
| 1        | Sarah | Apple        |
| 2        | John  | Banana       |
| 3        |       |              |

| Michael | Cherry |
| 4 | NULL | NULL |
| NULL | Sally | NULL |
| NULL | NULL | Dates |

Here, the product "Dates" doesn't match with any order or customer, so it returns NULL in those fields. Similarly, the customer Sally and `order_id 4` have no match, so they also return NULL where applicable.

## Example 3 - Using FULL JOIN with WHERE Clause

You can also use the `FULL JOIN` clause with the `WHERE` clause to filter the records.

```sql
SELECT order.order_id, customer.name, customer.country
FROM order
FULL JOIN customer
ON order.customer_id = customer.customer_id
WHERE customer.country = 'USA';
```

Result-set:

| order_id | name | country |
| -------- | ---- | ------- |
| 2        | John | USA     |

The result only includes customers from the USA. Other customers, or customers with no matching order, are not included in the result set.

## Example 4 - Using FULL JOIN with Aggregate Functions

`FULL JOIN` can also be used with aggregate functions like `COUNT()`, `SUM()`, `AVG()`, etc.

Let's say we want to count the number of orders each customer has made:

```sql
SELECT customer.name, COUNT(order.order_id) as number_of_orders
FROM order
FULL JOIN customer
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
| NULL    | 1                |

This query groups the orders by customer names, and counts the number of orders each customer has made. Sally has not made any orders, so the NumberOfOrders is 0. The NULL represents the order that doesn't have a matching customer.

1. [SQL FULL OUTER JOIN Keyword](https://www.w3schools.com/sql/sql_join_full.asp) - W3Schools
2. [The SQL FULL OUTER JOIN syntax](https://www.sqltutorial.org/sql-full-outer-join/) - SQL Tutorial
3. [FULL OUTER JOIN in SQL Server](https://docs.microsoft.com/en-us/sql/t-sql/queries/from-transact-sql?view=sql-server-ver15#full-outer-join) - Microsoft Docs
4. [SQL Aggregate Functions](https://www.w3schools.com/sql/sql_count_avg_sum.asp) - W3Schools
5. [The GROUP BY Statement in SQL](https://www.sqltutorial.org/sql-group-by/) - SQL Tutorial
6. [Using JOINs in SQL Server](https://docs.microsoft.com/en-us/sql/t-sql/queries/select-transact-sql?view=sql-server-ver15#using-joins) - Microsoft Docs
7. [Filtering Data with WHERE Clause in SQL Server](https://www.sqlservertutorial.net/sql-server-basics/sql-server-where/) - SQL Server Tutorial
