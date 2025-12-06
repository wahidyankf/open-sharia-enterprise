---
title: "DML: SELECT with GROUP BY"
date: 2025-03-16T07:20:00+07:00
draft: false
---

The `GROUP BY` statement in SQL is used with the `SELECT` statement to arrange identical data into groups. The `GROUP BY` statement comes after any `WHERE` statement but before `ORDER BY` or `HAVING`. It is often used with aggregate functions (`COUNT`, `MAX`, `MIN`, `SUM`, `AVG`) to group the result set by one or more columns.

## Basic Syntax

```sql
SELECT column_name(s), aggregate_function(column_name)
FROM table_name
WHERE condition
GROUP BY column_name(s);
```

- **column_name(s)**: The column(s) you want to select.
- **aggregate_function(column_name)**: An aggregate function (e.g., `SUM`, `COUNT`, `MIN`, `MAX`, `AVG`).
- **table_name**: The table from which to select.
- **condition**: An optional clause to filter rows.
- **GROUP BY column_name(s)**: The column(s) to group data.

## Example 1

Let's start with a basic example:

**Table "order"**

| order_id | customer_id | product_id |
| -------- | ----------- | ---------- |
| 1        | 3           | 1          |
| 2        | 1           | 2          |
| 3        | 2           | 3          |
| 4        | 5           | 1          |

To count the number of orders for each product, we can use the `GROUP BY` statement as follows:

```sql
SELECT product_id, COUNT(order_id) as number_of_orders
FROM order
GROUP BY product_id;
```

Result-set:

| product_id | number_of_orders |
| ---------- | ---------------- |
| 1          | 2                |
| 2          | 1                |
| 3          | 1                |

The result-set indicates that `product_id 1` has been ordered twice, and `product_id 2` and `product_id 3` have been ordered once.

## Example 2 - Using GROUP BY with WHERE Clause

You can use the `GROUP BY` clause with the `WHERE` clause to filter the records before grouping.

```sql
SELECT product_id, COUNT(order_id) as number_of_orders
FROM order
WHERE customer_id > 2
GROUP BY product_id;
```

Result-set:

| product_id | number_of_orders |
| ---------- | ---------------- |
| 1          | 2                |
| 3          | 1                |

The result-set shows the number of orders for each product, but only includes orders from customers with `customer_id` greater than 2.

## Example 3 - Using GROUP BY with HAVING Clause

The `HAVING` clause was added to SQL because the `WHERE` keyword could not be used with aggregate functions. You can use the `HAVING` clause to filter the results of the `GROUP BY` clause.

```sql
SELECT product_id, COUNT(order_id) as number_of_orders
FROM order
GROUP BY product_id
HAVING COUNT(order_id) > 1;
```

Result-set:

| product_id | number_of_orders |
| ---------- | ---------------- |
| 1          | 2                |

The result-set only includes products that have been ordered more than once.

## Example 4 - Using GROUP BY with JOIN Clause

The `GROUP BY` clause can also be used with the `JOIN` clause to group data from multiple tables.

Consider a new table "product":

**Table "product"**

| product_id | product_name | price |
| ---------- | ------------ | ----- |
| 1          | Apple        | 1.00  |
| 2          | Banana       | 0.50  |
| 3          | Cherry       | 2.00  |

To get the total price for each product sold, we can join the "order" table and the "product" table and group the results by the product name.

```sql
SELECT product.product_name, COUNT(order.order_id) as number_of_orders, SUM(product.price) as total_price
FROM order
JOIN product
ON order.product_id = product.product_id
GROUP BY product.product_name;
```

Result-set:

| product_name | number_of_orders | total_price |
| ------------ | ---------------- | ----------- |
| Apple        | 2                | 2.00        |
| Banana       | 1                | 0.50        |
| Cherry       | 1                | 2.00        |

The result set shows the total price for each product sold based on the number of orders.

1. [SQL GROUP BY Statement](https://www.w3schools.com/sql/sql_groupby.asp) - W3Schools
2. [The GROUP BY Statement in SQL](https://www.sqltutorial.org/sql-group-by/) - SQL Tutorial
3. [SQL Aggregate Functions](https://www.w3schools.com/sql/sql_count_avg_sum.asp) - W3Schools
4. [SQL HAVING Clause](https://www.sqlservertutorial.net/sql-server-basics/sql-server-having/) - SQL Server Tutorial
5. [Using JOINs in SQL Server](https://docs.microsoft.com/en-us/sql/t-sql/queries/select-transact-sql?view=sql-server-ver15#using-joins) - Microsoft Docs
6. [Filtering Data with WHERE Clause in SQL Server](https://www.sqlservertutorial.net/sql-server-basics/sql-server-where/) - SQL Server Tutorial
7. [Using GROUP BY with COUNT](https://mode.com/sql-tutorial/sql-group-by/) - Mode Analytics
