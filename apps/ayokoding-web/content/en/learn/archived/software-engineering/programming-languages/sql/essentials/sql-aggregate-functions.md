---
title: "SQL Aggregate Functions"
date: 2025-03-16T07:20:00+07:00
draft: false
---

SQL Aggregate Functions are used to calculate a set of values and return a single value. These functions are often used with the `SELECT` statement in SQL. These are several examples of aggregate functions in SQL:

## Examples

### COUNT

The `COUNT` function counts the number of rows in a table or that match a specific condition.

```sql
SELECT COUNT(column_name)
FROM table_name
WHERE condition;
```

For the "orders" table:

| order_id | customer_id | `order_date`          | amount |
| -------- | ----------- | --------------------- | ------ |
| 1        | 1           | `2021-01-01 14:22:10` | 100    |
| 2        | 2           | `2021-01-02 09:52:30` | 200    |
| 3        | 1           | `2021-01-03 18:04:55` | 150    |
| 4        | 3           | `2021-01-04 07:11:40` | 75     |
| 5        | 2           | `2021-01-05 13:59:20` | 300    |

If we want to count the number of orders, we can use the `COUNT` function:

```sql
SELECT COUNT(order_id)
FROM orders;
```

This will return the result:

**COUNT(order_id)**

---

5

---

If we want to count the number of orders for a specific customer, we can use the `COUNT` function with a `WHERE` clause:

```sql
SELECT COUNT(order_id)
FROM orders
WHERE customer_id = 1;
```

This will return the result:

**COUNT(order_id)**

---

2

---

### SUM

The `SUM` function calculates the sum of a set of values.

```sql
SELECT SUM(column_name)
FROM table_name
WHERE condition;
```

If we want to find the total amount of all orders, we can use the `SUM` function:

```sql
SELECT SUM(amount)
FROM orders;
```

This will return the result:

**SUM(amount)**

---

825

---

### AVG

The `AVG` function calculates the average of a set of values.

```sql
SELECT AVG(column_name)
FROM table_name
WHERE condition;
```

If we want to find the average amount of all orders, we can use the `AVG` function:

```sql
SELECT AVG(amount)
FROM orders;
```

This will return the result:

**AVG(amount)**

---

165

---

### MIN

The `MIN` function finds the minimum value in a set of values.

```sql
SELECT MIN(column_name)
FROM table_name
WHERE condition;
```

If we want to find the minimum amount of all orders, we can use the `MIN` function:

```sql
SELECT MIN(amount)
FROM orders;
```

This will return the result:

**MIN(amount)**

---

75

---

### MAX

The `MAX` function finds the maximum value in a set of values.

```sql
SELECT MAX(column_name)
FROM table_name
WHERE condition;
```

If we want to find the maximum amount of all orders, we can use the `MAX` function:

```sql
SELECT MAX(amount)
FROM orders;
```

This will return the result:

**MAX(amount)**

---

300

---

## Further Readings

1. [SQL Aggregate Functions](https://www.w3schools.com/sql/sql_aggregate_functions.asp) - W3Schools
2. [Aggregate Functions](https://www.sqltutorial.org/sql-aggregate-functions/) - SQL Tutorial
3. [SQL Aggregate Functions: A Beginner’s Guide](https://www.databasestar.com/sql-aggregate-functions/) - Database Star
4. [Aggregate Functions in MySQL](https://www.mysqltutorial.org/mysql-aggregate-functions.aspx) - MySQL Tutorial
5. [SQL Server Aggregate Functions](https://www.sqlservertutorial.net/sql-server-aggregate-functions/) - SQL Server Tutorial
6. [Aggregate Functions (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/functions/aggregate-functions-transact-sql?view=sql-server-ver15) - Microsoft Docs
