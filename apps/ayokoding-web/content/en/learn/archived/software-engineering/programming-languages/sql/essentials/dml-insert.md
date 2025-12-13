---
title: "DML: INSERT"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

The `INSERT INTO` statement in SQL inserts new records in a table.

## Basic Syntax

There are two ways to use the `INSERT INTO` statement:

1. **Insert data in all columns**:

   ```sql
   INSERT INTO table_name
   VALUES (value1, value2, ...);
   ```

   Here, you must insert values in the same order as the table's columns.

2. **Insert data in specific columns**:

   ```sql
   INSERT INTO table_name (column1, column2, ...)
   VALUES (value1, value2, ...);
   ```

   In this case, you don't need to insert values for all columns. Insert values in the specified columns in the same order.

## Example 1 - Insert into all columns

For the "customer" table:

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |

If we want to add a new customer, Sally from the UK, to the "customer" table, we can use the following SQL:

```sql
INSERT INTO customer
VALUES (4, 'Sally', 'UK');
```

The "customer" table would now look like this:

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |
| 4           | Sally   | UK      |

## Example 2 - Insert into specific columns

For the "product" table:

| product_id | product_name | price |
| ---------- | ------------ | ----- |
| 1          | Apple        | 1.00  |
| 2          | Banana       | 0.50  |
| 3          | Cherry       | 2.00  |

If we want to add a new product, Dates, with a price of $3.00, to the "product" table, we can use the following SQL:

```sql
INSERT INTO product (product_name, price)
VALUES ('Dates', 3.00);
```

The "product" table would now look like this:

| product_id | product_name | price |
| ---------- | ------------ | ----- |
| 1          | Apple        | 1.00  |
| 2          | Banana       | 0.50  |
| 3          | Cherry       | 2.00  |
| 4          | Dates        | 3.00  |

Here, `product_id` is assumed to be auto-incremented by the database itself, so we didn't need to insert it.

1. [SQL INSERT INTO Statement](https://www.w3schools.com/sql/sql_insert.asp) - W3Schools
2. [Inserting rows into a table](https://learn.microsoft.com/en-us/sql/t-sql/statements/insert-transact-sql?view=sql-server-ver15) - Microsoft Docs
