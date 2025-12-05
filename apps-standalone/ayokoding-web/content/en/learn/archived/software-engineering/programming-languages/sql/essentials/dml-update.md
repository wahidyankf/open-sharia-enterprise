---
title: "DML: UPDATE"
date: 2025-03-16T07:20:00+07:00
draft: false
---

The `UPDATE` statement in SQL is used to modify the existing records in a table.

## Basic Syntax

```sql
UPDATE table_name
SET column1 = value1, column2 = value2, ...
WHERE condition;
```

- **table_name**: The table to update.
- **column1 = value1, column2 = value2, ...**: The column(s) to update and the new value(s).
- **condition**: An optional clause to filter rows to update.

It's very important to always use the `WHERE` clause in an `UPDATE` statement. Without it, all records will be updated.

## Example 1 - Basic UPDATE

For the "customer" table:

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |
| 4           | Sally   | UK      |

If we want to update Sally's country to Canada, we can use the following SQL:

```sql
UPDATE customer
SET country = 'Canada'
WHERE name = 'Sally';
```

The "customer" table would now look like this:

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |
| 4           | Sally   | Canada  |

## Example 2 - Update Multiple Columns

If we want to update both Sally's name and country to Sam and Australia, we can use the following SQL:

```sql
UPDATE customer
SET name = 'Sam', country = 'Australia'
WHERE customer_id = 4;
```

The "customer" table would now look like this:

| customer_id | name    | country   |
| ----------- | ------- | --------- |
| 1           | John    | USA       |
| 2           | Michael | Germany   |
| 3           | Sarah   | France    |
| 4           | Sam     | Australia |

## Example 3 - Update All Rows

If we want to update all rows in a column, we can do so without the `WHERE` clause. However, be careful because this will change all rows in the specified column(s).

```sql
UPDATE product
SET price = price * 1.1;
```

This will increase the price of all products by 10%.

1. [SQL UPDATE Statement](https://www.w3schools.com/sql/sql_update.asp) - W3Schools
2. [The SQL UPDATE Statement](https://www.sqltutorial.org/sql-update/) - SQL Tutorial
3. [Update Data In a MySQL Table Using MySQLi and PDO](https://www.w3schools.com/sql/sql_update.asp) - W3Schools
4. [SQL Server UPDATE Statement](https://www.sqlservertutorial.net/sql-server-basics/sql-server-update/) - SQL Server Tutorial
5. [UPDATE (Transact-SQL)](https://docs.microsoft.com/en-us/sql/t-sql/queries/update-transact-sql?view=sql-server-ver15) - Microsoft Docs
