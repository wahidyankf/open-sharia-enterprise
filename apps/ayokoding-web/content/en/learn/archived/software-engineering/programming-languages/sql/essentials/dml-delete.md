---
title: "DML: DELETE"
date: 2025-03-16T07:20:00+07:00
draft: false
---

The `DELETE` statement in SQL is used to delete existing records from a table.

## Basic Syntax

```sql
DELETE FROM table_name
WHERE condition;
```

- **table_name**: The table to delete records from.
- **condition**: An optional clause to filter rows to delete.

It's very important to always use the `WHERE` clause in a `DELETE` statement. Without it, all records in the table will be deleted.

## Example 1 - Basic DELETE

For the "customer" table:

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |
| 4           | Sally   | UK      |

If we want to delete Sally's record from the table, we can use the following SQL:

```sql
DELETE FROM customer
WHERE name = 'Sally';
```

The "customer" table would now look like this:

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |

## Example 2 - Delete All Rows

If we want to delete all rows in a table, we can do so without the `WHERE` clause. However, be careful because this will delete all rows in the table.

```sql
DELETE FROM product;
```

This will delete all rows in the "product" table.

## Further Readings

1. [SQL DELETE Statement](https://www.w3schools.com/sql/sql_delete.asp) - W3Schools
2. [The SQL DELETE Statement](https://www.sqltutorial.org/sql-delete/) - SQL Tutorial
3. [Delete Data In a MySQL Table Using MySQLi and PDO](https://www.w3schools.com/sql/sql_delete.asp) - W3Schools
4. [SQL DELETE JOIN Statement](https://www.mysqltutorial.org/mysql-delete-join/) - MySQL Tutorial
5. [SQL Server DELETE Statement](https://www.sqlservertutorial.net/sql-server-basics/sql-server-delete/) - SQL Server Tutorial
6. [DELETE (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/statements/delete-transact-sql?view=sql-server-ver15) - Microsoft Docs
