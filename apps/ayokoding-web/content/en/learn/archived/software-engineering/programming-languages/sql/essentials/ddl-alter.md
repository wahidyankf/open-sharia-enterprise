---
title: "DDL: ALTER"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

The SQL `ALTER` command is a Data Definition Language (DDL) statement used to modify the structure of an existing database object like a table, view, index, etc. It's an essential tool for database schema management.

## Basic Syntax

The general syntax for the `ALTER` command for a table is as follows:

```sql
ALTER TABLE table_name
action;
```

The `action` in this context is what you want to do. This could be adding a new column, changing the column's data type, renaming a column, etc.

## Key ALTER Operations

### Adding a Column

If you wish to add a new column to a table, use the `ALTER TABLE` command with the `ADD` operation.

```sql
ALTER TABLE table_name
ADD column_name column_type;
```

Example:

```sql
ALTER TABLE employee
ADD birthdate DATE;
```

Consider the "employee" table:

| employee_id | first_name | last_name | email               |
| ----------- | ---------- | --------- | ------------------- |
| 1           | John       | Doe       | john.doe@email.com  |
| 2           | Jane       | Doe       | jane.doe@email.com  |
| 3           | Bob        | Smith     | bob.smith@email.com |

After adding the "birthdate" column, the table will look like:

| employee_id | first_name | last_name | email                      | birthdate |
| ----------- | ---------- | --------- | -------------------------- | --------- |
| 1           | John       | Doe       | mailto:john.doe@email.com  |           |
| 2           | Jane       | Doe       | mailto:jane.doe@email.com  |           |
| 3           | Bob        | Smith     | mailto:bob.smith@email.com |           |

### Modifying a Column

If you want to modify the data type of an existing column, you can use the `ALTER TABLE` command with the `MODIFY` clause.

```sql
ALTER TABLE table_name
MODIFY column_name column_type;
```

Example:

```sql
ALTER TABLE employee
MODIFY birthdate TIMESTAMP;
```

The structure remains the same as above.

### Dropping a Column

You can also remove a column from a table using the `ALTER TABLE` command with the `DROP COLUMN` clause.

```sql
ALTER TABLE table_name
DROP COLUMN column_name;
```

Example:

```sql
ALTER TABLE employee
DROP COLUMN birthdate;
```

After removing the "birthdate" column, the table goes back to its initial state:

| employee_id | first_name | last_name | email               |
| ----------- | ---------- | --------- | ------------------- |
| 1           | John       | Doe       | john.doe@email.com  |
| 2           | Jane       | Doe       | jane.doe@email.com  |
| 3           | Bob        | Smith     | bob.smith@email.com |

### Renaming a Column

To rename a column in a table, you can use the `ALTER TABLE` command with the `RENAME COLUMN` clause.

```sql
ALTER TABLE table_name
RENAME COLUMN old_column_name TO new_column_name;
```

Example:

```sql
ALTER TABLE employee
RENAME COLUMN birthdate TO date_of_birth;
```

After renaming the "birthdate" column to "date_of_birth", the table will look like:

| employee_id | first_name | last_name | email               | date_of_birth |
| ----------- | ---------- | --------- | ------------------- | ------------- |
| 1           | John       | Doe       | john.doe@email.com  |               |
| 2           | Jane       | Doe       | jane.doe@email.com  |               |
| 3           | Bob        | Smith     | bob.smith@email.com |               |

Please note that the syntax may vary slightly depending on your SQL dialect (like Oracle, MySQL, SQL Server, etc.).

## Further Readings

- [W3Schools: SQL ALTER TABLE Statement](https://www.w3schools.com/sql/sql_alter.asp)
- [Oracle: ALTER TABLE](https://docs.oracle.com/cd/B19306_01/server.102/b14200/statements_3001.htm)
- [SQL Server: ALTER TABLE (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/statements/alter-table-transact-sql?view=sql-server-ver15)
- [MySQL: ALTER TABLE Syntax](https://dev.mysql.com/doc/refman/8.0/en/alter-table.html)
