---
title: "DDL: DROP"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

SQL DDL or Data Definition Language comprises the SQL commands that can be used to define the database schema. It simply deals with descriptions of the database schema and is used to create and modify the structure of database objects.

One such DDL command is the `DROP` command.

## The DROP Statement

The `DROP` statement in SQL is used to delete a database object. You can use the `DROP` statement to delete a whole database, a table, an index, or a view.

### DROP DATABASE

The `DROP DATABASE` statement is used to drop an existing database in SQL. If the database is successfully dropped, all the tables and views will be deleted.

```sql
DROP DATABASE database_name;
```

### DROP TABLE

The `DROP TABLE` statement is used to drop an existing table in SQL. This will delete the complete data of the table along with the whole schema.

```sql
DROP TABLE table_name;
```

### DROP INDEX

The `DROP INDEX` statement is used to drop an index in SQL. Indexes, which are database objects used to speed up the performance of a server, can be deleted when they are no longer necessary.

```sql
DROP INDEX index_name;
```

### DROP VIEW

The `DROP VIEW` statement is used to drop views. A view is a virtual table based on the result-set of an SQL statement and can be dropped when it is no longer needed.

```sql
DROP VIEW view_name;
```

## Important Note

While `DROP` does delete the respective database object, be aware that this operation is irreversible. Once you drop a database, table, index, or view, it's gone for good, so it's crucial to be certain before executing a `DROP` command.

## Examples

Here are some examples of the `DROP` command:

**1. Drop a database:**

```sql
DROP DATABASE sample_db;
```

This command will drop the database named `sample_db`.

**2. Drop a table:**

```sql
DROP TABLE employee;
```

This command will drop the table named `employee`.

**3. Drop an index:**

```sql
DROP INDEX employee_index;
```

This command will drop the index named `employee_index`.

**4. Drop a view:**

```sql
DROP VIEW employee_view;
```

This command will drop the view named `employee_view`.

## Further Readings

1. [SQL DROP DATABASE Statement - Tutorialspoint](https://www.tutorialspoint.com/sql/sql-drop-database.htm)
2. [DROP TABLE statement - Microsoft SQL Server](https://learn.microsoft.com/en-us/sql/t-sql/statements/drop-table-transact-sql?view=sql-server-ver15)
3. [DROP VIEW statement - Oracle Help Center](https://docs.oracle.com/en/database/oracle/oracle-database/21/sqlrf/DROP-TABLE.html)
