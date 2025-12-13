---
title: "DCL: GRANT"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

The `GRANT` statement in SQL grants privileges to a user or role.

## Basic Syntax

```sql
GRANT privilege_type ON object_name TO user_or_role;
```

- **privilege_type**: The type of privilege to grant, such as `SELECT`, `INSERT`, `UPDATE`, `DELETE`, `ALL`, etc.
- **object_name**: The name of the object to grant the privilege on, such as a table, view, or stored procedure.
- **user_or_role**: The user's name or role to grant the privilege to.

## Example 1 - Grant SELECT Privilege

If we want to grant the `SELECT` privilege on the "customer" table to the user "jane", we can use the following SQL:

```sql
GRANT SELECT ON customer TO jane;
```

This will allow the user "jane" to select data from the "customer" table.

## Example 2 - Grant ALL Privileges

If we want to grant all privileges on the "product" table to the role "sales", we can use the following SQL:

```sql
GRANT ALL ON product TO sales;
```

This will allow the role "sales" to select, insert, update, and delete data from the "product" table.

## Example 3 - Grant Privileges with GRANT OPTION

If we want to grant the `SELECT` privilege on the "customer" table to the user "jane" and allow her to grant the same privilege to other users, we can use the following SQL:

```sql
GRANT SELECT ON customer TO jane WITH GRANT OPTION;
```

This will allow the user "jane" to select data from the "customer" table and grant the same privilege to other users.

## Further Readings

1. [Granting Privileges In MySQL](https://www.mysqltutorial.org/mysql-grant.aspx) - MySQL Tutorial
2. [SQL Server GRANT Statement](https://www.sqlservertutorial.net/sql-server-security/sql-server-grant/) - SQL Server Tutorial
3. [GRANT (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/statements/grant-transact-sql?view=sql-server-ver15) - Microsoft Docs
