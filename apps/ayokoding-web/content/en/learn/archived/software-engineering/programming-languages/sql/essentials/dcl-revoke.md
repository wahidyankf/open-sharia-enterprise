---
title: "DCL: REVOKE"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

The `REVOKE` statement in SQL is used to revoke privileges from a user or role.

## Basic Syntax

```sql
REVOKE privilege_type ON object_name FROM user_or_role;
```

- **privilege_type**: The type of privilege to revoke, such as `SELECT`, `INSERT`, `UPDATE`, `DELETE`, `ALL`, etc.
- **object_name**: The name of the object to revoke the privilege from, such as a table, view, or stored procedure.
- **user_or_role**: The user's name or role to revoke the privilege from.

## Example 1 - Revoke SELECT Privilege

If we want to revoke the `SELECT` privilege on the "customer" table from the user "jane", we can use the following SQL:

```sql
REVOKE SELECT ON customer FROM jane;
```

This will remove the user "jane"'s ability to select data from the "customer" table.

## Example 2 - Revoke ALL Privileges

If we want to revoke all privileges on the "product" table from the role "sales", we can use the following SQL:

```sql
REVOKE ALL ON product FROM sales;
```

This will remove the role "sales"'s ability to select, insert, update, and delete data from the "product" table.

## Example 3 - Revoke Privileges with GRANT OPTION

If we want to revoke the `SELECT` privilege on the "customer" table from the user "jane" and remove her ability to grant the same privilege to other users, we can use the following SQL:

```sql
REVOKE SELECT ON customer FROM jane CASCADE;
```

This will remove the user "jane"'s ability to select data from the "customer" table and remove her ability to grant the same privilege to other users.

## Further Readings

1. [Revoking Privileges In MySQL](https://www.mysqltutorial.org/mysql-revoke.aspx) - MySQL Tutorial
2. [SQL Server REVOKE Statement](https://www.sqlservertutorial.net/sql-server-security/sql-server-revoke/) - SQL Server Tutorial
3. [REVOKE (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/statements/revoke-transact-sql?view=sql-server-ver15) - Microsoft Docs
