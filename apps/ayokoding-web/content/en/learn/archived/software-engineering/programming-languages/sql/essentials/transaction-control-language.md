---
title: "Transaction Control Language"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

A transaction in SQL is a sequence of one or more SQL statements treated as a single unit of work. Transactions are used to ensure data consistency and integrity in a database.

## Basic Syntax

```sql
BEGIN TRANSACTION;
-- SQL statements
COMMIT;
```

- `BEGIN TRANSACTION`: Starts a new transaction.
- `COMMIT`: Ends the current transaction and makes all changes permanent.
- `ROLLBACK`: Ends the current transaction and undoes all changes made since the transaction started.
- `SAVEPOINT`: Creates a savepoint within a transaction that can be used to roll back to a specific point in the transaction.

## Example 1 - Basic Transaction

If we want to update the "customer" table and the "order" table as part of a single transaction, we can use the following SQL:

```sql
BEGIN TRANSACTION;

UPDATE customer
SET country = 'USA'
WHERE name = 'John';

UPDATE order
SET status = 'Shipped'
WHERE customer_id = 1;

COMMIT;
```

This will update the "customer" table and the "order" table as part of a single transaction. All changes will be rolled back if any errors occur during the transaction.

## Example 2 - Rollback Transaction

If we want to update the "customer" table and the "order" table as part of a single transaction, but we want to roll back the changes if any errors occur, we can use the following SQL:

```sql
BEGIN TRANSACTION;

UPDATE customer
SET country = 'USA'
WHERE name = 'John';

UPDATE order
SET status = 'Shipped'
WHERE customer_id = 1;

IF @@ERROR <> 0
BEGIN
    ROLLBACK;
END
ELSE
BEGIN
    COMMIT;
END
```

This will update the "customer" table and the "order" table as part of a single transaction. All changes will be rolled back if any errors occur during the transaction.

## Example 3 - Savepoint

If we want to create a savepoint within a transaction that we can roll back to if necessary, we can use the following SQL:

```sql
BEGIN TRANSACTION;

UPDATE customer
SET country = 'USA'
WHERE name = 'John';

SAVEPOINT update_order;

UPDATE order
SET status = 'Shipped'
WHERE customer_id = 1;

IF @@ERROR <> 0
BEGIN
    ROLLBACK TO update_order;
END
ELSE
BEGIN
    COMMIT;
END
```

This will update the "customer" table and create a savepoint called "update_order". If errors occur during the second update statement, the transaction will be rolled back to the "update_order" savepoint.

## Further Readings

1. [MySQL Transactions](https://www.mysqltutorial.org/mysql-transaction.aspx) - MySQL Tutorial
2. [BEGIN TRANSACTION (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/language-elements/begin-transaction-transact-sql?view=sql-server-ver15) - Microsoft Docs
3. [COMMIT TRANSACTION (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/language-elements/commit-transaction-transact-sql?view=sql-server-ver15) - Microsoft Docs
4. [ROLLBACK TRANSACTION (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/language-elements/rollback-transaction-transact-sql?view=sql-server-ver15) - Microsoft Docs
5. [SAVE TRANSACTION (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/language-elements/save-transaction-transact-sql?view=sql-server-ver15) - Microsoft Docs
