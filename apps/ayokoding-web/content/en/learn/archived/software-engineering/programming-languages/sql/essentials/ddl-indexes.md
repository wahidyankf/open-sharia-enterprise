---
title: "DDL: Indexes"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

In SQL, an index is a database object used to speed up data retrieval from a table. An index is created on one or more columns of a table, and it contains a sorted list of values from those columns, along with a pointer to the location of the corresponding data in the table.

## Types of Indexes

There are several types of indexes in SQL:

### Clustered Index

A clustered index is an index that determines the physical order of data in a table. A table can have only one clustered index, which is created on the table's primary key by default.

### Example

```sql
CREATE CLUSTERED INDEX idx_orders_order_id
ON orders (order_id);
```

### Non-Clustered Index

A non-clustered index is an index that does not determine the physical order of data in a table. A table can have multiple non-clustered indexes, and they are created on one or more columns of the table.

### Example

```sql
CREATE NONCLUSTERED INDEX idx_orders_customer_id
ON orders (customer_id);
```

### Unique Index

A unique index is an index that ensures that the values in a column or a set of columns are unique. A table can have multiple unique indexes, and they are created on one or more columns.

### Example

```sql
CREATE UNIQUE INDEX idx_customers_email
ON customers (email);
```

### Full-Text Index

A full-text index is an index that is used to perform full-text searches on a table. It is created on one or more table columns containing text data.

### Example

```sql
CREATE FULLTEXT INDEX idx_products_description
ON products (description);
```

## Creating an Index

To create an index in SQL, you can use the `CREATE INDEX` statement:

```sql
CREATE INDEX index_name
ON table_name (column1, column2, ...);
```

For example, to create a non-clustered index on the "orders" table for the "customer_id" column, you can use the following SQL statement:

```sql
CREATE INDEX idx_orders_customer_id
ON orders (customer_id);
```

## Using an Index

To use an index in SQL, you can include the `INDEX` hint in your SQL statement:

```sql
SELECT column1, column2, ...
FROM table_name WITH (INDEX (index_name))
WHERE condition;
```

For example, to use the "idx_orders_customer_id" index in the previous example, you can use the following SQL statement:

```sql
SELECT *
FROM orders WITH (INDEX (idx_orders_customer_id))
WHERE customer_id = 1;
```

## Dropping an Index

To drop an index in SQL, you can use the `DROP INDEX` statement:

```sql
DROP INDEX index_name
ON table_name;
```

For example, to drop the "idx_orders_customer_id" index in the previous example, you can use the following SQL statement:

```sql
DROP INDEX idx_orders_customer_id
ON orders;
```

## When to Use and Not to Use an Index

While indexes can speed up data retrieval from a table, they are not always the best solution. Here are some guidelines for when to use and not to use an index:

### When to Use an Index

- When you frequently query a table based on a specific column or set of columns.
- When you frequently join a table with another table based on a specific column or set of columns.
- When you frequently sort or group data based on a specific column or set of columns.
- When you need to enforce the uniqueness of data in a column or set of columns.

### When Not to Use an Index

- When you have a small table that is rarely queried.
- When you have a frequently modified table (inserted, updated, or deleted).
- When you have a table with a low selectivity column (a column with many duplicate values).
- When you have a table with a column that contains long strings or binary data.

Therefore, it is essential to carefully consider when to use and not to use an index before adding one to a table.

## The trade-off of Creating and Using an Index

While indexes can speed up the retrieval of data from a table, they also have some trade-offs:

### Pros

- Indexes can speed up data retrieval from a table, especially for large tables.
- Indexes can improve the performance of queries that involve sorting or grouping data.
- Indexes can enforce the uniqueness of data in a table.

### Cons

- Indexes can slow down the performance of data modification operations, such as inserting, updating, or deleting data from a table.
- Indexes can take up disk space, especially for large tables with multiple indexes.
- Indexes can become fragmented over time, which can reduce their performance.

Therefore, it is essential to carefully consider the trade-offs of creating and using an index before adding one to a table.

## Further Readings

- [SQL Server Indexes](https://www.sqlservertutorial.net/sql-server-indexes/) - SQL Server Tutorial
- [MySQL Indexes](https://www.mysqltutorial.org/mysql-index/) - MySQL Tutorial
- [PostgreSQL Indexes](https://www.postgresql.org/docs/current/indexes.html) - PostgreSQL Documentation
- [Oracle Indexes](https://docs.oracle.com/en/database/oracle/oracle-database/19/cncpt/indexes-and-index-organized-tables.html#GUID-5E7C5B3D-7B3C-4C5C-9C5C-9E9B7B7C7C5C) - Oracle Documentation
