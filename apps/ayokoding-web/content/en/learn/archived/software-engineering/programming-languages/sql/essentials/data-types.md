---
title: "Data Types"
date: 2025-03-16T07:20:00+07:00
draft: false
---

SQL, or Structured Query Language, is a programming language primarily used for managing and manipulating relational databases. At the heart of these operations are various data types that SQL uses to define the data that can be stored in a particular column in a table.

## Standard SQL Data Types

### Numeric Data Types

1. `integer`: For whole numbers, including both positive and negative values. For example, `123`, `456`.
2. `float`/`real`: For floating point numbers or numbers with decimal points. For example, `123.45`.
3. `decimal`/`numeric`: These are used to store exact numeric values where precision is essential. For example, `123.45`.

### String Data Types

1. `char(n)`: Fixed-length character string, where `n` defines the length. For example, `char(5)` could hold `apple`.
2. `varchar(n)`: Variable-length character string, where `n` defines the maximum string length. `varchar(5)` could hold `apple` or `ap`.
3. **`text`**: For holding large amounts of text data.

### Date and Time Data Types

1. `date`: For date values in the format `YYYY-MM-DD`. For example, `2023-06-03`.
2. `time`: For time values in the format `HH:MI:SS`. For example, `14:30:00`.
3. `timestamp`: For holding date and time values. For example, `2023-06-03 14:30:00`.

## SQL Data Types in Different Databases

While the above data types are common in most SQL databases, various databases (like MySQL, SQL Server, PostgreSQL, and SQLite) may have additional data types or variations of the standard ones. For example:

1. MySQL and PostgreSQL support a `boolean` data type for true/false values, while SQL Server uses `bit`.
2. PostgreSQL has `uuid` for universally unique identifiers.
3. SQL Server offers `money` for currency storage.

## Importance of Choosing Correct Data Types

Choosing the correct data type is essential for multiple reasons:

1. **Data integrity**: By choosing the correct data type, you ensure the data stored in the column adheres to the expected format.
2. **Performance**: Using the appropriate data type can reduce a database's space, improving performance.
3. **Preventing errors**: Certain operations are only valid on certain data types.

## Examples

Creating a table with different data types:

```sql
CREATE TABLE employee (
    id integer,
    first_name varchar(50),
    last_name char(50),
    salary decimal(10, 2),
    birth_date date,
    timestamp_column timestamp
);
```

Inserting data into the table:

```sql
INSERT INTO employee (id, first_name, last_name, salary, birth_date, timestamp_column)
VALUES (1, 'John', 'Doe', 50000.00, '1980-05-01', '2023-06-03 14:30:00');
```

## Further Readings

- [W3Schools - SQL Data Types](https://www.w3schools.com/sql/sql_datatypes.asp)
- [PostgreSQL - Data Types](https://www.postgresql.org/docs/9.5/datatype.html)
- [MySQL - Data Types](https://dev.mysql.com/doc/refman/8.0/en/data-types.html)
