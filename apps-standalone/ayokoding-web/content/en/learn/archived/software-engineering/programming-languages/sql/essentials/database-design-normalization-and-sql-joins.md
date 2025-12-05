---
title: "Database Design, Normalization, and SQL JOINs"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Database design, normalization, and SQL JOINs are fundamental concepts in managing and manipulating data effectively in relational databases. We will explore these concepts and their relationships with each other.

## Database Design

Database design is producing a detailed data model of a database. This model contains all the logical and physical design choices and physical storage parameters needed to generate a design. A good design can facilitate data integrity and consistency and pave the way for efficient data manipulation operations.

### Example

Consider a simplified library management system. Initially, you might have a single table containing all the information as follows:

**Table: library**

| id  | book_title | author_name | borrower_name |
| --- | ---------- | ----------- | ------------- |
| 1   | Book A     | Author 1    | Borrower 1    |
| 2   | Book B     | Author 2    | Borrower 2    |
| 3   | Book C     | Author 1    | Borrower 3    |
| 4   | Book D     | Author 3    | Borrower 1    |

## Normalization

Normalization is a method of organizing the data in the database to avoid data redundancy, insertion anomaly, update anomaly & deletion anomaly. The process involves getting data to 'normal form' to reduce redundancy and dependency by adequately designing the database's schema or structure.

There are several Normal forms in the theory of normalization:

- **First Normal Form (1NF)**: The table has a primary key, and all columns are atomic, i.e., no repeating groups or arrays.
- **Second Normal Form (2NF)**: It is in 1NF, and all non-key attributes are fully functional and dependent on the primary key.
- **Third Normal Form (3NF)**: It is in 2NF, with no transitive dependencies.

### Example

To eliminate redundancy and inconsistency in our library system, we can normalize the data and split it into multiple tables:

**Table: book**

| id  | title  | author_id |
| --- | ------ | --------- |
| 1   | Book A | 1         |
| 2   | Book B | 2         |
| 3   | Book C | 1         |
| 4   | Book D | 3         |

**Table: author**

| id  | name     |
| --- | -------- |
| 1   | Author 1 |
| 2   | Author 2 |
| 3   | Author 3 |

**Table: borrower**

| id  | name       |
| --- | ---------- |
| 1   | Borrower 1 |
| 2   | Borrower 2 |
| 3   | Borrower 3 |

**Table: transaction**

| id  | book_id | borrower_id |
| --- | ------- | ----------- |
| 1   | 1       | 1           |
| 2   | 2       | 2           |
| 3   | 3       | 3           |
| 4   | 4       | 1           |

We avoid redundancy and ensure data consistency by splitting the data into multiple tables.

## SQL JOINs

JOINs in SQL combine rows from two or more tables based on a related column. There are several types of SQL JOINs:

- **INNER JOIN**: Returns records that have matching values in both tables.
- **LEFT (OUTER) JOIN**: Returns all records from the left table and the matched records from the right table.
- **RIGHT (OUTER) JOIN**: Returns all records from the right table and the matched records from the left table.
- **FULL (OUTER) JOIN**: Returns all records when a match is in the left or right table.

### Example

If we want to find out which books 'Borrower 1' has borrowed, we could use SQL JOINs to combine data from the 'book' and 'transaction' tables based on the matching 'book_id':

```sql
SELECT borrower.name, book.title
FROM borrower
JOIN transaction ON borrower.id = transaction.borrower_id
JOIN book ON transaction.book_id = book.id
WHERE borrower.name = 'Borrower 1';
```

The SQL statement returns a combined result from the 'book' and 'transaction' tables where the 'book_id' matches.

1. [Database Design - W3Schools](https://www.w3schools.com/sql/sql_intro.asp)
2. [Normalization - GeeksforGeeks](https://www.geeksforgeeks.org/introduction-of-database-normalization/)
3. [SQL JOINs - Tutorialspoint](https://www.tutorialspoint.com/sql/sql-using-joins.htm)
4. [Database Systems: The Complete Book by Hector Garcia-Molina, Jeffrey D. Ullman, and Jennifer Widom](https://www.amazon.com/Database-Systems-Complete-Book-2nd/dp/0131873253)
5. [SQL and Relational Theory: How to Write Accurate SQL Code by C.J. Date](https://www.amazon.com/SQL-Relational-Theory-Write-Accurate/dp/1491941170)
