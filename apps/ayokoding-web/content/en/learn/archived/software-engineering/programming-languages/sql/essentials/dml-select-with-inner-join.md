---
title: "DML: SELECT with INNER JOIN"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

The `INNER JOIN` keyword in SQL combines records from two or more tables based on a related column. The `INNER JOIN` keyword selects records that have matching values in both tables.

## Syntax

```sql
SELECT column_name(s)
FROM table_1
INNER JOIN table_2
ON table_1.column_name = table_2.column_name;
```

- `table_1`: First table.
- `table_2`: Second table.
- `column_name(s)`: The name(s) of the column(s) to be retrieved from the table(s).
- `table_1.column_name = table_2.column_name`: The common field between the two tables.

## Simple Example

Consider the following two tables,

**Table: student**

| id  | name      |
| --- | --------- |
| 1   | John Doe  |
| 2   | Jane Doe  |
| 3   | Mary Jane |

**Table: grade**

| id  | subject | grade |
| --- | ------- | ----- |
| 1   | Math    | A     |
| 2   | Math    | B     |
| 3   | English | A     |

Here, the `id` column in both tables is the common field. An `INNER JOIN` of these two tables could look like this:

```sql
SELECT student.name, grade.subject, grade.grade
FROM student
INNER JOIN grade
ON student.id = grade.id;
```

This would output:

| name      | subject | grade |
| --------- | ------- | ----- |
| John Doe  | Math    | A     |
| Jane Doe  | Math    | B     |
| Mary Jane | English | A     |

As you can see, each row in the student table is combined with each row from the grade table with a matching `id`.

## Complex Example

`INNER JOIN` can be used to join more than two tables. Let's add another table to the mix.

**Table: subject**

| id  | name    |
| --- | ------- |
| 1   | Math    |
| 2   | English |
| 3   | Science |

Now, we can join all these tables as shown:

```sql
SELECT student.name, subject.name, grade.grade
FROM ((student
INNER JOIN grade ON student.id = grade.id)
INNER JOIN subject ON grade.subject = subject.id);
```

This would output:

| name      | name    | grade |
| --------- | ------- | ----- |
| John Doe  | Math    | A     |
| Jane Doe  | Math    | B     |
| Mary Jane | English | A     |

## Further Readings

- [SQL INNER JOIN Keyword - W3Schools](https://www.w3schools.com/sql/sql_join_inner.asp)
- [Understanding SQL Joins - MySQLTutorial](https://www.mysqltutorial.org/mysql-inner-join.aspx/)
