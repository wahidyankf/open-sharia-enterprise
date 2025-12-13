---
title: "DML: SELECT FROM"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

The `SELECT` statement is one of the most critical parts of SQL, and the `FROM` keyword is an essential part of it. They are part of SQL's Data Manipulation Language (DML) - a subset of SQL used for adding, updating, and deleting data in a database.

## The SELECT Statement

The `SELECT` statement is used to select data from a database. The data returned is stored in a result table called the result-set.

The basic syntax is as follows:

```sql
SELECT column1, column2, ...
FROM table_name;
```

Here, `column1, column2, ...` are the field names of the table you want to select data from. If you want to select all the fields available in the table, use the following syntax:

```sql
SELECT *
FROM table_name;
```

## The FROM Keyword

The `FROM` keyword specifies the table from which to retrieve the data. The table name follows the `FROM` keyword.

## Examples

Let's consider a `employee` table with the following data:

| employee_id | first_name | last_name | department |
| ----------- | ---------- | --------- | ---------- |
| 1           | John       | Doe       | IT         |
| 2           | Jane       | Smith     | HR         |
| 3           | Mike       | Davis     | Sales      |

If we want to select only the `first_name` and `last_name` for each record, we can use:

```sql
SELECT first_name, last_name
FROM employee;
```

This would return:

| first_name | last_name |
| ---------- | --------- |
| John       | Doe       |
| Jane       | Smith     |
| Mike       | Davis     |

And if we want to select all the information for each employee, we can use:

```sql
SELECT *
FROM employee;
```

This would return all data in the `employee` table.

## Further Readings

- [SQL SELECT statement - W3Schools](https://www.w3schools.com/sql/sql_select.asp)
- [SQL: SELECT Statement - SQLCourse](http://www.sqlcourse.com/select.html)
- [SQL SELECT Statement - SQL Tutorial](https://www.sqltutorial.org/sql-select/)
