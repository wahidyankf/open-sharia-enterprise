---
title: "DML: SELECT with ORDER BY"
date: 2025-03-16T07:20:00+07:00
draft: false
---

The `SELECT` statement in SQL is used to select data from a database. The data returned is stored in a result table, sometimes called the result set.

The `ORDER BY` keyword is used to sort the result-set in ascending or descending order according to some column(s).

## Basic Syntax

```sql
SELECT column1, column2, ...
FROM table_name
ORDER BY column1, column2, ... ASC|DESC;
```

- **column1, column2, ...**: The fields that you want to select.
- **table_name**: The name of the table.
- **column1, column2, ...**: The field(s) to sort by.
- **ASC|DESC**: Ascending or descending order. Ascending is the default.

## Example 1

Let's start with a basic example:

**Table "product"**

| product_id | product_name | price |
| ---------- | ------------ | ----- |
| 1          | Apple        | 1.00  |
| 2          | Banana       | 0.50  |
| 3          | Cherry       | 2.00  |
| 4          | Dates        | 3.00  |

We can select all products and order them by price in descending order:

```sql
SELECT product_id, product_name, price
FROM product
ORDER BY price DESC;
```

The result-set will look like this:

| product_id | product_name | price |
| ---------- | ------------ | ----- |
| 4          | Dates        | 3.00  |
| 3          | Cherry       | 2.00  |
| 1          | Apple        | 1.00  |
| 2          | Banana       | 0.50  |

## Example 2 - Order by More than One Column

You can also order by more than one column. The specified first column will sort the data, then by the next column.

Let's add a new column, "category" to the table "product":

**Table "product"**

| product_id | product_name | category  | price |
| ---------- | ------------ | --------- | ----- |
| 1          | Apple        | Fruit     | 1.00  |
| 2          | Banana       | Fruit     | 0.50  |
| 3          | Carrot       | Vegetable | 0.80  |
| 4          | Dates        | Fruit     | 3.00  |
| 5          | Eggplant     | Vegetable | 1.50  |

Now, let's select all products and order them by category in ascending order, and then by price in descending order:

```sql
SELECT product_id, product_name, category, price
FROM product
ORDER BY category ASC, price DESC;
```

The result set will look like this:

| product_id | product_name | category  | price |
| ---------- | ------------ | --------- | ----- |
| 4          | Dates        | Fruit     | 3.00  |
| 1          | Apple        | Fruit     | 1.00  |
| 2          | Banana       | Fruit     | 0.50  |
| 5          | Eggplant     | Vegetable | 1.50  |
| 3          | Carrot       | Vegetable | 0.80  |

1. [SQL ORDER BY Keyword](https://www.w3schools.com/sql/sql_orderby.asp) - W3Schools
2. [The SQL ORDER BY statement](https://www.sqltutorial.org/sql-order-by/) - SQL Tutorial
