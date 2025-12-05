---
title: "Key Concepts"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Relational databases are database management system (DBMS) that organizes data into one or more tables with a unique key identifying each row. The tables are related to each other through foreign keys, which allow data to be combined from multiple tables. Here are some key concepts to understand when working with relational databases:

## **Tables**

Tables are the basic building blocks of a relational database. Each table represents a collection of related data, with each row representing a single record and each column representing a specific attribute of that record. For example, a table of customers might have columns for name, address, and phone number.

Tables can be created using SQL, the standard language used to interact with relational databases. Here is an example of how to create a table in SQL:

```sql
CREATE TABLE customers (
	id INT PRIMARY KEY,
	name VARCHAR(50),
	address VARCHAR(100),
	phone VARCHAR(20)
);
```

This creates a table called "customers" with four columns: "id", "name", "address", and "phone". The "id" column is the primary key, which means that each row in the table will have a unique value for this column.

### **Keys**

Keys are used to identify each row in a table uniquely. The primary key is a column or set of columns uniquely identifying each row in the table. For example, in the "customers" table above, the "id" column is the primary key.

Foreign keys link tables together, with the foreign key in one table referencing the primary key in another table. For example, if we had another table called "orders", we could link it to the "customers" table using the "id" column as the primary key and a foreign key in the "orders" table. Here is an example of how to create a foreign key in SQL:

```sql
CREATE TABLE orders (
	id INT PRIMARY KEY,
	customer_id INT,
	order_date DATE,
	total DECIMAL(10,2),
	FOREIGN KEY (customer_id) REFERENCES customers(id)
);
```

This creates a table called "orders" with four columns: "id", "customer_id", "order_date", and "total". The "customer_id" column is a foreign key that references the "id" column in the "customers" table.

### **Relationships**

Relationships are established between tables using foreign keys. There are three types of relationships: one-to-one, one-to-many, and many-to-many.

In a one-to-one relationship, each row in one table is related to exactly one row in another. For example, if we had a table called "employees" and a table called "employee_details", we could establish a one-to-one relationship using the employee ID as the primary and foreign keys. Each employee would have exactly one row in the "employee_details" table.

In a one-to-many relationship, each row in one table is related to one or more rows in another table. For example, if we had a table called "customers" and a table called "orders", we could establish a one-to-many relationship between them using the customer ID as the primary key in the "customers" table and a foreign key in the "orders" table. Each customer could have multiple orders.

In a many-to-many relationship, each row in one table is related to one or more rows in another table, and vice versa. For example, if we had a table called "students" and a table called "courses", we could establish a many-to-many relationship between them using a third table called "enrollments". The "enrollments" table would have foreign keys referencing the "students" and "courses" tables.

### **Normalization**

Normalization is organizing data in a database to reduce redundancy and improve data integrity. There are several normal forms, with each successive normal form building on the previous one.

The most commonly used normal forms are the first normal form (1NF), second normal form (2NF), and third normal form (3NF).

The first normal form requires that each column in a table contain atomic values, meaning that each value is indivisible. For example, a column containing a comma-separated list of values would violate the first normal form.

The second normal form requires that each non-key column in a table be dependent on the entire primary key, not just part of it. For example, if we had a table with a composite primary key consisting of "customer_id" and "order_id", a column containing only the "order_date" would violate the second normal form.

The third normal form requires that each non-key column in a table be dependent only on the primary key, not on other non-key columns. For example, if we had a table with columns for "customer_name" and "customer_address", both of which were dependent on the "customer_id" primary key, we would violate the third normal form.

### **SQL**

Structured Query Language (SQL) is the standard language used to interact with relational databases. SQL is used to create tables, insert data, update data, and retrieve data from tables.

Here is an example of how to insert data into a table using SQL:

```jsx
INSERT INTO customers (id, name, address, phone)
VALUES (1, 'John Smith', '123 Main St', '555-1234');
```

This inserts a new row into the "customers" table with the values 1, 'John Smith', '123 Main St', and '555-1234' for the "id", "name", "address", and "phone" columns, respectively.

SQL is a powerful tool for working with relational databases and is used by developers, analysts, and data scientists worldwide.
