---
title: "Beginner"
date: 2025-12-29T09:07:25+07:00
draft: false
weight: 10000001
description: "Examples 1-30: SQL fundamentals covering installation, data types, basic queries, schema design, joins, and data manipulation (0-40% coverage)"
tags: ["sql", "database", "tutorial", "by-example", "beginner", "fundamentals", "standard-sql"]
---

## Group 1: First Steps

### Example 1: Installing SQLite and First Query

SQLite runs in a Docker container for isolated, reproducible environments across all platforms. This setup creates a lightweight database you can experiment with safely without installing server software.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph LR
    A["Docker Host"]
    B["SQLite Container<br/>nouchka/sqlite3"]
    C["sqlite3 Client<br/>Connected"]
    D["Query Execution<br/>Results"]

    A -->|docker run| B
    B -->|docker exec| C
    C -->|SQL statements| D

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#fff
```

**Code**:

```bash
# One-time setup: Create SQLite container with persistent storage
docker run --name sqlite-tutorial \
  -v sqlite-data:/data \
  -d nouchka/sqlite3:latest tail -f /dev/null
# => Container created with SQLite, data persisted in volume 'sqlite-data'

# Connect to SQLite
docker exec -it sqlite-tutorial sqlite3 /data/tutorial.db
# => Opens SQLite shell connected to tutorial.db file
```

**First query**:

```sql
-- Simple query to verify connection
SELECT sqlite_version();
-- => Returns SQLite version (e.g., "3.45.0")

-- Simple arithmetic
SELECT 2 + 2 AS result;
-- => Returns 4 in column named 'result'

-- String concatenation
SELECT 'Hello, ' || 'SQL!' AS greeting;
-- => Returns "Hello, SQL!" (|| is concatenation operator)

-- Current date and time
SELECT datetime('now') AS current_time;
-- => Returns current UTC timestamp (e.g., "2025-12-29 02:07:25")
```

**Key Takeaway**: SQLite runs in Docker containers with no server configuration needed. The `SELECT` statement executes queries and returns results - even simple expressions work without FROM clauses.

### Example 2: Creating Your First Table

Tables store related data in rows and columns. Each column has a name and data type. Tables are the fundamental storage unit in relational databases.

**Code**:

```sql
-- Create a simple table
CREATE TABLE users (
    id INTEGER,           -- => Integer column for user ID
    name TEXT,            -- => Text column for user name
    email TEXT,           -- => Text column for email address
    age INTEGER           -- => Integer column for age
);
-- => Table 'users' created with 4 columns

-- List all tables
.tables
-- => Shows: users

-- Show table structure
.schema users
-- => Displays CREATE TABLE statement

-- Verify table is empty
SELECT COUNT(*) FROM users;
-- => Returns 0 (no rows yet)
```

**Key Takeaway**: Use `CREATE TABLE` to define structure before storing data. Each column needs a name and type (INTEGER, TEXT, REAL, BLOB). Tables start empty - use INSERT to add rows.

### Example 3: Basic SELECT Queries

SELECT retrieves data from tables. The asterisk (`*`) selects all columns, while specific column names give precise control over what data returns.

**Code**:

```sql
-- Create table and insert sample data
CREATE TABLE products (
    id INTEGER,
    name TEXT,
    price REAL,
    category TEXT
);

INSERT INTO products (id, name, price, category)
VALUES
    (1, 'Laptop', 999.99, 'Electronics'),
    (2, 'Mouse', 29.99, 'Electronics'),
    (3, 'Desk Chair', 199.99, 'Furniture'),
    (4, 'Monitor', 299.99, 'Electronics');
-- => 4 rows inserted

-- Select all columns, all rows
SELECT * FROM products;
-- => Returns all 4 rows with columns: id | name | price | category

-- Select specific columns
SELECT name, price FROM products;
-- => Returns only 'name' and 'price' columns for all rows

-- Select with expressions
SELECT name, price, price * 1.10 AS price_with_tax FROM products;
-- => Calculates new column showing 10% tax
-- => Laptop: 1099.99, Mouse: 32.99, Desk Chair: 219.99, Monitor: 329.99
```

**Key Takeaway**: SELECT retrieves data from tables - use `*` for all columns or name specific columns. You can include expressions and calculations in SELECT to derive new values without modifying stored data.

### Example 4: Inserting Data with INSERT

INSERT adds new rows to tables. You can insert single rows, multiple rows at once, or specify only certain columns (others become NULL).

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["INSERT Statement"]
    B["Specify Columns<br/>(id, name, price)"]
    C["Provide Values<br/>(1, 'Laptop', 999.99)"]
    D["Row Added to Table"]

    A --> B
    B --> C
    C --> D

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#fff
```

**Code**:

```sql
CREATE TABLE inventory (
    id INTEGER,
    item TEXT,
    quantity INTEGER,
    warehouse TEXT
);

-- Insert single row with all columns
INSERT INTO inventory (id, item, quantity, warehouse)
VALUES (1, 'Widget A', 100, 'North');
-- => 1 row inserted

-- Insert multiple rows at once (more efficient)
INSERT INTO inventory (id, item, quantity, warehouse)
VALUES
    (2, 'Widget B', 200, 'South'),
    (3, 'Widget C', 150, 'East'),
    (4, 'Widget D', 75, 'West');
-- => 3 rows inserted in single statement

-- Insert partial columns (others become NULL)
INSERT INTO inventory (id, item)
VALUES (5, 'Widget E');
-- => Inserts row with id=5, item='Widget E', quantity=NULL, warehouse=NULL

-- Verify data
SELECT * FROM inventory;
-- => Returns all 5 rows
```

**Key Takeaway**: INSERT adds rows to tables - specify columns and values explicitly for clarity. Multi-row inserts are more efficient than multiple single-row inserts. Columns not specified get NULL unless a default value is defined.

### Example 5: Updating and Deleting Rows

UPDATE modifies existing rows matching a WHERE condition. DELETE removes rows. Both are dangerous without WHERE clauses - they affect ALL rows.

**Code**:

```sql
CREATE TABLE stock (
    id INTEGER,
    product TEXT,
    quantity INTEGER,
    price REAL
);

INSERT INTO stock (id, product, quantity, price)
VALUES
    (1, 'Apples', 100, 1.50),
    (2, 'Bananas', 150, 0.75),
    (3, 'Oranges', 80, 2.00);

-- Update single row
UPDATE stock
SET quantity = 120
WHERE id = 1;
-- => Updates Apples quantity to 120 (only row with id=1)

-- Update multiple columns
UPDATE stock
SET quantity = 200, price = 0.80
WHERE product = 'Bananas';
-- => Updates both quantity and price for Bananas

-- Update with calculation
UPDATE stock
SET price = price * 1.10
WHERE price < 2.00;
-- => Increases price by 10% for items under $2.00
-- => Apples: 1.50 -> 1.65, Bananas: 0.80 -> 0.88

-- Verify updates
SELECT * FROM stock;

-- Delete specific row
DELETE FROM stock
WHERE id = 3;
-- => Removes Oranges (id=3)

-- DANGEROUS: Update without WHERE affects ALL rows
UPDATE stock SET quantity = 0;
-- => Sets quantity to 0 for ALL remaining items (Apples, Bananas)

-- DANGEROUS: Delete without WHERE removes ALL rows
DELETE FROM stock;
-- => Removes all rows from table (table structure remains)
```

**Key Takeaway**: Always use WHERE clauses with UPDATE and DELETE to target specific rows - omitting WHERE modifies or removes ALL rows. Test your WHERE clause with SELECT before running UPDATE or DELETE.

## Group 2: Data Types

### Example 6: Numeric Types (INTEGER and REAL)

SQLite uses dynamic typing with type affinity. INTEGER stores whole numbers, REAL stores floating-point numbers. Unlike other databases, SQLite is flexible about type enforcement.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Data Types"]
    B["INTEGER<br/>Whole numbers<br/>-9.2e18 to 9.2e18"]
    C["REAL<br/>Floating point<br/>IEEE 754 double"]
    D["TEXT<br/>Character data"]
    E["Type Conversion<br/>CAST function"]
    F["Result"]

    A --> B
    A --> C
    A --> D
    B -->|"CAST AS REAL"| E
    C -->|"CAST AS INTEGER"| E
    B -->|"CAST AS TEXT"| E
    E --> F

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#fff
    style E fill:#CA9161,stroke:#000,color:#fff
    style F fill:#0173B2,stroke:#000,color:#fff
```

**Code**:

```sql
CREATE TABLE numeric_examples (
    id INTEGER,
    whole_number INTEGER,     -- => Stores integers: -9223372036854775808 to 9223372036854775807
    decimal_number REAL,      -- => Stores floating point (IEEE 754 double precision)
    scientific REAL           -- => Can store scientific notation
);

INSERT INTO numeric_examples (id, whole_number, decimal_number, scientific)
VALUES
    (1, 42, 3.14159, 6.022e23),
    (2, -100, 99.99, 1.602e-19);
-- => Inserts numbers demonstrating range and precision

SELECT * FROM numeric_examples;
-- => Returns: id=1, whole_number=42, decimal_number=3.14159, scientific=6.022e+23
-- =>          id=2, whole_number=-100, decimal_number=99.99, scientific=1.602e-19

-- Arithmetic operations
SELECT
    whole_number + 10 AS addition,
    decimal_number * 2 AS multiplication,
    whole_number / 3 AS division,
    whole_number % 3 AS modulo
FROM numeric_examples
WHERE id = 1;
-- => Returns: addition=52, multiplication=6.28318, division=14, modulo=0

-- Type conversion
SELECT
    CAST(decimal_number AS INTEGER) AS truncated,
    CAST(whole_number AS REAL) AS as_real
FROM numeric_examples
WHERE id = 1;
-- => Returns: truncated=3, as_real=42.0
```

**Key Takeaway**: Use INTEGER for whole numbers and REAL for decimals. SQLite's dynamic typing is flexible but can cause unexpected behavior - use explicit CAST when precision matters, especially for financial calculations.

### Example 7: Text Types and String Operations

TEXT stores character data of any length. SQLite treats TEXT, VARCHAR, and CHAR identically (unlike other databases where length matters).

**Code**:

```sql
CREATE TABLE text_examples (
    id INTEGER,
    short_text TEXT,
    long_text TEXT,
    varchar_col VARCHAR(50),  -- => Length hint ignored by SQLite
    char_col CHAR(10)         -- => Length hint ignored by SQLite
);

INSERT INTO text_examples (id, short_text, long_text, varchar_col, char_col)
VALUES
    (1, 'Hello', 'This is a longer text with multiple words', 'VARCHAR example', 'CHAR ex'),
    (2, 'SQL', 'Standard Query Language for databases', 'Another text', '1234567890');

-- String concatenation
SELECT short_text || ' ' || long_text AS combined
FROM text_examples
WHERE id = 1;
-- => Returns: "Hello This is a longer text with multiple words"

-- String functions
SELECT
    UPPER(short_text) AS uppercase,
    LOWER(short_text) AS lowercase,
    LENGTH(short_text) AS length,
    SUBSTR(long_text, 1, 10) AS first_10_chars
FROM text_examples
WHERE id = 1;
-- => Returns: uppercase='HELLO', lowercase='hello', length=5, first_10_chars='This is a '

-- Pattern matching with LIKE
SELECT * FROM text_examples
WHERE long_text LIKE '%database%';
-- => Returns row 2 (contains "database")

-- Replace function
SELECT REPLACE(short_text, 'SQL', 'Structured Query Language') AS replaced
FROM text_examples
WHERE id = 2;
-- => Returns: "Structured Query Language"
```

**Key Takeaway**: TEXT is the primary string type in SQLite and handles any length. Use `||` for concatenation, UPPER/LOWER for case conversion, LIKE for pattern matching, and SUBSTR for extraction.

### Example 8: NULL Handling

NULL represents missing or unknown data. NULL is NOT equal to anything, including itself. Special operators IS NULL and IS NOT NULL test for NULL values.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Test for NULL"]
    B["WHERE column = NULL<br/>❌ WRONG<br/>Always FALSE"]
    C["WHERE column IS NULL<br/>✅ CORRECT<br/>Returns NULL rows"]
    D["WHERE column IS NOT NULL<br/>✅ CORRECT<br/>Returns non-NULL rows"]
    E["COALESCE function<br/>Provide default value"]
    F["NULL in arithmetic<br/>Result is NULL"]

    A --> B
    A --> C
    A --> D
    A --> E
    A --> F

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#029E73,stroke:#000,color:#fff
    style E fill:#CC78BC,stroke:#000,color:#fff
    style F fill:#CA9161,stroke:#000,color:#fff
```

**Code**:

```sql
CREATE TABLE employees (
    id INTEGER,
    name TEXT,
    email TEXT,
    phone TEXT,
    salary REAL
);

INSERT INTO employees (id, name, email, phone, salary)
VALUES
    (1, 'Alice', 'alice@example.com', '555-1234', 75000),
    (2, 'Bob', NULL, '555-5678', 60000),
    (3, 'Charlie', 'charlie@example.com', NULL, 80000),
    (4, 'Diana', NULL, NULL, NULL);
-- => 4 rows inserted with various NULL values

-- WRONG: This doesn't work as expected
SELECT * FROM employees WHERE email = NULL;
-- => Returns 0 rows (NULL = NULL is always false!)

-- CORRECT: Use IS NULL
SELECT * FROM employees WHERE email IS NULL;
-- => Returns Bob and Diana (rows with NULL email)

-- Find rows with non-NULL values
SELECT * FROM employees WHERE phone IS NOT NULL;
-- => Returns Alice and Bob (rows with phone numbers)

-- COALESCE provides default for NULL
SELECT
    name,
    COALESCE(email, 'no-email@example.com') AS email_with_default,
    COALESCE(salary, 0) AS salary_or_zero
FROM employees;
-- => Diana's NULL email becomes 'no-email@example.com', NULL salary becomes 0

-- NULL in calculations
SELECT
    name,
    salary,
    salary * 1.10 AS salary_with_raise
FROM employees;
-- => Diana's salary_with_raise is NULL (NULL in arithmetic produces NULL)
```

**Key Takeaway**: Use `IS NULL` and `IS NOT NULL` to test for missing values - never use `= NULL`. COALESCE provides defaults for NULL values. NULL in arithmetic or comparisons produces NULL.

### Example 9: Date and Time Types

SQLite stores dates and times as TEXT (ISO8601), REAL (Julian day), or INTEGER (Unix timestamp). Use date/time functions for manipulation and formatting.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Date Storage Formats"]
    B["TEXT<br/>ISO8601 strings"]
    C["REAL<br/>Julian days"]
    D["INTEGER<br/>Unix timestamps"]
    E["Date Functions<br/>Convert & Format"]

    A --> B
    A --> C
    A --> D
    B --> E
    C --> E
    D --> E

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#fff
    style E fill:#CA9161,stroke:#000,color:#fff
```

**Code**:

```sql
CREATE TABLE events (
    id INTEGER,
    event_name TEXT,
    event_date TEXT,          -- => Stores date as TEXT (ISO8601: YYYY-MM-DD)
    event_datetime TEXT,      -- => Stores datetime as TEXT (ISO8601: YYYY-MM-DD HH:MM:SS)
    created_at INTEGER        -- => Stores Unix timestamp
);

-- Insert with various date formats
INSERT INTO events (id, event_name, event_date, event_datetime, created_at)
VALUES
    (1, 'Conference', '2025-06-15', '2025-06-15 09:00:00', 1735426800),
    (2, 'Meeting', '2025-07-20', '2025-07-20 14:30:00', 1737369000);

-- Current date and time
SELECT
    date('now') AS current_date,
    time('now') AS current_time,
    datetime('now') AS current_datetime;
-- => Returns current UTC date, time, and datetime

-- Date arithmetic
SELECT
    event_name,
    event_date,
    date(event_date, '+7 days') AS week_later,
    date(event_date, '-1 month') AS month_earlier
FROM events;
-- => Conference week_later: '2025-06-22', month_earlier: '2025-05-15'

-- Extract date parts
SELECT
    event_name,
    STRFTIME('%Y', event_date) AS year,
    STRFTIME('%m', event_date) AS month,
    STRFTIME('%d', event_date) AS day,
    STRFTIME('%w', event_date) AS weekday
FROM events;
-- => Conference: year='2025', month='06', day='15', weekday='0' (Sunday)

-- Date differences
SELECT
    event_name,
    JULIANDAY(event_date) - JULIANDAY('2025-01-01') AS days_from_new_year
FROM events;
-- => Conference: ~165 days from January 1, 2025
```

**Key Takeaway**: Store dates as TEXT in ISO8601 format (YYYY-MM-DD) for readability and portability. Use date(), time(), and datetime() functions for manipulation. STRFTIME() formats dates, JULIANDAY() enables date arithmetic.

### Example 10: Boolean Values and Truthiness

SQLite has no dedicated BOOLEAN type. Use INTEGER with 0 (false) and 1 (true) by convention. Comparisons and logical operators produce 0 or 1.

**Code**:

```sql
CREATE TABLE settings (
    id INTEGER,
    feature_name TEXT,
    is_enabled INTEGER,       -- => 0 = false, 1 = true by convention
    is_visible INTEGER
);

INSERT INTO settings (id, feature_name, is_enabled, is_visible)
VALUES
    (1, 'Dark Mode', 1, 1),
    (2, 'Notifications', 0, 1),
    (3, 'Beta Features', 1, 0);

-- Filter by boolean values
SELECT * FROM settings WHERE is_enabled = 1;
-- => Returns Dark Mode and Beta Features (enabled features)

-- Logical operators produce 0 or 1
SELECT
    feature_name,
    is_enabled,
    is_visible,
    is_enabled AND is_visible AS both_true,
    is_enabled OR is_visible AS either_true,
    NOT is_enabled AS inverted
FROM settings;
-- => Dark Mode: both_true=1, either_true=1, inverted=0
-- => Notifications: both_true=0, either_true=1, inverted=1

-- Comparison operators produce 0 or 1
SELECT
    feature_name,
    (is_enabled = 1) AS explicit_check,
    (id > 2) AS id_comparison
FROM settings;
-- => Dark Mode: explicit_check=1, id_comparison=0
```

**Key Takeaway**: Use INTEGER with 0/1 values to represent boolean data. Logical operators (AND, OR, NOT) and comparisons produce 0 (false) or 1 (true). This convention is portable to other SQL databases.

## Group 3: Filtering and Sorting

### Example 11: WHERE Clause Filtering

WHERE filters rows based on conditions. Only rows where the condition evaluates to true (non-zero) are returned. Combine multiple conditions with AND/OR.

**Code**:

```sql
CREATE TABLE orders (
    id INTEGER,
    customer TEXT,
    amount REAL,
    status TEXT,
    order_date TEXT
);

INSERT INTO orders (id, customer, amount, status, order_date)
VALUES
    (1, 'Alice', 150.00, 'completed', '2025-01-15'),
    (2, 'Bob', 75.50, 'pending', '2025-01-16'),
    (3, 'Charlie', 200.00, 'completed', '2025-01-17'),
    (4, 'Alice', 50.00, 'cancelled', '2025-01-18'),
    (5, 'Diana', 300.00, 'completed', '2025-01-19');

-- Single condition
SELECT * FROM orders WHERE status = 'completed';
-- => Returns rows 1, 3, 5 (completed orders)

-- Numeric comparison
SELECT * FROM orders WHERE amount > 100;
-- => Returns rows 1, 3, 5 (orders over $100)

-- Multiple conditions with AND
SELECT * FROM orders WHERE status = 'completed' AND amount > 150;
-- => Returns rows 3, 5 (completed orders over $150)

-- Multiple conditions with OR
SELECT * FROM orders WHERE customer = 'Alice' OR amount > 250;
-- => Returns rows 1, 4, 5 (Alice's orders or large orders)

-- Negation with NOT or !=
SELECT * FROM orders WHERE status != 'cancelled';
-- => Returns all rows except row 4

-- Range check with BETWEEN
SELECT * FROM orders WHERE amount BETWEEN 50 AND 150;
-- => Returns rows 1, 2, 4 (amounts from $50 to $150 inclusive)

-- List membership with IN
SELECT * FROM orders WHERE customer IN ('Alice', 'Bob');
-- => Returns rows 1, 2, 4 (orders from Alice or Bob)
```

**Key Takeaway**: WHERE filters rows using conditions - comparison operators (=, !=, <, >, <=, >=), BETWEEN for ranges, IN for lists. Combine conditions with AND (both must be true) or OR (either can be true).

### Example 12: Sorting with ORDER BY

ORDER BY sorts query results by one or more columns. Default is ascending (ASC), use DESC for descending. Multiple columns create hierarchical sorting.

**Code**:

```sql
CREATE TABLE students (
    id INTEGER,
    name TEXT,
    grade INTEGER,
    score REAL
);

INSERT INTO students (id, name, grade, score)
VALUES
    (1, 'Alice', 10, 95.5),
    (2, 'Bob', 9, 87.0),
    (3, 'Charlie', 10, 92.0),
    (4, 'Diana', 9, 94.5),
    (5, 'Eve', 10, 88.5);

-- Sort by single column (ascending by default)
SELECT * FROM students ORDER BY score;
-- => Returns: Bob (87.0), Eve (88.5), Charlie (92.0), Diana (94.5), Alice (95.5)

-- Sort descending
SELECT * FROM students ORDER BY score DESC;
-- => Returns: Alice (95.5), Diana (94.5), Charlie (92.0), Eve (88.5), Bob (87.0)

-- Multi-column sort (grade first, then score)
SELECT * FROM students ORDER BY grade, score DESC;
-- => Returns: Diana (grade 9, 94.5), Bob (grade 9, 87.0), Alice (grade 10, 95.5), Charlie (grade 10, 92.0), Eve (grade 10, 88.5)

-- Sort by expression
SELECT name, score, (score * 1.10) AS bonus_score
FROM students
ORDER BY bonus_score DESC;
-- => Calculates bonus_score and sorts by it

-- Sort with NULL handling (NULLs appear first in ASC, last in DESC)
INSERT INTO students (id, name, grade, score) VALUES (6, 'Frank', 10, NULL);
SELECT * FROM students ORDER BY score;
-- => Frank (NULL score) appears first
```

**Key Takeaway**: Use ORDER BY to sort results by one or more columns. ASC (default) sorts low to high, DESC sorts high to low. Multi-column sorting creates hierarchical order (primary sort, then secondary).

### Example 13: Limiting Results with LIMIT and OFFSET

LIMIT restricts the number of rows returned. OFFSET skips a specified number of rows before returning results. Together they enable pagination.

**Code**:

```sql
CREATE TABLE products (
    id INTEGER,
    name TEXT,
    price REAL,
    stock INTEGER
);

INSERT INTO products (id, name, price, stock)
VALUES
    (1, 'Widget A', 10.00, 100),
    (2, 'Widget B', 15.00, 50),
    (3, 'Widget C', 20.00, 75),
    (4, 'Widget D', 12.00, 120),
    (5, 'Widget E', 18.00, 90),
    (6, 'Widget F', 25.00, 60),
    (7, 'Widget G', 30.00, 40),
    (8, 'Widget H', 22.00, 80);

-- Get first 3 products
SELECT * FROM products LIMIT 3;
-- => Returns rows 1, 2, 3

-- Get 3 products starting from the 4th row (0-indexed offset)
SELECT * FROM products LIMIT 3 OFFSET 3;
-- => Returns rows 4, 5, 6

-- Pagination: Page 1 (3 items per page)
SELECT * FROM products ORDER BY price LIMIT 3 OFFSET 0;
-- => Returns 3 cheapest products

-- Pagination: Page 2
SELECT * FROM products ORDER BY price LIMIT 3 OFFSET 3;
-- => Returns next 3 cheapest products

-- Top N query: 5 most expensive products
SELECT * FROM products ORDER BY price DESC LIMIT 5;
-- => Returns Widget G (30), Widget F (25), Widget H (22), Widget C (20), Widget E (18)

-- OFFSET without LIMIT (gets all remaining rows after offset)
SELECT * FROM products OFFSET 5;
-- => Returns rows 6, 7, 8
```

**Key Takeaway**: LIMIT restricts result count, OFFSET skips rows. Use together for pagination: `LIMIT page_size OFFSET (page_number - 1) * page_size`. Always ORDER BY for consistent pagination.

### Example 14: DISTINCT for Unique Values

DISTINCT removes duplicate rows from results. When used with multiple columns, it considers the entire row for uniqueness.

**Code**:

```sql
CREATE TABLE purchases (
    id INTEGER,
    customer TEXT,
    product TEXT,
    quantity INTEGER
);

INSERT INTO purchases (id, customer, product, quantity)
VALUES
    (1, 'Alice', 'Laptop', 1),
    (2, 'Bob', 'Mouse', 2),
    (3, 'Alice', 'Keyboard', 1),
    (4, 'Charlie', 'Mouse', 1),
    (5, 'Bob', 'Monitor', 1),
    (6, 'Alice', 'Mouse', 3);

-- Get all customers (with duplicates)
SELECT customer FROM purchases;
-- => Returns: Alice, Bob, Alice, Charlie, Bob, Alice (6 rows)

-- Get unique customers
SELECT DISTINCT customer FROM purchases;
-- => Returns: Alice, Bob, Charlie (3 rows)

-- Get unique products
SELECT DISTINCT product FROM purchases;
-- => Returns: Laptop, Mouse, Keyboard, Monitor (4 rows)

-- DISTINCT with multiple columns (unique combinations)
SELECT DISTINCT customer, product FROM purchases;
-- => Returns unique (customer, product) pairs
-- => (Alice, Laptop), (Bob, Mouse), (Alice, Keyboard), (Charlie, Mouse), (Bob, Monitor), (Alice, Mouse)

-- Count distinct values
SELECT COUNT(DISTINCT customer) AS unique_customers FROM purchases;
-- => Returns 3 (Alice, Bob, Charlie)
```

**Key Takeaway**: DISTINCT removes duplicate rows from results. With multiple columns, it considers the complete row for uniqueness. Use COUNT(DISTINCT column) to count unique values.

### Example 15: Pattern Matching with LIKE and GLOB

LIKE performs case-insensitive pattern matching with wildcards: `%` (any characters) and `_` (single character). GLOB is case-sensitive with `*` and `?` wildcards.

**Code**:

```sql
CREATE TABLE files (
    id INTEGER,
    filename TEXT,
    size INTEGER,
    extension TEXT
);

INSERT INTO files (id, filename, size, extension)
VALUES
    (1, 'report_2025.pdf', 1024, 'pdf'),
    (2, 'image_001.jpg', 2048, 'jpg'),
    (3, 'Report_Final.PDF', 512, 'PDF'),
    (4, 'data_export.csv', 4096, 'csv'),
    (5, 'photo_vacation.JPG', 3072, 'JPG');

-- LIKE: Case-insensitive, % matches any characters
SELECT * FROM files WHERE filename LIKE '%report%';
-- => Returns rows 1, 3 (both 'report' and 'Report' match)

-- LIKE: _ matches single character
SELECT * FROM files WHERE filename LIKE 'image___%.jpg';
-- => Returns row 2 ('image_001.jpg' - 3 characters after 'image')

-- LIKE: Match file extensions
SELECT * FROM files WHERE filename LIKE '%.pdf';
-- => Returns rows 1, 3 (case-insensitive: both .pdf and .PDF)

-- GLOB: Case-sensitive, * matches any characters
SELECT * FROM files WHERE filename GLOB '*report*';
-- => Returns row 1 only ('report' matches, 'Report' doesn't)

-- GLOB: ? matches single character
SELECT * FROM files WHERE filename GLOB 'photo_*.JPG';
-- => Returns row 5 ('photo_vacation.JPG')

-- NOT LIKE for exclusion
SELECT * FROM files WHERE filename NOT LIKE '%.pdf';
-- => Returns rows 2, 4, 5 (excludes PDF files, but .PDF still matches due to case-insensitive)
```

**Key Takeaway**: Use LIKE for case-insensitive pattern matching (`%` = any characters, `_` = one character). Use GLOB for case-sensitive matching (`*` = any characters, `?` = one character). LIKE is more common across SQL databases.

## Group 4: Aggregation

### Example 16: COUNT, SUM, AVG, MIN, MAX

Aggregate functions compute single values from multiple rows. COUNT counts rows, SUM adds values, AVG calculates mean, MIN/MAX find extremes.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph LR
    A["Multiple Rows"]
    B["Aggregate Function<br/>COUNT/SUM/AVG/MIN/MAX"]
    C["Single Result"]

    A -->|Process all rows| B
    B -->|Compute| C

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
```

**Code**:

```sql
CREATE TABLE sales (
    id INTEGER,
    product TEXT,
    quantity INTEGER,
    price REAL,
    sale_date TEXT
);

INSERT INTO sales (id, product, quantity, price, sale_date)
VALUES
    (1, 'Widget A', 10, 15.00, '2025-01-15'),
    (2, 'Widget B', 5, 25.00, '2025-01-16'),
    (3, 'Widget A', 8, 15.00, '2025-01-17'),
    (4, 'Widget C', 12, 10.00, '2025-01-18'),
    (5, 'Widget B', 3, 25.00, '2025-01-19');

-- Count total rows
SELECT COUNT(*) AS total_sales FROM sales;
-- => Returns 5

-- Count non-NULL values in column
SELECT COUNT(quantity) AS quantity_count FROM sales;
-- => Returns 5 (all quantities are non-NULL)

-- Sum total quantity sold
SELECT SUM(quantity) AS total_quantity FROM sales;
-- => Returns 38 (10 + 5 + 8 + 12 + 3)

-- Average price
SELECT AVG(price) AS average_price FROM sales;
-- => Returns 18.0 ((15 + 25 + 15 + 10 + 25) / 5)

-- Minimum and maximum price
SELECT MIN(price) AS min_price, MAX(price) AS max_price FROM sales;
-- => Returns min_price=10.0, max_price=25.0

-- Multiple aggregates in one query
SELECT
    COUNT(*) AS num_sales,
    SUM(quantity) AS total_qty,
    AVG(price) AS avg_price,
    MIN(price) AS min_price,
    MAX(price) AS max_price
FROM sales;
-- => Returns all 5 aggregates in single row

-- Aggregate with calculation
SELECT SUM(quantity * price) AS total_revenue FROM sales;
-- => Returns 605.0 (10*15 + 5*25 + 8*15 + 12*10 + 3*25)
```

**Key Takeaway**: Aggregate functions reduce multiple rows to single values. COUNT(\*) counts rows, SUM/AVG work on numeric columns, MIN/MAX find extremes. Combine multiple aggregates in one SELECT for comprehensive statistics.

### Example 17: GROUP BY for Categorized Aggregation

GROUP BY partitions rows into groups and applies aggregate functions to each group separately. Commonly combined with aggregates to produce per-category statistics.

**Code**:

```sql
CREATE TABLE transactions (
    id INTEGER,
    account TEXT,
    type TEXT,
    amount REAL,
    transaction_date TEXT
);

INSERT INTO transactions (id, account, type, amount, transaction_date)
VALUES
    (1, 'Alice', 'deposit', 1000.00, '2025-01-15'),
    (2, 'Bob', 'deposit', 500.00, '2025-01-16'),
    (3, 'Alice', 'withdrawal', 200.00, '2025-01-17'),
    (4, 'Charlie', 'deposit', 1500.00, '2025-01-18'),
    (5, 'Bob', 'withdrawal', 100.00, '2025-01-19'),
    (6, 'Alice', 'deposit', 300.00, '2025-01-20');

-- Count transactions per account
SELECT account, COUNT(*) AS num_transactions
FROM transactions
GROUP BY account;
-- => Alice: 3, Bob: 2, Charlie: 1

-- Sum amounts per transaction type
SELECT type, SUM(amount) AS total_amount
FROM transactions
GROUP BY type;
-- => deposit: 3300.00, withdrawal: 300.00

-- Multiple aggregates per group
SELECT
    account,
    COUNT(*) AS num_trans,
    SUM(amount) AS total,
    AVG(amount) AS average,
    MIN(amount) AS smallest,
    MAX(amount) AS largest
FROM transactions
GROUP BY account;
-- => Alice: num_trans=3, total=1100.00, average=366.67, smallest=200.00, largest=1000.00

-- GROUP BY multiple columns
SELECT
    account,
    type,
    SUM(amount) AS total
FROM transactions
GROUP BY account, type;
-- => (Alice, deposit): 1300.00, (Alice, withdrawal): 200.00, (Bob, deposit): 500.00, etc.

-- GROUP BY with WHERE (filter before grouping)
SELECT account, COUNT(*) AS large_transactions
FROM transactions
WHERE amount > 500
GROUP BY account;
-- => Alice: 1, Charlie: 1 (transactions over $500)
```

**Key Takeaway**: GROUP BY partitions rows into categories and applies aggregates to each group. Combine with COUNT/SUM/AVG for per-category statistics. WHERE filters before grouping, HAVING filters after grouping.

### Example 18: HAVING Clause for Filtering Groups

HAVING filters groups AFTER aggregation (unlike WHERE which filters rows BEFORE aggregation). Use HAVING to filter based on aggregate results.

**Code**:

```sql
CREATE TABLE store_sales (
    id INTEGER,
    store TEXT,
    product TEXT,
    revenue REAL
);

INSERT INTO store_sales (id, store, product, revenue)
VALUES
    (1, 'Store A', 'Widget', 1000.00),
    (2, 'Store B', 'Widget', 500.00),
    (3, 'Store A', 'Gadget', 1500.00),
    (4, 'Store C', 'Widget', 800.00),
    (5, 'Store B', 'Gadget', 600.00),
    (6, 'Store A', 'Tool', 300.00);

-- Find stores with total revenue over $1500
SELECT store, SUM(revenue) AS total_revenue
FROM store_sales
GROUP BY store
HAVING SUM(revenue) > 1500;
-- => Store A: 2800.00 (only store with revenue > 1500)

-- Find stores selling more than 2 products
SELECT store, COUNT(*) AS product_count
FROM store_sales
GROUP BY store
HAVING COUNT(*) > 2;
-- => Store A: 3 products

-- Combining WHERE and HAVING
-- WHERE filters rows, HAVING filters groups
SELECT product, COUNT(*) AS store_count, AVG(revenue) AS avg_revenue
FROM store_sales
WHERE revenue > 500
GROUP BY product
HAVING COUNT(*) >= 2;
-- => Widget: 2 stores, avg_revenue=900.00 (Store A and Store C both over $500)

-- HAVING with multiple conditions
SELECT store, SUM(revenue) AS total, COUNT(*) AS products
FROM store_sales
GROUP BY store
HAVING SUM(revenue) > 1000 AND COUNT(*) > 1;
-- => Store A: total=2800.00, products=3

-- HAVING can reference column aliases (SQLite-specific)
SELECT store, SUM(revenue) AS total_revenue
FROM store_sales
GROUP BY store
HAVING total_revenue > 1500;
-- => Store A: 2800.00 (works in SQLite, not all databases)
```

**Key Takeaway**: Use WHERE to filter rows before grouping, HAVING to filter groups after aggregation. HAVING conditions typically use aggregate functions (COUNT, SUM, AVG). WHERE executes first, then GROUP BY, then HAVING.

## Group 5: Joins

### Example 19: INNER JOIN for Matching Rows

INNER JOIN combines rows from two tables where the join condition matches. Only rows with matches in both tables appear in results.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph LR
    A["Table A<br/>Orders"]
    B["Table B<br/>Customers"]
    C["INNER JOIN<br/>ON condition"]
    D["Matched Rows<br/>Result"]

    A --> C
    B --> C
    C --> D

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#fff
```

**Code**:

```sql
CREATE TABLE customers (
    id INTEGER,
    name TEXT,
    email TEXT
);

CREATE TABLE orders (
    id INTEGER,
    customer_id INTEGER,
    product TEXT,
    amount REAL
);

INSERT INTO customers (id, name, email)
VALUES
    (1, 'Alice', 'alice@example.com'),
    (2, 'Bob', 'bob@example.com'),
    (3, 'Charlie', 'charlie@example.com');

INSERT INTO orders (id, customer_id, product, amount)
VALUES
    (1, 1, 'Laptop', 1000.00),
    (2, 2, 'Mouse', 50.00),
    (3, 1, 'Keyboard', 100.00),
    (4, 4, 'Monitor', 300.00);  -- customer_id=4 doesn't exist in customers

-- INNER JOIN: Only matching rows
SELECT
    customers.name,
    customers.email,
    orders.product,
    orders.amount
FROM customers
INNER JOIN orders ON customers.id = orders.customer_id;
-- => Returns 3 rows:
-- => Alice, alice@example.com, Laptop, 1000.00
-- => Bob, bob@example.com, Mouse, 50.00
-- => Alice, alice@example.com, Keyboard, 100.00
-- => Note: Charlie (no orders) and order 4 (no customer) are excluded

-- Table aliases for shorter syntax
SELECT c.name, o.product, o.amount
FROM customers c
INNER JOIN orders o ON c.id = o.customer_id;

-- Multiple conditions in join
SELECT c.name, o.product, o.amount
FROM customers c
INNER JOIN orders o ON c.id = o.customer_id AND o.amount > 100;
-- => Returns: Alice/Laptop/1000.00 (only order over $100)

-- Aggregation with INNER JOIN
SELECT c.name, COUNT(o.id) AS num_orders, SUM(o.amount) AS total_spent
FROM customers c
INNER JOIN orders o ON c.id = o.customer_id
GROUP BY c.id, c.name;
-- => Alice: 2 orders, $1100.00 total
-- => Bob: 1 order, $50.00 total
```

**Key Takeaway**: INNER JOIN combines tables where join conditions match. Only rows with matches in BOTH tables appear. Use table aliases (AS) for cleaner syntax. Rows without matches are excluded.

### Example 20: LEFT JOIN for Optional Matches

LEFT JOIN returns all rows from the left table, with matched rows from the right table. When no match exists, right table columns become NULL.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph LR
    A["LEFT Table<br/>Departments<br/>(All rows)"]
    B["RIGHT Table<br/>Employees<br/>(Matching rows)"]
    C["LEFT JOIN<br/>ON condition"]
    D["Result<br/>All LEFT rows<br/>+ matched RIGHT<br/>+ NULLs"]

    A --> C
    B --> C
    C --> D

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#fff
```

**Code**:

```sql
CREATE TABLE departments (
    id INTEGER,
    name TEXT
);

CREATE TABLE employees (
    id INTEGER,
    name TEXT,
    department_id INTEGER
);

INSERT INTO departments (id, name)
VALUES
    (1, 'Engineering'),
    (2, 'Sales'),
    (3, 'Marketing');

INSERT INTO employees (id, name, department_id)
VALUES
    (1, 'Alice', 1),
    (2, 'Bob', 1),
    (3, 'Charlie', 2);

-- LEFT JOIN: All departments, even those without employees
SELECT
    d.name AS department,
    e.name AS employee
FROM departments d
LEFT JOIN employees e ON d.id = e.department_id;
-- => Returns 4 rows:
-- => Engineering, Alice
-- => Engineering, Bob
-- => Sales, Charlie
-- => Marketing, NULL (no employees in Marketing)

-- Count employees per department (including zero)
SELECT d.name AS department, COUNT(e.id) AS num_employees
FROM departments d
LEFT JOIN employees e ON d.id = e.department_id
GROUP BY d.id, d.name;
-- => Engineering: 2, Sales: 1, Marketing: 0

-- Filter for departments with no employees
SELECT d.name AS department
FROM departments d
LEFT JOIN employees e ON d.id = e.department_id
WHERE e.id IS NULL;
-- => Returns: Marketing

-- LEFT JOIN with WHERE on left table (filter before join)
SELECT d.name, e.name AS employee
FROM departments d
LEFT JOIN employees e ON d.id = e.department_id
WHERE d.name IN ('Engineering', 'Sales');
-- => Excludes Marketing department entirely
```

**Key Takeaway**: LEFT JOIN returns all rows from left table regardless of matches. Right table columns become NULL when no match exists. Use to find missing relationships (WHERE right.id IS NULL).

### Example 21: Self-Joins for Hierarchical Data

Self-joins join a table to itself, useful for hierarchical relationships (employees and managers) or comparing rows within the same table.

**Code**:

```sql
CREATE TABLE employees (
    id INTEGER,
    name TEXT,
    manager_id INTEGER  -- References id in same table
);

INSERT INTO employees (id, name, manager_id)
VALUES
    (1, 'Alice', NULL),      -- CEO (no manager)
    (2, 'Bob', 1),           -- Reports to Alice
    (3, 'Charlie', 1),       -- Reports to Alice
    (4, 'Diana', 2),         -- Reports to Bob
    (5, 'Eve', 2);           -- Reports to Bob

-- Self-join: List employees with their managers
SELECT
    e.name AS employee,
    m.name AS manager
FROM employees e
LEFT JOIN employees m ON e.manager_id = m.id;
-- => Returns:
-- => Alice, NULL (no manager)
-- => Bob, Alice
-- => Charlie, Alice
-- => Diana, Bob
-- => Eve, Bob

-- Find all employees reporting to a specific manager
SELECT
    e.name AS employee
FROM employees e
INNER JOIN employees m ON e.manager_id = m.id
WHERE m.name = 'Bob';
-- => Returns: Diana, Eve

-- Count direct reports per manager
SELECT
    m.name AS manager,
    COUNT(e.id) AS direct_reports
FROM employees m
LEFT JOIN employees e ON e.manager_id = m.id
GROUP BY m.id, m.name;
-- => Alice: 2, Bob: 2, Charlie: 0, Diana: 0, Eve: 0

-- Find employees at same level (same manager)
SELECT
    e1.name AS employee1,
    e2.name AS employee2,
    m.name AS common_manager
FROM employees e1
INNER JOIN employees e2 ON e1.manager_id = e2.manager_id AND e1.id < e2.id
INNER JOIN employees m ON e1.manager_id = m.id;
-- => Returns pairs: (Bob, Charlie, Alice), (Diana, Eve, Bob)
```

**Key Takeaway**: Self-joins treat one table as two separate tables with aliases. Essential for hierarchical data (manager-employee), comparing rows, or finding pairs/groups within same table.

### Example 22: Multiple Joins

Complex queries often join three or more tables. Each JOIN adds another table to the result set, combining data from multiple sources.

**Code**:

```sql
CREATE TABLE authors (
    id INTEGER,
    name TEXT
);

CREATE TABLE books (
    id INTEGER,
    title TEXT,
    author_id INTEGER,
    publisher_id INTEGER
);

CREATE TABLE publishers (
    id INTEGER,
    name TEXT,
    country TEXT
);

INSERT INTO authors (id, name)
VALUES (1, 'Alice Author'), (2, 'Bob Writer');

INSERT INTO publishers (id, name, country)
VALUES (1, 'Pub House A', 'USA'), (2, 'Pub House B', 'UK');

INSERT INTO books (id, title, author_id, publisher_id)
VALUES
    (1, 'SQL Mastery', 1, 1),
    (2, 'Database Design', 2, 1),
    (3, 'Query Optimization', 1, 2);

-- Join three tables
SELECT
    b.title,
    a.name AS author,
    p.name AS publisher,
    p.country
FROM books b
INNER JOIN authors a ON b.author_id = a.id
INNER JOIN publishers p ON b.publisher_id = p.id;
-- => Returns:
-- => SQL Mastery, Alice Author, Pub House A, USA
-- => Database Design, Bob Writer, Pub House A, USA
-- => Query Optimization, Alice Author, Pub House B, UK

-- Aggregation across multiple joins
SELECT
    a.name AS author,
    COUNT(b.id) AS num_books,
    COUNT(DISTINCT p.id) AS num_publishers
FROM authors a
LEFT JOIN books b ON a.id = b.author_id
LEFT JOIN publishers p ON b.publisher_id = p.id
GROUP BY a.id, a.name;
-- => Alice Author: 2 books, 2 publishers
-- => Bob Writer: 1 book, 1 publisher

-- Filter across joined tables
SELECT b.title, a.name AS author
FROM books b
INNER JOIN authors a ON b.author_id = a.id
INNER JOIN publishers p ON b.publisher_id = p.id
WHERE p.country = 'USA';
-- => Returns books published in USA
```

**Key Takeaway**: Chain multiple JOINs to combine data from 3+ tables. Each JOIN references the previous result. Order matters - start with the main table, then add related tables.

## Group 6: Schema Design

### Example 23: Primary Keys for Unique Identification

Primary keys uniquely identify each row in a table. Use INTEGER PRIMARY KEY for auto-incrementing IDs. Primary keys cannot be NULL and must be unique.

**Code**:

```sql
CREATE TABLE users (
    id INTEGER PRIMARY KEY,  -- => Auto-incrementing primary key
    username TEXT NOT NULL,
    email TEXT UNIQUE,       -- => Must be unique across all rows
    created_at TEXT
);

-- Insert with explicit ID
INSERT INTO users (id, username, email, created_at)
VALUES (1, 'alice', 'alice@example.com', '2025-01-15');

-- Insert without ID (auto-increment)
INSERT INTO users (username, email, created_at)
VALUES
    ('bob', 'bob@example.com', '2025-01-16'),
    ('charlie', 'charlie@example.com', '2025-01-17');
-- => SQLite assigns id=2 and id=3 automatically

-- Verify IDs
SELECT * FROM users;
-- => Returns: id=1, 2, 3

-- Try to insert duplicate primary key (fails)
INSERT INTO users (id, username, email) VALUES (1, 'duplicate', 'dup@example.com');
-- => ERROR: UNIQUE constraint failed: users.id

-- Try to insert duplicate email (fails)
INSERT INTO users (username, email) VALUES ('dave', 'alice@example.com');
-- => ERROR: UNIQUE constraint failed: users.email

-- Primary key enables fast lookup
SELECT * FROM users WHERE id = 2;
-- => Returns Bob's row instantly (indexed by primary key)
```

**Key Takeaway**: Use INTEGER PRIMARY KEY for auto-incrementing unique IDs. Primary keys ensure uniqueness and enable fast lookups. UNIQUE constraints enforce uniqueness on non-key columns like email.

### Example 24: Foreign Keys for Relationships

Foreign keys link tables by referencing primary keys in other tables. They enforce referential integrity - prevent orphaned records that reference non-existent parents.

**Code**:

```sql
-- Enable foreign key support (required in SQLite)
PRAGMA foreign_keys = ON;

CREATE TABLE categories (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE products (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    category_id INTEGER,
    price REAL,
    FOREIGN KEY (category_id) REFERENCES categories(id)
);

INSERT INTO categories (id, name)
VALUES (1, 'Electronics'), (2, 'Furniture');

-- Insert product with valid category
INSERT INTO products (name, category_id, price)
VALUES ('Laptop', 1, 999.99);
-- => Success: category_id=1 exists in categories

-- Try to insert product with non-existent category (fails)
INSERT INTO products (name, category_id, price)
VALUES ('Invalid Product', 99, 50.00);
-- => ERROR: FOREIGN KEY constraint failed (category_id=99 doesn't exist)

-- Join tables using foreign key
SELECT p.name AS product, c.name AS category, p.price
FROM products p
INNER JOIN categories c ON p.category_id = c.id;
-- => Returns: Laptop, Electronics, 999.99

-- Try to delete category with products (fails by default)
DELETE FROM categories WHERE id = 1;
-- => ERROR: FOREIGN KEY constraint failed (products reference this category)

-- Must delete products first, then category
DELETE FROM products WHERE category_id = 1;
DELETE FROM categories WHERE id = 1;
-- => Both succeed
```

**Key Takeaway**: Foreign keys enforce referential integrity by linking tables. They prevent orphaned records and deletion of referenced rows. Enable with `PRAGMA foreign_keys = ON` in SQLite before creating tables.

### Example 25: Constraints (NOT NULL, CHECK, DEFAULT)

Constraints enforce data integrity rules. NOT NULL prevents NULL values, CHECK validates conditions, DEFAULT provides fallback values.

**Code**:

```sql
CREATE TABLE products (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,                -- => Cannot be NULL
    price REAL NOT NULL CHECK (price > 0),  -- => Must be positive
    stock INTEGER DEFAULT 0,           -- => Defaults to 0 if not specified
    category TEXT CHECK (category IN ('Electronics', 'Furniture', 'Clothing')),
    created_at TEXT DEFAULT (datetime('now'))  -- => Defaults to current time
);

-- Insert valid row with defaults
INSERT INTO products (name, price, category)
VALUES ('Laptop', 999.99, 'Electronics');
-- => stock=0, created_at=current timestamp (from defaults)

-- Verify defaults
SELECT * FROM products;
-- => Returns: id=1, name='Laptop', price=999.99, stock=0, category='Electronics', created_at='2025-12-29 02:07:25'

-- Try to insert NULL name (fails)
INSERT INTO products (name, price) VALUES (NULL, 50.00);
-- => ERROR: NOT NULL constraint failed: products.name

-- Try to insert negative price (fails)
INSERT INTO products (name, price) VALUES ('Invalid', -10.00);
-- => ERROR: CHECK constraint failed: price > 0

-- Try to insert invalid category (fails)
INSERT INTO products (name, price, category) VALUES ('Toy', 20.00, 'Toys');
-- => ERROR: CHECK constraint failed: category IN (...)

-- Insert with explicit stock (overrides default)
INSERT INTO products (name, price, stock, category)
VALUES ('Mouse', 29.99, 50, 'Electronics');
-- => stock=50 (explicit value overrides default)

-- Multiple constraints on same column
CREATE TABLE accounts (
    id INTEGER PRIMARY KEY,
    balance REAL NOT NULL DEFAULT 0 CHECK (balance >= 0)
);
-- => balance must be non-NULL, defaults to 0, and cannot be negative
```

**Key Takeaway**: Constraints enforce data quality at database level. NOT NULL prevents missing data, CHECK validates conditions, DEFAULT provides fallback values. Constraints prevent invalid data from entering the database.

### Example 26: Indexes for Query Performance

Indexes speed up queries by creating sorted lookup structures. B-tree indexes (default) work for equality and range queries. Trade-off: faster reads, slower writes.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Table Scan<br/>Check every row"]
    B["Index Scan<br/>Use sorted structure"]
    C["Fast Lookup<br/>O(log n) time"]

    A -->|Without index| D["Slow for large tables"]
    B -->|With index| C

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#fff
```

**Code**:

```sql
CREATE TABLE customers (
    id INTEGER PRIMARY KEY,
    email TEXT,
    name TEXT,
    country TEXT
);

-- Insert many rows (simulating large dataset)
INSERT INTO customers (email, name, country)
VALUES
    ('alice@example.com', 'Alice', 'USA'),
    ('bob@example.com', 'Bob', 'UK'),
    ('charlie@example.com', 'Charlie', 'USA');

-- Query without index (table scan)
EXPLAIN QUERY PLAN
SELECT * FROM customers WHERE email = 'alice@example.com';
-- => Shows: SCAN TABLE customers (checks every row)

-- Create index on email column
CREATE INDEX idx_customers_email ON customers(email);
-- => Creates B-tree index for fast email lookups

-- Query with index (index scan)
EXPLAIN QUERY PLAN
SELECT * FROM customers WHERE email = 'alice@example.com';
-- => Shows: SEARCH TABLE customers USING INDEX idx_customers_email (fast!)

-- Unique index (enforces uniqueness + speeds up queries)
CREATE UNIQUE INDEX idx_customers_email_unique ON customers(email);
-- => ERROR if duplicate emails exist

-- Multi-column index (for queries filtering both columns)
CREATE INDEX idx_customers_country_name ON customers(country, name);
-- => Optimizes: WHERE country = 'USA' AND name LIKE 'A%'

-- List all indexes
.indices customers
-- => Shows: idx_customers_email, idx_customers_email_unique, idx_customers_country_name

-- Drop index
DROP INDEX idx_customers_email;
-- => Removes index, queries revert to table scans
```

**Key Takeaway**: Create indexes on columns used in WHERE, JOIN, and ORDER BY to speed up queries. Indexes trade write speed for read speed. Use EXPLAIN QUERY PLAN to verify index usage.

### Example 27: Transactions for Data Consistency

Transactions group multiple statements into atomic units - either all succeed or all fail. Use BEGIN to start, COMMIT to save, ROLLBACK to cancel.

**Code**:

```sql
CREATE TABLE accounts (
    id INTEGER PRIMARY KEY,
    owner TEXT,
    balance REAL NOT NULL CHECK (balance >= 0)
);

INSERT INTO accounts (owner, balance)
VALUES ('Alice', 1000.00), ('Bob', 500.00);

-- Transaction example: Transfer money atomically
BEGIN TRANSACTION;
-- => Start transaction (changes not visible to other connections yet)

UPDATE accounts SET balance = balance - 200 WHERE owner = 'Alice';
-- => Alice balance: 1000.00 -> 800.00 (not committed yet)

UPDATE accounts SET balance = balance + 200 WHERE owner = 'Bob';
-- => Bob balance: 500.00 -> 700.00 (not committed yet)

-- Verify within transaction
SELECT * FROM accounts;
-- => Shows: Alice=800.00, Bob=700.00 (temporary state)

COMMIT;
-- => Saves both updates atomically

-- Verify after commit
SELECT * FROM accounts;
-- => Shows: Alice=800.00, Bob=700.00 (permanent)

-- Transaction with ROLLBACK
BEGIN TRANSACTION;

UPDATE accounts SET balance = balance - 500 WHERE owner = 'Alice';
-- => Alice balance: 800.00 -> 300.00 (tentative)

-- Oops, mistake! Cancel transaction
ROLLBACK;
-- => Discards UPDATE, Alice balance reverts to 800.00

SELECT * FROM accounts;
-- => Shows: Alice=800.00, Bob=700.00 (unchanged)

-- Constraint violation triggers automatic rollback
BEGIN TRANSACTION;
UPDATE accounts SET balance = balance - 1000 WHERE owner = 'Alice';
-- => Would make balance negative (-200), violates CHECK constraint
-- => ERROR: CHECK constraint failed
-- => Transaction automatically rolled back
```

**Key Takeaway**: Use transactions to ensure related changes succeed or fail together. BEGIN starts transaction, COMMIT saves changes, ROLLBACK cancels. Constraint violations automatically rollback transactions.

### Example 28: Views for Query Simplification

Views are saved queries that act like virtual tables. They simplify complex queries, provide abstraction, and can restrict data access.

**Code**:

```sql
CREATE TABLE employees (
    id INTEGER PRIMARY KEY,
    name TEXT,
    department TEXT,
    salary REAL,
    hire_date TEXT
);

CREATE TABLE departments (
    id INTEGER PRIMARY KEY,
    name TEXT,
    budget REAL
);

INSERT INTO employees (name, department, salary, hire_date)
VALUES
    ('Alice', 'Engineering', 120000, '2020-01-15'),
    ('Bob', 'Sales', 80000, '2021-03-20'),
    ('Charlie', 'Engineering', 110000, '2019-06-10');

INSERT INTO departments (id, name, budget)
VALUES (1, 'Engineering', 500000), (2, 'Sales', 300000);

-- Create view: High earners (salary > 100k)
CREATE VIEW high_earners AS
SELECT name, department, salary
FROM employees
WHERE salary > 100000;

-- Query view like a table
SELECT * FROM high_earners;
-- => Returns: Alice (120000), Charlie (110000)

-- Views update automatically when underlying data changes
INSERT INTO employees (name, department, salary, hire_date)
VALUES ('Diana', 'Engineering', 130000, '2022-01-01');

SELECT * FROM high_earners;
-- => Now returns: Alice, Charlie, Diana

-- Create view with JOIN
CREATE VIEW employee_departments AS
SELECT
    e.name AS employee,
    e.salary,
    d.name AS department,
    d.budget
FROM employees e
INNER JOIN departments d ON e.department = d.name;

SELECT * FROM employee_departments WHERE department = 'Engineering';
-- => Returns employees in Engineering with department budget

-- Drop view
DROP VIEW high_earners;
-- => Removes view, underlying table unaffected
```

**Key Takeaway**: Views simplify repetitive queries by saving them as virtual tables. They automatically reflect underlying data changes. Use views for abstraction, security (hide columns), and query reuse.

## Group 7: Advanced Queries

### Example 29: Subqueries in WHERE

Subqueries are queries nested inside other queries. Use in WHERE clauses to filter based on results from another query.

**Code**:

```sql
CREATE TABLE employees (
    id INTEGER PRIMARY KEY,
    name TEXT,
    department TEXT,
    salary REAL
);

INSERT INTO employees (name, department, salary)
VALUES
    ('Alice', 'Engineering', 120000),
    ('Bob', 'Sales', 80000),
    ('Charlie', 'Engineering', 90000),
    ('Diana', 'Sales', 95000),
    ('Eve', 'Marketing', 75000);

-- Find employees earning more than average salary
SELECT name, salary
FROM employees
WHERE salary > (SELECT AVG(salary) FROM employees);
-- => Subquery returns 92000 (average)
-- => Returns: Alice (120000), Diana (95000)

-- Find employees in highest-paid department
SELECT name, department, salary
FROM employees
WHERE department = (
    SELECT department
    FROM employees
    GROUP BY department
    ORDER BY SUM(salary) DESC
    LIMIT 1
);
-- => Subquery returns 'Engineering' (highest total salaries: 210000)
-- => Returns: Alice, Charlie (Engineering employees)

-- IN with subquery: Find employees in departments with budget > 100k
CREATE TABLE departments (dept_name TEXT, budget REAL);
INSERT INTO departments VALUES ('Engineering', 500000), ('Sales', 300000), ('Marketing', 80000);

SELECT name, department
FROM employees
WHERE department IN (
    SELECT dept_name FROM departments WHERE budget > 100000
);
-- => Subquery returns: Engineering, Sales
-- => Returns: Alice, Bob, Charlie, Diana

-- EXISTS: Find departments with employees
SELECT dept_name
FROM departments d
WHERE EXISTS (
    SELECT 1 FROM employees e WHERE e.department = d.dept_name
);
-- => Returns: Engineering, Sales, Marketing (all have employees)
```

**Key Takeaway**: Subqueries enable filtering based on computed values or related data. Use scalar subqueries (return single value) with comparison operators, or list subqueries with IN/EXISTS.

### Example 30: CASE Expressions for Conditional Logic

CASE expressions provide if-then-else logic within SQL. Use for conditional values, categorization, or pivot-like transformations.

**Code**:

```sql
CREATE TABLE products (
    id INTEGER PRIMARY KEY,
    name TEXT,
    price REAL,
    stock INTEGER
);

INSERT INTO products (name, price, stock)
VALUES
    ('Laptop', 999.99, 5),
    ('Mouse', 29.99, 0),
    ('Keyboard', 79.99, 20),
    ('Monitor', 299.99, 2);

-- Simple CASE: Categorize stock levels
SELECT
    name,
    stock,
    CASE
        WHEN stock = 0 THEN 'Out of Stock'
        WHEN stock < 5 THEN 'Low Stock'
        ELSE 'In Stock'
    END AS stock_status
FROM products;
-- => Returns:
-- => Laptop: Low Stock (stock=5 is not <5, so ELSE)
-- => Mouse: Out of Stock
-- => Keyboard: In Stock
-- => Monitor: Low Stock

-- CASE with calculations: Apply tiered discounts
SELECT
    name,
    price,
    CASE
        WHEN price > 500 THEN price * 0.80  -- 20% discount
        WHEN price > 100 THEN price * 0.90  -- 10% discount
        ELSE price * 0.95                    -- 5% discount
    END AS discounted_price
FROM products;
-- => Laptop: 799.99, Mouse: 28.49, Keyboard: 75.99, Monitor: 269.99

-- CASE in aggregation: Count by category
SELECT
    COUNT(CASE WHEN stock = 0 THEN 1 END) AS out_of_stock,
    COUNT(CASE WHEN stock > 0 AND stock < 5 THEN 1 END) AS low_stock,
    COUNT(CASE WHEN stock >= 5 THEN 1 END) AS in_stock
FROM products;
-- => Returns: out_of_stock=1, low_stock=1, in_stock=2

-- Searched CASE (no column after CASE keyword)
SELECT
    name,
    CASE
        WHEN price < 50 THEN 'Budget'
        WHEN price < 200 THEN 'Mid-Range'
        ELSE 'Premium'
    END AS price_tier
FROM products;
-- => Mouse: Budget, Keyboard: Mid-Range, Laptop: Premium, Monitor: Premium
```

**Key Takeaway**: CASE expressions add conditional logic to SELECT statements. Use WHEN-THEN for conditions, ELSE for defaults. Powerful for categorization, pivoting, and conditional aggregation.
