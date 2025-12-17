---
title: "How to Handle Files and Resources"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 512
description: "Master Python file I/O with context managers, pathlib, and proper resource management"
tags: ["python", "files", "io", "pathlib", "context-managers"]
categories: ["learn"]
---

## Problem

File handling requires proper resource cleanup to avoid file descriptor leaks. Python's context managers provide automatic cleanup, but developers often forget to use them or handle encoding incorrectly. pathlib offers modern file handling but many still use outdated os.path patterns.

This guide shows effective file handling in Python.

## Context Managers (with Statement)

### Automatic Resource Cleanup

```python
# ❌ Manual cleanup - error-prone
def read_file_bad(filename):
    file = open(filename)
    try:
        data = file.read()
        return data
    finally:
        file.close()  # Easy to forget

# ✅ Context manager - automatic cleanup
def read_file(filename):
    with open(filename) as file:
        data = file.read()
        return data
    # File automatically closed even if exception occurs

# ✅ Multiple resources
def copy_file(source, dest):
    with open(source, 'rb') as src, open(dest, 'wb') as dst:
        dst.write(src.read())
    # Both files closed automatically
```

**Why it matters**: Context managers guarantee cleanup even when exceptions occur. No forgotten close() calls or resource leaks.

## pathlib for Modern File Handling

### Path Operations

```python
from pathlib import Path

# ✅ Create Path objects
path = Path('data/users.txt')
config_path = Path.home() / '.myapp' / 'config.yaml'

# ✅ Check existence
if path.exists():
    print("File exists")

# ✅ Check file vs directory
if path.is_file():
    print("Is a file")
if path.is_dir():
    print("Is a directory")

# ✅ Get file info
size = path.stat().st_size
modified = path.stat().st_mtime

# ✅ Path components
parent = path.parent  # data
name = path.name      # users.txt
stem = path.stem      # users
suffix = path.suffix  # .txt

# ✅ Join paths
base = Path('data')
file_path = base / 'users' / 'alice.json'
# data/users/alice.json
```

### Reading Files

```python
from pathlib import Path

# ✅ Read entire file
def read_text_file(filename):
    path = Path(filename)
    return path.read_text(encoding='utf-8')

# ✅ Read binary file
def read_binary_file(filename):
    path = Path(filename)
    return path.read_bytes()

# ✅ Read lines
def read_lines(filename):
    path = Path(filename)
    return path.read_text(encoding='utf-8').splitlines()

# ✅ Iterate lines (memory efficient)
def process_large_file(filename):
    path = Path(filename)
    with path.open(encoding='utf-8') as f:
        for line in f:
            process_line(line.strip())

def process_line(line):
    print(line)
```

### Writing Files

```python
from pathlib import Path

# ✅ Write text
def write_text_file(filename, content):
    path = Path(filename)
    path.write_text(content, encoding='utf-8')

# ✅ Write binary
def write_binary_file(filename, data):
    path = Path(filename)
    path.write_bytes(data)

# ✅ Append to file
def append_to_file(filename, content):
    path = Path(filename)
    with path.open('a', encoding='utf-8') as f:
        f.write(content + '\n')

# ✅ Write lines
def write_lines(filename, lines):
    path = Path(filename)
    path.write_text('\n'.join(lines), encoding='utf-8')
```

## Working with Directories

### Directory Operations

```python
from pathlib import Path

# ✅ Create directory
def create_directory(path):
    Path(path).mkdir(exist_ok=True)

# ✅ Create with parents
def create_directory_tree(path):
    Path(path).mkdir(parents=True, exist_ok=True)

# ✅ List directory contents
def list_files(directory):
    path = Path(directory)
    return [f.name for f in path.iterdir() if f.is_file()]

# ✅ List with glob pattern
def find_text_files(directory):
    path = Path(directory)
    return list(path.glob('*.txt'))

# ✅ Recursive glob
def find_all_python_files(directory):
    path = Path(directory)
    return list(path.rglob('*.py'))

# ✅ Walk directory tree
def walk_directory(directory):
    path = Path(directory)
    for item in path.rglob('*'):
        if item.is_file():
            print(f"File: {item}")
        elif item.is_dir():
            print(f"Directory: {item}")
```

### File Operations

```python
from pathlib import Path
import shutil

# ✅ Copy file
def copy_file(source, dest):
    shutil.copy2(source, dest)

# ✅ Move file
def move_file(source, dest):
    Path(source).rename(dest)

# ✅ Delete file
def delete_file(filename):
    Path(filename).unlink(missing_ok=True)

# ✅ Delete directory
def delete_directory(directory):
    shutil.rmtree(directory)

# ✅ Delete empty directory
def delete_empty_directory(directory):
    Path(directory).rmdir()
```

## CSV Files

### Reading CSV

```python
import csv
from pathlib import Path

# ✅ Read CSV with csv.reader
def read_csv(filename):
    rows = []
    with open(filename, newline='', encoding='utf-8') as f:
        reader = csv.reader(f)
        next(reader)  # Skip header
        for row in reader:
            rows.append(row)
    return rows

# ✅ Read CSV with DictReader
def read_csv_dict(filename):
    with open(filename, newline='', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        return list(reader)

# Usage
users = read_csv_dict('users.csv')
for user in users:
    print(user['name'], user['email'])

# ✅ Read CSV into dataclass
from dataclasses import dataclass

@dataclass
class User:
    name: str
    email: str
    age: int

def read_users_csv(filename):
    users = []
    with open(filename, newline='', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        for row in reader:
            users.append(User(
                name=row['name'],
                email=row['email'],
                age=int(row['age'])
            ))
    return users
```

### Writing CSV

```python
import csv

# ✅ Write CSV with csv.writer
def write_csv(filename, rows):
    with open(filename, 'w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerow(['Name', 'Email', 'Age'])  # Header
        writer.writerows(rows)

# ✅ Write CSV with DictWriter
def write_csv_dict(filename, data):
    if not data:
        return

    with open(filename, 'w', newline='', encoding='utf-8') as f:
        fieldnames = data[0].keys()
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(data)

# Usage
users = [
    {'name': 'Alice', 'email': 'alice@example.com', 'age': 25},
    {'name': 'Bob', 'email': 'bob@example.com', 'age': 30},
]
write_csv_dict('users.csv', users)
```

## JSON Files

### Reading and Writing JSON

```python
import json
from pathlib import Path

# ✅ Read JSON
def read_json(filename):
    with open(filename, encoding='utf-8') as f:
        return json.load(f)

# ✅ Write JSON
def write_json(filename, data):
    with open(filename, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=2, ensure_ascii=False)

# ✅ Read JSON with pathlib
def read_json_pathlib(filename):
    path = Path(filename)
    return json.loads(path.read_text(encoding='utf-8'))

# ✅ Write JSON with pathlib
def write_json_pathlib(filename, data):
    path = Path(filename)
    path.write_text(json.dumps(data, indent=2), encoding='utf-8')

# ✅ Handle JSON errors
def safe_read_json(filename):
    try:
        with open(filename, encoding='utf-8') as f:
            return json.load(f)
    except FileNotFoundError:
        return {}
    except json.JSONDecodeError as e:
        print(f"Invalid JSON in {filename}: {e}")
        return {}
```

## Custom Context Managers

### Creating Context Managers

```python
from contextlib import contextmanager

# ✅ Context manager with decorator
@contextmanager
def open_database(db_path):
    conn = sqlite3.connect(db_path)
    try:
        yield conn
    finally:
        conn.close()

# Usage
with open_database('app.db') as conn:
    cursor = conn.cursor()
    cursor.execute('SELECT * FROM users')

# ✅ Context manager class
class DatabaseConnection:
    def __init__(self, db_path):
        self.db_path = db_path
        self.conn = None

    def __enter__(self):
        self.conn = sqlite3.connect(self.db_path)
        return self.conn

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.conn:
            if exc_type is None:
                self.conn.commit()
            else:
                self.conn.rollback()
            self.conn.close()
        return False  # Don't suppress exceptions

# Usage
with DatabaseConnection('app.db') as conn:
    conn.execute('INSERT INTO users VALUES (?, ?)', ('Alice', 'alice@example.com'))
```

## Temporary Files

### Using tempfile

```python
import tempfile
from pathlib import Path

# ✅ Temporary file
def process_with_temp_file():
    with tempfile.NamedTemporaryFile(mode='w', delete=False) as temp:
        temp.write('temporary data')
        temp_path = temp.name

    # File still exists, can be accessed
    process_file(temp_path)

    # Clean up manually
    Path(temp_path).unlink()

# ✅ Auto-delete temporary file
def process_with_auto_delete():
    with tempfile.NamedTemporaryFile(mode='w', delete=True) as temp:
        temp.write('temporary data')
        temp.flush()
        process_file(temp.name)
    # File automatically deleted

# ✅ Temporary directory
def process_with_temp_directory():
    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)
        (temp_path / 'data.txt').write_text('test')
        process_directory(temp_dir)
    # Directory automatically deleted
```

## File Locking

### Preventing Concurrent Access

```python
import fcntl

# ✅ File locking (Unix)
def write_with_lock(filename, content):
    with open(filename, 'w') as f:
        fcntl.flock(f.fileno(), fcntl.LOCK_EX)
        try:
            f.write(content)
        finally:
            fcntl.flock(f.fileno(), fcntl.LOCK_UN)

# ✅ Cross-platform locking with filelock
from filelock import FileLock

def write_with_filelock(filename, content):
    lock = FileLock(f"{filename}.lock")
    with lock:
        with open(filename, 'w') as f:
            f.write(content)
```

## Summary

File handling in Python centers on context managers for automatic resource cleanup. with statements ensure files close even when exceptions occur, preventing resource leaks. Use context managers for all file operations, database connections, and custom resources.

pathlib provides modern, object-oriented file path handling over os.path functions. Path objects support / operator for joining paths, methods for reading/writing files, and queries for file metadata. Use pathlib for new code, os.path only for compatibility.

Reading files uses path.read_text() for small files, iteration with open() for large files. Always specify encoding='utf-8' explicitly. Write files with path.write_text() for simple cases, open() with context manager for control.

Directory operations through pathlib include mkdir() for creation, iterdir() for listing, glob() for pattern matching, rglob() for recursive search. Use exist_ok=True and parents=True for robust directory creation.

CSV handling with csv module provides reader/writer for lists, DictReader/DictWriter for dictionaries. DictReader maps columns to dictionary keys, making code clearer than index-based access. Always specify newline='' when opening CSV files.

JSON operations use json.load() for reading, json.dump() for writing. Set indent for readable output, ensure_ascii=False for Unicode characters. Handle FileNotFoundError and JSONDecodeError appropriately.

Custom context managers implement **enter** and **exit** methods or use @contextmanager decorator. **enter** returns resource, **exit** performs cleanup. Return False from **exit** to propagate exceptions.

Temporary files through tempfile module create files and directories that clean up automatically. Use NamedTemporaryFile for temporary files, TemporaryDirectory for temporary directories. Set delete=False to prevent automatic deletion if needed.

File locking prevents concurrent access issues. Use fcntl on Unix systems, filelock library for cross-platform locking. Lock files before writing to prevent corruption from simultaneous access.

## Related Content

- [Python Best Practices](/en/learn/swe/prog-lang/python/explanation/best-practices)
- [How to Write Effective Tests](/en/learn/swe/prog-lang/python/how-to/write-effective-tests)
- [How to Handle Errors Effectively](/en/learn/swe/prog-lang/python/how-to/handle-errors-effectively)
