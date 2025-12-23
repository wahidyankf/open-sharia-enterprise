---
title: "How to Work with Databases"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000019
description: "Practical techniques for working with databases in Python using SQLAlchemy, async SQLAlchemy, migrations with Alembic, and connection pooling"
---

## Problem

Python applications need efficient database interaction with proper connection management, type safety, and migration support. Raw SQL strings are error-prone and difficult to maintain across database vendors.

## Solution

### 1. SQLAlchemy Core - SQL Expression Language

```python
from sqlalchemy import create_engine, Table, Column, Integer, String, MetaData, select

# Create engine with connection pooling
engine = create_engine(
    "postgresql://user:password@localhost/dbname",
    pool_size=10,
    max_overflow=20,
    pool_pre_ping=True  # Verify connections before use
)

# Define table schema
metadata = MetaData()
users = Table(
    'users',
    metadata,
    Column('id', Integer, primary_key=True),
    Column('username', String(50), unique=True, nullable=False),
    Column('email', String(100), nullable=False),
    Column('age', Integer)
)

# Create tables
metadata.create_all(engine)

# Insert data
with engine.connect() as conn:
    stmt = users.insert().values(
        username="john_doe",
        email="john@example.com",
        age=30
    )
    result = conn.execute(stmt)
    conn.commit()
    print(f"Inserted user with id: {result.inserted_primary_key[0]}")

# Query data
with engine.connect() as conn:
    stmt = select(users).where(users.c.age > 25)
    result = conn.execute(stmt)

    for row in result:
        print(f"User: {row.username}, Age: {row.age}")
```

### 2. SQLAlchemy ORM - Declarative Models

```python
from sqlalchemy import create_engine, String, Integer
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column, Session, relationship
from typing import List

# Define base class
class Base(DeclarativeBase):
    pass

# Define models
class User(Base):
    __tablename__ = "users"

    id: Mapped[int] = mapped_column(primary_key=True)
    username: Mapped[str] = mapped_column(String(50), unique=True)
    email: Mapped[str] = mapped_column(String(100))
    age: Mapped[int] = mapped_column(Integer, nullable=True)

    # Relationship
    posts: Mapped[List["Post"]] = relationship(back_populates="author")

    def __repr__(self):
        return f"<User(username={self.username}, email={self.email})>"

class Post(Base):
    __tablename__ = "posts"

    id: Mapped[int] = mapped_column(primary_key=True)
    title: Mapped[str] = mapped_column(String(200))
    content: Mapped[str]
    user_id: Mapped[int] = mapped_column(Integer)

    # Relationship
    author: Mapped["User"] = relationship(back_populates="posts")

# Create engine and tables
engine = create_engine("sqlite:///example.db", echo=True)
Base.metadata.create_all(engine)

# CRUD operations
with Session(engine) as session:
    # Create
    new_user = User(username="jane_doe", email="jane@example.com", age=28)
    session.add(new_user)
    session.commit()

    # Read
    user = session.query(User).filter_by(username="jane_doe").first()
    print(f"Found user: {user}")

    # Update
    user.age = 29
    session.commit()

    # Delete
    session.delete(user)
    session.commit()
```

### 3. Async SQLAlchemy for High Concurrency

```python
from sqlalchemy.ext.asyncio import create_async_engine, AsyncSession, async_sessionmaker
from sqlalchemy import select
import asyncio

# Create async engine
async_engine = create_async_engine(
    "postgresql+asyncpg://user:password@localhost/dbname",
    echo=True,
    pool_size=20,
    max_overflow=40
)

# Create async session factory
AsyncSessionLocal = async_sessionmaker(
    async_engine,
    class_=AsyncSession,
    expire_on_commit=False
)

# Async CRUD operations
async def create_user(username: str, email: str):
    async with AsyncSessionLocal() as session:
        user = User(username=username, email=email)
        session.add(user)
        await session.commit()
        return user

async def get_user_by_username(username: str):
    async with AsyncSessionLocal() as session:
        stmt = select(User).where(User.username == username)
        result = await session.execute(stmt)
        return result.scalar_one_or_none()

async def get_users_with_posts():
    """Fetch users with eager loading of posts."""
    async with AsyncSessionLocal() as session:
        stmt = select(User).options(selectinload(User.posts))
        result = await session.execute(stmt)
        users = result.scalars().all()

        for user in users:
            print(f"{user.username} has {len(user.posts)} posts")

# Usage
async def main():
    user = await create_user("async_user", "async@example.com")
    found_user = await get_user_by_username("async_user")
    await get_users_with_posts()

asyncio.run(main())
```

### 4. Database Migrations with Alembic

```bash
# Initialize Alembic in your project
alembic init alembic

# Edit alembic.ini to set database URL
# sqlalchemy.url = postgresql://user:password@localhost/dbname
```

```python
# alembic/env.py - Configure target metadata
from myapp.models import Base

target_metadata = Base.metadata
```

```bash
# Create migration
alembic revision --autogenerate -m "Add users table"

# Apply migration
alembic upgrade head

# Rollback migration
alembic downgrade -1

# View migration history
alembic history
```

**Example Migration File**:

```python
"""Add users table

Revision ID: 001
Revises:
Create Date: 2025-12-21

"""
from alembic import op
import sqlalchemy as sa

# revision identifiers
revision = '001'
down_revision = None
branch_labels = None
depends_on = None

def upgrade() -> None:
    op.create_table(
        'users',
        sa.Column('id', sa.Integer(), nullable=False),
        sa.Column('username', sa.String(50), nullable=False),
        sa.Column('email', sa.String(100), nullable=False),
        sa.Column('created_at', sa.DateTime(), server_default=sa.text('now()')),
        sa.PrimaryKeyConstraint('id'),
        sa.UniqueConstraint('username')
    )

def downgrade() -> None:
    op.drop_table('users')
```

### 5. Connection Pooling Best Practices

```python
from sqlalchemy import create_engine, event
from sqlalchemy.pool import QueuePool
import logging

# Configure connection pooling
engine = create_engine(
    "postgresql://user:password@localhost/dbname",
    poolclass=QueuePool,
    pool_size=10,          # Maintain 10 connections
    max_overflow=20,       # Allow 20 additional connections
    pool_timeout=30,       # Wait 30s for available connection
    pool_recycle=3600,     # Recycle connections after 1 hour
    pool_pre_ping=True,    # Test connections before using
    echo_pool=True         # Log pool checkouts/checkins
)

# Listen to connection events
@event.listens_for(engine, "connect")
def receive_connect(dbapi_conn, connection_record):
    """Called when connection is created."""
    logging.info("New database connection established")

@event.listens_for(engine, "checkout")
def receive_checkout(dbapi_conn, connection_record, connection_proxy):
    """Called when connection is checked out from pool."""
    logging.debug("Connection checked out from pool")

# Context manager for session lifecycle
from contextlib import contextmanager
from sqlalchemy.orm import Session

@contextmanager
def get_db_session():
    """Provide transactional scope for database operations."""
    session = Session(engine)
    try:
        yield session
        session.commit()
    except Exception:
        session.rollback()
        raise
    finally:
        session.close()

# Usage
with get_db_session() as session:
    user = session.query(User).first()
    user.age += 1
    # Automatic commit on success, rollback on exception
```

### 6. Query Optimization and N+1 Prevention

```python
from sqlalchemy.orm import joinedload, selectinload, subqueryload

# ❌ N+1 Query Problem
users = session.query(User).all()
for user in users:
    # Each iteration executes separate query!
    print(f"{user.username} has {len(user.posts)} posts")

# ✅ Solution 1: Joined Load (single query with JOIN)
users = session.query(User).options(joinedload(User.posts)).all()
for user in users:
    print(f"{user.username} has {len(user.posts)} posts")

# ✅ Solution 2: Select In Load (2 queries total)
users = session.query(User).options(selectinload(User.posts)).all()
for user in users:
    print(f"{user.username} has {len(user.posts)} posts")

# ✅ Solution 3: Subquery Load
users = session.query(User).options(subqueryload(User.posts)).all()

# Complex queries with multiple relationships
from sqlalchemy import func

users_with_stats = (
    session.query(
        User.username,
        func.count(Post.id).label('post_count')
    )
    .outerjoin(Post)
    .group_by(User.id, User.username)
    .having(func.count(Post.id) > 5)
    .all()
)
```

## How It Works

```mermaid
graph TD
    A[Application] --> B[SQLAlchemy Session]
    B --> C[Connection Pool]
    C --> D{Available Connection?}
    D -->|Yes| E[Checkout Connection]
    D -->|No| F{Pool Full?}
    F -->|Yes| G[Wait for Connection]
    F -->|No| H[Create New Connection]
    E --> I[Execute Query]
    H --> I
    G --> E
    I --> J[Return Connection to Pool]
    J --> C

    style A fill:#0173B2
    style B fill:#DE8F05
    style C fill:#029E73
    style I fill:#CC78BC
```

**Key Components:**

1. **Engine**: Connection factory and pool manager
2. **Session**: Workspace for ORM operations and transaction management
3. **Connection Pool**: Reusable database connections for efficiency
4. **Unit of Work**: Tracks changes and flushes to database on commit

## Variations

### Repository Pattern for Clean Architecture

```python
from abc import ABC, abstractmethod
from typing import Optional, List

class UserRepository(ABC):
    @abstractmethod
    def get_by_id(self, user_id: int) -> Optional[User]:
        pass

    @abstractmethod
    def get_by_username(self, username: str) -> Optional[User]:
        pass

    @abstractmethod
    def create(self, user: User) -> User:
        pass

    @abstractmethod
    def update(self, user: User) -> User:
        pass

    @abstractmethod
    def delete(self, user_id: int) -> None:
        pass

class SQLAlchemyUserRepository(UserRepository):
    def __init__(self, session: Session):
        self.session = session

    def get_by_id(self, user_id: int) -> Optional[User]:
        return self.session.query(User).filter(User.id == user_id).first()

    def get_by_username(self, username: str) -> Optional[User]:
        return self.session.query(User).filter(User.username == username).first()

    def create(self, user: User) -> User:
        self.session.add(user)
        self.session.flush()
        return user

    def update(self, user: User) -> User:
        self.session.merge(user)
        self.session.flush()
        return user

    def delete(self, user_id: int) -> None:
        user = self.get_by_id(user_id)
        if user:
            self.session.delete(user)
            self.session.flush()

# Usage with dependency injection
def create_user_service(username: str, email: str):
    with get_db_session() as session:
        repo = SQLAlchemyUserRepository(session)
        user = User(username=username, email=email)
        return repo.create(user)
```

### Bulk Operations for Performance

```python
# Bulk insert (efficient for large datasets)
users_data = [
    {"username": f"user_{i}", "email": f"user_{i}@example.com"}
    for i in range(10000)
]

with Session(engine) as session:
    session.bulk_insert_mappings(User, users_data)
    session.commit()

# Bulk update
session.bulk_update_mappings(
    User,
    [{"id": 1, "age": 31}, {"id": 2, "age": 32}]
)
session.commit()
```

### Read Replicas for Scalability

```python
from sqlalchemy import create_engine
from sqlalchemy.orm import Session

# Primary database for writes
primary_engine = create_engine("postgresql://primary/dbname")

# Read replica for reads
replica_engine = create_engine("postgresql://replica/dbname")

class DatabaseRouter:
    """Route queries to appropriate database."""

    def get_session(self, for_write: bool = False) -> Session:
        engine = primary_engine if for_write else replica_engine
        return Session(engine)

# Usage
router = DatabaseRouter()

# Read from replica
with router.get_session(for_write=False) as session:
    users = session.query(User).all()

# Write to primary
with router.get_session(for_write=True) as session:
    user = User(username="new_user", email="new@example.com")
    session.add(user)
    session.commit()
```

## Common Pitfalls

### 1. Not Managing Session Lifecycle

**Problem**: Keeping sessions open too long causes memory leaks.

```python
# ❌ Bad: Long-lived session
session = Session(engine)
user = session.query(User).first()
# ... many operations later ...
session.commit()  # Memory accumulated!

# ✅ Good: Short-lived sessions
with Session(engine) as session:
    user = session.query(User).first()
    session.commit()
# Automatic cleanup
```

### 2. Forgetting to Commit Transactions

**Problem**: Changes not persisted to database.

```python
# ❌ Bad: No commit
session = Session(engine)
user = User(username="test")
session.add(user)
# Changes lost when session closes!

# ✅ Good: Always commit
with Session(engine) as session:
    user = User(username="test")
    session.add(user)
    session.commit()  # Persisted
```

### 3. N+1 Query Problem

**Problem**: Executing separate query for each relationship.

```python
# ❌ Bad: N+1 queries (1 for users + N for posts)
users = session.query(User).all()
for user in users:
    print(len(user.posts))  # Separate query each iteration!

# ✅ Good: Eager loading
users = session.query(User).options(selectinload(User.posts)).all()
for user in users:
    print(len(user.posts))  # No additional queries
```

### 4. Using Mutable Defaults

**Problem**: Shared mutable default values across instances.

```python
# ❌ Bad: Mutable default
class User(Base):
    __tablename__ = "users"
    tags = Column(JSON, default=[])  # Shared across instances!

# ✅ Good: Use server_default or callable
class User(Base):
    __tablename__ = "users"
    tags = Column(JSON, server_default='[]')
    # Or use callable default in Python
    created_at = Column(DateTime, default=datetime.utcnow)
```

### 5. Not Handling Database Errors

**Problem**: Connections fail, queries timeout, constraints violated.

```python
from sqlalchemy.exc import IntegrityError, OperationalError
import logging

# ❌ Bad: No error handling
user = User(username="duplicate")
session.add(user)
session.commit()  # Crashes on duplicate username!

# ✅ Good: Handle specific exceptions
try:
    user = User(username="duplicate")
    session.add(user)
    session.commit()
except IntegrityError as e:
    session.rollback()
    logging.error(f"Duplicate username: {e}")
except OperationalError as e:
    session.rollback()
    logging.error(f"Database connection failed: {e}")
```

## Related Patterns

**Related Tutorial**: See [Intermediate Tutorial - Database Integration](/en/learn/software-engineering/programming-language/python/tutorials/intermediate#database).
**Related How-To**: See [Use Advanced Async Patterns](/en/learn/software-engineering/programming-language/python/how-to/async-patterns-advanced).
**Related Cookbook**: See Cookbook recipe "Database Patterns".
