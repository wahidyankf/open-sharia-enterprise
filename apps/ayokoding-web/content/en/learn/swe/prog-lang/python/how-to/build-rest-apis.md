---
title: "How to Build REST APIs"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000020
description: "Practical techniques for building REST APIs in Python using FastAPI, Pydantic validation, dependency injection, and async endpoints"
---

## Problem

Building REST APIs requires request validation, proper error handling, documentation generation, and high performance. Traditional Flask approaches lack type safety and require manual validation logic.

## Solution

### 1. Basic FastAPI Application

```python
from fastapi import FastAPI, HTTPException, status
from pydantic import BaseModel, EmailStr, Field
from typing import List, Optional

app = FastAPI(
    title="User API",
    description="REST API for user management",
    version="1.0.0"
)

# Pydantic models for validation
class UserCreate(BaseModel):
    username: str = Field(..., min_length=3, max_length=50)
    email: EmailStr
    age: Optional[int] = Field(None, ge=0, le=150)

class UserResponse(BaseModel):
    id: int
    username: str
    email: str
    age: Optional[int]

    class Config:
        from_attributes = True  # Enable ORM mode

# In-memory storage (use database in production)
users_db = {}
user_id_counter = 1

@app.post("/users", response_model=UserResponse, status_code=status.HTTP_201_CREATED)
async def create_user(user: UserCreate):
    """Create a new user."""
    global user_id_counter

    # Check for duplicate username
    if any(u["username"] == user.username for u in users_db.values()):
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Username already exists"
        )

    # Create user
    user_data = user.dict()
    user_data["id"] = user_id_counter
    users_db[user_id_counter] = user_data
    user_id_counter += 1

    return user_data

@app.get("/users", response_model=List[UserResponse])
async def list_users(skip: int = 0, limit: int = 10):
    """List all users with pagination."""
    users_list = list(users_db.values())
    return users_list[skip : skip + limit]

@app.get("/users/{user_id}", response_model=UserResponse)
async def get_user(user_id: int):
    """Get user by ID."""
    if user_id not in users_db:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="User not found"
        )
    return users_db[user_id]

@app.put("/users/{user_id}", response_model=UserResponse)
async def update_user(user_id: int, user: UserCreate):
    """Update user by ID."""
    if user_id not in users_db:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="User not found"
        )

    user_data = user.dict()
    user_data["id"] = user_id
    users_db[user_id] = user_data
    return user_data

@app.delete("/users/{user_id}", status_code=status.HTTP_204_NO_CONTENT)
async def delete_user(user_id: int):
    """Delete user by ID."""
    if user_id not in users_db:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="User not found"
        )
    del users_db[user_id]
```

### 2. Dependency Injection for Database Sessions

```python
from fastapi import Depends
from sqlalchemy.ext.asyncio import AsyncSession, create_async_engine, async_sessionmaker
from typing import AsyncGenerator

# Database setup
DATABASE_URL = "postgresql+asyncpg://user:password@localhost/dbname"
engine = create_async_engine(DATABASE_URL, echo=True)
AsyncSessionLocal = async_sessionmaker(engine, class_=AsyncSession, expire_on_commit=False)

# Dependency for database sessions
async def get_db() -> AsyncGenerator[AsyncSession, None]:
    """Provide database session for request."""
    async with AsyncSessionLocal() as session:
        try:
            yield session
            await session.commit()
        except Exception:
            await session.rollback()
            raise

# Use dependency in endpoints
@app.post("/users", response_model=UserResponse)
async def create_user(
    user: UserCreate,
    db: AsyncSession = Depends(get_db)
):
    """Create user with database persistence."""
    db_user = User(**user.dict())
    db.add(db_user)
    await db.flush()
    await db.refresh(db_user)
    return db_user

@app.get("/users", response_model=List[UserResponse])
async def list_users(
    skip: int = 0,
    limit: int = 10,
    db: AsyncSession = Depends(get_db)
):
    """List users from database."""
    from sqlalchemy import select

    stmt = select(User).offset(skip).limit(limit)
    result = await db.execute(stmt)
    return result.scalars().all()
```

### 3. Authentication and Authorization

```python
from fastapi import Depends, HTTPException, status
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from jose import JWTError, jwt
from passlib.context import CryptContext
from datetime import datetime, timedelta
from typing import Optional

# Security configuration
SECRET_KEY = "your-secret-key-keep-it-secret"
ALGORITHM = "HS256"
ACCESS_TOKEN_EXPIRE_MINUTES = 30

pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")
security = HTTPBearer()

# Token models
class Token(BaseModel):
    access_token: str
    token_type: str

class TokenData(BaseModel):
    username: Optional[str] = None

# Password hashing
def verify_password(plain_password: str, hashed_password: str) -> bool:
    return pwd_context.verify(plain_password, hashed_password)

def get_password_hash(password: str) -> str:
    return pwd_context.hash(password)

# JWT token creation
def create_access_token(data: dict, expires_delta: Optional[timedelta] = None):
    to_encode = data.copy()
    expire = datetime.utcnow() + (expires_delta or timedelta(minutes=15))
    to_encode.update({"exp": expire})
    return jwt.encode(to_encode, SECRET_KEY, algorithm=ALGORITHM)

# Authentication dependency
async def get_current_user(
    credentials: HTTPAuthorizationCredentials = Depends(security),
    db: AsyncSession = Depends(get_db)
) -> User:
    """Verify JWT token and return current user."""
    credentials_exception = HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Could not validate credentials",
        headers={"WWW-Authenticate": "Bearer"},
    )

    try:
        token = credentials.credentials
        payload = jwt.decode(token, SECRET_KEY, algorithms=[ALGORITHM])
        username: str = payload.get("sub")
        if username is None:
            raise credentials_exception
    except JWTError:
        raise credentials_exception

    # Fetch user from database
    from sqlalchemy import select
    stmt = select(User).where(User.username == username)
    result = await db.execute(stmt)
    user = result.scalar_one_or_none()

    if user is None:
        raise credentials_exception

    return user

# Login endpoint
@app.post("/auth/login", response_model=Token)
async def login(
    username: str,
    password: str,
    db: AsyncSession = Depends(get_db)
):
    """Authenticate user and return JWT token."""
    from sqlalchemy import select

    stmt = select(User).where(User.username == username)
    result = await db.execute(stmt)
    user = result.scalar_one_or_none()

    if not user or not verify_password(password, user.hashed_password):
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Incorrect username or password"
        )

    access_token = create_access_token(
        data={"sub": user.username},
        expires_delta=timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    )

    return {"access_token": access_token, "token_type": "bearer"}

# Protected endpoint
@app.get("/users/me", response_model=UserResponse)
async def read_users_me(current_user: User = Depends(get_current_user)):
    """Get current authenticated user."""
    return current_user
```

### 4. Request Validation and Error Handling

```python
from fastapi import Request, status
from fastapi.responses import JSONResponse
from fastapi.exceptions import RequestValidationError
from pydantic import validator

# Custom validation in Pydantic models
class UserCreate(BaseModel):
    username: str = Field(..., min_length=3, max_length=50)
    email: EmailStr
    password: str = Field(..., min_length=8)

    @validator('username')
    def username_alphanumeric(cls, v):
        if not v.isalnum():
            raise ValueError('Username must be alphanumeric')
        return v

    @validator('password')
    def password_strength(cls, v):
        if not any(char.isdigit() for char in v):
            raise ValueError('Password must contain at least one digit')
        if not any(char.isupper() for char in v):
            raise ValueError('Password must contain at least one uppercase letter')
        return v

# Global exception handlers
@app.exception_handler(RequestValidationError)
async def validation_exception_handler(request: Request, exc: RequestValidationError):
    """Handle validation errors with custom response."""
    errors = []
    for error in exc.errors():
        errors.append({
            "field": ".".join(str(loc) for loc in error["loc"]),
            "message": error["msg"],
            "type": error["type"]
        })

    return JSONResponse(
        status_code=status.HTTP_422_UNPROCESSABLE_ENTITY,
        content={"detail": errors}
    )

@app.exception_handler(Exception)
async def general_exception_handler(request: Request, exc: Exception):
    """Handle unexpected errors."""
    return JSONResponse(
        status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
        content={"detail": "Internal server error"}
    )
```

### 5. API Versioning and Documentation

```python
from fastapi import APIRouter

# API v1
router_v1 = APIRouter(prefix="/api/v1", tags=["v1"])

@router_v1.get("/users")
async def list_users_v1():
    return {"version": "1.0", "users": []}

# API v2 with breaking changes
router_v2 = APIRouter(prefix="/api/v2", tags=["v2"])

@router_v2.get("/users")
async def list_users_v2(include_inactive: bool = False):
    return {"version": "2.0", "users": [], "includes_inactive": include_inactive}

# Include routers in app
app.include_router(router_v1)
app.include_router(router_v2)

# Customize OpenAPI documentation
from fastapi.openapi.utils import get_openapi

def custom_openapi():
    if app.openapi_schema:
        return app.openapi_schema

    openapi_schema = get_openapi(
        title="User Management API",
        version="2.0.0",
        description="Complete REST API for user management with authentication",
        routes=app.routes,
    )

    # Add custom fields
    openapi_schema["info"]["contact"] = {
        "name": "API Support",
        "email": "support@example.com"
    }

    app.openapi_schema = openapi_schema
    return app.openapi_schema

app.openapi = custom_openapi
```

### 6. Background Tasks and Async Processing

```python
from fastapi import BackgroundTasks
import smtplib
from email.message import EmailMessage

# Background task function
def send_email(email: str, message: str):
    """Send email in background."""
    msg = EmailMessage()
    msg.set_content(message)
    msg["Subject"] = "User Registration"
    msg["From"] = "noreply@example.com"
    msg["To"] = email

    # Send email (simplified)
    # with smtplib.SMTP("localhost") as server:
    #     server.send_message(msg)
    print(f"Sending email to {email}: {message}")

@app.post("/users", response_model=UserResponse)
async def create_user(
    user: UserCreate,
    background_tasks: BackgroundTasks,
    db: AsyncSession = Depends(get_db)
):
    """Create user and send welcome email in background."""
    db_user = User(**user.dict())
    db.add(db_user)
    await db.flush()

    # Add background task
    background_tasks.add_task(
        send_email,
        user.email,
        f"Welcome {user.username}!"
    )

    await db.refresh(db_user)
    return db_user
```

## How It Works

```mermaid
graph TD
    A[HTTP Request] --> B[FastAPI Router]
    B --> C{Path Matches?}
    C -->|Yes| D[Run Dependencies]
    D --> E[Validate Request with Pydantic]
    E --> F{Valid?}
    F -->|Yes| G[Execute Handler]
    F -->|No| H[Return 422 Validation Error]
    G --> I{Exception?}
    I -->|Yes| J[Exception Handler]
    I -->|No| K[Serialize Response]
    K --> L[Return HTTP Response]
    J --> L

    style A fill:#0173B2
    style E fill:#DE8F05
    style G fill:#029E73
    style L fill:#CC78BC
```

**Request Lifecycle:**

1. **Routing**: FastAPI matches request path and method to handler
2. **Dependencies**: Executes dependency functions (DB session, auth, etc.)
3. **Validation**: Pydantic validates request body/query/path parameters
4. **Handler Execution**: Async handler processes request
5. **Response Serialization**: Pydantic serializes response model
6. **Documentation**: Automatic OpenAPI schema generation

## Variations

### Rate Limiting Middleware

```python
from fastapi import Request, HTTPException
from starlette.middleware.base import BaseHTTPMiddleware
import time
from collections import defaultdict

class RateLimitMiddleware(BaseHTTPMiddleware):
    def __init__(self, app, requests_per_minute: int = 60):
        super().__init__(app)
        self.requests_per_minute = requests_per_minute
        self.requests = defaultdict(list)

    async def dispatch(self, request: Request, call_next):
        client_ip = request.client.host
        now = time.time()

        # Clean old requests
        self.requests[client_ip] = [
            req_time for req_time in self.requests[client_ip]
            if now - req_time < 60
        ]

        # Check rate limit
        if len(self.requests[client_ip]) >= self.requests_per_minute:
            raise HTTPException(
                status_code=status.HTTP_429_TOO_MANY_REQUESTS,
                detail="Rate limit exceeded"
            )

        self.requests[client_ip].append(now)
        response = await call_next(request)
        return response

# Add middleware
app.add_middleware(RateLimitMiddleware, requests_per_minute=100)
```

### CORS Configuration

```python
from fastapi.middleware.cors import CORSMiddleware

app.add_middleware(
    CORSMiddleware,
    allow_origins=["https://example.com"],  # Specific origins
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)
```

### File Upload Handling

```python
from fastapi import File, UploadFile
import shutil
from pathlib import Path

@app.post("/upload")
async def upload_file(file: UploadFile = File(...)):
    """Handle file upload."""
    upload_dir = Path("uploads")
    upload_dir.mkdir(exist_ok=True)

    file_path = upload_dir / file.filename

    with file_path.open("wb") as buffer:
        shutil.copyfileobj(file.file, buffer)

    return {
        "filename": file.filename,
        "content_type": file.content_type,
        "size": file_path.stat().st_size
    }
```

## Common Pitfalls

### 1. Blocking Operations in Async Handlers

**Problem**: Blocking calls freeze the event loop.

```python
# ❌ Bad: Blocking I/O in async handler
@app.get("/slow")
async def slow_endpoint():
    time.sleep(5)  # Blocks event loop!
    return {"status": "done"}

# ✅ Good: Use async sleep
@app.get("/fast")
async def fast_endpoint():
    await asyncio.sleep(5)  # Non-blocking
    return {"status": "done"}

# ✅ Better: Run blocking code in executor
from concurrent.futures import ThreadPoolExecutor
import asyncio

executor = ThreadPoolExecutor()

@app.get("/blocking")
async def blocking_endpoint():
    loop = asyncio.get_event_loop()
    result = await loop.run_in_executor(executor, blocking_function)
    return {"result": result}
```

### 2. Missing Response Model Validation

**Problem**: Returning data that doesn't match schema.

```python
# ❌ Bad: No response validation
@app.get("/users/{user_id}")
async def get_user(user_id: int):
    return users_db.get(user_id)  # Could return None!

# ✅ Good: Use response_model
@app.get("/users/{user_id}", response_model=UserResponse)
async def get_user(user_id: int):
    user = users_db.get(user_id)
    if not user:
        raise HTTPException(status_code=404, detail="User not found")
    return user  # Validated against UserResponse
```

### 3. Not Handling Database Transactions Properly

**Problem**: Uncommitted changes or resource leaks.

```python
# ❌ Bad: Manual session management
@app.post("/users")
async def create_user(user: UserCreate):
    session = AsyncSessionLocal()
    db_user = User(**user.dict())
    session.add(db_user)
    # Forgot to commit! Changes lost
    return db_user

# ✅ Good: Use dependency with proper lifecycle
@app.post("/users", response_model=UserResponse)
async def create_user(user: UserCreate, db: AsyncSession = Depends(get_db)):
    db_user = User(**user.dict())
    db.add(db_user)
    # Dependency handles commit/rollback
    return db_user
```

### 4. Exposing Internal Models Directly

**Problem**: Database models leak implementation details.

```python
# ❌ Bad: Return ORM models directly
@app.get("/users/{user_id}")
async def get_user(user_id: int, db: AsyncSession = Depends(get_db)):
    return await db.get(User, user_id)  # Exposes all fields, relations

# ✅ Good: Use separate response models
class UserResponse(BaseModel):
    id: int
    username: str
    email: str
    # password_hash excluded!

    class Config:
        from_attributes = True

@app.get("/users/{user_id}", response_model=UserResponse)
async def get_user(user_id: int, db: AsyncSession = Depends(get_db)):
    user = await db.get(User, user_id)
    return user  # Pydantic filters fields
```

### 5. Ignoring Security Best Practices

**Problem**: Vulnerable to common attacks.

```python
# ❌ Bad: No input sanitization, hardcoded secrets
SECRET_KEY = "password123"

@app.get("/search")
async def search(query: str):
    # SQL injection risk!
    return await db.execute(f"SELECT * FROM users WHERE name = '{query}'")

# ✅ Good: Use parameterized queries, environment variables
import os
from sqlalchemy import select

SECRET_KEY = os.getenv("SECRET_KEY")

@app.get("/search")
async def search(query: str, db: AsyncSession = Depends(get_db)):
    stmt = select(User).where(User.name == query)  # Parameterized
    result = await db.execute(stmt)
    return result.scalars().all()
```

## Related Patterns

**Related Tutorial**: See [Intermediate Tutorial - Web Development](/en/learn/swe/prog-lang/python/tutorials/intermediate#web).
**Related How-To**: See [Use Advanced Async Patterns](/en/learn/swe/prog-lang/python/how-to/async-patterns-advanced), [Work with Databases](/en/learn/swe/prog-lang/python/how-to/work-with-databases).
**Related Cookbook**: See Cookbook recipe "REST API Patterns".
