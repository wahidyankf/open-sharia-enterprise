---
title: "API Development Best Practices"
date: 2025-12-19T00:00:00+07:00
draft: false
description: "Build robust, scalable, and secure REST APIs in Python with FastAPI, Flask, and Django REST Framework"
weight: 622
tags: ["python", "api", "rest", "fastapi", "flask", "django", "how-to"]
---

**Building a REST API in Python?** This guide covers best practices for developing robust, scalable, and secure APIs using FastAPI, Flask, and Django REST Framework. Learn request/response patterns, authentication, validation, error handling, and documentation.

For related patterns, check [Build CLI Applications](/en/learn/swe/prog-lang/python/how-to/build-cli-applications) and [Handle Errors Effectively](/en/learn/swe/prog-lang/python/how-to/handle-errors-effectively).

## 🎯 Choosing a Framework

### FastAPI (Modern, Recommended for New Projects)

**Pros**: High performance, automatic documentation, type hints, async support
**Best for**: New APIs, microservices, data-heavy applications

```bash
pip install fastapi uvicorn[standard]
```

### Flask + Flask-RESTful (Lightweight, Flexible)

**Pros**: Simple, unopinionated, large ecosystem
**Best for**: Small to medium APIs, learning, rapid prototyping

```bash
pip install flask flask-restful
```

### Django REST Framework (Full-Featured)

**Pros**: Complete ecosystem, admin panel, ORM, authentication
**Best for**: Large applications, when using Django already

```bash
pip install djangorestframework
```

## 🚀 FastAPI Best Practices

### Pattern 1: Request/Response Models with Pydantic

```python
from fastapi import FastAPI, HTTPException, status
from pydantic import BaseModel, EmailStr, Field
from typing import Optional, List
from datetime import datetime

app = FastAPI(title="User API", version="1.0.0")

# Request models
class UserCreate(BaseModel):
    """User creation request."""
    name: str = Field(..., min_length=1, max_length=100)
    email: EmailStr
    age: int = Field(..., gt=0, lt=150)

# Response models
class UserResponse(BaseModel):
    """User response."""
    id: int
    name: str
    email: EmailStr
    age: int
    created_at: datetime

    class Config:
        from_attributes = True  # For ORM compatibility

# Endpoints
@app.post(
    "/users/",
    response_model=UserResponse,
    status_code=status.HTTP_201_CREATED,
    tags=["users"]
)
async def create_user(user: UserCreate):
    """Create a new user."""
    # Validation happens automatically
    user_data = user.dict()
    user_data["id"] = generate_id()
    user_data["created_at"] = datetime.now()
    return user_data

@app.get("/users/{user_id}", response_model=UserResponse)
async def get_user(user_id: int):
    """Get user by ID."""
    user = fetch_user_from_db(user_id)
    if not user:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"User {user_id} not found"
        )
    return user
```

### Pattern 2: Dependency Injection

```python
from fastapi import Depends, Header, HTTPException
from typing import Optional

# Database dependency
async def get_db():
    """Database connection dependency."""
    db = Database()
    try:
        yield db
    finally:
        db.close()

# Authentication dependency
async def get_current_user(
    authorization: str = Header(...)
) -> dict:
    """Verify JWT token and return user."""
    if not authorization.startswith("Bearer "):
        raise HTTPException(status_code=401, detail="Invalid token")

    token = authorization[7:]
    user = verify_token(token)
    if not user:
        raise HTTPException(status_code=401, detail="Invalid credentials")

    return user

# Use dependencies in endpoints
@app.get("/users/me")
async def get_current_user_info(
    current_user: dict = Depends(get_current_user),
    db = Depends(get_db)
):
    """Get current authenticated user."""
    return current_user
```

### Pattern 3: Error Handling with Custom Exceptions

```python
from fastapi import FastAPI, Request, status
from fastapi.responses import JSONResponse

class APIException(Exception):
    """Base API exception."""
    def __init__(self, detail: str, status_code: int = 400):
        self.detail = detail
        self.status_code = status_code

class UserNotFoundError(APIException):
    """User not found."""
    def __init__(self, user_id: int):
        super().__init__(
            detail=f"User {user_id} not found",
            status_code=status.HTTP_404_NOT_FOUND
        )

class ValidationError(APIException):
    """Validation failed."""
    def __init__(self, detail: str):
        super().__init__(
            detail=detail,
            status_code=status.HTTP_422_UNPROCESSABLE_ENTITY
        )

# Exception handler
@app.exception_handler(APIException)
async def api_exception_handler(request: Request, exc: APIException):
    return JSONResponse(
        status_code=exc.status_code,
        content={"detail": exc.detail}
    )

# Usage
@app.get("/users/{user_id}")
async def get_user(user_id: int):
    user = fetch_user(user_id)
    if not user:
        raise UserNotFoundError(user_id)
    return user
```

### Pattern 4: Pagination

```python
from fastapi import Query
from typing import Generic, TypeVar, List

T = TypeVar('T')

class PaginatedResponse(BaseModel, Generic[T]):
    """Paginated response wrapper."""
    items: List[T]
    total: int
    page: int
    size: int
    pages: int

@app.get("/users/", response_model=PaginatedResponse[UserResponse])
async def list_users(
    page: int = Query(1, ge=1),
    size: int = Query(10, ge=1, le=100),
    db = Depends(get_db)
):
    """List users with pagination."""
    offset = (page - 1) * size
    users = db.query(User).offset(offset).limit(size).all()
    total = db.query(User).count()

    return PaginatedResponse(
        items=users,
        total=total,
        page=page,
        size=size,
        pages=(total + size - 1) // size
    )
```

### Pattern 5: Rate Limiting

```python
from fastapi import Request
from slowapi import Limiter, _rate_limit_exceeded_handler
from slowapi.util import get_remote_address
from slowapi.errors import RateLimitExceeded

limiter = Limiter(key_func=get_remote_address)
app.state.limiter = limiter
app.add_exception_handler(RateLimitExceeded, _rate_limit_exceeded_handler)

@app.get("/api/data")
@limiter.limit("10/minute")
async def get_data(request: Request):
    """Rate limited endpoint - 10 requests per minute."""
    return {"data": "value"}
```

## 🔐 Authentication & Authorization

### Pattern 6: JWT Authentication

```python
from datetime import datetime, timedelta
from jose import JWTError, jwt
from passlib.context import CryptContext

SECRET_KEY = "your-secret-key-here"
ALGORITHM = "HS256"
ACCESS_TOKEN_EXPIRE_MINUTES = 30

pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")

def create_access_token(data: dict) -> str:
    """Create JWT access token."""
    to_encode = data.copy()
    expire = datetime.utcnow() + timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    to_encode.update({"exp": expire})
    return jwt.encode(to_encode, SECRET_KEY, algorithm=ALGORITHM)

def verify_token(token: str) -> Optional[dict]:
    """Verify JWT token."""
    try:
        payload = jwt.decode(token, SECRET_KEY, algorithms=[ALGORITHM])
        return payload
    except JWTError:
        return None

def hash_password(password: str) -> str:
    """Hash password."""
    return pwd_context.hash(password)

def verify_password(plain_password: str, hashed_password: str) -> bool:
    """Verify password against hash."""
    return pwd_context.verify(plain_password, hashed_password)

# Login endpoint
@app.post("/auth/login")
async def login(username: str, password: str, db = Depends(get_db)):
    """Authenticate and return JWT token."""
    user = db.query(User).filter(User.username == username).first()

    if not user or not verify_password(password, user.hashed_password):
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Incorrect username or password"
        )

    token = create_access_token(data={"sub": user.username})
    return {"access_token": token, "token_type": "bearer"}
```

### Pattern 7: Role-Based Access Control

```python
from enum import Enum
from fastapi import Depends

class Role(str, Enum):
    ADMIN = "admin"
    USER = "user"
    GUEST = "guest"

def require_role(required_role: Role):
    """Dependency to check user role."""
    def role_checker(current_user: dict = Depends(get_current_user)):
        if current_user.get("role") != required_role:
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="Insufficient permissions"
            )
        return current_user
    return role_checker

# Admin-only endpoint
@app.delete("/users/{user_id}")
async def delete_user(
    user_id: int,
    admin: dict = Depends(require_role(Role.ADMIN))
):
    """Delete user (admin only)."""
    delete_user_from_db(user_id)
    return {"message": "User deleted"}
```

## 📝 Validation & Sanitization

### Pattern 8: Custom Validators

```python
from pydantic import BaseModel, validator, root_validator

class ProductCreate(BaseModel):
    name: str
    price: float
    discount: Optional[float] = None

    @validator('price')
    def price_must_be_positive(cls, v):
        """Ensure price is positive."""
        if v <= 0:
            raise ValueError('Price must be positive')
        return v

    @validator('discount')
    def discount_must_be_valid(cls, v):
        """Ensure discount is between 0 and 1."""
        if v is not None and not (0 <= v <= 1):
            raise ValueError('Discount must be between 0 and 1')
        return v

    @root_validator
    def check_final_price(cls, values):
        """Validate final price after discount."""
        price = values.get('price')
        discount = values.get('discount', 0)

        if discount and price * (1 - discount) < 1:
            raise ValueError('Final price cannot be less than $1')

        return values
```

### Pattern 9: SQL Injection Prevention

```python
from sqlalchemy import text

# ❌ Bad: Vulnerable to SQL injection
@app.get("/users/search")
async def search_users_bad(name: str):
    query = f"SELECT * FROM users WHERE name = '{name}'"
    # SQL injection: name = "' OR '1'='1"
    return db.execute(text(query)).fetchall()

# ✅ Good: Parameterized queries
@app.get("/users/search")
async def search_users_good(name: str):
    query = text("SELECT * FROM users WHERE name = :name")
    return db.execute(query, {"name": name}).fetchall()

# ✅ Better: ORM
@app.get("/users/search")
async def search_users_orm(name: str, db = Depends(get_db)):
    return db.query(User).filter(User.name == name).all()
```

## 📚 Documentation

### Pattern 10: OpenAPI Documentation

```python
from fastapi import FastAPI

app = FastAPI(
    title="My API",
    description="API for managing users and products",
    version="1.0.0",
    docs_url="/docs",          # Swagger UI
    redoc_url="/redoc",        # ReDoc
    openapi_url="/openapi.json"
)

# Customize endpoint documentation
@app.post(
    "/users/",
    summary="Create a new user",
    description="Create a new user with the provided data. Email must be unique.",
    response_description="The created user object",
    tags=["users"],
    responses={
        201: {"description": "User created successfully"},
        422: {"description": "Validation error"},
        409: {"description": "Email already exists"}
    }
)
async def create_user(user: UserCreate):
    """Create a new user in the system."""
    return create_user_in_db(user)
```

## 🔄 Versioning

### Pattern 11: URL Versioning

```python
from fastapi import APIRouter

# Version 1
v1_router = APIRouter(prefix="/api/v1", tags=["v1"])

@v1_router.get("/users/")
async def list_users_v1():
    return {"version": 1, "users": []}

# Version 2
v2_router = APIRouter(prefix="/api/v2", tags=["v2"])

@v2_router.get("/users/")
async def list_users_v2():
    return {"version": 2, "users": [], "total": 0}

# Register routers
app.include_router(v1_router)
app.include_router(v2_router)
```

## 🎯 Performance Optimization

### Pattern 12: Caching

```python
from functools import lru_cache
from fastapi_cache import FastAPICache
from fastapi_cache.backends.redis import RedisBackend
from fastapi_cache.decorator import cache

# In-memory caching
@lru_cache(maxsize=128)
def get_expensive_data(param: str):
    """Expensive computation with caching."""
    return compute_result(param)

# Redis caching (requires setup)
@app.get("/data")
@cache(expire=300)  # Cache for 5 minutes
async def get_cached_data():
    """Cached endpoint."""
    return fetch_data_from_db()
```

### Pattern 13: Database Query Optimization

```python
# ❌ Bad: N+1 query problem
@app.get("/users/")
async def list_users_bad(db = Depends(get_db)):
    users = db.query(User).all()
    return [
        {
            "id": u.id,
            "name": u.name,
            "posts": u.posts  # Separate query for each user!
        }
        for u in users
    ]

# ✅ Good: Eager loading
from sqlalchemy.orm import joinedload

@app.get("/users/")
async def list_users_good(db = Depends(get_db)):
    users = db.query(User).options(joinedload(User.posts)).all()
    return users  # Single query
```

## 🚫 Common Mistakes

### Mistake 1: Returning Sensitive Data

```python
# ❌ Bad: Exposing password hash
@app.get("/users/{user_id}")
async def get_user_bad(user_id: int):
    user = fetch_user(user_id)
    return user  # Includes hashed_password!

# ✅ Good: Use response model
class UserPublic(BaseModel):
    id: int
    name: str
    email: EmailStr
    # No password fields

@app.get("/users/{user_id}", response_model=UserPublic)
async def get_user_good(user_id: int):
    user = fetch_user(user_id)
    return user  # Only public fields returned
```

### Mistake 2: Ignoring HTTP Status Codes

```python
# ❌ Bad: Always returning 200
@app.delete("/users/{user_id}")
async def delete_user_bad(user_id: int):
    delete_user(user_id)
    return {"message": "deleted"}  # 200 OK

# ✅ Good: Use appropriate status codes
@app.delete("/users/{user_id}", status_code=status.HTTP_204_NO_CONTENT)
async def delete_user_good(user_id: int):
    delete_user(user_id)
    return  # 204 No Content
```

### Mistake 3: Not Handling CORS

```python
from fastapi.middleware.cors import CORSMiddleware

# Add CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["https://yourdomain.com"],  # Specific domains
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)
```

## 🎯 Best Practices Summary

1. **Use Pydantic Models**: Automatic validation and documentation
2. **Proper Status Codes**: 200, 201, 204, 400, 401, 403, 404, 422, 500
3. **Authentication**: JWT or OAuth2 for stateless auth
4. **Rate Limiting**: Protect against abuse
5. **Versioning**: API versioning for backward compatibility
6. **Pagination**: Never return unbounded lists
7. **Documentation**: Auto-generated with FastAPI
8. **Error Handling**: Consistent error responses
9. **Testing**: Unit and integration tests
10. **Logging**: Structured logging for debugging

## 🛠️ Essential Libraries

**FastAPI Ecosystem**:

- fastapi
- uvicorn (ASGI server)
- pydantic (validation)
- python-jose[cryptography] (JWT)
- passlib[bcrypt] (password hashing)
- python-multipart (file uploads)
- fastapi-cache (caching)
- slowapi (rate limiting)

**Database**:

- sqlalchemy (ORM)
- alembic (migrations)
- asyncpg (async PostgreSQL)

**Testing**:

- pytest
- httpx (async client)

## 📚 See Also

- [Handle Errors Effectively](/en/learn/swe/prog-lang/python/how-to/handle-errors-effectively) - Error handling patterns
- [Advanced Async Patterns](/en/learn/swe/prog-lang/python/how-to/advanced-async-patterns) - Async programming
- [FastAPI Documentation](https://fastapi.tiangolo.com/) - Official docs
- [REST API Design Best Practices](https://www.restapitutorial.com/) - REST principles
