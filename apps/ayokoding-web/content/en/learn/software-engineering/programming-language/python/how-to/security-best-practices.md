---
title: "How to Implement Security Best Practices"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000022
description: "Practical techniques for securing Python applications with password hashing, input validation, SQL injection prevention, and cryptography"
---

## Problem

Security vulnerabilities lead to data breaches, unauthorized access, and compromised systems. Common mistakes include storing plaintext passwords, SQL injection, and insecure cryptography.

## Solution

### 1. Secure Password Hashing with bcrypt

```python
from passlib.context import CryptContext

# Configure password hashing
pwd_context = CryptContext(
    schemes=["bcrypt"],
    deprecated="auto",
    bcrypt__rounds=12  # Work factor (higher = slower but more secure)
)

class PasswordManager:
    """Secure password management."""

    @staticmethod
    def hash_password(plain_password: str) -> str:
        """Hash password using bcrypt."""
        return pwd_context.hash(plain_password)

    @staticmethod
    def verify_password(plain_password: str, hashed_password: str) -> bool:
        """Verify password against hash."""
        return pwd_context.verify(plain_password, hashed_password)

# Usage
password = "SecureP@ssw0rd123"
hashed = PasswordManager.hash_password(password)
print(f"Hashed: {hashed}")
# $2b$12$... (different each time due to salt)

# Verify password
is_valid = PasswordManager.verify_password(password, hashed)
print(f"Valid: {is_valid}")  # True

is_invalid = PasswordManager.verify_password("WrongPassword", hashed)
print(f"Valid: {is_invalid}")  # False
```

### 2. SQL Injection Prevention

```python
from sqlalchemy import create_engine, text, select
from sqlalchemy.orm import Session

engine = create_engine("postgresql://user:password@localhost/dbname")

# ❌ NEVER: String concatenation (SQL injection vulnerable)
def unsafe_query(username: str):
    with Session(engine) as session:
        # Attacker can inject: ' OR '1'='1
        query = f"SELECT * FROM users WHERE username = '{username}'"
        result = session.execute(text(query))
        return result.fetchall()

# ✅ ALWAYS: Parameterized queries
def safe_query_text(username: str):
    """Safe query using bound parameters."""
    with Session(engine) as session:
        query = text("SELECT * FROM users WHERE username = :username")
        result = session.execute(query, {"username": username})
        return result.fetchall()

# ✅ BETTER: ORM queries (inherently safe)
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column

class Base(DeclarativeBase):
    pass

class User(Base):
    __tablename__ = "users"
    id: Mapped[int] = mapped_column(primary_key=True)
    username: Mapped[str]

def safe_query_orm(username: str):
    """Safe query using SQLAlchemy ORM."""
    with Session(engine) as session:
        stmt = select(User).where(User.username == username)
        result = session.execute(stmt)
        return result.scalar_one_or_none()
```

### 3. Input Validation and Sanitization

```python
from pydantic import BaseModel, validator, Field
from html import escape
import re

class UserInput(BaseModel):
    """Validate and sanitize user input."""
    username: str = Field(..., min_length=3, max_length=50)
    email: str
    bio: str = Field(..., max_length=500)

    @validator('username')
    def validate_username(cls, v):
        """Allow only alphanumeric and underscores."""
        if not re.match(r'^[a-zA-Z0-9_]+$', v):
            raise ValueError('Username contains invalid characters')
        return v

    @validator('email')
    def validate_email(cls, v):
        """Validate email format."""
        if not re.match(r'^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$', v):
            raise ValueError('Invalid email format')
        return v

    @validator('bio')
    def sanitize_bio(cls, v):
        """Escape HTML to prevent XSS."""
        return escape(v)

# Usage
safe_input = UserInput(
    username="john_doe",
    email="john@example.com",
    bio="<script>alert('XSS')</script>"  # Escaped to &lt;script&gt;...
)

print(safe_input.bio)
# &lt;script&gt;alert(&#x27;XSS&#x27;)&lt;/script&gt;
```

### 4. Secure Token Generation with Secrets Module

```python
import secrets
import hashlib
import hmac
from datetime import datetime, timedelta

class SecureTokenManager:
    """Generate and validate secure tokens."""

    def __init__(self, secret_key: str):
        self.secret_key = secret_key.encode()

    def generate_token(self, length: int = 32) -> str:
        """Generate cryptographically secure random token."""
        return secrets.token_urlsafe(length)

    def generate_reset_token(self, user_id: int, expires_hours: int = 24) -> str:
        """Generate password reset token with expiration."""
        # Create token with user_id and expiration
        expiry = datetime.utcnow() + timedelta(hours=expires_hours)
        data = f"{user_id}:{expiry.isoformat()}"

        # Create HMAC signature
        signature = hmac.new(
            self.secret_key,
            data.encode(),
            hashlib.sha256
        ).hexdigest()

        # Combine data and signature
        token = f"{data}:{signature}"
        return secrets.token_urlsafe(32)  # In production, encrypt data+signature

    def verify_token_signature(self, data: str, signature: str) -> bool:
        """Verify HMAC signature."""
        expected_signature = hmac.new(
            self.secret_key,
            data.encode(),
            hashlib.sha256
        ).hexdigest()

        # Use constant-time comparison to prevent timing attacks
        return hmac.compare_digest(signature, expected_signature)

# Usage
token_manager = SecureTokenManager(secret_key="your-secret-key-from-env")

# API token
api_token = token_manager.generate_token()
print(f"API Token: {api_token}")

# Password reset token
reset_token = token_manager.generate_reset_token(user_id=123, expires_hours=1)
print(f"Reset Token: {reset_token}")
```

### 5. Encryption with cryptography Library

```python
from cryptography.fernet import Fernet
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2
import base64
import os

class DataEncryption:
    """Encrypt and decrypt sensitive data."""

    @staticmethod
    def generate_key() -> bytes:
        """Generate new Fernet key."""
        return Fernet.generate_key()

    @staticmethod
    def derive_key_from_password(password: str, salt: bytes = None) -> tuple:
        """Derive encryption key from password using PBKDF2."""
        if salt is None:
            salt = os.urandom(16)

        kdf = PBKDF2(
            algorithm=hashes.SHA256(),
            length=32,
            salt=salt,
            iterations=100000  # Adjust based on security requirements
        )

        key = base64.urlsafe_b64encode(kdf.derive(password.encode()))
        return key, salt

    def __init__(self, key: bytes):
        self.cipher = Fernet(key)

    def encrypt(self, data: str) -> str:
        """Encrypt data."""
        encrypted = self.cipher.encrypt(data.encode())
        return encrypted.decode()

    def decrypt(self, encrypted_data: str) -> str:
        """Decrypt data."""
        decrypted = self.cipher.decrypt(encrypted_data.encode())
        return decrypted.decode()

# Usage
# Generate key (store securely, don't hardcode!)
key = DataEncryption.generate_key()
encryptor = DataEncryption(key)

# Encrypt sensitive data
sensitive_data = "SSN: 123-45-6789"
encrypted = encryptor.encrypt(sensitive_data)
print(f"Encrypted: {encrypted}")

# Decrypt when needed
decrypted = encryptor.decrypt(encrypted)
print(f"Decrypted: {decrypted}")

# Password-based encryption
password = "SecurePassword123"
key, salt = DataEncryption.derive_key_from_password(password)
encryptor2 = DataEncryption(key)

encrypted2 = encryptor2.encrypt("Secret message")
print(f"Password-encrypted: {encrypted2}")
```

### 6. Secure Session Management

```python
from datetime import datetime, timedelta
import secrets
from typing import Optional, Dict

class SessionManager:
    """Secure session management with expiration."""

    def __init__(self, session_lifetime_minutes: int = 30):
        self.sessions: Dict[str, dict] = {}
        self.session_lifetime = timedelta(minutes=session_lifetime_minutes)

    def create_session(self, user_id: int) -> str:
        """Create new session with secure token."""
        session_token = secrets.token_urlsafe(32)

        self.sessions[session_token] = {
            "user_id": user_id,
            "created_at": datetime.utcnow(),
            "last_accessed": datetime.utcnow(),
            "ip_address": None,  # Store IP for validation
            "user_agent": None   # Store user agent for validation
        }

        return session_token

    def validate_session(self, session_token: str) -> Optional[int]:
        """Validate session and return user_id if valid."""
        session = self.sessions.get(session_token)

        if not session:
            return None

        # Check expiration
        now = datetime.utcnow()
        if now - session["last_accessed"] > self.session_lifetime:
            # Session expired
            self.destroy_session(session_token)
            return None

        # Update last accessed time
        session["last_accessed"] = now

        return session["user_id"]

    def destroy_session(self, session_token: str):
        """Destroy session."""
        if session_token in self.sessions:
            del self.sessions[session_token]

    def cleanup_expired_sessions(self):
        """Remove expired sessions."""
        now = datetime.utcnow()
        expired = [
            token for token, session in self.sessions.items()
            if now - session["last_accessed"] > self.session_lifetime
        ]

        for token in expired:
            del self.sessions[token]

# Usage
session_manager = SessionManager(session_lifetime_minutes=30)

# Login creates session
session_token = session_manager.create_session(user_id=123)
print(f"Session created: {session_token}")

# Validate session on subsequent requests
user_id = session_manager.validate_session(session_token)
if user_id:
    print(f"Valid session for user: {user_id}")
else:
    print("Invalid or expired session")

# Logout destroys session
session_manager.destroy_session(session_token)
```

## How It Works

```mermaid
graph TD
    A[User Input] --> B{Input Validation}
    B -->|Invalid| C[Reject with Error]
    B -->|Valid| D{Sensitive Data?}
    D -->|Yes| E[Encrypt/Hash]
    D -->|No| F[Process Normally]
    E --> G[Store Securely]
    F --> G
    G --> H{Database Operation?}
    H -->|Yes| I[Parameterized Query]
    H -->|No| J[Business Logic]
    I --> K[Audit Log]
    J --> K

    style A fill:#0173B2
    style B fill:#DE8F05
    style E fill:#029E73
    style K fill:#CC78BC
```

**Security Layers:**

1. **Input Validation**: Reject malformed/malicious input early
2. **Sanitization**: Escape/encode data to prevent injection
3. **Encryption**: Protect sensitive data at rest
4. **Parameterization**: Prevent SQL injection in queries
5. **Auditing**: Log security-relevant events

## Variations

### Rate Limiting to Prevent Brute Force

```python
from datetime import datetime, timedelta
from collections import defaultdict

class RateLimiter:
    """Rate limiter for login attempts."""

    def __init__(self, max_attempts: int = 5, window_minutes: int = 15):
        self.max_attempts = max_attempts
        self.window = timedelta(minutes=window_minutes)
        self.attempts = defaultdict(list)

    def is_allowed(self, identifier: str) -> bool:
        """Check if request is allowed."""
        now = datetime.utcnow()

        # Clean old attempts
        self.attempts[identifier] = [
            attempt for attempt in self.attempts[identifier]
            if now - attempt < self.window
        ]

        # Check if limit exceeded
        if len(self.attempts[identifier]) >= self.max_attempts:
            return False

        # Record attempt
        self.attempts[identifier].append(now)
        return True

# Usage
rate_limiter = RateLimiter(max_attempts=5, window_minutes=15)

username = "john_doe"
if rate_limiter.is_allowed(username):
    # Process login
    print("Login attempt allowed")
else:
    print("Too many attempts, try again later")
```

### Secure File Upload Validation

```python
import magic
from pathlib import Path

class SecureFileUpload:
    """Validate file uploads."""

    ALLOWED_EXTENSIONS = {'.jpg', '.jpeg', '.png', '.pdf'}
    ALLOWED_MIME_TYPES = {
        'image/jpeg',
        'image/png',
        'application/pdf'
    }
    MAX_FILE_SIZE = 10 * 1024 * 1024  # 10 MB

    @classmethod
    def validate_file(cls, file_path: Path, file_size: int) -> tuple[bool, str]:
        """Validate uploaded file."""

        # Check file size
        if file_size > cls.MAX_FILE_SIZE:
            return False, "File too large"

        # Check extension
        if file_path.suffix.lower() not in cls.ALLOWED_EXTENSIONS:
            return False, "Invalid file extension"

        # Check actual MIME type (prevents extension spoofing)
        mime = magic.Magic(mime=True)
        actual_mime = mime.from_file(str(file_path))

        if actual_mime not in cls.ALLOWED_MIME_TYPES:
            return False, f"Invalid file type: {actual_mime}"

        return True, "File valid"

# Usage
file_path = Path("upload.jpg")
file_size = file_path.stat().st_size

is_valid, message = SecureFileUpload.validate_file(file_path, file_size)
if is_valid:
    print("File validated successfully")
else:
    print(f"Validation failed: {message}")
```

### CSRF Protection

```python
import secrets
import hmac
import hashlib

class CSRFProtection:
    """CSRF token generation and validation."""

    def __init__(self, secret_key: str):
        self.secret_key = secret_key.encode()

    def generate_token(self, session_id: str) -> str:
        """Generate CSRF token for session."""
        # Create random token
        random_token = secrets.token_urlsafe(32)

        # Create signature based on session
        message = f"{session_id}:{random_token}"
        signature = hmac.new(
            self.secret_key,
            message.encode(),
            hashlib.sha256
        ).hexdigest()

        return f"{random_token}:{signature}"

    def validate_token(self, session_id: str, token: str) -> bool:
        """Validate CSRF token."""
        try:
            random_token, signature = token.split(':')
        except ValueError:
            return False

        # Recreate expected signature
        message = f"{session_id}:{random_token}"
        expected_signature = hmac.new(
            self.secret_key,
            message.encode(),
            hashlib.sha256
        ).hexdigest()

        # Constant-time comparison
        return hmac.compare_digest(signature, expected_signature)

# Usage
csrf = CSRFProtection(secret_key="your-secret-key")

session_id = "user-session-123"
csrf_token = csrf.generate_token(session_id)

# In form
print(f'<input type="hidden" name="csrf_token" value="{csrf_token}">')

# Validate on submission
is_valid = csrf.validate_token(session_id, csrf_token)
if not is_valid:
    print("CSRF validation failed!")
```

## Common Pitfalls

### 1. Storing Passwords in Plain Text

**Problem**: Database breach exposes all passwords.

```python
# ❌ NEVER: Plain text passwords
class BadUserStorage:
    def store_user(self, username: str, password: str):
        # NEVER store plain text!
        db.execute("INSERT INTO users VALUES (?, ?)", (username, password))

# ✅ ALWAYS: Hash passwords
class GoodUserStorage:
    def store_user(self, username: str, password: str):
        hashed = pwd_context.hash(password)
        db.execute("INSERT INTO users VALUES (?, ?)", (username, hashed))
```

### 2. Using Weak Random Number Generators

**Problem**: Predictable tokens enable attacks.

```python
import random

# ❌ Bad: random module is NOT cryptographically secure
def bad_token():
    return ''.join(random.choices('0123456789', k=32))  # Predictable!

# ✅ Good: Use secrets module
import secrets

def good_token():
    return secrets.token_urlsafe(32)  # Cryptographically secure
```

### 3. Hardcoding Secrets

**Problem**: Secrets in source code leak via version control.

```python
# ❌ Bad: Hardcoded secrets
SECRET_KEY = "my-secret-key-123"  # Committed to git!
DATABASE_PASSWORD = "password123"

# ✅ Good: Environment variables
import os

SECRET_KEY = os.getenv("SECRET_KEY")
if not SECRET_KEY:
    raise ValueError("SECRET_KEY environment variable not set")

DATABASE_PASSWORD = os.getenv("DATABASE_PASSWORD")
```

### 4. Insufficient Error Messages

**Problem**: Too much information aids attackers.

```python
# ❌ Bad: Reveals if username exists
def bad_login(username: str, password: str):
    user = get_user(username)
    if not user:
        return "Username not found"  # Reveals username doesn't exist!
    if not verify_password(password, user.password_hash):
        return "Incorrect password"  # Confirms username exists!
    return "Success"

# ✅ Good: Generic error message
def good_login(username: str, password: str):
    user = get_user(username)
    if not user or not verify_password(password, user.password_hash):
        return "Invalid username or password"  # Doesn't reveal which
    return "Success"
```

### 5. Not Using HTTPS

**Problem**: Data transmitted in clear text.

```python
# ❌ Bad: HTTP API (credentials exposed)
import requests

response = requests.post(
    "http://api.example.com/login",  # Unencrypted!
    json={"username": "user", "password": "pass"}
)

# ✅ Good: HTTPS only
response = requests.post(
    "https://api.example.com/login",  # Encrypted
    json={"username": "user", "password": "pass"}
)

# ✅ Better: Enforce HTTPS in application
from fastapi import FastAPI, Request, HTTPException

app = FastAPI()

@app.middleware("http")
async def enforce_https(request: Request, call_next):
    if not request.url.scheme == "https":
        raise HTTPException(status_code=403, detail="HTTPS required")
    return await call_next(request)
```

## Related Patterns

**Related Tutorial**: See [Advanced Tutorial - Security](/en/learn/software-engineering/programming-language/python/tutorials/advanced#security).
**Related How-To**: See [Build REST APIs](/en/learn/software-engineering/programming-language/python/how-to/build-rest-apis), [Implement Data Validation](/en/learn/software-engineering/programming-language/python/how-to/data-validation-patterns).
**Related Cookbook**: See Cookbook recipe "Security Patterns".
