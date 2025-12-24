---
title: "Manage Configuration"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 1000009
description: "Handle configuration with environment variables, config files, and Pydantic Settings"
tags: ["python", "configuration", "environment-variables", "pydantic", "config"]
categories: ["learn"]
---

## Problem

Hardcoded configuration makes applications inflexible. Different environments need different settings without code changes. Managing secrets securely while avoiding version control exposure is challenging. Type validation ensures configuration correctness.

This guide shows effective configuration management in Python.

## Environment Variables

### Reading Environment Variables

```python
import os

database_url = os.getenv('DATABASE_URL')

port = os.getenv('PORT', '8080')

def get_required_env(key):
    value = os.getenv(key)
    if value is None:
        raise ValueError(f"Required environment variable not set: {key}")
    return value

api_key = get_required_env('API_KEY')

max_connections = int(os.getenv('MAX_CONNECTIONS', '10'))

debug = os.getenv('DEBUG', 'false').lower() in ('true', '1', 'yes')
```

### Environment Variable Patterns

```python
from dataclasses import dataclass
import os

@dataclass
class Config:
    database_url: str
    port: int
    debug: bool
    max_connections: int
    api_key: str

def load_config():
    return Config(
        database_url=get_required_env('DATABASE_URL'),
        port=int(os.getenv('PORT', '8080')),
        debug=os.getenv('DEBUG', 'false').lower() in ('true', '1'),
        max_connections=int(os.getenv('MAX_CONNECTIONS', '10')),
        api_key=get_required_env('API_KEY')
    )

def get_required_env(key):
    value = os.getenv(key)
    if value is None:
        raise ValueError(f"Required: {key}")
    return value

config = load_config()
print(f"Server running on port {config.port}")
```

## python-dotenv for .env Files

### Loading .env Files

```python
from dotenv import load_dotenv
import os

load_dotenv()  # Loads .env from current directory

database_url = os.getenv('DATABASE_URL')

load_dotenv('.env.development')

load_dotenv(override=True)  # Override existing env vars

from pathlib import Path

env_path = Path('.') / '.env'
load_dotenv(dotenv_path=env_path)
```

**.env file format:**

```bash
DATABASE_URL=postgresql://localhost:5432/mydb
DB_MAX_CONNECTIONS=20

API_KEY=sk_test_abc123
API_BASE_URL=https://api.example.com

PORT=8080
DEBUG=true
LOG_LEVEL=info
```

**.gitignore:**

```gitignore
.env
.env.local
.env.*.local

!.env.example
```

## Configuration Files

### YAML Configuration

```python
import yaml
from pathlib import Path

def load_yaml_config(filename):
    path = Path(filename)
    with path.open() as f:
        return yaml.safe_load(f)

"""
database:
  url: postgresql://localhost:5432/mydb
  max_connections: 20
  timeout: 30

api:
  base_url: https://api.example.com
  timeout: 10
  retry_attempts: 3

logging:
  level: info
  format: json
"""

config = load_yaml_config('config.yaml')
db_url = config['database']['url']
api_timeout = config['api']['timeout']
```

### JSON Configuration

```python
import json
from pathlib import Path

def load_json_config(filename):
    path = Path(filename)
    with path.open() as f:
        return json.load(f)

def save_json_config(filename, config):
    path = Path(filename)
    with path.open('w') as f:
        json.dump(config, f, indent=2)

"""
{
  "database": {
    "url": "postgresql://localhost:5432/mydb",
    "max_connections": 20
  },
  "debug": false
}
"""

config = load_json_config('config.json')
```

### TOML Configuration

```python
import tomli  # Python < 3.11
from pathlib import Path

def load_toml_config(filename):
    path = Path(filename)
    with path.open('rb') as f:
        return tomli.load(f)

"""
[database]
url = "postgresql://localhost:5432/mydb"
max_connections = 20

[api]
base_url = "https://api.example.com"
timeout = 10

[logging]
level = "info"
"""

config = load_toml_config('config.toml')
```

## Pydantic Settings

### Type-Safe Configuration

```python
from pydantic_settings import BaseSettings
from pydantic import Field, validator

class Settings(BaseSettings):
    # Database
    database_url: str
    db_max_connections: int = 10

    # API
    api_key: str
    api_base_url: str = "https://api.example.com"

    # Application
    app_name: str = "MyApp"
    debug: bool = False
    port: int = 8080

    # Custom validation
    @validator('port')
    def validate_port(cls, v):
        if v < 1024 or v > 65535:
            raise ValueError('Port must be between 1024 and 65535')
        return v

    class Config:
        env_file = '.env'
        env_file_encoding = 'utf-8'
        case_sensitive = False

settings = Settings()

print(f"Database: {settings.database_url}")
print(f"Port: {settings.port}")

settings = Settings(_env_file='.env.production')
```

### Nested Configuration

```python
from pydantic_settings import BaseSettings
from pydantic import BaseModel

class DatabaseConfig(BaseModel):
    url: str
    max_connections: int = 10
    timeout: int = 30

class APIConfig(BaseModel):
    base_url: str
    key: str
    timeout: int = 10

class Settings(BaseSettings):
    database: DatabaseConfig
    api: APIConfig
    debug: bool = False

    class Config:
        env_file = '.env'
        env_nested_delimiter = '__'


settings = Settings()
print(settings.database.url)
print(settings.api.key)
```

## Configuration Priority

### Layered Configuration

```python
from pydantic_settings import BaseSettings

class Settings(BaseSettings):
    database_url: str = "postgresql://localhost/default"
    port: int = 8080
    debug: bool = False

    class Config:
        # Priority (highest to lowest):
        # 1. Environment variables
        # 2. .env file
        # 3. Class defaults
        env_file = '.env'

```

### Multiple Configuration Sources

```python
import os
from pathlib import Path

def load_config():
    # 1. Load defaults
    config = {
        'port': 8080,
        'debug': False,
        'max_connections': 10,
    }

    # 2. Load from config file
    config_file = Path('config.yaml')
    if config_file.exists():
        import yaml
        with config_file.open() as f:
            file_config = yaml.safe_load(f)
            config.update(file_config)

    # 3. Override with environment variables
    env_mapping = {
        'PORT': 'port',
        'DEBUG': 'debug',
        'MAX_CONNECTIONS': 'max_connections',
    }

    for env_key, config_key in env_mapping.items():
        value = os.getenv(env_key)
        if value is not None:
            # Type conversion based on default
            if isinstance(config[config_key], bool):
                config[config_key] = value.lower() in ('true', '1', 'yes')
            elif isinstance(config[config_key], int):
                config[config_key] = int(value)
            else:
                config[config_key] = value

    return config

config = load_config()
```

## argparse for Command-Line Args

### Basic Argument Parsing

```python
import argparse

def parse_args():
    parser = argparse.ArgumentParser(
        description='My Application'
    )

    # ✅ Positional argument
    parser.add_argument('input', help='Input file path')

    # ✅ Optional argument
    parser.add_argument(
        '--output', '-o',
        default='output.txt',
        help='Output file path'
    )

    # ✅ Boolean flag
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Verbose output'
    )

    # ✅ Integer argument
    parser.add_argument(
        '--port', '-p',
        type=int,
        default=8080,
        help='Server port'
    )

    # ✅ Choices
    parser.add_argument(
        '--format',
        choices=['json', 'xml', 'csv'],
        default='json',
        help='Output format'
    )

    return parser.parse_args()

args = parse_args()
print(f"Input: {args.input}")
print(f"Output: {args.output}")
print(f"Verbose: {args.verbose}")
```

## Security Best Practices

### Avoiding Hardcoded Secrets

```python
import os

API_KEY = "sk_live_secret123"  # NEVER
DATABASE_PASSWORD = "password"  # NEVER

def get_api_key():
    key = os.getenv('API_KEY')
    if not key:
        raise ValueError("API_KEY not set")
    return key

class Settings(BaseSettings):
    api_key: str  # Required - will raise error if missing
    db_password: str  # Required

    class Config:
        env_file = '.env'

settings = Settings()
```

### Secret Management

```python
import boto3
import json

def get_secret(secret_name):
    client = boto3.client('secretsmanager')
    response = client.get_secret_value(SecretId=secret_name)
    return json.loads(response['SecretString'])

db_credentials = get_secret('prod/database')
database_url = db_credentials['url']
password = db_credentials['password']

class SecretCache:
    def __init__(self):
        self._cache = {}

    def get(self, key):
        if key not in self._cache:
            self._cache[key] = get_secret(key)
        return self._cache[key]

cache = SecretCache()
db_creds = cache.get('prod/database')
```

## Configuration Validation

### Validating Configuration

```python
from pydantic_settings import BaseSettings
from pydantic import validator, Field
from typing import Literal

class Settings(BaseSettings):
    port: int = Field(..., ge=1024, le=65535)
    max_connections: int = Field(..., gt=0, le=100)
    log_level: Literal['DEBUG', 'INFO', 'WARNING', 'ERROR']
    database_url: str

    @validator('database_url')
    def validate_database_url(cls, v):
        if not v.startswith(('postgresql://', 'mysql://')):
            raise ValueError('Invalid database URL')
        return v

    @validator('max_connections')
    def validate_connections(cls, v, values):
        # Cross-field validation
        if values.get('debug') and v > 10:
            raise ValueError('Max 10 connections in debug mode')
        return v

try:
    settings = Settings()
except ValueError as e:
    print(f"Configuration error: {e}")
```

## Summary

Configuration management in Python uses environment variables as the primary mechanism with python-dotenv for development convenience. Environment variables work across deployment platforms and keep secrets out of version control.

python-dotenv loads .env files automatically without requiring environment variable setup on each machine. Production uses actual environment variables. Never commit .env files - use .env.example templates showing required variables.

Configuration files in YAML, JSON, or TOML formats provide structured configuration. YAML offers readability with comments, JSON integrates with web APIs, TOML provides clear syntax for nested configuration. Choose based on complexity and tooling.

Pydantic Settings combines environment variables, .env files, and type validation. Define configuration as typed classes with validators, pydantic handles parsing and validation. Nested models organize complex configuration hierarchically.

Configuration priority follows environment variables > config files > defaults. This ordering lets operators override settings at runtime. Pydantic Settings implements this priority automatically with env_file and class defaults.

argparse handles command-line arguments with type checking and help text generation. Parse arguments at startup, combine with environment variables for flexible configuration. Use for CLI applications and scripts.

Never hardcode secrets in source code. Load from environment variables, secret management services (AWS Secrets Manager, HashiCorp Vault), or encrypted files. Fail immediately if required secrets are missing rather than using defaults.

Validation ensures configuration correctness at startup. Check required fields exist, validate ranges for numbers, verify URLs and connection strings. Use Pydantic validators for automatic validation with clear error messages.

Type-safe configuration with Pydantic provides autocomplete, type checking, and validation. Define fields with types, defaults, and constraints. Validation catches configuration errors during initialization, not during request handling.

Configuration best practices include explicit environment variable naming, .env files for development, separate configurations per environment, and validation on startup. These patterns produce reliable, secure, maintainable configuration management.

## Related Content

- [Python Best Practices](/en/learn/software-engineering/programming-language/python/explanation/best-practices)
- [How to Handle Files and Resources](/en/learn/software-engineering/programming-language/python/how-to/handle-files-and-resources)
- [How to Build CLI Applications](/en/learn/software-engineering/programming-language/python/how-to/build-cli-applications)
