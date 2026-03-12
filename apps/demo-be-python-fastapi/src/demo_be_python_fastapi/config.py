"""Application configuration using pydantic-settings."""

from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    """Application settings loaded from environment variables."""

    model_config = SettingsConfigDict(env_file=".env", extra="ignore")

    database_url: str = "sqlite:///:memory:"
    app_jwt_secret: str = "dev-jwt-secret-at-least-32-chars-long-for-dev"
    app_jwt_issuer: str = "demo-be-python-fastapi"
    max_failed_login_attempts: int = 5
    max_attachment_size_bytes: int = 10 * 1024 * 1024  # 10MB


settings = Settings()
