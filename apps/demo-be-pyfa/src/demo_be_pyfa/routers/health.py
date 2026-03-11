"""Health check router."""

from fastapi import APIRouter
from pydantic import BaseModel

router = APIRouter()


class HealthResponse(BaseModel):
    """Health check response model."""

    status: str


@router.get("/health", response_model=HealthResponse)
def get_health() -> HealthResponse:
    """Return service health status."""
    return HealthResponse(status="UP")
