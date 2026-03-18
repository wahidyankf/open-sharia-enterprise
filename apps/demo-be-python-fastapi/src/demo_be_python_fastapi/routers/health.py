"""Health check router."""

from fastapi import APIRouter

from generated_contracts import HealthResponse

router = APIRouter()


@router.get("/health", response_model=HealthResponse)
def get_health() -> HealthResponse:
    """Return service health status."""
    return HealthResponse(status="UP")
