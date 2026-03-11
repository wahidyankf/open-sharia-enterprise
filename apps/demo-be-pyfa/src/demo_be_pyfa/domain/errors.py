"""Domain error hierarchy."""


class DomainError(Exception):
    """Base domain error."""


class ValidationError(DomainError):
    """Validation error with field information."""

    def __init__(self, field: str, message: str) -> None:
        self.field = field
        self.message = message
        super().__init__(message)


class NotFoundError(DomainError):
    """Resource not found."""


class ForbiddenError(DomainError):
    """Access forbidden."""


class ConflictError(DomainError):
    """Resource conflict (e.g. duplicate username)."""


class UnauthorizedError(DomainError):
    """Authentication required or failed."""


class AccountLockedError(DomainError):
    """Account is locked."""


class FileTooLargeError(DomainError):
    """Uploaded file exceeds size limit."""


class UnsupportedMediaTypeError(DomainError):
    """Uploaded file has unsupported media type."""
