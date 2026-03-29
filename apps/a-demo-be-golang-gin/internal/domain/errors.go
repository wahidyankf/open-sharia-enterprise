package domain

// DomainErrorCode represents the type of domain error.
type DomainErrorCode int

const (
	// ErrValidation indicates invalid input data.
	ErrValidation DomainErrorCode = iota
	// ErrNotFound indicates the requested resource was not found.
	ErrNotFound
	// ErrForbidden indicates the user lacks permission.
	ErrForbidden
	// ErrConflict indicates a resource conflict (e.g. duplicate username).
	ErrConflict
	// ErrUnauthorized indicates authentication failure.
	ErrUnauthorized
	// ErrFileTooLarge indicates the uploaded file exceeds the size limit.
	ErrFileTooLarge
	// ErrUnsupportedMediaType indicates an unsupported file type.
	ErrUnsupportedMediaType
)

// DomainError is a structured error returned from domain operations.
type DomainError struct {
	Code    DomainErrorCode
	Message string
	Field   string
}

// Error implements the error interface.
func (e *DomainError) Error() string {
	return e.Message
}

// NewValidationError creates a validation domain error.
func NewValidationError(message, field string) *DomainError {
	return &DomainError{Code: ErrValidation, Message: message, Field: field}
}

// NewNotFoundError creates a not-found domain error.
func NewNotFoundError(message string) *DomainError {
	return &DomainError{Code: ErrNotFound, Message: message}
}

// NewForbiddenError creates a forbidden domain error.
func NewForbiddenError(message string) *DomainError {
	return &DomainError{Code: ErrForbidden, Message: message}
}

// NewConflictError creates a conflict domain error.
func NewConflictError(message string) *DomainError {
	return &DomainError{Code: ErrConflict, Message: message}
}

// NewUnauthorizedError creates an unauthorized domain error.
func NewUnauthorizedError(message string) *DomainError {
	return &DomainError{Code: ErrUnauthorized, Message: message}
}

// NewFileTooLargeError creates a file-too-large domain error.
func NewFileTooLargeError(message string) *DomainError {
	return &DomainError{Code: ErrFileTooLarge, Message: message}
}

// NewUnsupportedMediaTypeError creates an unsupported-media-type domain error.
func NewUnsupportedMediaTypeError(message string) *DomainError {
	return &DomainError{Code: ErrUnsupportedMediaType, Message: message}
}
