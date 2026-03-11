// Package domain contains the core business entities and validation logic
// for the demo-be-gogn expense tracking application.
package domain

import (
	"regexp"
	"time"
)

// UserStatus represents the status of a user account.
type UserStatus string

const (
	// StatusActive indicates an active user account.
	StatusActive UserStatus = "ACTIVE"
	// StatusInactive indicates a self-deactivated account.
	StatusInactive UserStatus = "INACTIVE"
	// StatusDisabled indicates an admin-disabled account.
	StatusDisabled UserStatus = "DISABLED"
	// StatusLocked indicates a locked-out account.
	StatusLocked UserStatus = "LOCKED"
)

// Role represents the user's role.
type Role string

const (
	// RoleUser is the default user role.
	RoleUser Role = "USER"
	// RoleAdmin is the administrator role.
	RoleAdmin Role = "ADMIN"
)

// User represents a user account in the system.
type User struct {
	ID             string     `gorm:"primaryKey" json:"id"`
	Username       string     `gorm:"uniqueIndex;not null" json:"username"`
	Email          string     `gorm:"not null" json:"email"`
	PasswordHash   string     `gorm:"not null" json:"-"`
	DisplayName    string     `json:"display_name"`
	Status         UserStatus `gorm:"not null;default:ACTIVE" json:"status"`
	Role           Role       `gorm:"not null;default:USER" json:"role"`
	FailedAttempts int        `gorm:"default:0" json:"failed_attempts,omitempty"`
	LockedAt       *time.Time `json:"locked_at,omitempty"`
	CreatedAt      time.Time  `json:"created_at"`
	UpdatedAt      time.Time  `json:"updated_at"`
}

// RefreshToken represents a stored refresh token.
type RefreshToken struct {
	ID        string    `gorm:"primaryKey"`
	UserID    string    `gorm:"not null;index"`
	TokenStr  string    `gorm:"uniqueIndex;not null"`
	Revoked   bool      `gorm:"default:false"`
	ExpiresAt time.Time `gorm:"not null"`
	CreatedAt time.Time
}

// BlacklistedToken represents a blacklisted access token JTI.
type BlacklistedToken struct {
	JTI       string    `gorm:"primaryKey"`
	ExpiresAt time.Time `gorm:"not null"`
	CreatedAt time.Time
}

var (
	emailRegex    = regexp.MustCompile(`^[a-zA-Z0-9._%+\-]+@[a-zA-Z0-9.\-]+\.[a-zA-Z]{2,}$`)
	usernameRegex = regexp.MustCompile(`^[a-zA-Z0-9_\-]{3,50}$`)
	upperRegex    = regexp.MustCompile(`[A-Z]`)
	specialRegex  = regexp.MustCompile(`[^a-zA-Z0-9]`)
)

// ValidateEmail checks if the email is valid.
func ValidateEmail(email string) error {
	if !emailRegex.MatchString(email) {
		return NewValidationError("invalid email format", "email")
	}
	return nil
}

// ValidateUsername checks if the username is valid.
func ValidateUsername(username string) error {
	if !usernameRegex.MatchString(username) {
		return NewValidationError("username must be 3-50 characters and contain only letters, numbers, underscores, or hyphens", "username")
	}
	return nil
}

// ValidatePasswordStrength checks if the password meets strength requirements.
func ValidatePasswordStrength(password string) error {
	if len(password) == 0 {
		return NewValidationError("password is required", "password")
	}
	if len(password) < 12 {
		return NewValidationError("password must be at least 12 characters", "password")
	}
	if !upperRegex.MatchString(password) {
		return NewValidationError("password must contain at least one uppercase letter", "password")
	}
	if !specialRegex.MatchString(password) {
		return NewValidationError("password must contain at least one special character", "password")
	}
	return nil
}
