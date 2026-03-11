package domain

import "time"

// MaxAttachmentSize is the maximum allowed attachment size in bytes (10 MB).
const MaxAttachmentSize = 10 * 1024 * 1024

// AllowedMIMETypes lists the supported file MIME types for attachments.
var AllowedMIMETypes = map[string]bool{
	"image/jpeg":      true,
	"image/png":       true,
	"application/pdf": true,
}

// Attachment represents a file attached to an expense entry.
type Attachment struct {
	ID          string    `gorm:"primaryKey" json:"id"`
	ExpenseID   string    `gorm:"not null;index" json:"expense_id"`
	Filename    string    `gorm:"not null" json:"filename"`
	ContentType string    `gorm:"not null" json:"content_type"`
	Size        int64     `gorm:"not null" json:"size"`
	URL         string    `json:"url"`
	CreatedAt   time.Time `json:"created_at"`
}

// ValidateMIMEType checks if the MIME type is allowed.
func ValidateMIMEType(contentType string) error {
	if !AllowedMIMETypes[contentType] {
		return NewUnsupportedMediaTypeError("unsupported file type: " + contentType)
	}
	return nil
}

// ValidateFileSize checks if the file size is within limits.
func ValidateFileSize(size int64) error {
	if size > MaxAttachmentSize {
		return NewFileTooLargeError("file size exceeds the 10 MB limit")
	}
	return nil
}
