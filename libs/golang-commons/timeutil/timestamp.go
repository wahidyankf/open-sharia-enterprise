// Package timeutil provides timestamp utilities shared across Go CLI tools and libraries.
package timeutil

import "time"

// Timestamp returns the current time in RFC3339 format.
func Timestamp() string {
	return time.Now().Format(time.RFC3339)
}

// JakartaTimestamp returns the current time as ISO 8601 in the Asia/Jakarta timezone.
func JakartaTimestamp() string {
	loc, _ := time.LoadLocation("Asia/Jakarta")
	return time.Now().In(loc).Format("2006-01-02T15:04:05-07:00")
}
