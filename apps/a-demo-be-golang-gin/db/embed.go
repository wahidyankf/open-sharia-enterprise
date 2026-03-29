// Package db provides the embedded SQL migration files for goose.
package db

import "embed"

// EmbedMigrations holds the SQL migration files embedded at compile time.
//
//go:embed migrations/*.sql
var EmbedMigrations embed.FS
