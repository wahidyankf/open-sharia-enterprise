package cmd

import (
	"io/fs"
	"time"
)

// mockFileInfo implements fs.FileInfo for mocked os.Stat results.
type mockFileInfo struct {
	name  string
	size  int64
	isDir bool
	mode  fs.FileMode
}

func (m *mockFileInfo) Name() string       { return m.name }
func (m *mockFileInfo) Size() int64        { return m.size }
func (m *mockFileInfo) Mode() fs.FileMode  { return m.mode }
func (m *mockFileInfo) ModTime() time.Time { return time.Time{} }
func (m *mockFileInfo) IsDir() bool        { return m.isDir }
func (m *mockFileInfo) Sys() any           { return nil }
