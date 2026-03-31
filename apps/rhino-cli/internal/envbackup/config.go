package envbackup

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
)

// ConfigPattern defines a known uncommitted config file.
type ConfigPattern struct {
	RelPath     string // Exact relative path from repo root
	Description string // Human-readable description
	Category    string // "ai-tools", "docker", "version-mgrs", "environment"
}

// DefaultConfigPatterns lists known uncommitted local configuration files.
var DefaultConfigPatterns = []ConfigPattern{
	// AI Tools
	{RelPath: ".claude/settings.local.json", Description: "Claude Code local settings", Category: "ai-tools"},
	{RelPath: ".claude/settings.local.json.bkup", Description: "Claude Code settings backup", Category: "ai-tools"},
	{RelPath: ".cursor/mcp.json", Description: "Cursor MCP configuration", Category: "ai-tools"},
	{RelPath: ".windsurfrules", Description: "Windsurf project rules", Category: "ai-tools"},
	{RelPath: ".clinerules", Description: "Cline project rules", Category: "ai-tools"},
	{RelPath: ".aider.conf.yml", Description: "Aider configuration", Category: "ai-tools"},
	{RelPath: ".aiderignore", Description: "Aider ignore patterns", Category: "ai-tools"},
	{RelPath: ".continue/config.json", Description: "Continue configuration", Category: "ai-tools"},
	{RelPath: ".gemini/settings.json", Description: "Gemini CLI settings", Category: "ai-tools"},
	{RelPath: ".amazonq/mcp.json", Description: "Amazon Q MCP configuration", Category: "ai-tools"},
	{RelPath: ".roomodes", Description: "Roo Code custom modes", Category: "ai-tools"},
	// Docker
	{RelPath: "docker-compose.override.yml", Description: "Docker Compose local overrides", Category: "docker"},
	// Version Managers
	{RelPath: "mise.local.toml", Description: "mise local overrides", Category: "version-mgrs"},
	// Environment
	{RelPath: ".envrc", Description: "direnv environment setup", Category: "environment"},
}

// DiscoverConfig checks each pattern against repoRoot and returns FileEntry
// items for files that exist. Applies the same symlink and size checks as
// Discover(). Each returned entry has Source: "config".
func DiscoverConfig(repoRoot string, patterns []ConfigPattern, maxSize int64) ([]FileEntry, error) {
	if maxSize <= 0 {
		maxSize = DefaultMaxSize
	}

	var entries []FileEntry

	for _, p := range patterns {
		absPath := filepath.Join(repoRoot, p.RelPath)

		fi, err := os.Lstat(absPath)
		if err != nil {
			if os.IsNotExist(err) {
				continue // silently skip missing patterns
			}
			return nil, fmt.Errorf("lstat %s: %w", p.RelPath, err)
		}

		// Skip directories.
		if fi.IsDir() {
			continue
		}

		// Skip symlinks.
		if fi.Mode()&os.ModeSymlink != 0 {
			entries = append(entries, FileEntry{
				RelPath: p.RelPath,
				AbsPath: absPath,
				Skipped: true,
				Reason:  "symlink",
				Source:  "config",
			})
			continue
		}

		// Skip oversized files.
		if fi.Size() > maxSize {
			entries = append(entries, FileEntry{
				RelPath: p.RelPath,
				AbsPath: absPath,
				Size:    fi.Size(),
				Skipped: true,
				Reason:  fmt.Sprintf("file too large (%d bytes > %d)", fi.Size(), maxSize),
				Source:  "config",
			})
			continue
		}

		entries = append(entries, FileEntry{
			RelPath: p.RelPath,
			AbsPath: absPath,
			Size:    fi.Size(),
			Source:  "config",
		})
	}

	sort.Slice(entries, func(i, j int) bool {
		return entries[i].RelPath < entries[j].RelPath
	})

	return entries, nil
}
