// Package envbackup provides backup and restore operations for .env files in a git repository.
package envbackup

// Options configures a backup or restore operation.
type Options struct {
	RepoRoot      string                       // Absolute path to git root (or worktree root)
	BackupDir     string                       // Absolute path to backup directory
	SkipDirs      []string                     // Directory basenames to skip during walk
	MaxSize       int64                        // Max file size in bytes (default 1 MB)
	WorktreeAware bool                         // If true, namespace backup by worktree/repo name
	WorktreeName  string                       // Set by cmd layer from detectWorktree(); used to populate Result
	Force         bool                         // Skip confirmation prompt
	IncludeConfig bool                         // Also discover config files
	ConfirmFn     func(existing []string) bool // Called when destinations exist; nil = force
}

// FileEntry represents a single .env file found or processed.
type FileEntry struct {
	RelPath string `json:"relPath"`           // Relative to repo root
	AbsPath string `json:"absPath,omitempty"` // Absolute path in source location
	Size    int64  `json:"size,omitempty"`    // File size in bytes
	Skipped bool   `json:"skipped,omitempty"` // True if skipped (symlink, too large)
	Reason  string `json:"reason,omitempty"`  // Skip reason (empty if not skipped)
	Source  string `json:"source,omitempty"`  // "env" or "config" — empty defaults to "env"
}

// Result holds the outcome of a backup or restore operation.
type Result struct {
	Direction    string      // "backup" or "restore"
	Dir          string      // Backup directory path
	Files        []FileEntry // All discovered files (including skipped)
	Copied       int         // Count of successfully copied files
	Skipped      int         // Count of skipped files
	Errors       []string    // Non-fatal warnings
	WorktreeName string      // Worktree/repo name when --worktree-aware is used
	Cancelled    bool        // True if user declined confirmation prompt
}

// DefaultSkipDirs lists directory basenames to skip during file discovery.
var DefaultSkipDirs = []string{
	".git",
	"node_modules", "bower_components",
	".nx", ".next", ".turbo", ".cache", ".parcel-cache", ".nyc_output",
	"dist", "build", "coverage",
	"__pycache__", ".venv", "venv",
	"target", ".gradle",
	"vendor",
	"_build", "deps", ".elixir_ls", ".mix",
	".dart_tool",
	".cargo",
	"zig-cache",
	".stack-work",
	"elm-stuff",
	"_deps",
	".terraform", ".pulumi",
	"generated-contracts",
}

const DefaultMaxSize = 1024 * 1024 // 1 MB
const DefaultBackupDir = "ose-open-env-backup"
