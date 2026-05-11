package repogovernance

import (
	"fmt"
	"os"
)

// AGENTS.md size thresholds in bytes. The 30000-byte target keeps the canonical
// instruction surface short enough for coding agents to load efficiently; the
// 40000-byte hard limit is the absolute maximum.
const (
	// AgentsMdTargetSize is the soft target size for AGENTS.md (30 KB).
	AgentsMdTargetSize = 30000
	// AgentsMdWarningSize is the second-tier warning threshold (35 KB).
	AgentsMdWarningSize = 35000
	// AgentsMdHardLimitSize is the hard upper bound that fails the audit (40 KB).
	AgentsMdHardLimitSize = 40000
)

// AgentsMdSizeFinding is the result of a single AGENTS.md size audit run.
// Severity is one of "ok", "warn", or "fail". File holds the audited path,
// Size the measured byte count, and Message a human-readable summary.
//
// NOTE: This is named AgentsMdSizeFinding (not the bare Finding requested in
// the plan's tech-docs) because the existing vendor-audit Finding type in
// this same package owns the Finding identifier with a different shape. The
// distinct name keeps both types unambiguous without renaming the existing
// public API.
type AgentsMdSizeFinding struct {
	// File is the absolute path to the audited AGENTS.md file.
	File string
	// Size is the measured byte count of the file.
	Size int64
	// Severity is one of "ok", "warn", or "fail".
	Severity string
	// Message is the human-readable summary of the classification.
	Message string
}

// CheckAgentsMdSize reads the file at path, measures its size in bytes, and
// classifies it against the AGENTS.md size thresholds. Files at or below the
// target (30 KB) are "ok"; files above the hard limit (40 KB) are "fail";
// everything between is "warn". A missing file returns an error.
func CheckAgentsMdSize(path string) (AgentsMdSizeFinding, error) {
	info, err := os.Stat(path)
	if err != nil {
		return AgentsMdSizeFinding{}, fmt.Errorf("stat AGENTS.md: %w", err)
	}
	size := info.Size()
	severity, message := classifyAgentsMdSize(size)
	return AgentsMdSizeFinding{
		File:     path,
		Size:     size,
		Severity: severity,
		Message:  message,
	}, nil
}

// classifyAgentsMdSize returns the severity label and human-readable message
// for a given byte count, applying the documented thresholds.
func classifyAgentsMdSize(size int64) (severity, message string) {
	switch {
	case size <= AgentsMdTargetSize:
		return "ok", fmt.Sprintf("AGENTS.md is %d bytes (within %d-byte target)", size, AgentsMdTargetSize)
	case size <= AgentsMdWarningSize:
		return "warn", fmt.Sprintf("AGENTS.md is %d bytes (over %d-byte target)", size, AgentsMdTargetSize)
	case size <= AgentsMdHardLimitSize:
		return "warn", fmt.Sprintf("AGENTS.md is %d bytes (over %d-byte warning threshold)", size, AgentsMdWarningSize)
	default:
		return "fail", fmt.Sprintf("AGENTS.md is %d bytes (over %d-byte hard limit)", size, AgentsMdHardLimitSize)
	}
}
