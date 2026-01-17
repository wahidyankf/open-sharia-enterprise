#!/bin/bash
# Claude Code PostToolUse hook for markdown formatting and linting
# Auto-formats and lints markdown files after Edit/Write/MultiEdit operations

# Extract file path from stdin JSON using jq
file_path=$(jq -r '.tool_input.file_path' 2>/dev/null)

# Only process markdown files
if [[ "$file_path" =~ \.md$ ]]; then
  # Run Prettier first (formatting)
  prettier --write "$file_path" 2>/dev/null || true
  
  # Run markdownlint-cli2 second (linting and auto-fixing)
  markdownlint-cli2 --fix "$file_path" 2>/dev/null || true
fi

# Always exit 0 to avoid blocking Claude Code
exit 0
