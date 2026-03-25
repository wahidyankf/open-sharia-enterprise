#!/usr/bin/env bash
# Claude Code PreToolUse hook: warm Nx cache before git push
#
# The pre-push git hook runs `nx affected -t typecheck lint test:quick`
# which can take 20+ minutes on a cold cache in this polyglot monorepo.
# This hook runs the SAME commands beforehand so the pre-push hook
# hits cached results and completes in seconds.
#
# Key: we source ~/.zshrc to pick up the correct PATH for all tools
# (dotnet, uv, oapi-codegen, sdkman, pyenv, cargo, etc.) which the
# git hook's /bin/sh environment may not have.

set -euo pipefail

# --- Parse stdin JSON to check if this is a git push command ---
INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty' 2>/dev/null)

# Only intercept git push commands (not other bash commands)
if ! echo "$COMMAND" | grep -qE '^\s*git\s+push\b'; then
  exit 0
fi

echo "Warming Nx cache before push (mirrors .husky/pre-push)..." >&2

# --- Source user environment for all polyglot tools ---
# shellcheck disable=SC1091
if [ -f "$HOME/.zshrc" ]; then
  # Use zsh to properly source .zshrc (it may use zsh-specific syntax)
  ENV_EXPORTS=$(zsh -c 'source ~/.zshrc 2>/dev/null; env' 2>/dev/null || true)
  # Extract key PATH and tool variables
  NEW_PATH=$(echo "$ENV_EXPORTS" | grep '^PATH=' | head -1 | cut -d= -f2-)
  NEW_DOTNET_ROOT=$(echo "$ENV_EXPORTS" | grep '^DOTNET_ROOT=' | head -1 | cut -d= -f2-)
  NEW_JAVA_HOME=$(echo "$ENV_EXPORTS" | grep '^JAVA_HOME=' | head -1 | cut -d= -f2-)
  if [ -n "$NEW_PATH" ]; then export PATH="$NEW_PATH"; fi
  if [ -n "$NEW_DOTNET_ROOT" ]; then export DOTNET_ROOT="$NEW_DOTNET_ROOT"; fi
  if [ -n "$NEW_JAVA_HOME" ]; then export JAVA_HOME="$NEW_JAVA_HOME"; fi
fi

# Ensure standalone tool paths are available
export PATH="$HOME/.local/bin:$HOME/go/bin:$PATH"

# Set JAVA_HOME_21_X64 for Clojure/Gradle (mirrors CI)
if [ -d "$HOME/.sdkman/candidates/java/21.0.1-tem" ]; then
  export JAVA_HOME_21_X64="$HOME/.sdkman/candidates/java/21.0.1-tem"
fi

# Activate JDK 25 if available (mirrors CI's default Java)
JAVA25_HOME=$(find "$HOME/.sdkman/candidates/java" -maxdepth 1 -name "25.*" -type d 2>/dev/null | head -1)
if [ -n "$JAVA25_HOME" ]; then
  export JAVA_HOME="$JAVA25_HOME"
  export PATH="$JAVA_HOME/bin:$PATH"
fi

# --- Compute parallelism (same formula as .husky/pre-push) ---
PARALLEL=$(( $(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4) - 1 ))

# --- Run the exact same commands as .husky/pre-push ---
echo "Running: npx nx affected -t typecheck lint test:quick --parallel=$PARALLEL" >&2
npx nx affected -t typecheck lint test:quick --parallel="$PARALLEL" 2>&1 || true

echo "Running: npm run lint:md" >&2
npm run lint:md 2>&1 || true

echo "Cache warming complete. Pre-push hook should now hit cache." >&2

# Always exit 0 — this is a cache-warming optimization, not a gate.
# The actual pre-push git hook is the real quality gate.
exit 0
