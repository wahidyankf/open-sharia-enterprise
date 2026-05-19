#!/usr/bin/env bash
# Guards against per-repo [user] overrides in .git/config.
# The repo's policy is: identity comes from your global ~/.gitconfig (or
# ~/.config/git/config), never from a per-repo override. A local override
# made the repo briefly attribute commits to an unintended identity until
# someone noticed.
#
# This check is intentionally identity-agnostic: it does NOT compare
# against any specific name or email. It only verifies that no local
# override exists, regardless of what value the override carries.
#
# Exit codes:
#   0 — no local override, OK to commit
#   1 — local override present, abort
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel 2>/dev/null || true)"
if [ -z "$repo_root" ]; then
  exit 0
fi

local_name="$(git config --local --get user.name 2>/dev/null || true)"
local_email="$(git config --local --get user.email 2>/dev/null || true)"

if [ -n "$local_name" ] || [ -n "$local_email" ]; then
  cat >&2 <<EOF
ERROR: Per-repo git identity override detected in $repo_root/.git/config

  [user]
$([ -n "$local_name" ]  && echo "      name  = $local_name")
$([ -n "$local_email" ] && echo "      email = $local_email")

Repo policy is identity-from-global-config only. To clear the override:

  git -C "$repo_root" config --local --unset user.name
  git -C "$repo_root" config --local --unset user.email
  git -C "$repo_root" config --local --remove-section user 2>/dev/null || true

Then retry your commit. The global config from ~/.gitconfig will take effect.
EOF
  exit 1
fi
