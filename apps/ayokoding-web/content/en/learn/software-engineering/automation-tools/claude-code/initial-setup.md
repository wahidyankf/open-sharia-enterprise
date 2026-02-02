---
title: "Initial Setup"
date: 2026-02-02T00:00:00+07:00
draft: false
weight: 100001
description: "Install and configure Claude Code CLI with API authentication, project initialization, and environment setup (0-5% coverage)"
tags: ["claude-code", "installation", "setup", "authentication", "cli", "configuration"]
---

**Get Claude Code running in 10 minutes.** This guide walks you through installation, API authentication, and initial configuration to start using AI-powered coding assistance in your terminal.

## Prerequisites

Before installing Claude Code, ensure you have:

- **Operating System**: macOS, Linux, or Windows (WSL2 recommended)
- **Terminal**: Bash, Zsh, or Fish shell
- **Node.js**: 18.x or later (for npm installation method)
- **Anthropic API Access**: Active account at [console.anthropic.com](https://console.anthropic.com)
- **Internet Connection**: Required for API calls

**No prior AI experience required** - this guide assumes only basic terminal skills.

## Installation

### Method 1: npm (Recommended)

**For users with Node.js** installed:

```bash
npm install -g @anthropic/claude-code
```

**Verify installation**:

```bash
claude --version
```

Expected output: `claude-code version X.Y.Z`

### Method 2: Homebrew (macOS/Linux)

```bash
brew tap anthropic/claude-code
brew install claude-code
```

### Method 3: Direct Download

**Download from GitHub releases**:

1. Visit [github.com/anthropics/claude-code/releases](https://github.com/anthropics/claude-code/releases)
2. Download binary for your platform (macOS, Linux, Windows)
3. Extract and move to PATH:

```bash
# macOS/Linux example
tar -xzf claude-code-*.tar.gz
sudo mv claude-code /usr/local/bin/
chmod +x /usr/local/bin/claude-code
```

### Verify Installation

```bash
# Check version
claude --version

# Check help
claude --help
```

**If `claude: command not found`**:

- Ensure installation directory is in PATH
- Restart terminal to reload PATH
- Check installation with `which claude`

## API Authentication

### Step 1: Get API Key

1. Visit [console.anthropic.com](https://console.anthropic.com)
2. Sign in or create account
3. Navigate to **API Keys** section
4. Click **Create Key**
5. Copy the key (starts with `sk-ant-...`)

**Security warning**: Treat API keys like passwords. Never commit to version control or share publicly.

### Step 2: Configure API Key

**Option A: Environment variable (recommended for single user)**

```bash
# Add to ~/.bashrc, ~/.zshrc, or ~/.config/fish/config.fish
export ANTHROPIC_API_KEY="sk-ant-api03-..."

# Reload shell configuration
source ~/.bashrc  # or ~/.zshrc
```

**Option B: Configuration file (recommended for multiple projects)**

```bash
# Create config directory
mkdir -p ~/.config/claude-code

# Create configuration file
cat > ~/.config/claude-code/config.json <<EOF
{
  "apiKey": "sk-ant-api03-...",
  "model": "claude-sonnet-4-5-20250929",
  "maxTokens": 4096
}
EOF

# Secure the config file
chmod 600 ~/.config/claude-code/config.json
```

**Option C: Project-specific (recommended for teams)**

```bash
# In your project root
cat > .claude.json <<EOF
{
  "apiKey": "sk-ant-api03-...",
  "model": "claude-sonnet-4-5-20250929"
}
EOF

# Add to .gitignore
echo ".claude.json" >> .gitignore
```

### Step 3: Verify Authentication

```bash
# Test API connection
claude test

# Expected output:
# ✓ API key valid
# ✓ Connected to Anthropic API
# ✓ Model: claude-sonnet-4-5-20250929
```

**If authentication fails**:

- Check API key is correct (no extra spaces)
- Verify key hasn't expired in console.anthropic.com
- Ensure internet connection is active
- Check firewall isn't blocking api.anthropic.com

## Project Initialization

### Create First Project

```bash
# Navigate to your project
cd ~/projects/my-app

# Initialize Claude Code
claude init

# Expected output:
# ✓ Created .claude/ directory
# ✓ Created .claude/context.json
# ✓ Indexed 142 files
# ✓ Ready to use Claude Code
```

**What `claude init` does**:

- Creates `.claude/` directory for context storage
- Indexes project files for codebase awareness
- Generates `.claude/context.json` with project metadata
- Respects `.gitignore` (won't index ignored files)

### Verify Project Setup

```bash
# Check project status
claude status

# Expected output:
# Project: my-app
# Files indexed: 142
# Context: Ready
# Model: claude-sonnet-4-5-20250929
```

## Configuration Options

### Basic Configuration

**Model selection**:

```bash
# Use faster model (Haiku) for simple tasks
claude config set model claude-haiku-3-5-20250929

# Use more capable model (Sonnet) for complex tasks
claude config set model claude-sonnet-4-5-20250929

# Use most powerful model (Opus) for critical tasks
claude config set model claude-opus-4-5-20251101
```

**Token limits**:

```bash
# Set maximum tokens per response
claude config set maxTokens 8192

# For larger codebases, increase context window
claude config set maxTokens 16384
```

**Temperature** (creativity):

```bash
# Lower temperature (more deterministic)
claude config set temperature 0.3

# Default temperature (balanced)
claude config set temperature 0.7

# Higher temperature (more creative)
claude config set temperature 1.0
```

### Advanced Configuration

**Context window settings**:

```json
{
  "apiKey": "sk-ant-...",
  "model": "claude-sonnet-4-5-20250929",
  "maxTokens": 8192,
  "temperature": 0.7,
  "contextWindow": {
    "maxFiles": 100,
    "maxFileSize": 1048576,
    "excludePatterns": ["node_modules", "dist", "build", ".git"]
  }
}
```

**Auto-index settings**:

```json
{
  "autoIndex": {
    "enabled": true,
    "watchFiles": true,
    "debounceMs": 500,
    "excludeExtensions": [".jpg", ".png", ".pdf", ".zip"]
  }
}
```

**Custom prompts**:

```json
{
  "prompts": {
    "systemPrompt": "You are a helpful coding assistant specializing in...",
    "codeStyle": "Follow Google Java Style Guide"
  }
}
```

### View Current Configuration

```bash
# Show all configuration
claude config list

# Show specific setting
claude config get model
```

## Directory Structure

After setup, your project has:

```
my-app/
├── .claude/
│   ├── context.json      # Project metadata and file index
│   ├── conversations/    # Conversation history
│   └── cache/            # Cached responses
├── .claude.json          # Project-specific config (optional)
└── [your project files]
```

**Important files**:

- `.claude/context.json` - Codebase index, safe to commit
- `.claude.json` - API key and config, **DO NOT commit**
- `.claude/conversations/` - Chat history, optional to commit

## Workspace Configuration

### Multiple Projects

**Manage multiple projects** with different configurations:

```bash
# Project A (uses Haiku for speed)
cd ~/projects/project-a
claude init
claude config set model claude-haiku-3-5-20250929

# Project B (uses Sonnet for quality)
cd ~/projects/project-b
claude init
claude config set model claude-sonnet-4-5-20250929
```

Each project maintains independent configuration.

### Global vs. Project Config

**Precedence** (highest to lowest):

1. Project-specific: `.claude.json` in project root
2. User-specific: `~/.config/claude-code/config.json`
3. Environment variable: `ANTHROPIC_API_KEY`

**Best practice**: Use global config for API key, project config for model/settings.

## Troubleshooting

### Common Issues

**Issue**: `claude: command not found`

**Solution**:

```bash
# Check installation
which claude

# If not in PATH, add installation directory to PATH
export PATH="$PATH:/usr/local/bin"
```

**Issue**: `API key invalid`

**Solution**:

```bash
# Verify key format (should start with sk-ant-)
echo $ANTHROPIC_API_KEY

# Re-enter key without extra spaces
export ANTHROPIC_API_KEY="sk-ant-api03-..."
```

**Issue**: `Failed to index project`

**Solution**:

```bash
# Check for large files
find . -type f -size +10M

# Exclude large files in .gitignore
echo "*.bin" >> .gitignore

# Re-initialize
rm -rf .claude
claude init
```

**Issue**: `Connection timeout`

**Solution**:

```bash
# Check internet connection
ping api.anthropic.com

# Check firewall/proxy settings
# Configure proxy if needed:
export HTTPS_PROXY="http://proxy.company.com:8080"
```

### Getting Help

```bash
# Command help
claude --help
claude init --help

# Check status
claude status

# Test connection
claude test

# View logs
claude logs --tail 50
```

## Security Best Practices

**Protect your API key**:

```bash
# Use environment variable
export ANTHROPIC_API_KEY="sk-ant-..."

# Or config file with restricted permissions
chmod 600 ~/.config/claude-code/config.json
```

**Exclude from version control**:

```bash
# Add to .gitignore
cat >> .gitignore <<EOF
.claude.json
.claude/conversations/
.env
EOF
```

**Rotate keys periodically**:

1. Create new key in Anthropic Console
2. Update configuration
3. Revoke old key
4. Verify new key works: `claude test`

**Monitor usage**:

- Check usage in [console.anthropic.com](https://console.anthropic.com)
- Set spending limits if available
- Review conversation history regularly

## Verification Checklist

Before proceeding to Quick Start, verify:

- [ ] Claude Code installed: `claude --version` works
- [ ] API key configured: `claude test` succeeds
- [ ] Project initialized: `.claude/` directory exists
- [ ] Configuration set: `claude config list` shows settings
- [ ] Codebase indexed: `claude status` shows file count
- [ ] `.claude.json` in `.gitignore` if using project-specific config

## What's Next?

Setup complete! You're ready to:

- **Quick Start** → [Quick Start Tutorial](/en/learn/software-engineering/automation-tools/claude-code/quick-start) - Build your first project with Claude Code
- **By Example** → [By Example (75+ Examples)](/en/learn/software-engineering/automation-tools/claude-code/by-example) - Learn through heavily annotated code examples
- **Overview** → [Claude Code Overview](/en/learn/software-engineering/automation-tools/claude-code/overview) - Understand capabilities and use cases

## Additional Resources

- **Official Documentation**: [docs.anthropic.com/claude-code](https://docs.anthropic.com/claude-code)
- **GitHub Repository**: [github.com/anthropics/claude-code](https://github.com/anthropics/claude-code)
- **API Documentation**: [docs.anthropic.com/api](https://docs.anthropic.com/api)
- **Community**: [community.anthropic.com](https://community.anthropic.com)

**Coverage achieved**: 0-5% (installation and configuration basics)

**Next step**: Quick Start tutorial for first practical experience with Claude Code.
