# Delivery Plan: OpenCode Adoption

## Implementation Phases

This plan is divided into 6 phases, ordered by priority and dependency:

| Phase | Focus                         | Priority     | Dependencies |
| ----- | ----------------------------- | ------------ | ------------ |
| **0** | **Skill Renaming (REQUIRED)** | **CRITICAL** | **None**     |
| 1     | Skills Validation             | Must         | Phase 0      |
| 2     | Core Configuration            | Must         | Phase 1      |
| 3     | Instructions File             | Must         | Phase 2      |
| 4     | MCP Integration               | Should       | Phase 2      |
| 5     | Agent Translation             | Could        | Phase 3      |

---

## Phase 0: Skills and Agents Renaming (CRITICAL PREREQUISITE)

**Goal**: Fix skill AND agent naming to comply with both Claude Code and OpenCode specs

**Scope**: 65 files total (19 skills + 46 agents)

### Why This Is Required

**CONFIRMED**: Both Claude Code AND OpenCode require skill AND agent names matching `[a-z0-9-]+`:

- Underscores (`_`) are **NOT allowed** in skill or agent names
- Consecutive hyphens (`--`) are **NOT allowed** in skill or agent names
- Current names like `docs__applying-content-quality` (skills) and `docs__checker` (agents) are **INVALID**
- **Correct format**: Replace `__` with **SINGLE hyphen** `-`
- Skills and agents will NOT load in OpenCode until renamed
- Agent renaming is **MORE CRITICAL** because workflows depend on agent names

**Impact**:

- 19 skill files need renaming
- 46 agent files need renaming (2.4x more than skills)
- All workflow agent references must be updated
- Agent renaming affects core automation infrastructure

### Renaming Tasks

#### Part A: Skills Renaming (19 files)

- [ ] **0.1 Create and validate skill renaming script**

  ```bash
  #!/bin/bash
  # Rename all skill directories from underscore to hyphen format
  # CRITICAL: Replace __ with SINGLE hyphen -, NOT double hyphen --

  echo "Renaming skills from __ to - format..."
  for dir in .claude/skills/*__*; do
    newname=$(echo "$dir" | sed 's/__/-/g')
    echo "  $dir -> $newname"
    git mv "$dir" "$newname"
  done

  echo ""
  echo "Validating: checking for invalid patterns..."

  # Validate no double hyphens
  invalid=$(find .claude/skills -type d -name "*--*" | wc -l)
  if [ "$invalid" -gt 0 ]; then
    echo "ERROR: Found skills with double hyphens (--)"
    find .claude/skills -type d -name "*--*"
    exit 1
  fi

  # Validate no underscores
  invalid=$(find .claude/skills -type d -name "*_*" | wc -l)
  if [ "$invalid" -gt 0 ]; then
    echo "ERROR: Found skills with underscores (_)"
    find .claude/skills -type d -name "*_*"
    exit 1
  fi

  echo "✓ All skill names are valid (no __ or --)"
  echo "✓ Skill renaming complete!"
  ```

- [ ] **0.2 Rename all 19 skill directories**

  | Current Name                                 | New Name                                    |
  | -------------------------------------------- | ------------------------------------------- |
  | `docs__applying-content-quality`             | `docs-applying-content-quality`             |
  | `docs__applying-diataxis-framework`          | `docs-applying-diataxis-framework`          |
  | `docs__creating-accessible-diagrams`         | `docs-creating-accessible-diagrams`         |
  | `docs__creating-by-example-tutorials`        | `docs-creating-by-example-tutorials`        |
  | `docs__validating-factual-accuracy`          | `docs-validating-factual-accuracy`          |
  | `docs__validating-links`                     | `docs-validating-links`                     |
  | `readme__writing-readme-files`               | `readme-writing-readme-files`               |
  | `plan__creating-project-plans`               | `plan-creating-project-plans`               |
  | `plan__writing-gherkin-criteria`             | `plan-writing-gherkin-criteria`             |
  | `agent__developing-agents`                   | `agent-developing-agents`                   |
  | `apps__ayokoding-web__developing-content`    | `apps-ayokoding-web-developing-content`     |
  | `apps__ose-platform-web__developing-content` | `apps-ose-platform-web-developing-content`  |
  | `wow__applying-maker-checker-fixer`          | `wow-applying-maker-checker-fixer`          |
  | `wow__assessing-criticality-confidence`      | `wow-assessing-criticality-confidence`      |
  | `wow__defining-workflows`                    | `wow-defining-workflows`                    |
  | `wow__generating-validation-reports`         | `wow-generating-validation-reports`         |
  | `wow__multi-file-template`                   | `wow-multi-file-template`                   |
  | `wow__practicing-trunk-based-development`    | `wow-practicing-trunk-based-development`    |
  | `wow__understanding-repository-architecture` | `wow-understanding-repository-architecture` |

- [ ] **0.3 Update SKILL.md `name` field in each skill**
  - Each SKILL.md must have `name:` matching directory name
  - Example: `name: docs-applying-content-quality` (SINGLE hyphen, not `--`)
  - Validation command:

    ```bash
    # Check all SKILL.md name fields match regex [a-z0-9-]+ (no __ or --)
    for skill in .claude/skills/*/SKILL.md; do
      name=$(grep "^name:" "$skill" | cut -d: -f2 | xargs)
      if echo "$name" | grep -E '__|--' > /dev/null; then
        echo "ERROR: Invalid name in $skill: $name"
      fi
    done
    ```

- [ ] **0.4 Update all agent files**
  - Search for `skills:` field in `.claude/agents/*.md`
  - Replace underscore skill references with hyphen versions

- [ ] **0.5 Update documentation references**
  - `.claude/skills/README.md` - update skill listings
  - `CLAUDE.md` - update any skill references
  - `docs/` - update any skill references

- [ ] **0.6 Test Claude Code still works**
  - Run `claude` and verify skills load
  - Invoke a skill and verify it works
  - Document any issues

- [ ] **0.7 Final validation - Prevent double hyphens**

  Run comprehensive validation to ensure no `__` or `--` patterns:

  ```bash
  #!/bin/bash
  # Final validation script for skill renaming

  echo "=== Skill Renaming Validation ==="
  echo ""

  # 1. Check directory names
  echo "1. Validating skill directory names..."
  invalid_dirs=$(find .claude/skills -maxdepth 1 -type d \( -name "*__*" -o -name "*--*" \) | wc -l)
  if [ "$invalid_dirs" -gt 0 ]; then
    echo "   ❌ FAILED: Found invalid directory names"
    find .claude/skills -maxdepth 1 -type d \( -name "*__*" -o -name "*--*" \)
    exit 1
  fi
  echo "   ✓ All directory names valid (19 skills)"

  # 2. Check SKILL.md name fields
  echo "2. Validating SKILL.md name fields..."
  for skill in .claude/skills/*/SKILL.md; do
    name=$(grep "^name:" "$skill" | cut -d: -f2 | xargs)
    if echo "$name" | grep -E '__|--' > /dev/null; then
      echo "   ❌ FAILED: Invalid name in $skill: $name"
      exit 1
    fi
  done
  echo "   ✓ All SKILL.md name fields valid"

  # 3. Verify count
  echo "3. Validating skill count..."
  skill_count=$(find .claude/skills -maxdepth 1 -type d | tail -n +2 | wc -l)
  if [ "$skill_count" -ne 19 ]; then
    echo "   ❌ FAILED: Expected 19 skills, found $skill_count"
    exit 1
  fi
  echo "   ✓ Correct count (19 skills)"

  echo ""
  echo "=== ✓ ALL VALIDATIONS PASSED ==="
  echo "Skills are ready for OpenCode compatibility!"
  ```

#### Part B: Agents Renaming (46 files) ⚠️ **HIGHER PRIORITY**

- [ ] **0.8 Create and validate agent renaming script**

  ```bash
  #!/bin/bash
  # Rename all agent files from underscore to hyphen format
  # CRITICAL: Replace __ with SINGLE hyphen -, NOT double hyphen --

  echo "Renaming agents from __ to - format..."
  for agent in .claude/agents/*__*.md; do
    newname=$(echo "$agent" | sed 's/__/-/g')
    echo "  $agent -> $newname"
    git mv "$agent" "$newname"
  done

  echo ""
  echo "Validating: checking for invalid patterns..."

  # Validate no double hyphens
  invalid=$(find .claude/agents -type f -name "*--*.md" | wc -l)
  if [ "$invalid" -gt 0 ]; then
    echo "ERROR: Found agents with double hyphens (--)"
    find .claude/agents -type f -name "*--*.md"
    exit 1
  fi

  # Validate no underscores
  invalid=$(find .claude/agents -type f -name "*_*.md" | grep -v README.md | wc -l)
  if [ "$invalid" -gt 0 ]; then
    echo "ERROR: Found agents with underscores (_)"
    find .claude/agents -type f -name "*_*.md" | grep -v README.md
    exit 1
  fi

  echo "✓ All agent names are valid (no __ or --)"
  echo "✓ Agent renaming complete!"
  ```

- [ ] **0.9 Rename all 46 agent files**

  Sample of critical agent renames:

  | Current Name                            | New Name                              |
  | --------------------------------------- | ------------------------------------- |
  | `docs__checker.md`                      | `docs-checker.md`                     |
  | `docs__maker.md`                        | `docs-maker.md`                       |
  | `docs__fixer.md`                        | `docs-fixer.md`                       |
  | `plan__checker.md`                      | `plan-checker.md`                     |
  | `plan__maker.md`                        | `plan-maker.md`                       |
  | `plan__executor.md`                     | `plan-executor.md`                    |
  | `apps__ayokoding-web__general-maker.md` | `apps-ayokoding-web-general-maker.md` |
  | `wow__workflow-checker.md`              | `wow-workflow-checker.md`             |
  | `wow__workflow-maker.md`                | `wow-workflow-maker.md`               |
  | `wow__workflow-fixer.md`                | `wow-workflow-fixer.md`               |

  **Note**: Full list of 46 agents in tech-docs.md Agent Renaming Strategy section.

- [ ] **0.10 Update agent frontmatter `name:` fields**

  ```bash
  # Update name field in each agent file to match new filename
  for agent in .claude/agents/*.md; do
    # Extract new name from filename
    newname=$(basename "$agent" .md)

    # Update name field in frontmatter
    sed -i "s/^name:.*/name: $newname/" "$agent"

    echo "Updated name field in $agent"
  done

  # Validation
  echo "Validating agent name fields..."
  for agent in .claude/agents/*.md; do
    filename=$(basename "$agent" .md)
    namefield=$(grep "^name:" "$agent" | cut -d: -f2 | xargs)

    if [ "$filename" != "$namefield" ]; then
      echo "ERROR: Mismatch in $agent - filename=$filename, name field=$namefield"
      exit 1
    fi
  done
  echo "✓ All agent name fields match filenames"
  ```

- [ ] **0.11 Update all workflow agent references** ⚠️ **CRITICAL**

  ```bash
  # Find all workflow files and update agent references
  for workflow in .claude/workflows/*.md; do
    echo "Updating agent references in $workflow"

    # Update subagent_type fields in workflow steps
    sed -i 's/subagent_type: [a-z]*__/subagent_type: /g' "$workflow"
    sed -i 's/subagent_type: \([a-z]*\)-__/subagent_type: \1-/g' "$workflow"

    # Update agent references if present
    sed -i 's/agent: [a-z]*__/agent: /g' "$workflow"
    sed -i 's/agent: \([a-z]*\)-__/agent: \1-/g' "$workflow"
  done

  # Validation - no underscores should remain in agent references
  echo "Validating workflow agent references..."
  invalid_refs=$(grep -h "subagent_type:" .claude/workflows/*.md | grep "__" | wc -l)
  if [ "$invalid_refs" -gt 0 ]; then
    echo "ERROR: Found workflow agent references with underscores"
    grep -h "subagent_type:" .claude/workflows/*.md | grep "__"
    exit 1
  fi
  echo "✓ All workflow agent references updated"
  ```

- [ ] **0.12 Update documentation references**
  - [ ] `.claude/agents/README.md` - update all agent listings
  - [ ] `CLAUDE.md` - update agent descriptions
  - [ ] `docs/explanation/development/agents/` - update agent references
  - [ ] Any plan files mentioning agent names

- [ ] **0.13 Test Claude Code agents still work**

  ```bash
  # Test agent invocation
  claude "Run plan__checker agent"  # Should fail with new name
  claude "Run plan-checker agent"   # Should work with new name

  # Test workflow execution
  claude "Execute plan-quality-gate workflow"  # Tests agent invocation
  ```

- [ ] **0.14 Final validation - agents**

  Run comprehensive validation for agents:

  ```bash
  #!/bin/bash
  # Final validation script for agent renaming

  echo "=== Agent Renaming Validation ==="
  echo ""

  # 1. Check agent file names
  echo "1. Validating agent file names..."
  invalid_files=$(find .claude/agents -type f \( -name "*__*.md" -o -name "*--*.md" \) | grep -v README.md | wc -l)
  if [ "$invalid_files" -gt 0 ]; then
    echo "   ❌ FAILED: Found invalid agent file names"
    find .claude/agents -type f \( -name "*__*.md" -o -name "*--*.md" \) | grep -v README.md
    exit 1
  fi
  echo "   ✓ All agent file names valid (46 agents)"

  # 2. Check agent frontmatter name fields
  echo "2. Validating agent name fields..."
  for agent in .claude/agents/*.md; do
    filename=$(basename "$agent" .md)
    namefield=$(grep "^name:" "$agent" | cut -d: -f2 | xargs)

    if echo "$namefield" | grep -E '__|--' > /dev/null; then
      echo "   ❌ FAILED: Invalid name in $agent: $namefield"
      exit 1
    fi

    if [ "$filename" != "$namefield" ]; then
      echo "   ❌ FAILED: Name mismatch in $agent (file=$filename, field=$namefield)"
      exit 1
    fi
  done
  echo "   ✓ All agent name fields valid and match filenames"

  # 3. Verify agent count
  echo "3. Validating agent count..."
  agent_count=$(ls -1 .claude/agents/*.md 2>/dev/null | grep -v README.md | wc -l)
  if [ "$agent_count" -ne 46 ]; then
    echo "   ❌ FAILED: Expected 46 agents, found $agent_count"
    exit 1
  fi
  echo "   ✓ Correct count (46 agents)"

  # 4. Validate workflow agent references
  echo "4. Validating workflow agent references..."
  invalid_refs=$(grep -h "subagent_type:" .claude/workflows/*.md 2>/dev/null | grep "__" | wc -l)
  if [ "$invalid_refs" -gt 0 ]; then
    echo "   ❌ FAILED: Found workflow agent references with underscores"
    grep -h "subagent_type:" .claude/workflows/*.md | grep "__"
    exit 1
  fi
  echo "   ✓ All workflow agent references use hyphenated names"

  echo ""
  echo "=== ✓ ALL VALIDATIONS PASSED ==="
  echo "Agents are ready for OpenCode compatibility!"
  ```

### Phase 0 Completion Criteria

```gherkin
Given all 19 skills have been renamed with hyphens
And all 46 agents have been renamed with hyphens
And all workflow agent references have been updated
When Claude Code lists available skills and agents
Then all 19 skills appear with new names
And all 46 agents appear with new names
And skill invocation works correctly
And agent invocation works correctly
And workflows execute successfully with new agent names
And no underscore names remain in skills or agents
```

---

## Phase 1: OpenCode Skills Validation

**Goal**: Confirm renamed skills work with OpenCode

### Prerequisites

- [ ] **Phase 0 completed** (skills renamed)
- [ ] Install OpenCode CLI

  ```bash
  # Option 1: npm global
  npm install -g @opencode/cli

  # Option 2: Direct download from releases
  # https://github.com/sst/opencode/releases
  ```

### Validation Tasks

- [ ] **1.1 Verify skill discovery**
  - Run `opencode` in repository root
  - Check if skills from `.claude/skills/` are detected
  - Expected: All 19 skills should appear

- [ ] **1.2 Verify skill names are valid**
  - Confirm no validation errors on startup
  - Confirm skill names match `^[a-z0-9]+(-[a-z0-9]+)*$`

- [ ] **1.3 Test skill invocation**
  - Invoke skill via OpenCode: "Use the docs-applying-content-quality skill"
  - Verify skill content loads correctly
  - Verify skill instructions are followed

- [ ] **1.4 Document any issues**
  - List any skills that don't load
  - Note error messages
  - Fix any remaining issues

### Phase 1 Completion Criteria

```gherkin
Given OpenCode is installed and running
When the skill tool lists available skills
Then all 19 skills from .claude/skills/ are listed
And skill invocation loads correct content
And skill behavior matches Claude Code
```

---

## Phase 2: Core Configuration

**Goal**: Create opencode.json with GLM-4.7 model (cost-optimized)

### Tasks

- [ ] **2.1 Create Z.AI account and get API key**
  1. Visit https://bigmodel.cn/
  2. Create account or sign in
  3. Navigate to API console
  4. Generate API key
  5. Store securely (will be used in `/connect` command)

- [ ] **2.2 Create opencode.json with GLM-4.7**

  ```json
  {
    "$schema": "https://opencode.ai/config.json",
    "model": "zai/glm-4.7",
    "small_model": "zai/glm-4.5-air",
    "provider": {
      "zai": {
        "options": {
          "timeout": 600000
        }
      }
    }
  }
  ```

  **Why GLM-4.7?**
  - **Cost**: 8.6x-20x cheaper than Claude Sonnet 4.5
  - **Performance**: 90.6% tool calling (vs Claude's 89.5%)
  - **Speed**: 20-30% faster response times
  - **Quality**: Outperforms on several benchmarks

- [ ] **2.3 Configure Z.AI provider in OpenCode**

  ```bash
  # Run OpenCode
  opencode

  # In TUI, run:
  /connect

  # Select "Z.AI" from provider list
  # Enter your API key from bigmodel.cn

  # Verify connection
  /models
  # Should list GLM-4.7 and GLM-4.5-Air
  ```

- [ ] **2.4 Configure tool permissions**
  - Enable all development tools
  - Set permission levels matching Claude Code defaults
  - Allow bash, write, edit without prompts

- [ ] **2.5 Configure TUI settings**
  - Set scroll speed
  - Configure diff display style
  - Disable auto-share for privacy

- [ ] **2.6 Test configuration**
  - Run `opencode` and verify GLM-4.7 loads
  - Verify tools are available
  - Test model response speed and quality
  - Verify settings are applied

- [ ] **2.7 Add to git**
  - Commit opencode.json
  - Verify not gitignored
  - Add setup instructions to README

### Phase 2 Completion Criteria

```gherkin
Given opencode.json exists with GLM-4.7 configuration
And Z.AI API key is configured via /connect
When OpenCode starts in the repository
Then the model is set to zai/glm-4.7 (cost-optimized)
And the small model is set to zai/glm-4.5-air (fast operations)
And all tools (bash, read, write, edit, grep, glob) are available
And tool permissions allow development workflow
And model response quality matches or exceeds Claude Sonnet
And cost savings of 8.6x-20x are realized
```

---

## Phase 3: Instructions File

**Goal**: Create AGENTS.md for OpenCode project guidance

### Tasks

- [ ] **3.1 Draft AGENTS.md structure**
  - Project overview (2-3 sentences)
  - Environment setup (Volta, Node.js, npm)
  - Key conventions summary
  - File structure overview
  - Link to CLAUDE.md for details

- [ ] **3.2 Write AGENTS.md content**
  - Keep under 10KB
  - Focus on actionable guidance
  - Use clear markdown formatting
  - Include code examples where helpful

- [ ] **3.3 Configure instructions loading**
  - Add `instructions` field to opencode.json
  - Include AGENTS.md as primary
  - Optionally include key convention docs

- [ ] **3.4 Test instructions loading**
  - Start OpenCode fresh
  - Verify AGENTS.md content is recognized
  - Test that guidance is followed

- [ ] **3.5 Update README.md**
  - Add note about OpenCode support
  - Reference AGENTS.md
  - Explain dual-tool compatibility

### Phase 3 Completion Criteria

```gherkin
Given AGENTS.md exists at repository root
When OpenCode initializes
Then project instructions from AGENTS.md are loaded
And OpenCode follows the documented conventions
And Claude Code continues to work with CLAUDE.md
```

---

## Phase 4: MCP Integration

**Goal**: Configure MCP servers for both Claude Code and OpenCode

**SECURITY REQUIREMENT**: MCP servers requiring API keys must be configured in local/global config only. NEVER commit API keys to the repository.

### Prerequisites

- [ ] **Phase 2 completed** (Z.AI account created, API key obtained - for optional enhanced capabilities)
- [ ] Z.AI API key available from https://bigmodel.cn/ (optional, for enhanced capabilities)

### Repository MCP Servers (Safe to Commit)

- [ ] **4.1 Add Playwright MCP** (Browser automation)

  **Purpose**: Browser automation, testing, web scraping
  **API Key Required**: No

  **Repository Configuration** (opencode.json - safe to commit):

  ```json
  {
    "mcp": {
      "playwright": {
        "type": "local",
        "command": ["npx", "@playwright/mcp@latest"],
        "enabled": true
      }
    }
  }
  ```

  **Claude Code** (.mcp.json - repository level):

  ```json
  {
    "mcpServers": {
      "playwright": {
        "type": "stdio",
        "command": "npx",
        "args": ["@playwright/mcp@latest"]
      }
    }
  }
  ```

- [ ] **4.2 Add Context7 MCP** (Documentation lookup)

  **Purpose**: Library documentation lookup, code examples
  **API Key Required**: No

  **Repository Configuration** (opencode.json - safe to commit):

  ```json
  {
    "mcp": {
      "context7": {
        "type": "local",
        "command": ["npx", "-y", "@context7/mcp-server"],
        "enabled": true
      }
    }
  }
  ```

  **Claude Code** (.mcp.json - repository level):

  ```json
  {
    "mcpServers": {
      "context7": {
        "type": "stdio",
        "command": "npx",
        "args": ["-y", "@context7/mcp-server"]
      }
    }
  }
  ```

### Local/Global MCP Servers (Optional, Enhanced Capabilities)

**SECURITY**: Configure these in local/global config only. NEVER commit to repository.

- [ ] **4.3 Add Vision MCP Server** (GLM-4.6V multimodal) - OPTIONAL

  **Purpose**: UI screenshots to code, OCR, error diagnosis, diagram understanding, charts comparison
  **API Key Required**: Yes (Z.AI)
  **Configure In**: Local/global config only

  **Claude Code** (configure in `~/.config/claude-code/mcp.json` global or `.claude/settings.local.json` local):

  ```json
  {
    "mcpServers": {
      "zai-mcp-server": {
        "type": "stdio",
        "command": "npx",
        "args": ["-y", "@z_ai/mcp-server"],
        "env": {
          "Z_AI_API_KEY": "your_actual_api_key",
          "Z_AI_MODE": "ZAI"
        }
      }
    }
  }
  ```

  **OpenCode** (configure in global or local opencode config, NOT in repository):

  ```json
  {
    "mcp": {
      "zai-mcp-server": {
        "type": "local",
        "command": ["npx", "-y", "@z_ai/mcp-server"],
        "environment": {
          "Z_AI_API_KEY": "your_actual_api_key",
          "Z_AI_MODE": "ZAI"
        }
      }
    }
  }
  ```

- [ ] **4.4 Add Web Search MCP Server** (Real-time search) - OPTIONAL

  **Purpose**: Real-time web search with titles, URLs, summaries, site icons
  **API Key Required**: Yes (Z.AI)
  **Configure In**: Local/global config only

  **Claude Code** (configure in `~/.config/claude-code/mcp.json` global or `.claude/settings.local.json` local):

  ```json
  {
    "mcpServers": {
      "web-search-prime": {
        "type": "http",
        "url": "https://api.z.ai/api/mcp/web_search_prime/mcp",
        "headers": {
          "Authorization": "Bearer your_actual_api_key"
        }
      }
    }
  }
  ```

  **OpenCode** (configure in global or local opencode config, NOT in repository):

  ```json
  {
    "mcp": {
      "web-search-prime": {
        "type": "remote",
        "url": "https://api.z.ai/api/mcp/web_search_prime/mcp",
        "headers": {
          "Authorization": "Bearer your_actual_api_key"
        }
      }
    }
  }
  ```

- [ ] **4.5 Add Web Reader MCP Server** (Web content fetching) - OPTIONAL

  **Purpose**: Fetch webpage content (markdown format), metadata, links list
  **API Key Required**: Yes (Z.AI)
  **Configure In**: Local/global config only

  **Claude Code** (configure in `~/.config/claude-code/mcp.json` global or `.claude/settings.local.json` local):

  ```json
  {
    "mcpServers": {
      "web-reader": {
        "type": "http",
        "url": "https://api.z.ai/api/mcp/web_reader/mcp",
        "headers": {
          "Authorization": "Bearer your_actual_api_key"
        }
      }
    }
  }
  ```

  **OpenCode** (configure in global or local opencode config, NOT in repository):

  ```json
  {
    "mcp": {
      "web-reader": {
        "type": "remote",
        "url": "https://api.z.ai/api/mcp/web_reader/mcp",
        "headers": {
          "Authorization": "Bearer your_actual_api_key"
        }
      }
    }
  }
  ```

- [ ] **4.6 Add Zread MCP Server** (GitHub integration) - OPTIONAL

  **Purpose**: GitHub repo search, directory structure, file reading
  **API Key Required**: Yes (Z.AI)
  **Configure In**: Local/global config only

  **Claude Code** (configure in `~/.config/claude-code/mcp.json` global or `.claude/settings.local.json` local):

  ```json
  {
    "mcpServers": {
      "zread": {
        "type": "http",
        "url": "https://api.z.ai/api/mcp/zread/mcp",
        "headers": {
          "Authorization": "Bearer your_actual_api_key"
        }
      }
    }
  }
  ```

  **OpenCode** (configure in global or local opencode config, NOT in repository):

  ```json
  {
    "mcp": {
      "zread": {
        "type": "remote",
        "url": "https://api.z.ai/api/mcp/zread/mcp",
        "headers": {
          "Authorization": "Bearer your_actual_api_key"
        }
      }
    }
  }
  ```

### Testing and Validation

- [ ] **4.7 Test Repository MCP Servers** (Playwright, Context7)
  - Start Claude Code
  - Verify Playwright and Context7 MCP servers are listed
  - Test Playwright: Browser automation
  - Test Context7: Documentation lookup
  - Start OpenCode
  - Verify same 2 MCP servers are listed
  - Test tools work identically to Claude Code
  - Verify no API keys are present in repository

- [ ] **4.8 Test Optional Z.AI MCP Servers** (if configured locally)
  - Configure Z.AI MCP servers in local/global config
  - Start Claude Code
  - Verify 4 Z.AI MCP servers are listed (Vision, Web Search, Web Reader, Zread)
  - Test Vision MCP: Analyze a screenshot
  - Test Web Search: Search for latest tech news
  - Test Web Reader: Fetch a webpage
  - Test Zread: Search a GitHub repo
  - Start OpenCode
  - Verify same 4 Z.AI MCP servers are listed
  - Test tools work identically to Claude Code

- [ ] **4.9 Security Validation**
  - Verify no API keys in repository `opencode.json`
  - Verify no API keys in repository `.mcp.json`
  - Check `.gitignore` includes `*.local.json` patterns
  - Run `git grep -i "api[_-]key\|authorization.*bearer"` to find leaked keys
  - Verify all Z.AI MCP server configs reference local/global paths

- [ ] **4.10 Update .mcp.json for Claude Code** (repository level)
  - Create/update `.mcp.json` in repository root
  - Include only Playwright and Context7 (no API keys required)
  - Document that Z.AI MCP servers go in local config

- [ ] **4.11 Create documentation for Z.AI MCP setup**
  - Document local/global configuration process
  - Include security warnings about API keys
  - Provide examples for both Claude Code and OpenCode
  - Add to README.md

- [ ] **4.12 Update opencode.json for OpenCode** (repository level)
  - Create/update `opencode.json` in repository root
  - Include only Playwright and Context7 MCP servers (no API keys)
  - Add to git
  - Verify no API keys are present

### Phase 4 Completion Criteria

```gherkin
# Repository MCP servers (required)
Given Playwright and Context7 MCP servers are configured in repository
And no API keys are present in repository config
When Claude Code starts
Then Playwright and Context7 MCP servers connect successfully
And browser automation tools work
And documentation lookup tools work

When OpenCode starts
Then Playwright and Context7 MCP servers connect successfully
And tools work identically to Claude Code
And no API keys are present in opencode.json

# Optional Z.AI MCP servers (local config)
Given developer has configured Z.AI MCP servers in local/global config
When Claude Code starts
Then 4 Z.AI MCP servers connect using local API keys
And Vision MCP tools work
And Web Search tool works
And Web Reader tool works
And Zread tools work

When OpenCode starts
Then 4 Z.AI MCP servers connect using local API keys
And tools work identically to Claude Code

# Security validation
Given the repository configuration is complete
When security audit is performed
Then no API keys are found in repository
And .gitignore prevents API key commits
And documentation clearly indicates local config requirements
```

---

## Phase 5: Agent Translation (Optional)

**Goal**: Create OpenCode-specific agents for key workflows

### Tasks

- [ ] **5.1 Create .opencode/agent/ directory**

  ```bash
  mkdir -p .opencode/agent
  ```

- [ ] **5.2 Translate high-priority agents**
  - Start with 3-5 most-used agents:
    - docs-maker (from docs\_\_maker)
    - plan-executor (from plan\_\_executor)
    - rules-checker (from wow\_\_rules-checker)

- [ ] **5.3 Document translation process**
  - Create guide for translating Claude agents to OpenCode
  - Note format differences
  - Provide examples

- [ ] **5.4 Test translated agents**
  - Invoke each translated agent
  - Verify behavior matches Claude Code equivalent
  - Document any differences

- [ ] **5.5 Consider automation**
  - Evaluate if agent translation can be scripted
  - Create translation tool if beneficial

### Phase 5 Completion Criteria

```gherkin
Given translated agents exist in .opencode/agent/
When OpenCode lists available agents
Then translated agents appear and are usable
And agent behavior matches Claude Code equivalents
And documentation explains translation process
```

---

## Validation Checklist

### Pre-Completion Checks

- [ ] OpenCode starts without errors
- [ ] All 19 skills are discoverable
- [ ] Model is set to zai/glm-4.7 (cost-optimized)
- [ ] MCP servers connect successfully
- [ ] AGENTS.md is loaded as instructions
- [ ] Tools (bash, read, write, edit, grep, glob) work
- [ ] Z.AI provider is connected and authenticated
- [ ] Model response quality meets expectations

### Regression Checks

- [ ] Claude Code still starts correctly
- [ ] Claude Code agents still load
- [ ] Claude Code skills still work
- [ ] Claude Code MCP servers still connect
- [ ] No configuration conflicts

### Documentation Checks

- [ ] README.md updated with OpenCode mention
- [ ] AGENTS.md is clear and actionable
- [ ] tech-docs.md accurately reflects implementation
- [ ] Any workarounds are documented

---

## Rollback Plan

If issues are discovered:

1. **Configuration issues**: Remove opencode.json
2. **Skills incompatibility**: Create .opencode/skill/ with compatible copies
3. **MCP conflicts**: Adjust port configuration or disable one set
4. **Instruction conflicts**: Rename AGENTS.md temporarily

---

## Post-Implementation Tasks

- [ ] Add to wow\_\_rules-checker: OpenCode configuration validation
- [ ] Create agent: opencode-adoption-checker (if needed)
- [ ] Update CLAUDE.md with dual-tool notes (brief, link to docs)
- [ ] Create how-to guide: Using OpenCode with this repository
- [ ] Periodic validation: Test compatibility quarterly

---

## Completion Status

| Phase | Status          | Completed Date | Notes                                                 |
| ----- | --------------- | -------------- | ----------------------------------------------------- |
| **0** | **Not Started** | -              | **CRITICAL: Skills + Agents renaming required first** |
| 1     | Not Started     | -              | Skills validation (after Phase 0)                     |
| 2     | Not Started     | -              | Core configuration                                    |
| 3     | Not Started     | -              | AGENTS.md                                             |
| 4     | Not Started     | -              | MCP integration                                       |
| 5     | Not Started     | -              | Agent translation (optional)                          |

**Overall Status**: Backlog
**Ready for Implementation**: Yes
**Critical Prerequisite**: Phase 0 (19 skills + 46 agents renaming) MUST be completed first
