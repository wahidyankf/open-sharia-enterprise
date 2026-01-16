#!/bin/bash
# Sync Claude Code configuration to OpenCode format
# Usage: ./scripts/sync-claude-to-opencode.sh [--dry-run] [--agents-only] [--skills-only]

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Parse arguments
DRY_RUN=false
AGENTS_ONLY=false
SKILLS_ONLY=false

for arg in "$@"; do
  case $arg in
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    --agents-only)
      AGENTS_ONLY=true
      shift
      ;;
    --skills-only)
      SKILLS_ONLY=true
      shift
      ;;
    *)
      echo "Unknown option: $arg"
      echo "Usage: $0 [--dry-run] [--agents-only] [--skills-only]"
      exit 1
      ;;
  esac
done

if $DRY_RUN; then
  echo -e "${YELLOW}DRY RUN MODE - No files will be modified${NC}"
  echo ""
fi

# Directories
CLAUDE_AGENTS_DIR=".claude/agents"
CLAUDE_SKILLS_DIR=".claude/skills"
OPENCODE_AGENTS_DIR=".opencode/agent"
OPENCODE_SKILLS_DIR=".opencode/skill"

# Counters
agents_converted=0
agents_failed=0
skills_copied=0
skills_failed=0

# Convert single agent from Claude Code to OpenCode format
convert_agent() {
  local input_file="$1"
  local output_file="$2"
  
  if [[ ! -f "$input_file" ]]; then
    echo -e "${RED}Error: Input file not found: $input_file${NC}"
    return 1
  fi
  
  # Extract description (handle multi-line)
  description=$(awk '
    BEGIN { in_front=0; in_desc=0 }
    /^---$/ { in_front++; if (in_front==2) exit; next }
    in_front==1 && /^description:/ {
      in_desc=1
      desc=$0
      sub(/^description: */, "", desc)
      next
    }
    in_front==1 && in_desc && /^[a-z]+:/ {
      in_desc=0
    }
    in_front==1 && in_desc {
      desc=desc " " $0
      next
    }
    END {
      gsub(/^ +| +$/, "", desc)
      gsub(/  +/, " ", desc)
      print desc
    }
  ' "$input_file")
  
  # Extract tools array and convert to boolean flags
  tools_yaml=$(awk '
    BEGIN { in_front=0; in_tools=0 }
    /^---$/ { in_front++; if (in_front==2) exit; next }
    in_front==1 && /^tools: \[/ {
      # Array format: tools: [Read, Write, Edit]
      line=$0
      sub(/^tools: \[/, "", line)
      sub(/\].*$/, "", line)
      # Split by comma and process each tool
      split(line, tools, /, */)
      for (i in tools) {
        tool=tolower(tools[i])
        gsub(/^ +| +$/, "", tool)
        if (tool) print "  " tool ": true"
      }
      exit
    }
  ' "$input_file")
  
  # If no tools found, default to empty
  if [[ -z "$tools_yaml" ]]; then
    tools_yaml="  read: false"
  fi
  
  # Extract model and convert
  model_line=$(awk '
    BEGIN { in_front=0 }
    /^---$/ { in_front++; if (in_front==2) exit; next }
    in_front==1 && /^model: *sonnet/ { print "model: zai/glm-4.7"; exit }
    in_front==1 && /^model: *haiku/ { print "model: zai/glm-4.5-air"; exit }
    in_front==1 && /^model: *opus/ { print "model: zai/glm-4.7"; exit }
  ' "$input_file")
  
  # Default to inherit if no model specified
  if [[ -z "$model_line" ]]; then
    model_line="model: inherit"
  fi
  
  # Extract markdown body
  body=$(awk '
    BEGIN { count=0; printing=0 }
    /^---$/ { count++; if (count>=2) { printing=1; next } }
    printing { print }
  ' "$input_file")
  
  # Write OpenCode format
  if ! $DRY_RUN; then
    {
      echo "---"
      echo "description: $description"
      echo "$model_line"
      echo "tools:"
      echo "$tools_yaml"
      echo "---"
      echo "$body"
    } > "$output_file"
  fi
  
  return 0
}

# Sync agents
sync_agents() {
  echo -e "${BLUE}=== Syncing Agents: Claude Code → OpenCode ===${NC}"
  echo ""
  
  if [[ ! -d "$CLAUDE_AGENTS_DIR" ]]; then
    echo -e "${RED}Error: Claude agents directory not found: $CLAUDE_AGENTS_DIR${NC}"
    return 1
  fi
  
  for agent_file in "$CLAUDE_AGENTS_DIR"/*.md; do
    basename_agent=$(basename "$agent_file")
    
    # Skip README
    if [[ "$basename_agent" == "README.md" ]]; then
      continue
    fi
    
    output_file="$OPENCODE_AGENTS_DIR/$basename_agent"
    
    if $DRY_RUN; then
      echo -e "${YELLOW}[DRY RUN]${NC} Would convert: $basename_agent"
    else
      if convert_agent "$agent_file" "$output_file"; then
        echo -e "${GREEN}✓${NC} Converted: $basename_agent"
        agents_converted=$((agents_converted + 1))
      else
        echo -e "${RED}✗${NC} Failed: $basename_agent"
        agents_failed=$((agents_failed + 1))
      fi
    fi
  done
  
  echo ""
  if $DRY_RUN; then
    echo -e "${YELLOW}Dry run complete (no files modified)${NC}"
  else
    echo -e "${GREEN}Agents conversion complete${NC}"
    echo "  Converted: $agents_converted"
    if [[ $agents_failed -gt 0 ]]; then
      echo -e "  ${RED}Failed: $agents_failed${NC}"
    fi
  fi
  echo ""
}

# Sync skills (direct copy, no conversion)
sync_skills() {
  echo -e "${BLUE}=== Syncing Skills: Direct Copy ===${NC}"
  echo ""
  
  if [[ ! -d "$CLAUDE_SKILLS_DIR" ]]; then
    echo -e "${RED}Error: Claude skills directory not found: $CLAUDE_SKILLS_DIR${NC}"
    return 1
  fi
  
  for skill_dir in "$CLAUDE_SKILLS_DIR"/*/; do
    skill_name=$(basename "$skill_dir")
    skill_file="$skill_dir/SKILL.md"
    
    if [[ ! -f "$skill_file" ]]; then
      echo -e "${RED}✗${NC} Missing SKILL.md: $skill_name"
      skills_failed=$((skills_failed + 1))
      continue
    fi
    
    output_dir="$OPENCODE_SKILLS_DIR/$skill_name"
    output_file="$output_dir/SKILL.md"
    
    if $DRY_RUN; then
      echo -e "${YELLOW}[DRY RUN]${NC} Would copy: $skill_name/SKILL.md"
    else
      mkdir -p "$output_dir"
      cp "$skill_file" "$output_file"
      echo -e "${GREEN}✓${NC} Copied: $skill_name/SKILL.md"
      skills_copied=$((skills_copied + 1))
    fi
  done
  
  echo ""
  if $DRY_RUN; then
    echo -e "${YELLOW}Dry run complete (no files modified)${NC}"
  else
    echo -e "${GREEN}Skills copy complete${NC}"
    echo "  Copied: $skills_copied"
    if [[ $skills_failed -gt 0 ]]; then
      echo -e "  ${RED}Failed: $skills_failed${NC}"
    fi
  fi
  echo ""
}

# Main execution
main() {
  echo ""
  echo -e "${BLUE}╔═══════════════════════════════════════════════════╗${NC}"
  echo -e "${BLUE}║  Sync Claude Code → OpenCode Configuration       ║${NC}"
  echo -e "${BLUE}╚═══════════════════════════════════════════════════╝${NC}"
  echo ""
  
  if [[ "$SKILLS_ONLY" == "true" ]]; then
    sync_skills
  elif [[ "$AGENTS_ONLY" == "true" ]]; then
    sync_agents
  else
    sync_agents
    sync_skills
  fi
  
  # Summary
  echo -e "${BLUE}=== Sync Summary ===${NC}"
  if $DRY_RUN; then
    echo -e "${YELLOW}DRY RUN - No changes made${NC}"
  else
    echo -e "${GREEN}✓ Sync complete${NC}"
    echo "  Agents converted: $agents_converted"
    echo "  Skills copied: $skills_copied"
    if [[ $((agents_failed + skills_failed)) -gt 0 ]]; then
      echo -e "  ${RED}Total failures: $((agents_failed + skills_failed))${NC}"
    fi
  fi
  echo ""
  
  if ! $DRY_RUN; then
    echo "Next steps:"
    echo "  1. Review changes in .opencode/ directory"
    echo "  2. Run validation: npm run validate:sync"
    echo "  3. Test agents in both Claude Code and OpenCode"
    echo ""
  fi
}

main
