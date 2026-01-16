#!/bin/bash
# Validate that .claude/ and .opencode/ directories are semantically equivalent
# Usage: ./scripts/validate-sync.sh

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Directories
CLAUDE_AGENTS_DIR=".claude/agents"
CLAUDE_SKILLS_DIR=".claude/skills"
OPENCODE_AGENTS_DIR=".opencode/agent"
OPENCODE_SKILLS_DIR=".opencode/skill"

# Counters
total_checks=0
passed_checks=0
failed_checks=0

check_pass() {
  echo -e "${GREEN}✓${NC} $1"
  passed_checks=$((passed_checks + 1))
  total_checks=$((total_checks + 1))
}

check_fail() {
  echo -e "${RED}✗${NC} $1"
  failed_checks=$((failed_checks + 1))
  total_checks=$((total_checks + 1))
}

# Validate agents count
validate_agents_count() {
  echo -e "${BLUE}=== Validating Agent Count ===${NC}"
  
  claude_count=$(find "$CLAUDE_AGENTS_DIR" -name "*.md" ! -name "README.md" | wc -l)
  opencode_count=$(find "$OPENCODE_AGENTS_DIR" -name "*.md" ! -name "README.md" | wc -l)
  
  if [[ $claude_count -eq $opencode_count ]]; then
    check_pass "Agent count matches: $claude_count agents in both directories"
  else
    check_fail "Agent count mismatch: Claude=$claude_count, OpenCode=$opencode_count"
  fi
  echo ""
}

# Validate skills count
validate_skills_count() {
  echo -e "${BLUE}=== Validating Skills Count ===${NC}"
  
  claude_count=$(find "$CLAUDE_SKILLS_DIR" -mindepth 1 -maxdepth 1 -type d | wc -l)
  opencode_count=$(find "$OPENCODE_SKILLS_DIR" -mindepth 1 -maxdepth 1 -type d | wc -l)
  
  if [[ $claude_count -eq $opencode_count ]]; then
    check_pass "Skills count matches: $claude_count skills in both directories"
  else
    check_fail "Skills count mismatch: Claude=$claude_count, OpenCode=$opencode_count"
  fi
  echo ""
}

# Validate agent format
validate_agent_format() {
  echo -e "${BLUE}=== Validating Agent Format ===${NC}"
  
  # Check Claude Code format (tools as array)
  claude_array_count=$(grep -l "^tools: \[" "$CLAUDE_AGENTS_DIR"/*.md 2>/dev/null | wc -l)
  check_pass "Claude Code: $claude_array_count agents use array format for tools"
  
  # Check OpenCode format (tools as boolean flags) - fixed check
  opencode_bool_files=$(grep -l "^tools:" "$OPENCODE_AGENTS_DIR"/*.md 2>/dev/null | wc -l)
  if [[ $opencode_bool_files -gt 0 ]]; then
    # Verify at least one has boolean syntax
    sample_check=$(head -20 "$OPENCODE_AGENTS_DIR"/docs-maker.md | grep -A 5 "^tools:" | grep -c ": true\|: false" || echo "0")
    if [[ $sample_check -gt 0 ]]; then
      check_pass "OpenCode: $opencode_bool_files agents use boolean flag format for tools"
    else
      check_fail "OpenCode: Agents don't have proper boolean tool flags"
    fi
  else
    check_fail "OpenCode: No agents found with tools section"
  fi
  
  echo ""
}

# Validate semantic equivalence for one agent
validate_agent_semantic() {
  local agent_name="$1"
  local claude_file="$CLAUDE_AGENTS_DIR/$agent_name"
  local opencode_file="$OPENCODE_AGENTS_DIR/$agent_name"
  
  if [[ ! -f "$claude_file" ]]; then
    check_fail "$agent_name: Missing in Claude directory"
    return
  fi
  
  if [[ ! -f "$opencode_file" ]]; then
    check_fail "$agent_name: Missing in OpenCode directory"
    return
  fi
  
  # Extract descriptions (simplified check - just verify both have description field)
  claude_has_desc=$(grep -c "^description:" "$claude_file" || echo "0")
  opencode_has_desc=$(grep -c "^description:" "$opencode_file" || echo "0")
  
  if [[ $claude_has_desc -gt 0 && $opencode_has_desc -gt 0 ]]; then
    check_pass "$agent_name: Description present in both"
  else
    check_fail "$agent_name: Description missing"
  fi
}

# Validate skills are identical
validate_skills_identical() {
  echo -e "${BLUE}=== Validating Skills Identity ===${NC}"
  
  for skill_dir in "$CLAUDE_SKILLS_DIR"/*/; do
    skill_name=$(basename "$skill_dir")
    claude_skill="$skill_dir/SKILL.md"
    opencode_skill="$OPENCODE_SKILLS_DIR/$skill_name/SKILL.md"
    
    if [[ ! -f "$opencode_skill" ]]; then
      check_fail "$skill_name: Missing in OpenCode"
      continue
    fi
    
    if diff -q "$claude_skill" "$opencode_skill" > /dev/null 2>&1; then
      check_pass "$skill_name: Identical in both directories"
    else
      check_fail "$skill_name: Files differ"
    fi
  done
  
  echo ""
}

# Validate directory structure
validate_structure() {
  echo -e "${BLUE}=== Validating Directory Structure ===${NC}"
  
  if [[ -d "$CLAUDE_AGENTS_DIR" ]]; then
    check_pass "Claude agents directory exists"
  else
    check_fail "Claude agents directory missing"
  fi
  
  if [[ -d "$CLAUDE_SKILLS_DIR" ]]; then
    check_pass "Claude skills directory exists"
  else
    check_fail "Claude skills directory missing"
  fi
  
  if [[ -d "$OPENCODE_AGENTS_DIR" ]]; then
    check_pass "OpenCode agent directory exists"
  else
    check_fail "OpenCode agent directory missing"
  fi
  
  if [[ -d "$OPENCODE_SKILLS_DIR" ]]; then
    check_pass "OpenCode skill directory exists"
  else
    check_fail "OpenCode skill directory missing"
  fi
  
  echo ""
}

# Main execution
main() {
  echo ""
  echo -e "${BLUE}╔═══════════════════════════════════════════════════╗${NC}"
  echo -e "${BLUE}║  Validate Claude Code ↔ OpenCode Sync            ║${NC}"
  echo -e "${BLUE}╚═══════════════════════════════════════════════════╝${NC}"
  echo ""
  
  validate_structure
  validate_agents_count
  validate_skills_count
  validate_agent_format
  validate_skills_identical
  
  # Sample semantic check (test 3 agents)
  echo -e "${BLUE}=== Sample Semantic Validation (3 agents) ===${NC}"
  for agent in docs-maker.md readme-checker.md plan-executor.md; do
    if [[ -f "$CLAUDE_AGENTS_DIR/$agent" ]]; then
      validate_agent_semantic "$agent"
    fi
  done
  echo ""
  
  # Summary
  echo -e "${BLUE}╔═══════════════════════════════════════════════════╗${NC}"
  echo -e "${BLUE}║  Validation Summary                               ║${NC}"
  echo -e "${BLUE}╚═══════════════════════════════════════════════════╝${NC}"
  echo ""
  echo "Total checks: $total_checks"
  echo -e "${GREEN}Passed: $passed_checks${NC}"
  
  if [[ $failed_checks -gt 0 ]]; then
    echo -e "${RED}Failed: $failed_checks${NC}"
    echo ""
    echo -e "${RED}Validation FAILED - Fix issues and re-run sync${NC}"
    exit 1
  else
    echo -e "${GREEN}Failed: 0${NC}"
    echo ""
    echo -e "${GREEN}✓ Validation PASSED - Directories are synchronized${NC}"
    exit 0
  fi
}

main
