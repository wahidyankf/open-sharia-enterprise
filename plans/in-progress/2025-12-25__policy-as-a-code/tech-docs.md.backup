# Technical Documentation - Policy-as-Code Governance Layer

## Architecture Overview

### Current Six-Layer Architecture

```
Layer 0: Vision (WHY) → docs/explanation/vision/
Layer 1: Principles (WHY values) → docs/explanation/principles/
Layer 2: Conventions (WHAT docs rules) → docs/explanation/conventions/
Layer 3: Development (HOW software) → docs/explanation/development/
Layer 4: AI Agents (WHO enforces) → .claude/agents/
Layer 5: Workflows (WHEN orchestrate) → docs/explanation/workflows/
```

**Key Characteristics:**

- 41 documented standards (6 principles, 22 conventions, 13 development practices)
- Strong traceability: Every layer links to layer above
- 34+ agents consuming rules via embedded prose
- Maker-Checker-Fixer pattern across 7 families

### Proposed Architecture: Insert Policy Layer 3.5

```
Layer 0: Vision (WHY)
    ↓ inspires
Layer 1: Principles (WHY values)
    ↓ governs
Layer 2: Conventions (WHAT docs) ──┐
Layer 3: Development (HOW software) ─┤
    ↓ both define                    │
Layer 3.5: POLICIES (machine-readable rules) ← NEW
    ↓ consumed by
Layer 4: AI Agents (WHO enforces)
    ↓ orchestrated by
Layer 5: Workflows (WHEN)
```

**Policy Layer 3.5 Characteristics:**

- **Source of truth**: Conventions/Development (prose remains primary for humans)
- **Derived format**: Policies are machine-readable versions of prose rules
- **Synchronization**: repo-rules-maker updates both prose and policies atomically
- **Consumption**: Agents load policies via governance-cli (Cobra-based CLI) instead of parsing prose
- **Traceability**: Policies link back to principles (Layer 1) and conventions (Layer 2)

## Policy Storage Design

### Decision: Embedded YAML in Convention Markdown

**Chosen Approach**: Embed policies as YAML sections within existing convention markdown files

**Rationale:**

- ✅ Single source of truth (policy lives with human explanation)
- ✅ Git history already tracks changes
- ✅ No new directory structure to maintain
- ✅ Backwards compatible (existing agents continue reading markdown)
- ✅ Discoverable (policies found where conventions are documented)

**Rejected Alternatives:**

- ❌ Separate `/policies/` directory - Creates duplication, harder to maintain sync
- ❌ JSON files - Less human-readable, doesn't embed with prose
- ❌ Database storage - Adds operational complexity, loses version control

### Storage Format Example

**File**: `docs/explanation/conventions/meta/ex-co-me__file-naming.md`

````markdown
---
title: "File Naming Convention"
description: Standards for naming files in the repository
category: explanation
subcategory: conventions
tags: [file-naming, conventions]
created: 2025-12-01
updated: 2025-12-24
---

# File Naming Convention

[Human-readable prose explanation for developers...]

## Policy Definition

```yaml
policy:
  id: file-naming-v1
  version: "1.0.0"

  metadata:
    name: "File Naming Convention"
    category: structural
    severity: high
    created: "2025-12-24"

  traceability:
    principles:
      - id: explicit-over-implicit
        path: docs/explanation/principles/software-engineering/ex-pr-se__explicit-over-implicit.md
        reason: "Explicit prefixes make file locations transparent without opening files"
    conventions:
      - id: ex-co__file-naming-convention
        path: docs/explanation/conventions/meta/ex-co-me__file-naming.md
        section: "Prefix Pattern Rules"

  scope:
    paths:
      include:
        - "docs/**/*.md"
        - ".claude/agents/*.md"
      exclude:
        - "docs/metadata/**"
    agents:
      - docs-checker
      - repo-rules-checker
      - docs-file-manager

  rules:
    - id: FN001
      name: "Prefix must match directory"
      severity: critical
      description: "Files in docs/ must use prefix matching directory path"

      validation:
        type: regex
        pattern: '^(tu|hoto|re|ex|ex-pr-ge|ex-pr-co|ex-pr-se|ex-co|ex-de|ex-wf)__[\w\-]+\.md$'

      examples:
        valid:
          - "docs/explanation/conventions/formatting/ex-co-fo__indentation.md"
          - "docs/tutorials/tu__getting-started.md"
        invalid:
          - "docs/explanation/conventions/wrong-prefix__example.md"
          - "docs/tutorials/tutorial__example.md"

      autofix: false
      confidence: medium

      remediation:
        guidance: |
          1. Identify correct prefix from directory path
          2. Rename file with correct prefix
          3. Update all cross-references
```
````

## Policy Schema Specification

### Core Schema Structure

```yaml
policy:
  id: string # Unique ID (kebab-case, e.g., "file-naming-v1")
  version: string # Semantic versioning (MAJOR.MINOR.PATCH)

  metadata:
    name: string
    category: enum # structural|formatting|content-quality|accessibility|workflow|agent-config|validation-config
    severity: enum # blocking|high|medium|low|advisory
    created: date # ISO 8601 (YYYY-MM-DD)
    updated: date # ISO 8601 (YYYY-MM-DD)

  traceability: # MANDATORY
    principles:
      - id: string
        path: string
        reason: string
    conventions:
      - id: string
        path: string
        section: string # Optional

  scope:
    paths:
      include: glob[]
      exclude: glob[]
    agents: string[] # Which agents enforce this policy

  rules:
    - id: string # Rule ID (e.g., "FN001")
      name: string
      severity: enum
      description: string

      validation:
        type: enum # regex|schema|function|external-check
        pattern: string # For regex type
        schema: object # For schema type
        function: string # For function type
        command: string # For external-check type

      examples:
        valid: string[]
        invalid: string[]

      autofix: boolean
      confidence: enum # high|medium|low (for fixer agents)
      fix_template: string # Optional: how to apply fix

      remediation:
        guidance: string # Human-readable fix instructions
```

### Seven Rule Types

Based on analysis of existing agents, policies must support seven distinct rule types:

#### 1. Structural Rules

**Purpose**: Document structure requirements
**Examples**: H1 count, heading hierarchy, frontmatter fields
**Validation**: AST parsing, schema validation

```yaml
- id: structural-single-h1
  validation:
    type: schema
    schema:
      h1_count: 1
      h1_position: first_heading
```

#### 2. Formatting Rules

**Purpose**: Syntax and visual formatting
**Examples**: Indentation (2 spaces), code block language tags
**Validation**: Regex, AST parsing

````yaml
- id: formatting-code-block-lang
  validation:
    type: regex
    pattern: '^```[a-z]+\n'
````

#### 3. Content Quality Rules

**Purpose**: Writing quality standards
**Examples**: Active voice, paragraph length, alt text
**Validation**: NLP patterns, regex, length checks

```yaml
- id: content-quality-active-voice
  validation:
    type: regex
    pattern: '\b(is|are|was|were)\s+\w+(ed|en)\b'
    match_type: avoid
```

#### 4. Accessibility Rules

**Purpose**: WCAG compliance
**Examples**: Color contrast (4.5:1), alt text, ARIA labels
**Validation**: Computed checks, pattern matching

```yaml
- id: accessibility-alt-text
  validation:
    type: regex
    pattern: '!\[\]\('
    match_type: avoid
```

#### 5. Workflow Rules

**Purpose**: Multi-step orchestration process definitions (Layer 5)
**Examples**: Frontmatter structure, agent references, state validation, termination criteria
**Validation**: Schema validation, reference checking, dependency analysis

Workflows orchestrate agents and have structural requirements similar to agents themselves. The workflow family (workflow-maker, workflow-checker, workflow-fixer) validates workflow definitions in `docs/explanation/workflows/`.

**Example 1: Frontmatter Structure**

```yaml
- id: WF001
  name: "Required workflow frontmatter fields"
  severity: blocking
  description: "Workflow files must have complete frontmatter with all required fields"

  validation:
    type: schema
    schema:
      required: [name, goal, termination, inputs, outputs]
      properties:
        name:
          type: string
          pattern: "^[a-z][a-z0-9-]*$" # Kebab-case
        goal:
          type: string
          minLength: 20 # Must explain purpose
        termination:
          type: string
          minLength: 15 # Clear exit condition
        inputs:
          type: array
          items:
            required: [name, type, description]
        outputs:
          type: array
          items:
            required: [name, type, description]

  examples:
    valid:
      - "name: docs-check-fix, goal: Validate and fix documentation..., termination: Zero findings remain"
    invalid:
      - "name: DocsCheckFix (PascalCase not allowed)"
      - "goal: Fix docs (too short, not descriptive)"

  autofix: false
  confidence: high # Objective structural requirement
```

**Example 2: Agent Reference Validation**

```yaml
- id: WF002
  name: "Agent references must exist"
  severity: high
  description: "All agent names in workflow steps must correspond to actual agent files"

  validation:
    type: function
    function: |
      For each step in workflow:
        1. Extract agent name from step definition
        2. Check if file exists: .claude/agents/{agent-name}.md
        3. If not found, report violation

  examples:
    valid:
      - "agent: docs-checker (file: .claude/agents/docs__checker.md exists)"
    invalid:
      - "agent: nonexistent-agent (file not found)"
      - "agent: docs_checker (wrong naming convention)"

  autofix: false
  confidence: high # Objectively verifiable
```

**Example 3: State Reference Validation**

```yaml
- id: WF003
  name: "State references must be valid"
  severity: high
  description: "References like {input.x} and {stepN.outputs.y} must use correct syntax and refer to defined states"

  validation:
    type: function
    function: |
      Parse workflow for state references:
        - {input.varname} must match defined input names
        - {step1.outputs.varname} must match step1's declared outputs
        - {step2.outputs.x} can only reference previous steps (step1), not future steps (step3)

  examples:
    valid:
      - "{input.scope} where scope is defined in inputs"
      - "{step1.outputs.report} where step1 declares report in outputs"
    invalid:
      - "{input.undefined} where undefined is not in inputs"
      - "{step3.outputs.x} referenced in step1 (future reference)"

  autofix: false
  confidence: high # Syntax and scope are objective
```

**Example 4: Termination Criteria Clarity**

```yaml
- id: WF004
  name: "Clear termination criteria required"
  severity: medium
  description: "Workflows must specify measurable exit conditions"

  validation:
    type: regex
    pattern: |
      (?i)(zero|no|all|complete|pass|fail|success|error|threshold|when|until|after)

  examples:
    valid:
      - "Zero findings remain after validation"
      - "All tasks completed successfully"
      - "Error count below threshold"
    invalid:
      - "Done" (too vague)
      - "Finished" (not measurable)

  autofix: false
  confidence: medium  # Some subjectivity in what's "clear enough"
```

#### 6. Agent Configuration Rules

**Purpose**: Agent tool permissions, responsibilities
**Examples**: Tools whitelist, model selection, color categorization
**Validation**: Schema validation, enum checks

```yaml
- id: agent-config-color
  validation:
    type: schema
    schema:
      color: { enum: [blue, green, yellow, purple] }
```

#### 7. Validation Configuration Rules

**Purpose**: Validation execution parameters
**Examples**: Timeouts, cache expiry, progressive writing requirements
**Validation**: Range checks, boolean validation

```yaml
- id: validation-config-timeout
  validation:
    type: schema
    schema:
      timeout: { type: number, min: 1000, max: 600000 }
```

### Severity Levels

Align with existing fixer confidence system:

| Severity     | Description                   | Agent Behavior      | Example                                         |
| ------------ | ----------------------------- | ------------------- | ----------------------------------------------- |
| **blocking** | MUST comply (CI fails)        | Reject commit/PR    | Missing frontmatter field, invalid YAML         |
| **high**     | Auto-fix with high confidence | Immediate fix       | Wrong indentation, missing code block language  |
| **medium**   | Review recommended before fix | Manual review       | Passive voice, long paragraphs                  |
| **low**      | Advisory only                 | Report, no auto-fix | Style improvements, optional optimizations      |
| **advisory** | Informational                 | Report only         | Best practice suggestions, deprecation warnings |

### Traceability Requirements

Every policy MUST link back to principles (Layer 1) and conventions (Layer 2):

```yaml
traceability:
  principles:
    - id: accessibility-first
      path: docs/explanation/principles/content/ex-pr-co__accessibility-first.md
      reason: "Active voice benefits non-native speakers and screen readers"
    - id: simplicity-over-complexity
      path: docs/explanation/principles/general/ex-pr-ge__simplicity-over-complexity.md
      reason: "Clear language reduces cognitive load"

  conventions:
    - id: ex-co__content-quality
      path: docs/explanation/conventions/content/ex-co-co__quality.md
      section: "Writing Style and Tone > Active Voice"
      line_range: "77-106" # Optional: specific line references
```

## PolicyEngine Implementation

### Technology Choice: Go with Cobra Framework

**Decision**: New `apps/governance-cli/` Go application built with Cobra CLI framework

**Rationale:**

- ✅ **Performance**: Go compiles to native binaries with excellent execution speed
- ✅ **Cobra Framework**: Industry-standard CLI framework (used by Kubernetes, Hugo, GitHub CLI)
- ✅ **Proper CLI UX**: Subcommands, flags, help text, auto-completion
- ✅ Strong typing for policy definitions (Go structs)
- ✅ No separate service deployment (standalone binary)
- ✅ Rich ecosystem for YAML parsing (github.com/goccy/go-yaml), glob matching (filepath, doublestar)
- ✅ Can be invoked from agents via CLI
- ✅ Single binary distribution (easy deployment)
- ✅ Comprehensive testing with Go's built-in testing framework

**Alternative Considered:**

- **TypeScript**: Would work but slower execution, requires Node.js runtime
- **Rego/OPA**: Too complex for current scale, learning curve high
- **Python**: Another runtime dependency, slower than Go
- **Custom DSL**: High development cost, limited tooling support

### Core API Design

````go
// apps/governance-cli/internal/policy/types.go

package policy

import "time"

type Severity string

const (
	SeverityBlocking  Severity = "blocking"
	SeverityHigh      Severity = "high"
	SeverityMedium    Severity = "medium"
	SeverityLow       Severity = "low"
	SeverityAdvisory  Severity = "advisory"
)

type Confidence string

const (
	ConfidenceHigh   Confidence = "high"
	ConfidenceMedium Confidence = "medium"
	ConfidenceLow    Confidence = "low"
)

type PolicyRule struct {
	ID          string            `yaml:"id" json:"id"`                     // e.g., "FN001"
	Name        string            `yaml:"name" json:"name"`
	Severity    Severity          `yaml:"severity" json:"severity"`
	Description string            `yaml:"description" json:"description"`
	Validation  ValidationSpec    `yaml:"validation" json:"validation"`
	Autofix     bool              `yaml:"autofix" json:"autofix"`
	FixTemplate *string           `yaml:"fix_template,omitempty" json:"fix_template,omitempty"`
	Examples    *Examples         `yaml:"examples,omitempty" json:"examples,omitempty"`
}

type Examples struct {
	Valid   []string `yaml:"valid" json:"valid"`
	Invalid []string `yaml:"invalid" json:"invalid"`
}

type Policy struct {
	ID           string        `yaml:"id" json:"id"`                     // e.g., "file-naming-v1"
	Version      string        `yaml:"version" json:"version"`           // e.g., "1.0.0"
	Metadata     Metadata      `yaml:"metadata" json:"metadata"`
	Traceability Traceability  `yaml:"traceability" json:"traceability"`
	Scope        Scope         `yaml:"scope" json:"scope"`
	Rules        []PolicyRule  `yaml:"rules" json:"rules"`
}

type Metadata struct {
	Name     string    `yaml:"name" json:"name"`
	Category string    `yaml:"category" json:"category"`
	Severity Severity  `yaml:"severity" json:"severity"`
	Created  time.Time `yaml:"created" json:"created"`
	Updated  *time.Time `yaml:"updated,omitempty" json:"updated,omitempty"`
}

type Traceability struct {
	Principles  []Reference `yaml:"principles" json:"principles"`
	Conventions []Reference `yaml:"conventions" json:"conventions"`
}

type Reference struct {
	ID     string  `yaml:"id" json:"id"`
	Path   string  `yaml:"path" json:"path"`
	Reason *string `yaml:"reason,omitempty" json:"reason,omitempty"`
	Section *string `yaml:"section,omitempty" json:"section,omitempty"`
}

type Scope struct {
	Paths  PathScope `yaml:"paths" json:"paths"`
	Agents []string  `yaml:"agents" json:"agents"`
}

type PathScope struct {
	Include []string `yaml:"include" json:"include"`
	Exclude []string `yaml:"exclude,omitempty" json:"exclude,omitempty"`
}

type ValidationResult struct {
	RuleID       string      `json:"rule_id"`
	PolicyID     string      `json:"policy_id"`
	Passed       bool        `json:"passed"`
	Severity     Severity    `json:"severity"`
	Message      string      `json:"message"`
	Location     *Location   `json:"location,omitempty"`
	SuggestedFix *string     `json:"suggested_fix,omitempty"`
	AutoFixable  bool        `json:"auto_fixable"`
	Confidence   Confidence  `json:"confidence"`
}

type Location struct {
	File   string `json:"file"`
	Line   *int   `json:"line,omitempty"`
	Column *int   `json:"column,omitempty"`
}

type FixResult struct {
	Success        bool    `json:"success"`
	OriginalContent string  `json:"original_content"`
	FixedContent   *string `json:"fixed_content,omitempty"`
	Error          *string `json:"error,omitempty"`
}

// apps/governance-cli/internal/policy/engine.go

package policy

import (
	"fmt"
	"io/fs"
	"path/filepath"
	"regexp"

	"github.com/bmatcuk/doublestar/v4"
	"github.com/goccy/go-yaml"
)

// PolicyEngine manages policy loading, validation, and fixing
type PolicyEngine struct {
	policies map[string]*Policy
}

// NewPolicyEngine creates a new policy engine instance
func NewPolicyEngine() *PolicyEngine {
	return &PolicyEngine{
		policies: make(map[string]*Policy),
	}
}

// LoadPolicies loads all policies from markdown convention files
// Extracts YAML from ## Policy Definition sections
func (e *PolicyEngine) LoadPolicies(conventionDir string) error {
	return filepath.WalkDir(conventionDir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}

		if d.IsDir() || filepath.Ext(path) != ".md" {
			return nil
		}

		policy, err := e.extractPolicyFromFile(path)
		if err != nil {
			return fmt.Errorf("failed to extract policy from %s: %w", path, err)
		}

		if policy != nil {
			e.policies[policy.ID] = policy
		}

		return nil
	})
}

// extractPolicyFromFile reads markdown file and extracts embedded policy YAML
func (e *PolicyEngine) extractPolicyFromFile(filePath string) (*Policy, error) {
	content, err := os.ReadFile(filePath)
	if err != nil {
		return nil, err
	}

	// Extract YAML from ## Policy Definition section
	re := regexp.MustCompile(`## Policy Definition\s+` + "```yaml\n([\\s\\S]+?)```")
	matches := re.FindSubmatch(content)
	if matches == nil {
		return nil, nil // No policy in this file
	}

	yamlContent := matches[1]
	return e.parsePolicy(yamlContent)
}

// parsePolicy parses YAML bytes into Policy struct
func (e *PolicyEngine) parsePolicy(yamlBytes []byte) (*Policy, error) {
	var wrapper struct {
		Policy Policy `yaml:"policy"`
	}

	if err := yaml.Unmarshal(yamlBytes, &wrapper); err != nil {
		return nil, fmt.Errorf("failed to parse policy YAML: %w", err)
	}

	// TODO: Validate against JSON Schema

	return &wrapper.Policy, nil
}

// ValidateFile validates a file against all applicable policies
func (e *PolicyEngine) ValidateFile(filePath string, content []byte) ([]ValidationResult, error) {
	applicablePolicies := e.GetPoliciesForFile(filePath)
	var results []ValidationResult

	for _, policy := range applicablePolicies {
		for _, rule := range policy.Rules {
			result, err := e.validateRule(filePath, content, policy, &rule)
			if err != nil {
				return nil, err
			}
			results = append(results, result)
		}
	}

	return results, nil
}

// GetPoliciesForFile returns all policies applicable to given file path
func (e *PolicyEngine) GetPoliciesForFile(filePath string) []*Policy {
	var applicable []*Policy

	for _, policy := range e.policies {
		if e.fileMatchesScope(filePath, &policy.Scope) {
			applicable = append(applicable, policy)
		}
	}

	return applicable
}

// fileMatchesScope checks if file path matches policy scope
func (e *PolicyEngine) fileMatchesScope(filePath string, scope *Scope) bool {
	// Check include patterns
	included := false
	for _, pattern := range scope.Paths.Include {
		if matched, _ := doublestar.Match(pattern, filePath); matched {
			included = true
			break
		}
	}

	if !included {
		return false
	}

	// Check exclude patterns
	for _, pattern := range scope.Paths.Exclude {
		if matched, _ := doublestar.Match(pattern, filePath); matched {
			return false
		}
	}

	return true
}

// ApplyFix applies autofix based on policy rule
func (e *PolicyEngine) ApplyFix(filePath string, violation *ValidationResult) (*FixResult, error) {
	policy, ok := e.policies[violation.PolicyID]
	if !ok {
		return &FixResult{
			Success: false,
			Error:   ptr("Policy not found"),
		}, nil
	}

	var rule *PolicyRule
	for i := range policy.Rules {
		if policy.Rules[i].ID == violation.RuleID {
			rule = &policy.Rules[i]
			break
		}
	}

	if rule == nil || !rule.Autofix || rule.FixTemplate == nil {
		originalContent, _ := os.ReadFile(filePath)
		return &FixResult{
			Success:         false,
			OriginalContent: string(originalContent),
			Error:           ptr("Rule is not auto-fixable"),
		}, nil
	}

	originalContent, err := os.ReadFile(filePath)
	if err != nil {
		return nil, err
	}

	fixedContent := e.applyFixTemplate(string(originalContent), *rule.FixTemplate)

	return &FixResult{
		Success:         true,
		OriginalContent: string(originalContent),
		FixedContent:    &fixedContent,
	}, nil
}

// GetPolicy returns policy by ID
func (e *PolicyEngine) GetPolicy(policyID string) *Policy {
	return e.policies[policyID]
}

// ListPolicies returns metadata for all loaded policies
func (e *PolicyEngine) ListPolicies() []PolicyMetadata {
	metadata := make([]PolicyMetadata, 0, len(e.policies))

	for _, policy := range e.policies {
		metadata = append(metadata, PolicyMetadata{
			ID:        policy.ID,
			Version:   policy.Version,
			Name:      policy.Metadata.Name,
			Category:  policy.Metadata.Category,
			RuleCount: len(policy.Rules),
		})
	}

	return metadata
}

// Helper function to create string pointer
func ptr(s string) *string {
	return &s
}

// GetCoverageReport generates coverage report for evaluated rules
func (e *PolicyEngine) GetCoverageReport(evaluatedRules []string) *CoverageReport {
	totalPolicies := len(e.policies)

	totalRules := 0
	for _, policy := range e.policies {
		totalRules += len(policy.Rules)
	}

	evaluatedPoliciesMap := make(map[string]bool)
	for _, ruleID := range evaluatedRules {
		if policy := e.getPolicyForRule(ruleID); policy != nil {
			evaluatedPoliciesMap[policy.ID] = true
		}
	}
	evaluatedPolicies := len(evaluatedPoliciesMap)

	coveragePercentage := 0.0
	if totalRules > 0 {
		coveragePercentage = (float64(len(evaluatedRules)) / float64(totalRules)) * 100
	}

	return &CoverageReport{
		TotalPolicies:        totalPolicies,
		EvaluatedPolicies:    evaluatedPolicies,
		TotalRules:           totalRules,
		EvaluatedRules:       len(evaluatedRules),
		CoveragePercentage:   coveragePercentage,
		UnevaluatedPolicies:  e.getUnevaluatedPolicies(evaluatedRules),
	}
}
````

### Dependencies

```go
// apps/governance-cli/go.mod

module github.com/wahidyankf/open-sharia-enterprise/apps/governance-cli

go 1.24.11 // Security fixes for CVE-2025-61729, CVE-2025-61727 (crypto/x509)

require (
	github.com/spf13/cobra v1.10.2           // CLI framework
	github.com/bmatcuk/doublestar/v4 v4.8.1  // Glob pattern matching
	github.com/goccy/go-yaml v1.15.14        // YAML parsing (actively maintained, better than archived gopkg.in/yaml.v3)
	github.com/xeipuuv/gojsonschema v1.2.0   // JSON Schema validation
	github.com/stretchr/testify v1.8.4       // Testing assertions
)
```

### Testing Standards

**Table-Driven Tests:**

All PolicyEngine validation logic must use Go's table-driven test pattern for comprehensive coverage:

```go
func TestValidateRule(t *testing.T) {
	tests := []struct {
		name      string
		rule      PolicyRule
		content   string
		wantPass  bool
		wantError bool
	}{
		{
			name: "FN001: valid prefix",
			rule: PolicyRule{ID: "FN001", Validation: ValidationSpec{Type: "regex", Pattern: "^ex-co__"}},
			content: "ex-co__example.md",
			wantPass: true,
		},
		{
			name: "FN001: invalid prefix",
			rule: PolicyRule{ID: "FN001", Validation: ValidationSpec{Type: "regex", Pattern: "^ex-co__"}},
			content: "wrong__example.md",
			wantPass: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := validateRule(tt.rule, tt.content)
			if (err != nil) != tt.wantError {
				t.Errorf("validateRule() error = %v, wantError %v", err, tt.wantError)
				return
			}
			if result.Passed != tt.wantPass {
				t.Errorf("validateRule() passed = %v, want %v", result.Passed, tt.wantPass)
			}
		})
	}
}
```

**Coverage Targets:**

- **PolicyEngine core**: >80% line coverage (critical path)
- **Validation functions**: >90% coverage (rule logic)
- **Integration tests**: All seven rule types validated
- **Edge cases**: Empty files, malformed YAML, circular dependencies

**Testing Tools:**

- Built-in `testing` package for unit tests
- `testify/assert` for readable assertions
- `testify/require` for fatal assertions
- Table-driven pattern for comprehensive scenarios

### Agent Integration Patterns

This section provides detailed technical guidance for integrating agents with the PolicyEngine.

#### Integration Approach: CLI vs Library

Agents can consume PolicyEngine in two ways:

**Option 1: CLI Invocation (Recommended for Phase 1)**

```bash
# Via Bash tool in agent prompt
governance-cli policy validate docs/example.md --format=json
```

**Pros:**

- Simpler integration (no Go import complexity)
- Clear separation of concerns
- Easy to debug and monitor

**Cons:**

- Additional process overhead (~10-50ms per invocation)
- JSON parsing required

**Option 2: Go Library Import (Future optimization)**

```go
// Direct Go library usage (requires agents to execute Go code)
import "github.com/wahidyankf/open-sharia-enterprise/apps/governance-cli/internal/policy"

engine := policy.NewPolicyEngine()
engine.LoadPolicies("docs/explanation/conventions")
results, _ := engine.ValidateFile("docs/example.md", content)
```

**Pros:**

- Lower latency (no process spawn)
- Direct access to Go types

**Cons:**

- Requires agents to invoke Go binaries or wrap in CLI
- More complex integration

**Decision for Phase 1:** Use CLI invocation via Bash tool for simplicity. Optimize to library import in Phase 5 if performance metrics warrant it.

#### repo-rules-checker Integration

**Initialization:**

```bash
# At agent startup, verify governance-cli is available
governance-cli policy list --convention-dir=docs/explanation/conventions > /tmp/policies.json

# Count policies loaded
POLICY_COUNT=$(jq length /tmp/policies.json)
echo "Loaded $POLICY_COUNT policies"
```

**Validation Loop:**

```bash
# For each file in scope
for file in $(glob "docs/**/*.md" ".claude/agents/*.md"); do
  # Skip metadata directory
  if [[ "$file" == docs/metadata/* ]]; then continue; fi

  # Validate file against policies
  RESULTS=$(governance-cli policy validate "$file" --format=json)

  # Parse results and write to audit report
  echo "$RESULTS" | jq -r '.violations[] | "### Finding: \(.rule_name)\n**Rule**: \(.rule_id) (\(.policy_id))\n**Location**: \(.location.file):\(.location.line)\n**Severity**: \(.severity)\n**Issue**: \(.message)\n**Policy Source**: \(.policy_source)\n**Auto-fixable**: \(.auto_fixable)\n**Confidence**: \(.confidence)\n"'
done

# Add coverage report at end
COVERAGE=$(governance-cli policy coverage --format=json)
echo "$COVERAGE" | jq -r '"## Policy Coverage Report\n**Policies Evaluated**: \(.evaluated_policies)/\(.total_policies) (\(.coverage_percentage)%)\n**Rules Checked**: \(.evaluated_rules)/\(.total_rules)"'
```

**Output Format (Enhanced Audit Report):**

```markdown
# Repository Rules Validation Report

**Generated**: 2025-12-24T10:30:00+07:00
**Scope**: docs/**, .claude/agents/**
**Policy Engine Version**: 1.0.0

## Findings

### Finding 1: File Naming Violation

**Rule**: FN001 (file-naming-v1)
**Location**: docs/example.md:1
**Severity**: Critical
**Issue**: Prefix 'wrong' does not match directory path
**Policy Source**: docs/explanation/conventions/ex-co\_\_file-naming-convention.md#policy-fn001
**Auto-fixable**: No
**Confidence**: MEDIUM (requires human judgment for correct prefix)

**Remediation**:

1. Identify correct prefix from directory path
2. Rename file with correct prefix
3. Update all cross-references

---

## Policy Coverage Report

**Policies Evaluated**: 12/15 (80%)
**Rules Checked**: 47/52 (90%)

✅ file-naming-v1 (5 rules, 0 violations)
⚠️ frontmatter-v1 (8 rules, 3 violations)
✅ active-voice-v1 (3 rules, 0 violations)
...
```

#### repo-rules-maker Integration

**Atomic Update Workflow:**

````bash
# 1. User requests convention change
# Example: "Add new rule for emoji usage in agent files"

# 2. repo-rules-maker updates convention prose
# (Uses Edit tool to update docs/explanation/conventions/formatting/ex-co-fo__emoji.md)

# 3. repo-rules-maker generates policy YAML
cat >> docs/explanation/conventions/formatting/ex-co-fo__emoji.md << 'EOF'

## Policy Definition

```yaml
policy:
  id: emoji-usage-v1
  version: "1.0.0"

  metadata:
    name: "Emoji Usage Convention"
    category: content-quality
    severity: medium
    created: "2025-12-24"

  traceability:
    principles:
      - id: accessibility-first
        path: docs/explanation/principles/content/ex-pr-co__accessibility-first.md
        reason: "Semantic emojis improve scannability for readers"
    conventions:
      - id: ex-co__emoji-usage
        path: docs/explanation/conventions/formatting/ex-co-fo__emoji.md
        section: "Usage Rules"

  scope:
    paths:
      include:
        - "docs/**/*.md"
        - ".claude/agents/*.md"
      exclude:
        - "CLAUDE.md"
        - ".claude/agents/*.md" # Agent prompt files

  rules:
    - id: EU001
      name: "No emojis in CLAUDE.md"
      severity: high
      description: "CLAUDE.md must not contain emojis for professional tone"

      validation:
        type: regex
        pattern: '[\u{1F300}-\u{1F9FF}]'
        match_type: avoid

      autofix: false
      confidence: high
````

EOF

# 4. Validate policy syntax

governance-cli policy validate-schema docs/explanation/conventions/ex-co\_\_emoji-usage.md

# 5. Update CLAUDE.md summary (if needed)

# (Uses Edit tool)

# 6. Trigger repo-rules-checker to validate

# (Spawns repo-rules-checker agent)

# 7. Commit changes atomically

git add docs/explanation/conventions/ex-co\_\_emoji-usage.md CLAUDE.md
git commit -m "feat(conventions): add emoji usage policy

- Add prose explanation of emoji usage rules
- Embed emoji-usage-v1 policy for agent consumption
- Update CLAUDE.md with policy reference"

````

**Key Responsibilities:**

1. **Prose authoring**: Write clear human-readable explanations
2. **Policy generation**: Translate rules into YAML schema
3. **Traceability linking**: Connect policies to principles/conventions
4. **Version management**: Increment version numbers appropriately
5. **Validation**: Ensure policy YAML syntax is valid
6. **Synchronization**: Keep prose and policy aligned

#### repo-rules-fixer Integration

**Policy-Driven Fix Workflow:**

```bash
# 1. Read audit report from repo-rules-checker
AUDIT_REPORT="generated-reports/repo-rules__2025-12-24--10-30__audit.md"

# 2. Extract findings with Rule IDs
FINDINGS=$(grep -A 10 "^### Finding" "$AUDIT_REPORT")

# 3. For each finding, query policy
RULE_ID="FN001"
POLICY_ID="file-naming-v1"

# Get policy metadata
POLICY=$(governance-cli policy get "$POLICY_ID" --format=json)

# Check if autofix enabled
AUTOFIX=$(echo "$POLICY" | jq -r ".rules[] | select(.id == \"$RULE_ID\") | .autofix")

if [ "$AUTOFIX" = "true" ]; then
  # Get confidence level
  CONFIDENCE=$(echo "$POLICY" | jq -r ".rules[] | select(.id == \"$RULE_ID\") | .confidence")

  if [ "$CONFIDENCE" = "high" ]; then
    # 4. Re-validate to confirm issue still exists
    CURRENT_RESULT=$(governance-cli policy validate "$FILE" --rule="$RULE_ID" --format=json)

    if [ "$(echo "$CURRENT_RESULT" | jq -r '.violations | length')" -gt 0 ]; then
      # 5. Apply fix
      # (Use Edit tool based on fix_template or inferred fix)

      echo "✅ Applied fix for $RULE_ID (HIGH confidence)"
    else
      echo "ℹ️ Issue already resolved for $RULE_ID"
    fi
  else
    echo "⚠️ Skipping $RULE_ID (confidence=$CONFIDENCE, not HIGH)"
  fi
else
  echo "ℹ️ Rule $RULE_ID not auto-fixable"
fi

# 6. Write fix report with policy metadata
cat >> "generated-reports/repo-rules__2025-12-24--10-35__fix.md" << EOF
### Fix 1: $RULE_ID Applied

**Rule**: $RULE_ID ($POLICY_ID)
**Confidence**: HIGH (objective structural rule)
**File**: $FILE
**Change**: Renamed file from wrong__example.md to ex-co__example.md
**Policy Source**: docs/explanation/conventions/meta/ex-co-me__file-naming.md#policy-fn001
**Rationale**: Policy specifies files in docs/explanation/conventions/ must use ex-co__ prefix
EOF
````

**Key Behavior Changes:**

1. **No embedded confidence logic**: All confidence assessment comes from policy metadata
2. **Mandatory re-validation**: Always confirm issue exists before fixing
3. **Policy-driven decisions**: Only fix if `autofix=true` AND `confidence=high`
4. **Transparency**: Link to policy source in fix reports

## Data Flow Architecture

```mermaid
%%{init: {'theme': 'base', 'themeVariables': { 'fontSize': '16px'}}}%%
graph TB
    %% Storage Layer
    ConvDocs["Convention Docs<br/>(docs/explanation/conventions/)<br/>Markdown + Embedded YAML Policies"]
    DevDocs["Development Docs<br/>(docs/explanation/development/)<br/>Markdown + Embedded YAML Policies"]

    %% Policy Engine
    PolicyEngine["PolicyEngine Library<br/>(apps/governance-cli/internal/policy/)<br/>Go + Cobra"]
    PolicyLoader["Policy Loader<br/>(Parses YAML from Markdown)"]
    Validator["Validation Engine<br/>(Regex, Schema, Function)"]
    AutoFixer["Auto-Fix Engine<br/>(Applies fix_template)"]

    %% Agents
    Checkers["Checker Agents<br/>(repo-rules-checker, docs-checker, etc.)<br/>Read-only + Report Generation"]
    Fixers["Fixer Agents<br/>(repo-rules-fixer, docs-fixer, etc.)<br/>Edit + Report Generation"]
    Validators["Validation Agents<br/>(policy-validator, policy-coverage-analyzer)<br/>Policy Meta-Validation"]

    %% Reports
    AuditReports["Audit Reports<br/>(generated-reports/*__audit.md)<br/>Enhanced with Policy Metadata"]
    FixReports["Fix Reports<br/>(generated-reports/*__fix.md)<br/>Policy Traceability"]

    %% Relationships
    ConvDocs -->|Contains| PolicyLoader
    DevDocs -->|Contains| PolicyLoader

    PolicyLoader -->|Loads| PolicyEngine
    PolicyEngine -->|Uses| Validator
    PolicyEngine -->|Uses| AutoFixer

    Checkers -->|Imports| PolicyEngine
    Fixers -->|Imports| PolicyEngine
    Validators -->|Validates| ConvDocs
    Validators -->|Validates| DevDocs

    Checkers -->|Generates| AuditReports
    Fixers -->|Generates| FixReports

    AuditReports -->|Read by| Fixers

    %% Styling
    classDef storage fill:#0173B2,stroke:#000,color:#fff
    classDef engine fill:#DE8F05,stroke:#000,color:#000
    classDef agent fill:#029E73,stroke:#000,color:#fff
    classDef report fill:#CC78BC,stroke:#000,color:#000

    class ConvDocs,DevDocs storage
    class PolicyEngine,PolicyLoader,Validator,AutoFixer engine
    class Checkers,Fixers,Validators agent
    class AuditReports,FixReports report
```

## New Agents/Tools

### 1. policy-validator (Green Checker Agent)

**Purpose**: Validate policy YAML syntax and completeness

**Frontmatter**:

```yaml
name: policy-validator
description: Validates policy YAML syntax, completeness, and consistency
tools: Read, Glob, Grep, Write, Bash
model: inherit
color: green
```

**Validation Checks**:

- YAML syntax valid
- Required fields present (id, version, metadata, traceability, scope, rules)
- Rule IDs unique within policy
- Severity values valid (blocking/high/medium/low/advisory)
- Glob patterns syntactically correct
- Regex patterns compile successfully
- Traceability links point to existing files
- No circular dependencies between policies

**Output**: `generated-reports/policy-validator__{timestamp}__audit.md`

### 2. policy-coverage-analyzer (Green Checker Agent)

**Purpose**: Analyze policy enforcement coverage

**Frontmatter**:

```yaml
name: policy-coverage-analyzer
description: Analyzes coverage of policies across agents and workflows
tools: Read, Glob, Grep, Write, Bash
model: inherit
color: green
```

**Analysis Checks**:

- Which policies have corresponding checker agents
- Which rules are actually validated in audit reports
- Dead policies (never referenced by any agent)
- Gaps (conventions without policies, policies without validators)
- Coverage percentage (evaluated rules / total rules)

**Output**: `generated-reports/policy-coverage__{timestamp}__analysis.md`

### 3. PolicyEngine CLI Tool

**Purpose**: Standalone CLI for policy operations

**Commands**:

```bash
# Validate a single file against policies
governance-cli policy validate docs/example.md

# List all policies
governance-cli policy list --convention-dir=docs/explanation/conventions

# Test a policy rule against test cases
governance-cli policy test-rule FN001 --policy=file-naming-v1

# Generate policy documentation
governance-cli policy docs --output=docs/reference/re__policy-reference.md

# Coverage report
governance-cli policy coverage --reports=generated-reports/*__audit.md
```

## Implementation Files

### Critical Files

**Phase 0 (Foundation):**

- `apps/governance-cli/` (NEW - entire app)
  - Nx project configuration
  - Go module setup with Cobra

- `docs/explanation/development/infra/ex-de-in__policy-as-code.md` (NEW)
  - Policy schema documentation
  - Authoring guidelines
  - Agent consumption patterns

- `apps/governance-cli/internal/policy/types.go` (NEW)
  - Go structs for Policy, PolicyRule, ValidationResult, etc.
  - Type definitions with YAML/JSON tags

- `apps/governance-cli/internal/policy/engine.go` (NEW)
  - Core PolicyEngine implementation
  - API methods (LoadPolicies, ValidateFile, ApplyFix, etc.)

- `apps/governance-cli/internal/policy/validators/` (NEW)
  - Rule type validators (regex, schema, function-based)

- `apps/governance-cli/cmd/root.go` (NEW)
  - Cobra root command setup
  - Global flags and configuration

- `apps/governance-cli/cmd/policy.go` (NEW)
  - Policy subcommand with Cobra
  - Subcommands: validate, list, test-rule, docs, coverage

- `apps/governance-cli/internal/policy/engine_test.go` (NEW)
  - Unit tests for PolicyEngine
  - Table-driven tests for validation logic

- `.claude/agents/policy-validator.md` (NEW)
  - Policy YAML validator agent

**Phase 1 (Pilot - repo-rules Family):**

- `docs/explanation/conventions/meta/ex-co-me__file-naming.md` (MODIFY)
  - Add ## Policy Definition section with YAML

- `.claude/agents/wow__rules-checker.md` (MODIFY: 953→400 lines)
  - Add PolicyEngine integration
  - Remove embedded rule definitions

- `.claude/agents/wow__rules-maker.md` (MODIFY: 851→350 lines)
  - Add policy sync logic
  - Update workflow documentation

- `.claude/agents/wow__rules-fixer.md` (MODIFY: 510→250 lines)
  - Add policy-driven fix logic
  - Query policies for autofix eligibility

- `CLAUDE.md` (MODIFY)
  - Add policy workflow summary
  - Link to ex-de\_\_policy-as-code.md

## Testing Strategy

### Unit Tests

Go table-driven tests for PolicyEngine core functionality:

```go
// apps/governance-cli/internal/policy/engine_test.go

func TestLoadPolicies(t *testing.T) {
	tests := []struct {
		name          string
		conventionDir string
		wantPolicies  int
		wantError     bool
	}{
		{
			name:          "Load all convention policies",
			conventionDir: "testdata/conventions",
			wantPolicies:  3,
			wantError:     false,
		},
		{
			name:          "Reject invalid YAML",
			conventionDir: "testdata/invalid",
			wantPolicies:  0,
			wantError:     true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			engine := NewPolicyEngine()
			err := engine.LoadPolicies(tt.conventionDir)

			if (err != nil) != tt.wantError {
				t.Errorf("LoadPolicies() error = %v, wantError %v", err, tt.wantError)
			}

			if len(engine.policies) != tt.wantPolicies {
				t.Errorf("LoadPolicies() loaded %d policies, want %d", len(engine.policies), tt.wantPolicies)
			}
		})
	}
}
```

### Integration Tests

End-to-end validation tests:

```go
func TestValidateFile_PrefixMismatch(t *testing.T) {
	engine := NewPolicyEngine()
	engine.LoadPolicies("../../docs/explanation/conventions")

	content := []byte("# Example File\nContent here")
	results, err := engine.ValidateFile("docs/explanation/conventions/wrong__example.md", content)

	require.NoError(t, err)
	require.NotEmpty(t, results)

	// Should detect FN001 violation
	found := false
	for _, result := range results {
		if result.RuleID == "FN001" && !result.Passed {
			found = true
			assert.Contains(t, result.Message, "Prefix mismatch")
			break
		}
	}
	assert.True(t, found, "Should detect FN001 prefix mismatch violation")
}
```

### CLI Integration Tests

Test CLI commands work correctly:

```go
func TestCLI_ValidateCommand(t *testing.T) {
	// Test governance-cli policy validate command
	cmd := exec.Command("governance-cli", "policy", "validate", "testdata/test-file.md")
	output, err := cmd.CombinedOutput()

	require.NoError(t, err)
	assert.Contains(t, string(output), "Validation complete")
}
```

## Performance Considerations

### Optimization Strategies

1. **Lazy Loading**: Load only policies relevant to current agent scope
2. **Caching**: Parse policies once per agent execution (not per validation)
3. **Batch Validation**: Validate multiple rules against file in single pass
4. **Glob Optimization**: Pre-compile glob patterns for faster matching

### Performance Targets

- Policy loading time: <200ms per agent
- Validation overhead: <5% vs. current baseline
- Memory usage: <50MB for PolicyEngine instance
- Concurrent agents: Support 5+ agents running simultaneously

### Benchmarking

Before and after migration:

- Measure agent execution time
- Measure memory consumption
- Measure audit report generation time
- Track false positive rate
- Monitor user-reported issues
