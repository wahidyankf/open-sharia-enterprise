package sync

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"gopkg.in/yaml.v3"
	"regexp"
)

// normalizeYAML fixes common YAML formatting issues in Claude agent files
// Specifically, adds spaces after colons where missing (e.g., "name:value" -> "name: value")
func normalizeYAML(content []byte) []byte {
	// Pattern: word character or hyphen followed by colon, then non-whitespace
	// This matches "name:value" but not "name: value" or "  - item"
	re := regexp.MustCompile(`(?m)^([a-zA-Z0-9_-]+):([^\s])`)

	// Replace with space after colon
	normalized := re.ReplaceAll(content, []byte("$1: $2"))

	return normalized
}

// ExtractFrontmatter extracts YAML frontmatter and body from markdown content
func ExtractFrontmatter(content []byte) (frontmatter []byte, body []byte, err error) {
	// Look for frontmatter between --- markers
	lines := bytes.Split(content, []byte("\n"))

	if len(lines) < 3 {
		return nil, content, fmt.Errorf("file too short to contain frontmatter")
	}

	// First line should be ---
	if !bytes.Equal(bytes.TrimSpace(lines[0]), []byte("---")) {
		return nil, content, fmt.Errorf("frontmatter does not start with ---")
	}

	// Find the closing ---
	endIndex := -1
	for i := 1; i < len(lines); i++ {
		if bytes.Equal(bytes.TrimSpace(lines[i]), []byte("---")) {
			endIndex = i
			break
		}
	}

	if endIndex == -1 {
		return nil, content, fmt.Errorf("frontmatter closing --- not found")
	}

	// Extract frontmatter (without the --- markers)
	frontmatter = bytes.Join(lines[1:endIndex], []byte("\n"))

	// Normalize YAML (fix formatting issues like missing spaces after colons)
	frontmatter = normalizeYAML(frontmatter)

	// Extract body (everything after closing ---)
	if endIndex+1 < len(lines) {
		body = bytes.Join(lines[endIndex+1:], []byte("\n"))
	} else {
		body = []byte("")
	}

	return frontmatter, body, nil
}

// ParseClaudeTools parses tools from Claude format (comma-separated or array)
func ParseClaudeTools(toolsRaw interface{}) []string {
	var tools []string

	switch v := toolsRaw.(type) {
	case []interface{}:
		// Already an array
		for _, tool := range v {
			if toolStr, ok := tool.(string); ok {
				tools = append(tools, toolStr)
			}
		}
	case string:
		// Comma-separated string
		parts := strings.Split(v, ",")
		for _, part := range parts {
			trimmed := strings.TrimSpace(part)
			if trimmed != "" {
				tools = append(tools, trimmed)
			}
		}
	}

	return tools
}

// ConvertTools converts Claude tools array to OpenCode tools map
func ConvertTools(claudeTools []string) map[string]bool {
	tools := make(map[string]bool)

	for _, tool := range claudeTools {
		toolLower := strings.ToLower(strings.TrimSpace(tool))
		if toolLower != "" {
			tools[toolLower] = true
		}
	}

	return tools
}

// ConvertModel converts Claude model to OpenCode model
func ConvertModel(claudeModel string) string {
	model := strings.TrimSpace(claudeModel)

	switch model {
	case "sonnet", "opus":
		return "zai/glm-4.7"
	case "haiku":
		return "zai/glm-4.5-air"
	default:
		return "inherit"
	}
}

// ConvertAgent converts a Claude agent to OpenCode format
func ConvertAgent(inputPath, outputPath string, dryRun bool) error {
	// 1. Read file
	content, err := os.ReadFile(inputPath)
	if err != nil {
		return fmt.Errorf("failed to read file: %w", err)
	}

	// 2. Extract frontmatter and body
	frontmatterBytes, body, err := ExtractFrontmatter(content)
	if err != nil {
		return fmt.Errorf("failed to extract frontmatter: %w", err)
	}

	// 3. Parse Claude YAML as generic map to handle tools field
	var claudeData map[string]interface{}
	if err := yaml.Unmarshal(frontmatterBytes, &claudeData); err != nil {
		return fmt.Errorf("failed to parse YAML: %w", err)
	}

	// 4. Parse tools
	var tools []string
	if toolsRaw, ok := claudeData["tools"]; ok {
		tools = ParseClaudeTools(toolsRaw)
	}

	// 5. Get other fields
	description := ""
	if desc, ok := claudeData["description"].(string); ok {
		description = desc
	}

	model := ""
	if m, ok := claudeData["model"].(string); ok {
		model = m
	}

	var skills []string
	if skillsRaw, ok := claudeData["skills"].([]interface{}); ok {
		for _, skill := range skillsRaw {
			if skillStr, ok := skill.(string); ok {
				skills = append(skills, skillStr)
			}
		}
	}

	// 6. Convert to OpenCode format
	opencodeAgent := OpenCodeAgent{
		Description: description,
		Model:       ConvertModel(model),
		Tools:       ConvertTools(tools),
		Skills:      skills,
	}

	// 7. Marshal to YAML with 2-space indentation (Prettier standard)
	var buf bytes.Buffer
	encoder := yaml.NewEncoder(&buf)
	encoder.SetIndent(2)
	if err := encoder.Encode(opencodeAgent); err != nil {
		return fmt.Errorf("failed to marshal YAML: %w", err)
	}
	if err := encoder.Close(); err != nil {
		return fmt.Errorf("failed to close YAML encoder: %w", err)
	}
	newFrontmatter := buf.Bytes()

	// 8. Reconstruct markdown
	var output bytes.Buffer
	output.WriteString("---\n")
	output.Write(newFrontmatter)
	output.WriteString("---\n")
	output.Write(body)

	// 9. Write (if not dry run)
	if !dryRun {
		// Ensure output directory exists
		if err := os.MkdirAll(filepath.Dir(outputPath), 0755); err != nil {
			return fmt.Errorf("failed to create output directory: %w", err)
		}

		if err := os.WriteFile(outputPath, output.Bytes(), 0644); err != nil {
			return fmt.Errorf("failed to write file: %w", err)
		}
	}

	return nil
}

// ConvertAllAgents converts all agents from .claude/agents to .opencode/agent
func ConvertAllAgents(repoRoot string, dryRun bool) (converted int, failed int, failedFiles []string, err error) {
	claudeAgentsDir := filepath.Join(repoRoot, ".claude", "agents")
	opencodeAgentDir := filepath.Join(repoRoot, ".opencode", "agent")

	// Read all agent files
	entries, err := os.ReadDir(claudeAgentsDir)
	if err != nil {
		return 0, 0, nil, fmt.Errorf("failed to read .claude/agents directory: %w", err)
	}

	for _, entry := range entries {
		if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".md") {
			continue
		}

		// Skip README.md
		if entry.Name() == "README.md" {
			continue
		}

		inputPath := filepath.Join(claudeAgentsDir, entry.Name())
		outputPath := filepath.Join(opencodeAgentDir, entry.Name())

		if err := ConvertAgent(inputPath, outputPath, dryRun); err != nil {
			failed++
			failedFiles = append(failedFiles, entry.Name())
		} else {
			converted++
		}
	}

	return converted, failed, failedFiles, nil
}
