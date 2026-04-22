package mermaid

import (
	"testing"
)

func TestExtractBlocks(t *testing.T) {
	tests := []struct {
		name     string
		filePath string
		content  string
		wantLen  int
		checks   func(t *testing.T, blocks []MermaidBlock)
	}{
		{
			name:     "no mermaid blocks",
			filePath: "doc.md",
			content: `# Title

Some text here.

` + "```go" + `
fmt.Println("hello")
` + "```" + `
`,
			wantLen: 0,
		},
		{
			name:     "one flowchart block",
			filePath: "doc.md",
			content: `# Title

Some intro.

` + "```mermaid" + `
flowchart TD
    A --> B
` + "```" + `

End.
`,
			wantLen: 1,
			checks: func(t *testing.T, blocks []MermaidBlock) {
				t.Helper()
				b := blocks[0]
				if b.FilePath != "doc.md" {
					t.Errorf("FilePath = %q, want %q", b.FilePath, "doc.md")
				}
				if b.BlockIndex != 0 {
					t.Errorf("BlockIndex = %d, want 0", b.BlockIndex)
				}
				// Opening fence is on line 5 (1-based)
				if b.StartLine != 5 {
					t.Errorf("StartLine = %d, want 5", b.StartLine)
				}
				if b.Source == "" {
					t.Error("Source must not be empty")
				}
			},
		},
		{
			name:     "two separate mermaid blocks",
			filePath: "multi.md",
			content:  "```mermaid\nflowchart TD\n    A --> B\n```\n\nsome text\n\n```mermaid\nflowchart LR\n    C --> D\n```\n",
			wantLen:  2,
			checks: func(t *testing.T, blocks []MermaidBlock) {
				t.Helper()
				if blocks[0].BlockIndex != 0 {
					t.Errorf("first block index = %d, want 0", blocks[0].BlockIndex)
				}
				if blocks[1].BlockIndex != 1 {
					t.Errorf("second block index = %d, want 1", blocks[1].BlockIndex)
				}
				if blocks[0].StartLine != 1 {
					t.Errorf("first block StartLine = %d, want 1", blocks[0].StartLine)
				}
			},
		},
		{
			name:     "non-mermaid fenced blocks not extracted",
			filePath: "code.md",
			content:  "```bash\necho hello\n```\n\n```python\nprint('hi')\n```\n",
			wantLen:  0,
		},
		{
			name:     "empty mermaid block",
			filePath: "empty.md",
			content:  "```mermaid\n```\n",
			wantLen:  1,
			checks: func(t *testing.T, blocks []MermaidBlock) {
				t.Helper()
				if blocks[0].Source != "" {
					t.Errorf("Source = %q, want empty string", blocks[0].Source)
				}
				if blocks[0].StartLine != 1 {
					t.Errorf("StartLine = %d, want 1", blocks[0].StartLine)
				}
			},
		},
		{
			name:     "tilde fence opening",
			filePath: "tilde.md",
			content:  "~~~mermaid\nflowchart TD\n    A --> B\n~~~\n",
			wantLen:  1,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			blocks := ExtractBlocks(tt.filePath, tt.content)
			if len(blocks) != tt.wantLen {
				t.Errorf("len(blocks) = %d, want %d", len(blocks), tt.wantLen)
			}
			if tt.checks != nil {
				tt.checks(t, blocks)
			}
		})
	}
}
