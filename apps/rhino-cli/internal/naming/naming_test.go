package naming

import (
	"testing"
)

func TestValidateSuffix(t *testing.T) {
	roles := []string{"maker", "checker", "fixer", "dev", "deployer", "executor", "manager"}
	types := []string{"quality-gate", "execution", "setup"}

	tests := []struct {
		name         string
		path         string
		allowed      []string
		kind         string
		wantViolates bool
	}{
		{"agent with maker suffix", "/x/plan-maker.md", roles, "role-suffix", false},
		{"agent with dev suffix", "/x/swe-golang-dev.md", roles, "role-suffix", false},
		{"agent multi qualifier", "/x/apps-ayokoding-web-general-checker.md", roles, "role-suffix", false},
		{"agent wrong suffix", "/x/web-researcher.md", roles, "role-suffix", true},
		{"agent bare suffix no scope", "/x/maker.md", roles, "role-suffix", true},
		{"agent empty-like", "/x/something-else.md", roles, "role-suffix", true},
		{"workflow quality-gate", "/x/ci-quality-gate.md", types, "type-suffix", false},
		{"workflow execution", "/x/plan-execution.md", types, "type-suffix", false},
		{"workflow setup", "/x/development-environment-setup.md", types, "type-suffix", false},
		{"workflow no known suffix", "/x/repo-validation.md", types, "type-suffix", true},
		{"workflow multi-hyphen type matches longest", "/x/docs-quality-gate.md", types, "type-suffix", false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := ValidateSuffix(tt.path, tt.allowed, tt.kind)
			gotViolates := v != nil
			if gotViolates != tt.wantViolates {
				t.Fatalf("ValidateSuffix(%q): violates=%v, want=%v (v=%+v)", tt.path, gotViolates, tt.wantViolates, v)
			}
			if v != nil && v.Kind != tt.kind {
				t.Errorf("violation kind=%q, want %q", v.Kind, tt.kind)
			}
			if v != nil && v.Path != tt.path {
				t.Errorf("violation path=%q, want %q", v.Path, tt.path)
			}
		})
	}
}

func TestValidateFrontmatterName(t *testing.T) {
	tests := []struct {
		name         string
		path         string
		content      string
		wantViolates bool
	}{
		{
			name:    "matching name",
			path:    "/a/plan-maker.md",
			content: "---\nname: plan-maker\ndescription: x\n---\nbody",
		},
		{
			name:         "mismatched name",
			path:         "/a/plan-maker.md",
			content:      "---\nname: something-else\n---\nbody",
			wantViolates: true,
		},
		{
			name:    "no frontmatter",
			path:    "/a/plan-maker.md",
			content: "# Just markdown\n",
		},
		{
			name:    "frontmatter without name field (opencode style)",
			path:    "/a/plan-maker.md",
			content: "---\ndescription: x\nmodel: foo\n---\nbody",
		},
		{
			name:    "quoted name field",
			path:    "/a/plan-maker.md",
			content: "---\nname: \"plan-maker\"\ndescription: x\n---\nbody",
		},
		{
			name:    "crlf line endings",
			path:    "/a/plan-maker.md",
			content: "---\r\nname: plan-maker\r\ndescription: x\r\n---\r\nbody",
		},
		{
			name:         "crlf line endings mismatched",
			path:         "/a/plan-maker.md",
			content:      "---\r\nname: wrong\r\n---\r\nbody",
			wantViolates: true,
		},
		{
			name:    "unclosed frontmatter",
			path:    "/a/plan-maker.md",
			content: "---\nname: plan-maker\n",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := ValidateFrontmatterName(tt.path, []byte(tt.content))
			gotViolates := v != nil
			if gotViolates != tt.wantViolates {
				t.Fatalf("violates=%v, want=%v (v=%+v)", gotViolates, tt.wantViolates, v)
			}
			if v != nil && v.Kind != "frontmatter-mismatch" {
				t.Errorf("kind=%q want frontmatter-mismatch", v.Kind)
			}
		})
	}
}

func TestValidateMirror(t *testing.T) {
	tests := []struct {
		name     string
		claude   []string
		opencode []string
		wantN    int
	}{
		{
			name:     "perfect mirror",
			claude:   []string{"/c/plan-maker.md", "/c/docs-maker.md"},
			opencode: []string{"/o/plan-maker.md", "/o/docs-maker.md"},
			wantN:    0,
		},
		{
			name:     "missing in opencode",
			claude:   []string{"/c/plan-maker.md", "/c/docs-maker.md"},
			opencode: []string{"/o/plan-maker.md"},
			wantN:    1,
		},
		{
			name:     "missing in claude",
			claude:   []string{"/c/plan-maker.md"},
			opencode: []string{"/o/plan-maker.md", "/o/docs-maker.md"},
			wantN:    1,
		},
		{
			name:     "drift both sides",
			claude:   []string{"/c/a-maker.md", "/c/b-maker.md"},
			opencode: []string{"/o/b-maker.md", "/o/c-maker.md"},
			wantN:    2,
		},
		{
			name:     "empty sets",
			claude:   nil,
			opencode: nil,
			wantN:    0,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := ValidateMirror(tt.claude, tt.opencode)
			if len(got) != tt.wantN {
				t.Fatalf("got %d violations, want %d: %+v", len(got), tt.wantN, got)
			}
			for _, v := range got {
				if v.Kind != "mirror-drift" {
					t.Errorf("unexpected kind %q", v.Kind)
				}
			}
		})
	}
}

func TestBasenameSansExt(t *testing.T) {
	tests := []struct {
		in, want string
	}{
		{"/a/b/plan-maker.md", "plan-maker"},
		{"plan-maker.md", "plan-maker"},
		{"foo", "foo"},
	}
	for _, tt := range tests {
		if got := basenameSansExt(tt.in); got != tt.want {
			t.Errorf("basenameSansExt(%q)=%q want %q", tt.in, got, tt.want)
		}
	}
}
