package repogovernance

import (
	"encoding/json"
	"strings"
	"testing"
)

// TestAuditEnvelopeMarshalKeyOrder verifies that AuditEnvelope.MarshalJSON
// emits keys in the canonical order: schema → status → result, then within
// result: git_sha → ran_at → total_findings → by_severity → by_category →
// categories → skipped_false_positives. The test inspects the raw byte
// sequence so any future drift in key order is caught immediately.
func TestAuditEnvelopeMarshalKeyOrder(t *testing.T) {
	env := AuditEnvelope{
		Schema: AuditEnvelopeSchema,
		Status: "ok",
		Result: AuditResult{
			GitSHA:        "abc1234",
			RanAt:         "2026-05-12T00:00:00Z",
			TotalFindings: 0,
			BySeverity:    map[string]int{"high": 0},
			ByCategory:    map[string]int{"agents-md-size": 0},
			Categories: []AuditCategoryResult{
				{Name: "agents-md-size", Command: "repo-governance agents-md-size", Passed: true},
			},
			SkippedFalsePositives: nil,
		},
	}
	data, err := json.Marshal(env)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	s := string(data)

	wantOrderTop := []string{`"schema"`, `"status"`, `"result"`}
	if !indicesAscending(s, wantOrderTop) {
		t.Errorf("top-level key order wrong; got: %s", s)
	}
	wantOrderResult := []string{
		`"git_sha"`,
		`"ran_at"`,
		`"total_findings"`,
		`"by_severity"`,
		`"by_category"`,
		`"categories"`,
		`"skipped_false_positives"`,
	}
	if !indicesAscending(s, wantOrderResult) {
		t.Errorf("result-level key order wrong; got: %s", s)
	}
	wantOrderCategory := []string{`"name"`, `"command"`, `"passed"`, `"findings"`}
	if !indicesAscending(s, wantOrderCategory) {
		t.Errorf("category-level key order wrong; got: %s", s)
	}
}

// TestAuditEnvelopeFindingKeyOrder verifies the key order at the finding
// level (key → severity → criticality → file → line → message).
func TestAuditEnvelopeFindingKeyOrder(t *testing.T) {
	env := AuditEnvelope{
		Schema: AuditEnvelopeSchema,
		Status: "failed",
		Result: AuditResult{
			TotalFindings: 1,
			Categories: []AuditCategoryResult{
				{
					Name:    "agents-md-size",
					Command: "repo-governance agents-md-size",
					Passed:  false,
					Findings: []AuditFinding{
						{
							Key:         "agents-md-size|AGENTS.md|deadbeef",
							Severity:    "high",
							Criticality: "HIGH",
							File:        "AGENTS.md",
							Line:        1,
							Message:     "AGENTS.md exceeds hard limit",
						},
					},
				},
			},
		},
	}
	data, err := json.Marshal(env)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	s := string(data)
	wantOrder := []string{`"key"`, `"severity"`, `"criticality"`, `"file"`, `"line"`, `"message"`}
	if !indicesAscending(s, wantOrder) {
		t.Errorf("finding-level key order wrong; got: %s", s)
	}
}

// TestAuditEnvelopeBytesDeterministic verifies that re-marshaling the same
// envelope twice produces byte-identical output. Map iteration order is the
// only conceivable source of drift; encoding/json sorts map keys
// deterministically so the output is stable.
func TestAuditEnvelopeBytesDeterministic(t *testing.T) {
	env := AuditEnvelope{
		Schema: AuditEnvelopeSchema,
		Status: "failed",
		Result: AuditResult{
			GitSHA:        "abc1234",
			RanAt:         "2026-05-12T00:00:00Z",
			TotalFindings: 3,
			BySeverity:    map[string]int{"high": 3, "medium": 0, "low": 0},
			ByCategory:    map[string]int{"agents-md-size": 2, "frontmatter-audit": 1},
			Categories: []AuditCategoryResult{
				{Name: "agents-md-size", Command: "x", Passed: false},
				{Name: "frontmatter-audit", Command: "y", Passed: false},
			},
		},
	}
	a, errA := json.Marshal(env)
	b, errB := json.Marshal(env)
	if errA != nil || errB != nil {
		t.Fatalf("Marshal errors: %v %v", errA, errB)
	}
	if string(a) != string(b) {
		t.Errorf("non-deterministic marshal: %s vs %s", a, b)
	}
}

// TestAuditEnvelopeNilSlicesEmitEmptyArrays verifies the wire projection
// converts nil Categories and SkippedFalsePositives into empty arrays "[]"
// rather than JSON null. Empty arrays are the documented contract for
// downstream consumers.
func TestAuditEnvelopeNilSlicesEmitEmptyArrays(t *testing.T) {
	env := AuditEnvelope{
		Schema: AuditEnvelopeSchema,
		Status: "ok",
		Result: AuditResult{},
	}
	data, err := json.Marshal(env)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	s := string(data)
	for _, want := range []string{`"categories":[]`, `"skipped_false_positives":[]`, `"by_severity":{}`, `"by_category":{}`} {
		if !strings.Contains(s, want) {
			t.Errorf("expected %q in output, got: %s", want, s)
		}
	}
}

// TestAuditEnvelopeFindingOmitEmpty verifies the omitempty behaviour for
// finding.file and finding.line: empty file and zero line are omitted.
func TestAuditEnvelopeFindingOmitEmpty(t *testing.T) {
	env := AuditEnvelope{
		Schema: AuditEnvelopeSchema,
		Status: "failed",
		Result: AuditResult{
			TotalFindings: 1,
			Categories: []AuditCategoryResult{
				{
					Name:    "x",
					Command: "y",
					Passed:  false,
					Findings: []AuditFinding{
						{Key: "k", Severity: "high", Criticality: "HIGH", Message: "m"},
					},
				},
			},
		},
	}
	data, err := json.Marshal(env)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	s := string(data)
	if strings.Contains(s, `"file"`) {
		t.Errorf(`expected "file" to be omitted, got: %s`, s)
	}
	if strings.Contains(s, `"line"`) {
		t.Errorf(`expected "line" to be omitted, got: %s`, s)
	}
}

// indicesAscending reports whether each substring in want appears in s in the
// listed order. Each substring must appear at least once; later substrings
// must start at an index strictly greater than earlier substrings' indices.
func indicesAscending(s string, want []string) bool {
	last := -1
	for _, w := range want {
		i := strings.Index(s[last+1:], w)
		if i < 0 {
			return false
		}
		last = last + 1 + i
	}
	return true
}
