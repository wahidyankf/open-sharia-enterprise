package repogovernance

import "encoding/json"

// AuditEnvelopeSchema is the canonical schema identifier for the orchestrator
// JSON envelope. Any change to the wire shape MUST bump this string.
const AuditEnvelopeSchema = "rhino-cli/repo-governance-audit/v1"

// AuditEnvelope is the top-level JSON envelope returned by the orchestrator.
// Field encoding order is fixed at marshal time (schema → status → result) to
// guarantee byte-determinism across runs.
type AuditEnvelope struct {
	// Schema is the canonical schema identifier — always AuditEnvelopeSchema.
	Schema string
	// Status is "ok" when TotalFindings is zero, "failed" otherwise.
	Status string
	// Result holds the aggregated audit payload.
	Result AuditResult
}

// AuditResult is the inner payload of the orchestrator envelope. Map fields
// (BySeverity, ByCategory) serialize with sorted keys via Go's encoding/json
// package, which orders maps lexicographically for deterministic output.
type AuditResult struct {
	// GitSHA is the short SHA returned by `git rev-parse --short HEAD`, or
	// "unknown" when the SHA cannot be determined.
	GitSHA string
	// RanAt is the RFC 3339 timestamp at which the orchestrator ran.
	RanAt string
	// TotalFindings is the count across categories[].findings (excluding skipped).
	TotalFindings int
	// BySeverity maps severity ("critical"|"high"|"medium"|"low") to finding count.
	BySeverity map[string]int
	// ByCategory maps category name to finding count.
	ByCategory map[string]int
	// Categories holds one entry per category in fixed orchestrator order.
	Categories []AuditCategoryResult
	// SkippedFalsePositives holds findings whose Key matched a known-false-positives
	// entry. These do NOT count toward TotalFindings.
	SkippedFalsePositives []AuditFinding
}

// AuditCategoryResult is one category's contribution to the envelope.
type AuditCategoryResult struct {
	// Name is the stable category name (e.g., "agents-md-size").
	Name string
	// Command is the human-facing CLI invocation (e.g., "repo-governance agents-md-size").
	Command string
	// Passed is true when the category produced zero findings.
	Passed bool
	// Findings holds the per-category finding list. Never nil — empty slice when clean.
	Findings []AuditFinding
}

// AuditFinding is the generic shape that every per-command finding type
// converts to for inclusion in the orchestrator envelope.
type AuditFinding struct {
	// Key is a stable identifier — "<category>|<File>|<short-message-hash>".
	Key string
	// Severity is one of "critical", "high", "medium", "low".
	Severity string
	// Criticality is the AI-checker-aligned tier: "CRITICAL", "HIGH", "MEDIUM", "LOW".
	Criticality string
	// File is the offending file path; may be empty for whole-repo findings.
	File string
	// Line is the 1-based line number; 0 when the finding has no line position.
	Line int
	// Message is the human-readable description.
	Message string
}

// auditEnvelopeJSON is the on-the-wire shape of AuditEnvelope with explicit
// JSON tags fixing the encoded key order.
type auditEnvelopeJSON struct {
	Schema string          `json:"schema"`
	Status string          `json:"status"`
	Result auditResultJSON `json:"result"`
}

// auditResultJSON is the on-the-wire shape of AuditResult.
type auditResultJSON struct {
	GitSHA                string                    `json:"git_sha"`
	RanAt                 string                    `json:"ran_at"`
	TotalFindings         int                       `json:"total_findings"`
	BySeverity            map[string]int            `json:"by_severity"`
	ByCategory            map[string]int            `json:"by_category"`
	Categories            []auditCategoryResultJSON `json:"categories"`
	SkippedFalsePositives []auditFindingJSON        `json:"skipped_false_positives"`
}

// auditCategoryResultJSON is the on-the-wire shape of AuditCategoryResult.
type auditCategoryResultJSON struct {
	Name     string             `json:"name"`
	Command  string             `json:"command"`
	Passed   bool               `json:"passed"`
	Findings []auditFindingJSON `json:"findings"`
}

// auditFindingJSON is the on-the-wire shape of AuditFinding.
type auditFindingJSON struct {
	Key         string `json:"key"`
	Severity    string `json:"severity"`
	Criticality string `json:"criticality"`
	File        string `json:"file,omitempty"`
	Line        int    `json:"line,omitempty"`
	Message     string `json:"message"`
}

// MarshalJSON encodes the envelope with a guaranteed canonical key order at
// every level: schema → status → result, then git_sha → ran_at → total_findings
// → by_severity → by_category → categories → skipped_false_positives.
func (e AuditEnvelope) MarshalJSON() ([]byte, error) {
	return json.Marshal(auditEnvelopeJSON{
		Schema: e.Schema,
		Status: e.Status,
		Result: toAuditResultJSON(e.Result),
	})
}

// toAuditResultJSON projects an AuditResult to its wire shape, normalizing
// nil slices to empty slices and nil maps to empty maps so JSON output is
// "[]" / "{}" rather than "null".
func toAuditResultJSON(r AuditResult) auditResultJSON {
	bySev := r.BySeverity
	if bySev == nil {
		bySev = map[string]int{}
	}
	byCat := r.ByCategory
	if byCat == nil {
		byCat = map[string]int{}
	}
	cats := make([]auditCategoryResultJSON, 0, len(r.Categories))
	for _, c := range r.Categories {
		cats = append(cats, toAuditCategoryJSON(c))
	}
	skipped := make([]auditFindingJSON, 0, len(r.SkippedFalsePositives))
	for _, f := range r.SkippedFalsePositives {
		skipped = append(skipped, toAuditFindingJSON(f))
	}
	return auditResultJSON{
		GitSHA:                r.GitSHA,
		RanAt:                 r.RanAt,
		TotalFindings:         r.TotalFindings,
		BySeverity:            bySev,
		ByCategory:            byCat,
		Categories:            cats,
		SkippedFalsePositives: skipped,
	}
}

// toAuditCategoryJSON projects an AuditCategoryResult to its wire shape.
func toAuditCategoryJSON(c AuditCategoryResult) auditCategoryResultJSON {
	out := make([]auditFindingJSON, 0, len(c.Findings))
	for _, f := range c.Findings {
		out = append(out, toAuditFindingJSON(f))
	}
	return auditCategoryResultJSON{
		Name:     c.Name,
		Command:  c.Command,
		Passed:   c.Passed,
		Findings: out,
	}
}

// toAuditFindingJSON projects an AuditFinding to its wire shape. The two
// types are field-compatible (only the JSON tags differ) so Go's struct
// conversion does the work — keeping this helper as the single rename point.
func toAuditFindingJSON(f AuditFinding) auditFindingJSON {
	return auditFindingJSON(f)
}
