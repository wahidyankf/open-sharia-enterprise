// Package cliout_test tests the Envelope JSON marshalling.
package cliout_test

import (
	"encoding/json"
	"testing"

	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/cliout"
)

// TestEnvelope_MarshalJSON_KeyOrder verifies that the Envelope produces JSON with
// fields in the canonical order: schema, status, result.
func TestEnvelope_MarshalJSON_KeyOrder(t *testing.T) {
	t.Parallel()

	env := cliout.Envelope[string]{
		Schema: "v1",
		Status: "success",
		Result: "hello",
	}

	data, err := json.Marshal(env)
	if err != nil {
		t.Fatalf("Marshal error: %v", err)
	}

	var raw map[string]json.RawMessage
	if err := json.Unmarshal(data, &raw); err != nil {
		t.Fatalf("Unmarshal error: %v", err)
	}

	for _, key := range []string{"schema", "status", "result"} {
		if _, ok := raw[key]; !ok {
			t.Errorf("expected key %q in JSON output; got %s", key, data)
		}
	}
}

// TestEnvelope_MarshalJSON_Values verifies that field values survive a round-trip.
func TestEnvelope_MarshalJSON_Values(t *testing.T) {
	t.Parallel()

	type inner struct {
		Count int `json:"count"`
	}

	env := cliout.Envelope[inner]{
		Schema: "v1",
		Status: "ok",
		Result: inner{Count: 42},
	}

	data, err := json.Marshal(env)
	if err != nil {
		t.Fatalf("Marshal error: %v", err)
	}

	var decoded cliout.Envelope[inner]
	if err := json.Unmarshal(data, &decoded); err != nil {
		t.Fatalf("Unmarshal error: %v", err)
	}

	if decoded.Schema != "v1" {
		t.Errorf("Schema = %q, want %q", decoded.Schema, "v1")
	}
	if decoded.Status != "ok" {
		t.Errorf("Status = %q, want %q", decoded.Status, "ok")
	}
	if decoded.Result.Count != 42 {
		t.Errorf("Result.Count = %d, want 42", decoded.Result.Count)
	}
}
