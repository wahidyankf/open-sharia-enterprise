package cliout

import "encoding/json"

// Envelope is a generic JSON wrapper that enforces a canonical key ordering of
// schema → status → result. It is provided for future use by commands that need
// a structured envelope; existing commands use their own per-command JSON structs.
type Envelope[T any] struct {
	// Schema is the schema version string (e.g., "v1").
	Schema string
	// Status is the machine-readable status string (e.g., "success", "failure").
	Status string
	// Result holds the command-specific payload.
	Result T
}

// envelopeJSON is the internal JSON representation with explicit field ordering.
type envelopeJSON[T any] struct {
	Schema string `json:"schema"`
	Status string `json:"status"`
	Result T      `json:"result"`
}

// MarshalJSON produces JSON with canonical key order: schema, status, result.
func (e Envelope[T]) MarshalJSON() ([]byte, error) {
	return json.Marshal(envelopeJSON[T](e))
}

// UnmarshalJSON populates the Envelope from JSON.
func (e *Envelope[T]) UnmarshalJSON(data []byte) error {
	var raw envelopeJSON[T]
	if err := json.Unmarshal(data, &raw); err != nil {
		return err
	}
	e.Schema = raw.Schema
	e.Status = raw.Status
	e.Result = raw.Result
	return nil
}
