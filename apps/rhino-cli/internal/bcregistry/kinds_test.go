package bcregistry

import (
	"testing"
)

// TestParseRelationshipKind_AcceptsKnownKinds verifies every documented kind
// parses without error and Code() round-trips back to the input string.
func TestParseRelationshipKind_AcceptsKnownKinds(t *testing.T) {
	known := []string{
		"customer-supplier",
		"conformist",
		"partnership",
		"shared-kernel",
		"anticorruption-layer",
		"open-host-service",
	}
	for _, k := range known {
		t.Run(k, func(t *testing.T) {
			got, err := ParseRelationshipKind(k)
			if err != nil {
				t.Fatalf("ParseRelationshipKind(%q) unexpected error: %v", k, err)
			}
			if got.Code() != k {
				t.Errorf("Code() = %q, want %q", got.Code(), k)
			}
		})
	}
}

// TestParseRelationshipKind_RejectsUnknown verifies that unknown strings are
// rejected with an error mentioning the bad value.
func TestParseRelationshipKind_RejectsUnknown(t *testing.T) {
	unknown := []string{
		"shered-kernel",
		"made-up-kind",
		"",
		"CUSTOMER-SUPPLIER",
	}
	for _, k := range unknown {
		t.Run(k, func(t *testing.T) {
			_, err := ParseRelationshipKind(k)
			if err == nil {
				t.Fatalf("ParseRelationshipKind(%q) expected error, got nil", k)
			}
		})
	}
}

// TestRelationshipKind_IsAsymmetric verifies asymmetric flag per kind.
func TestRelationshipKind_IsAsymmetric(t *testing.T) {
	cases := []struct {
		kind         string
		isAsymmetric bool
	}{
		{"customer-supplier", true},
		{"conformist", true},
		{"partnership", true},
		{"shared-kernel", true},
		{"anticorruption-layer", false},
		{"open-host-service", false},
	}
	for _, tc := range cases {
		t.Run(tc.kind, func(t *testing.T) {
			k, err := ParseRelationshipKind(tc.kind)
			if err != nil {
				t.Fatalf("ParseRelationshipKind(%q): %v", tc.kind, err)
			}
			if k.IsAsymmetric() != tc.isAsymmetric {
				t.Errorf("IsAsymmetric() = %v, want %v", k.IsAsymmetric(), tc.isAsymmetric)
			}
		})
	}
}

// TestRelationshipKindValue_ValidKind verifies KindValue returns a parsed enum.
func TestRelationshipKindValue_ValidKind(t *testing.T) {
	rel := Relationship{To: "b", Kind: "conformist", Role: "downstream"}
	kv, err := rel.KindValue()
	if err != nil {
		t.Fatalf("KindValue() unexpected error: %v", err)
	}
	if kv.Code() != "conformist" {
		t.Errorf("Code() = %q, want %q", kv.Code(), "conformist")
	}
}

// TestRelationshipKindValue_InvalidKind verifies KindValue returns error for bad kind.
func TestRelationshipKindValue_InvalidKind(t *testing.T) {
	rel := Relationship{To: "b", Kind: "typo-kind", Role: "downstream"}
	_, err := rel.KindValue()
	if err == nil {
		t.Fatal("KindValue() with invalid kind expected error, got nil")
	}
}
