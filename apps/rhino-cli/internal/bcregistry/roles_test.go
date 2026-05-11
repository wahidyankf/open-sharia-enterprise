package bcregistry

import (
	"testing"
)

// TestParseRelationshipRole_AcceptsKnownRoles verifies every documented role
// parses without error and Code() round-trips back to the input string.
func TestParseRelationshipRole_AcceptsKnownRoles(t *testing.T) {
	known := []string{
		"customer",
		"supplier",
		"upstream",
		"downstream",
	}
	for _, r := range known {
		r := r
		t.Run(r, func(t *testing.T) {
			got, err := ParseRelationshipRole(r)
			if err != nil {
				t.Fatalf("ParseRelationshipRole(%q) unexpected error: %v", r, err)
			}
			if got.Code() != r {
				t.Errorf("Code() = %q, want %q", got.Code(), r)
			}
		})
	}
}

// TestParseRelationshipRole_RejectsUnknown verifies that unknown strings are
// rejected with an error.
func TestParseRelationshipRole_RejectsUnknown(t *testing.T) {
	unknown := []string{
		"peer",
		"vendor",
		"",
		"CUSTOMER",
	}
	for _, r := range unknown {
		r := r
		t.Run(r, func(t *testing.T) {
			_, err := ParseRelationshipRole(r)
			if err == nil {
				t.Fatalf("ParseRelationshipRole(%q) expected error, got nil", r)
			}
		})
	}
}

// TestRelationshipRoleValue_ValidRole verifies RoleValue returns a parsed enum.
func TestRelationshipRoleValue_ValidRole(t *testing.T) {
	rel := Relationship{To: "b", Kind: "conformist", Role: "downstream"}
	rv, err := rel.RoleValue()
	if err != nil {
		t.Fatalf("RoleValue() unexpected error: %v", err)
	}
	if rv.Code() != "downstream" {
		t.Errorf("Code() = %q, want %q", rv.Code(), "downstream")
	}
}

// TestRelationshipRoleValue_InvalidRole verifies RoleValue returns error for bad role.
func TestRelationshipRoleValue_InvalidRole(t *testing.T) {
	rel := Relationship{To: "b", Kind: "conformist", Role: "peer"}
	_, err := rel.RoleValue()
	if err == nil {
		t.Fatal("RoleValue() with invalid role expected error, got nil")
	}
}
