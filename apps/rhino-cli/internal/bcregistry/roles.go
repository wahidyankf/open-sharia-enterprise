package bcregistry

// Closed-set enumeration of relationship roles found in
// specs/apps/*/ddd/bounded-contexts.yaml (audited 2026-05-11).
//
// Roles observed in specs:
//   - customer
//   - downstream
//   - supplier
//   - upstream

import "fmt"

//sumtype:decl

// RelationshipRole is a sealed interface representing all valid
// bounded-context relationship roles.
type RelationshipRole interface {
	isRelationshipRole()
	// Code returns the YAML wire string for this role.
	Code() string
	// String returns the human-readable name.
	String() string
}

// RelationshipRoleCustomer represents the customer role.
type RelationshipRoleCustomer struct{}

func (RelationshipRoleCustomer) isRelationshipRole() {}

// Code implements RelationshipRole.
func (RelationshipRoleCustomer) Code() string { return "customer" }

// String implements RelationshipRole.
func (RelationshipRoleCustomer) String() string { return "customer" }

// RelationshipRoleSupplier represents the supplier role.
type RelationshipRoleSupplier struct{}

func (RelationshipRoleSupplier) isRelationshipRole() {}

// Code implements RelationshipRole.
func (RelationshipRoleSupplier) Code() string { return "supplier" }

// String implements RelationshipRole.
func (RelationshipRoleSupplier) String() string { return "supplier" }

// RelationshipRoleUpstream represents the upstream role.
type RelationshipRoleUpstream struct{}

func (RelationshipRoleUpstream) isRelationshipRole() {}

// Code implements RelationshipRole.
func (RelationshipRoleUpstream) Code() string { return "upstream" }

// String implements RelationshipRole.
func (RelationshipRoleUpstream) String() string { return "upstream" }

// RelationshipRoleDownstream represents the downstream role.
type RelationshipRoleDownstream struct{}

func (RelationshipRoleDownstream) isRelationshipRole() {}

// Code implements RelationshipRole.
func (RelationshipRoleDownstream) Code() string { return "downstream" }

// String implements RelationshipRole.
func (RelationshipRoleDownstream) String() string { return "downstream" }

// ParseRelationshipRole parses a YAML wire string into a RelationshipRole.
// Returns an error for unknown roles.
func ParseRelationshipRole(s string) (RelationshipRole, error) {
	switch s {
	case "customer":
		return RelationshipRoleCustomer{}, nil
	case "supplier":
		return RelationshipRoleSupplier{}, nil
	case "upstream":
		return RelationshipRoleUpstream{}, nil
	case "downstream":
		return RelationshipRoleDownstream{}, nil
	default:
		return nil, fmt.Errorf("unknown relationship role %q: must be one of customer, supplier, upstream, downstream", s)
	}
}
