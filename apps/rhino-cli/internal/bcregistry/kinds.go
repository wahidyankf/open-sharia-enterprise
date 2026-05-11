// Package bcregistry loads and validates the DDD bounded-context registry.
package bcregistry

import "fmt"

// Closed-set enumeration of relationship kinds found in
// specs/apps/*/ddd/bounded-contexts.yaml (audited 2026-05-11).
//
// Kinds observed in specs:
//   - conformist
//   - customer-supplier
//
// Kinds defined by validator (Fix #10) but not yet in specs:
//   - anticorruption-layer
//   - open-host-service
//   - partnership
//   - shared-kernel
//
// All six are encoded here so the parser accepts every kind the
// validator permits; new kinds added to the validator must also be
// added here.

//sumtype:decl

// RelationshipKind is a sealed interface representing all valid
// bounded-context relationship kinds.
type RelationshipKind interface {
	isRelationshipKind()
	// Code returns the YAML wire string for this kind.
	Code() string
	// IsAsymmetric reports whether reciprocal validation is required.
	IsAsymmetric() bool
	// String returns the human-readable name.
	String() string
}

// RelationshipKindCustomerSupplier represents the customer-supplier pattern.
type RelationshipKindCustomerSupplier struct{}

func (RelationshipKindCustomerSupplier) isRelationshipKind() {}

// Code implements RelationshipKind.
func (RelationshipKindCustomerSupplier) Code() string { return "customer-supplier" }

// IsAsymmetric implements RelationshipKind.
func (RelationshipKindCustomerSupplier) IsAsymmetric() bool { return true }

// String implements RelationshipKind.
func (RelationshipKindCustomerSupplier) String() string { return "customer-supplier" }

// RelationshipKindConformist represents the conformist pattern.
type RelationshipKindConformist struct{}

func (RelationshipKindConformist) isRelationshipKind() {}

// Code implements RelationshipKind.
func (RelationshipKindConformist) Code() string { return "conformist" }

// IsAsymmetric implements RelationshipKind.
func (RelationshipKindConformist) IsAsymmetric() bool { return true }

// String implements RelationshipKind.
func (RelationshipKindConformist) String() string { return "conformist" }

// RelationshipKindPartnership represents the partnership pattern.
type RelationshipKindPartnership struct{}

func (RelationshipKindPartnership) isRelationshipKind() {}

// Code implements RelationshipKind.
func (RelationshipKindPartnership) Code() string { return "partnership" }

// IsAsymmetric implements RelationshipKind.
func (RelationshipKindPartnership) IsAsymmetric() bool { return true }

// String implements RelationshipKind.
func (RelationshipKindPartnership) String() string { return "partnership" }

// RelationshipKindSharedKernel represents the shared-kernel pattern.
type RelationshipKindSharedKernel struct{}

func (RelationshipKindSharedKernel) isRelationshipKind() {}

// Code implements RelationshipKind.
func (RelationshipKindSharedKernel) Code() string { return "shared-kernel" }

// IsAsymmetric implements RelationshipKind.
func (RelationshipKindSharedKernel) IsAsymmetric() bool { return true }

// String implements RelationshipKind.
func (RelationshipKindSharedKernel) String() string { return "shared-kernel" }

// RelationshipKindAnticorruptionLayer represents the anti-corruption layer pattern.
type RelationshipKindAnticorruptionLayer struct{}

func (RelationshipKindAnticorruptionLayer) isRelationshipKind() {}

// Code implements RelationshipKind.
func (RelationshipKindAnticorruptionLayer) Code() string { return "anticorruption-layer" }

// IsAsymmetric implements RelationshipKind.
func (RelationshipKindAnticorruptionLayer) IsAsymmetric() bool { return false }

// String implements RelationshipKind.
func (RelationshipKindAnticorruptionLayer) String() string { return "anticorruption-layer" }

// RelationshipKindOpenHostService represents the open-host-service pattern.
type RelationshipKindOpenHostService struct{}

func (RelationshipKindOpenHostService) isRelationshipKind() {}

// Code implements RelationshipKind.
func (RelationshipKindOpenHostService) Code() string { return "open-host-service" }

// IsAsymmetric implements RelationshipKind.
func (RelationshipKindOpenHostService) IsAsymmetric() bool { return false }

// String implements RelationshipKind.
func (RelationshipKindOpenHostService) String() string { return "open-host-service" }

// ParseRelationshipKind parses a YAML wire string into a RelationshipKind.
// Returns an error for unknown kinds.
func ParseRelationshipKind(s string) (RelationshipKind, error) {
	switch s {
	case "customer-supplier":
		return RelationshipKindCustomerSupplier{}, nil
	case "conformist":
		return RelationshipKindConformist{}, nil
	case "partnership":
		return RelationshipKindPartnership{}, nil
	case "shared-kernel":
		return RelationshipKindSharedKernel{}, nil
	case "anticorruption-layer":
		return RelationshipKindAnticorruptionLayer{}, nil
	case "open-host-service":
		return RelationshipKindOpenHostService{}, nil
	default:
		return nil, fmt.Errorf("unknown relationship kind %q: must be one of customer-supplier, conformist, partnership, shared-kernel, anticorruption-layer, open-host-service", s)
	}
}
