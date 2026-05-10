package mermaid

import "testing"

// TestDirectionSealedInterface covers all Direction variants and their methods.
// Marker methods (isDirection) are unexported but callable within the package.
func TestDirectionSealedInterface(t *testing.T) {
	dirs := []Direction{DirectionTB{}, DirectionTD{}, DirectionBT{}, DirectionLR{}, DirectionRL{}}
	wantCodes := []string{"TB", "TD", "BT", "LR", "RL"}
	for i, d := range dirs {
		d.isDirection()
		if d.Code() != wantCodes[i] {
			t.Errorf("Direction[%d].Code()=%q, want %q", i, d.Code(), wantCodes[i])
		}
		if d.String() != wantCodes[i] {
			t.Errorf("Direction[%d].String()=%q, want %q", i, d.String(), wantCodes[i])
		}
	}
}

// TestViolationKindSealedInterface covers all ViolationKind variants.
func TestViolationKindSealedInterface(t *testing.T) {
	kinds := []ViolationKind{ViolationLabelTooLong{}, ViolationWidthExceeded{}, ViolationMultipleDiagrams{}}
	wantCodes := []string{"label_too_long", "width_exceeded", "multiple_diagrams"}
	for i, k := range kinds {
		k.isViolationKind()
		if k.Code() != wantCodes[i] {
			t.Errorf("ViolationKind[%d].Code()=%q, want %q", i, k.Code(), wantCodes[i])
		}
		if k.String() != wantCodes[i] {
			t.Errorf("ViolationKind[%d].String()=%q, want %q", i, k.String(), wantCodes[i])
		}
	}
}

// TestWarningKindSealedInterface covers all WarningKind variants.
func TestWarningKindSealedInterface(t *testing.T) {
	kinds := []WarningKind{WarningComplexDiagram{}, WarningSubgraphDense{}}
	wantCodes := []string{"complex_diagram", "subgraph_density"}
	for i, k := range kinds {
		k.isWarningKind()
		if k.Code() != wantCodes[i] {
			t.Errorf("WarningKind[%d].Code()=%q, want %q", i, k.Code(), wantCodes[i])
		}
		if k.String() != wantCodes[i] {
			t.Errorf("WarningKind[%d].String()=%q, want %q", i, k.String(), wantCodes[i])
		}
	}
}
