// Package mermaid provides structural validation for Mermaid flowchart diagrams
// embedded in markdown files. It enforces three rules: label length, parallel rank
// width, and single-diagram-per-block. Non-flowchart diagram types are silently ignored.
package mermaid

// Direction is a sealed interface for the layout direction of a Mermaid flowchart.
//
//sumtype:decl
type Direction interface {
	isDirection()
	Code() string
	String() string
}

// DirectionTB represents top-to-bottom flow.
type DirectionTB struct{}

func (DirectionTB) isDirection() {}

// Code implements Direction.
func (DirectionTB) Code() string   { return "TB" }
func (DirectionTB) String() string { return "TB" }

// DirectionTD is an alias for top-to-bottom (same as TB).
type DirectionTD struct{}

func (DirectionTD) isDirection() {}

// Code implements Direction.
func (DirectionTD) Code() string   { return "TD" }
func (DirectionTD) String() string { return "TD" }

// DirectionBT represents bottom-to-top flow.
type DirectionBT struct{}

func (DirectionBT) isDirection() {}

// Code implements Direction.
func (DirectionBT) Code() string   { return "BT" }
func (DirectionBT) String() string { return "BT" }

// DirectionLR represents left-to-right flow.
type DirectionLR struct{}

func (DirectionLR) isDirection() {}

// Code implements Direction.
func (DirectionLR) Code() string   { return "LR" }
func (DirectionLR) String() string { return "LR" }

// DirectionRL represents right-to-left flow.
type DirectionRL struct{}

func (DirectionRL) isDirection() {}

// Code implements Direction.
func (DirectionRL) Code() string   { return "RL" }
func (DirectionRL) String() string { return "RL" }

// ParseDirection converts a string to a Direction variant.
// Returns DirectionTB{} as default for unknown values.
func ParseDirection(s string) Direction {
	switch s {
	case "TD":
		return DirectionTD{}
	case "BT":
		return DirectionBT{}
	case "LR":
		return DirectionLR{}
	case "RL":
		return DirectionRL{}
	default:
		return DirectionTB{} // TB is the default
	}
}

// ViolationKind is a sealed interface identifying the category of a rule violation.
//
//sumtype:decl
type ViolationKind interface {
	isViolationKind()
	Code() string
	String() string
}

// ViolationLabelTooLong indicates a node label exceeds the maximum length.
type ViolationLabelTooLong struct{}

func (ViolationLabelTooLong) isViolationKind() {}

// Code implements ViolationKind.
func (ViolationLabelTooLong) Code() string   { return "label_too_long" }
func (ViolationLabelTooLong) String() string { return "label_too_long" }

// ViolationWidthExceeded indicates parallel rank width exceeds the maximum.
type ViolationWidthExceeded struct{}

func (ViolationWidthExceeded) isViolationKind() {}

// Code implements ViolationKind.
func (ViolationWidthExceeded) Code() string   { return "width_exceeded" }
func (ViolationWidthExceeded) String() string { return "width_exceeded" }

// ViolationMultipleDiagrams indicates a block contains multiple flowchart headers.
type ViolationMultipleDiagrams struct{}

func (ViolationMultipleDiagrams) isViolationKind() {}

// Code implements ViolationKind.
func (ViolationMultipleDiagrams) Code() string   { return "multiple_diagrams" }
func (ViolationMultipleDiagrams) String() string { return "multiple_diagrams" }

// WarningKind is a sealed interface identifying the category of a warning.
//
//sumtype:decl
type WarningKind interface {
	isWarningKind()
	Code() string
	String() string
}

// WarningComplexDiagram indicates a complex diagram (both span and depth exceeded).
type WarningComplexDiagram struct{}

func (WarningComplexDiagram) isWarningKind() {}

// Code implements WarningKind.
func (WarningComplexDiagram) Code() string   { return "complex_diagram" }
func (WarningComplexDiagram) String() string { return "complex_diagram" }

// WarningSubgraphDense indicates a dense subgraph with too many direct children.
type WarningSubgraphDense struct{}

func (WarningSubgraphDense) isWarningKind() {}

// Code implements WarningKind.
func (WarningSubgraphDense) Code() string   { return "subgraph_density" }
func (WarningSubgraphDense) String() string { return "subgraph_density" }

// MermaidBlock holds the raw source of a single ```mermaid fenced code block.
type MermaidBlock struct {
	FilePath   string
	BlockIndex int
	Source     string
	StartLine  int
}

// Node is a flowchart node with an ID and optional label.
type Node struct {
	ID    string
	Label string
}

// Edge is a directed connection between two nodes.
type Edge struct {
	From string
	To   string
}

// Subgraph is a Mermaid `subgraph ... end` block. NodeIDs holds direct children
// only (not transitive). StartLine is 1-indexed within the parent block.
type Subgraph struct {
	ID        string
	Label     string
	NodeIDs   []string
	StartLine int
}

// ParsedDiagram is the result of parsing a single MermaidBlock.
type ParsedDiagram struct {
	Block     MermaidBlock
	Direction Direction
	Nodes     []Node
	Edges     []Edge
	Subgraphs []Subgraph
}

// Warning is non-blocking (exit 0). Emitted by complex-diagram and
// subgraph-density rules.
type Warning struct {
	Kind       WarningKind
	FilePath   string
	BlockIndex int
	StartLine  int

	// complex_diagram fields
	ActualWidth int
	ActualDepth int
	MaxWidth    int
	MaxDepth    int

	// subgraph_density fields
	SubgraphLabel     string
	SubgraphNodeCount int
	MaxSubgraphNodes  int
}

// Violation is a rule violation that causes a non-zero exit.
type Violation struct {
	Kind        ViolationKind
	FilePath    string
	BlockIndex  int
	StartLine   int
	NodeID      string
	LabelText   string
	LabelLen    int
	MaxLabelLen int
	ActualWidth int
	MaxWidth    int
}

// ValidationResult aggregates all findings from a validation run.
type ValidationResult struct {
	FilesScanned  int
	BlocksScanned int
	Violations    []Violation
	Warnings      []Warning
}
