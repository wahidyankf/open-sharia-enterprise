package navigation

import (
	"os"
	"path/filepath"
	"testing"
)

func createTestFile(t *testing.T, path string, content string) {
	t.Helper()
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("Failed to create directory %s: %v", dir, err)
	}
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatalf("Failed to create file %s: %v", path, err)
	}
}

func TestScanDirectory_SimpleStructure(t *testing.T) {
	tmpDir := t.TempDir()

	// Create structure:
	// tmpDir/
	//   _index.md
	//   overview.md (weight: 1)
	//   installation.md (weight: 2)

	createTestFile(t, filepath.Join(tmpDir, "_index.md"), `---
title: Root
weight: 100
---`)

	createTestFile(t, filepath.Join(tmpDir, "overview.md"), `---
title: Overview
weight: 1
---`)

	createTestFile(t, filepath.Join(tmpDir, "installation.md"), `---
title: Installation
weight: 2
---`)

	items, err := ScanDirectory(tmpDir, "/test", 1, 2)
	if err != nil {
		t.Fatalf("ScanDirectory failed: %v", err)
	}

	if len(items) != 2 {
		t.Fatalf("Expected 2 items, got %d", len(items))
	}

	// Verify sorting by weight
	if items[0].Title != "Overview" {
		t.Errorf("First item should be Overview, got %s", items[0].Title)
	}
	if items[1].Title != "Installation" {
		t.Errorf("Second item should be Installation, got %s", items[1].Title)
	}

	// Verify paths
	if items[0].Path != "/test/overview" {
		t.Errorf("Overview path should be '/test/overview', got %s", items[0].Path)
	}
	if items[1].Path != "/test/installation" {
		t.Errorf("Installation path should be '/test/installation', got %s", items[1].Path)
	}
}

func TestScanDirectory_WithSubdirectories(t *testing.T) {
	tmpDir := t.TempDir()

	// Create structure:
	// tmpDir/
	//   overview.md (weight: 1)
	//   tutorials/
	//     _index.md (weight: 2)
	//     beginner.md (weight: 1)
	//     advanced.md (weight: 2)

	createTestFile(t, filepath.Join(tmpDir, "overview.md"), `---
title: Overview
weight: 1
---`)

	createTestFile(t, filepath.Join(tmpDir, "tutorials/_index.md"), `---
title: Tutorials
weight: 2
---`)

	createTestFile(t, filepath.Join(tmpDir, "tutorials/beginner.md"), `---
title: Beginner
weight: 1
---`)

	createTestFile(t, filepath.Join(tmpDir, "tutorials/advanced.md"), `---
title: Advanced
weight: 2
---`)

	items, err := ScanDirectory(tmpDir, "/test", 1, 2)
	if err != nil {
		t.Fatalf("ScanDirectory failed: %v", err)
	}

	if len(items) != 2 {
		t.Fatalf("Expected 2 items, got %d", len(items))
	}

	// First should be overview.md
	if items[0].Title != "Overview" || items[0].IsDir {
		t.Error("First item should be Overview file")
	}

	// Second should be tutorials directory
	if items[1].Title != "Tutorials" || !items[1].IsDir {
		t.Error("Second item should be Tutorials directory")
	}

	// Check children of tutorials
	if len(items[1].Children) != 2 {
		t.Fatalf("Tutorials should have 2 children, got %d", len(items[1].Children))
	}

	if items[1].Children[0].Title != "Beginner" {
		t.Errorf("First child should be Beginner, got %s", items[1].Children[0].Title)
	}
	if items[1].Children[1].Title != "Advanced" {
		t.Errorf("Second child should be Advanced, got %s", items[1].Children[1].Title)
	}
}

func TestScanDirectory_TwoLayers(t *testing.T) {
	tmpDir := t.TempDir()

	// Create structure with 2 layers (layer 3 should not be scanned):
	// tmpDir/
	//   learn/
	//     _index.md (weight: 1)
	//     programming/
	//       _index.md (weight: 1)
	//       python/
	//         _index.md (weight: 1) - should NOT be scanned

	createTestFile(t, filepath.Join(tmpDir, "learn/_index.md"), `---
title: Learn
weight: 1
---`)

	createTestFile(t, filepath.Join(tmpDir, "learn/programming/_index.md"), `---
title: Programming
weight: 1
---`)

	createTestFile(t, filepath.Join(tmpDir, "learn/programming/python/_index.md"), `---
title: Python
weight: 1
---`)

	items, err := ScanDirectory(tmpDir, "/test", 1, 2)
	if err != nil {
		t.Fatalf("ScanDirectory failed: %v", err)
	}

	// Should have 1 item (learn)
	if len(items) != 1 {
		t.Fatalf("Expected 1 item, got %d", len(items))
	}

	// Learn should have 1 child (programming)
	if len(items[0].Children) != 1 {
		t.Fatalf("Learn should have 1 child, got %d", len(items[0].Children))
	}

	// Programming should have NO children (layer 3 not scanned with maxLayers=2)
	if len(items[0].Children[0].Children) != 0 {
		t.Fatalf("Programming should have no children (layer 3 not scanned), got %d", len(items[0].Children[0].Children))
	}
}

func TestScanDirectory_MaxDepthLimit(t *testing.T) {
	tmpDir := t.TempDir()

	// Create structure deeper than maxLayers:
	// tmpDir/
	//   l1/
	//     _index.md
	//     l2/
	//       _index.md
	//       l3/
	//         _index.md (should not be scanned with maxLayers=2)

	createTestFile(t, filepath.Join(tmpDir, "l1/_index.md"), `---
title: Layer 1
weight: 1
---`)

	createTestFile(t, filepath.Join(tmpDir, "l1/l2/_index.md"), `---
title: Layer 2
weight: 1
---`)

	createTestFile(t, filepath.Join(tmpDir, "l1/l2/l3/_index.md"), `---
title: Layer 3
weight: 1
---`)

	// Scan with maxLayers = 2
	items, err := ScanDirectory(tmpDir, "/test", 1, 2)
	if err != nil {
		t.Fatalf("ScanDirectory failed: %v", err)
	}

	// Navigate to layer 2
	l2Items := items[0].Children

	// Layer 2 should exist and have 1 item (l2)
	if len(l2Items) != 1 {
		t.Fatalf("Expected 1 item at layer 2, got %d", len(l2Items))
	}

	// Layer 2 item should NOT have children (layer 3 should not be scanned)
	if len(l2Items[0].Children) != 0 {
		t.Errorf("Layer 2 item should have no children (layer 3 not scanned), got %d children", len(l2Items[0].Children))
	}
}

func TestScanDirectory_MissingIndexSkipsDirectory(t *testing.T) {
	tmpDir := t.TempDir()

	// Create structure:
	// tmpDir/
	//   valid/
	//     _index.md
	//   invalid/
	//     (no _index.md)

	createTestFile(t, filepath.Join(tmpDir, "valid/_index.md"), `---
title: Valid
weight: 1
---`)

	// Create directory without _index.md
	if err := os.MkdirAll(filepath.Join(tmpDir, "invalid"), 0755); err != nil {
		t.Fatalf("Failed to create directory: %v", err)
	}

	items, err := ScanDirectory(tmpDir, "/test", 1, 2)
	if err != nil {
		t.Fatalf("ScanDirectory failed: %v", err)
	}

	// Should only have 1 item (valid)
	if len(items) != 1 {
		t.Fatalf("Expected 1 item, got %d", len(items))
	}

	if items[0].Title != "Valid" {
		t.Errorf("Expected Valid, got %s", items[0].Title)
	}
}

func TestScanDirectory_HiddenFilesIgnored(t *testing.T) {
	tmpDir := t.TempDir()

	// Create structure with hidden files:
	// tmpDir/
	//   visible.md
	//   .hidden.md

	createTestFile(t, filepath.Join(tmpDir, "visible.md"), `---
title: Visible
weight: 1
---`)

	createTestFile(t, filepath.Join(tmpDir, ".hidden.md"), `---
title: Hidden
weight: 2
---`)

	items, err := ScanDirectory(tmpDir, "/test", 1, 2)
	if err != nil {
		t.Fatalf("ScanDirectory failed: %v", err)
	}

	// Should only have 1 item (visible)
	if len(items) != 1 {
		t.Fatalf("Expected 1 item, got %d", len(items))
	}

	if items[0].Title != "Visible" {
		t.Errorf("Expected Visible, got %s", items[0].Title)
	}
}

func TestScanDirectory_WeightSorting(t *testing.T) {
	tmpDir := t.TempDir()

	// Create files with different weights
	createTestFile(t, filepath.Join(tmpDir, "high.md"), `---
title: High Weight
weight: 100
---`)

	createTestFile(t, filepath.Join(tmpDir, "low.md"), `---
title: Low Weight
weight: 10
---`)

	createTestFile(t, filepath.Join(tmpDir, "medium.md"), `---
title: Medium Weight
weight: 50
---`)

	items, err := ScanDirectory(tmpDir, "/test", 1, 2)
	if err != nil {
		t.Fatalf("ScanDirectory failed: %v", err)
	}

	if len(items) != 3 {
		t.Fatalf("Expected 3 items, got %d", len(items))
	}

	// Should be sorted by weight (ascending)
	if items[0].Title != "Low Weight" {
		t.Errorf("First item should be Low Weight, got %s", items[0].Title)
	}
	if items[1].Title != "Medium Weight" {
		t.Errorf("Second item should be Medium Weight, got %s", items[1].Title)
	}
	if items[2].Title != "High Weight" {
		t.Errorf("Third item should be High Weight, got %s", items[2].Title)
	}
}

func TestScanDirectory_MissingWeight(t *testing.T) {
	tmpDir := t.TempDir()

	// Create file without weight
	createTestFile(t, filepath.Join(tmpDir, "no-weight.md"), `---
title: No Weight
---`)

	createTestFile(t, filepath.Join(tmpDir, "with-weight.md"), `---
title: With Weight
weight: 10
---`)

	items, err := ScanDirectory(tmpDir, "/test", 1, 2)
	if err != nil {
		t.Fatalf("ScanDirectory failed: %v", err)
	}

	if len(items) != 2 {
		t.Fatalf("Expected 2 items, got %d", len(items))
	}

	// File with weight should come first
	if items[0].Title != "With Weight" {
		t.Errorf("First item should be With Weight, got %s", items[0].Title)
	}

	// File without weight should have default weight 999999
	if items[1].Weight != 999999 {
		t.Errorf("Missing weight should default to 999999, got %d", items[1].Weight)
	}
}
