package contracts

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

var (
	dartWriteFile = os.WriteFile
	dartMkdirAll  = os.MkdirAll
	dartGlob      = filepath.Glob
)

const pubspecContent = `name: demo_contracts
publish_to: "none"
version: 1.0.0
environment:
  sdk: ^3.11.1
dependencies:
  collection: ^1.18.0
`

const barrelHeader = `// AUTO-GENERATED — do not edit. Recreated by rhino-cli contracts dart-scaffold.
// @dart=2.18
// ignore_for_file: type=lint
library openapi.api;

import 'package:collection/collection.dart';
`

const barrelUtils = `
const _deepEquality = DeepCollectionEquality();
final _dateFormatter = _DateFormatter();

class _DateFormatter {
  String format(DateTime dt) =>
      '${dt.year.toString().padLeft(4, '0')}'
      '-${dt.month.toString().padLeft(2, '0')}'
      '-${dt.day.toString().padLeft(2, '0')}';
}

T? mapValueOfType<T>(Map<String, dynamic> map, String key) {
  final v = map[key];
  return v is T ? v : null;
}

DateTime? mapDateTime(Map<String, dynamic> map, String key, String? f) {
  final v = map[key];
  return v is String && v.isNotEmpty ? DateTime.tryParse(v) : null;
}

Map<K, V>? mapCastOfType<K, V>(Map<String, dynamic> map, String key) {
  final v = map[key];
  return v is Map ? v.cast<K, V>() : null;
}
`

// ScaffoldDart creates pubspec.yaml and a barrel library for the Dart generated-contracts package.
// It writes pubspec.yaml, creates the lib directory, and builds a barrel file with part directives
// for each .dart model file found in lib/model/.
func ScaffoldDart(opts DartScaffoldOptions) (*DartScaffoldResult, error) {
	result := &DartScaffoldResult{
		ModelFiles: []string{},
	}

	// Step 1: Write pubspec.yaml.
	pubspecPath := filepath.Join(opts.Dir, "pubspec.yaml")
	if err := dartWriteFile(pubspecPath, []byte(pubspecContent), 0644); err != nil {
		return nil, fmt.Errorf("writing pubspec.yaml: %w", err)
	}
	result.PubspecCreated = true

	// Step 2: Ensure lib directory exists.
	libDir := filepath.Join(opts.Dir, "lib")
	if err := dartMkdirAll(libDir, 0755); err != nil {
		return nil, fmt.Errorf("creating lib directory: %w", err)
	}

	// Step 3: Glob model files.
	modelPattern := filepath.Join(opts.Dir, "lib", "model", "*.dart")
	matches, err := dartGlob(modelPattern)
	if err != nil {
		return nil, fmt.Errorf("globbing model files: %w", err)
	}

	basenames := make([]string, 0, len(matches))
	for _, m := range matches {
		basenames = append(basenames, filepath.Base(m))
	}
	sort.Strings(basenames)
	result.ModelFiles = basenames

	// Step 4: Build barrel file content.
	var sb strings.Builder
	sb.WriteString(barrelHeader)

	for _, base := range basenames {
		_, _ = fmt.Fprintf(&sb, "part 'model/%s';\n", base)
	}

	sb.WriteString(barrelUtils)

	// Step 5: Write barrel file.
	barrelPath := filepath.Join(opts.Dir, "lib", "demo_contracts.dart")
	if err := dartWriteFile(barrelPath, []byte(sb.String()), 0644); err != nil {
		return nil, fmt.Errorf("writing barrel library: %w", err)
	}
	result.BarrelCreated = true

	return result, nil
}
