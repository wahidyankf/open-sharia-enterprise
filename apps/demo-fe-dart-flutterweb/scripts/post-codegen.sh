#!/bin/sh
# Post-codegen scaffolding for Dart generated-contracts package.
# Creates pubspec.yaml and barrel library file needed to make the
# generated model parts importable as a Dart package.

DIR="$(cd "$(dirname "$0")/.." && pwd)/generated-contracts"

# Create pubspec.yaml
cat > "$DIR/pubspec.yaml" << 'PUBSPEC'
name: demo_contracts
publish_to: "none"
version: 1.0.0
environment:
  sdk: ^3.11.1
dependencies:
  collection: ^1.18.0
PUBSPEC

# Create barrel library file
mkdir -p "$DIR/lib"
cat > "$DIR/lib/demo_contracts.dart" << 'BARREL'
// AUTO-GENERATED — do not edit. Recreated by post-codegen.sh.
// @dart=2.18
// ignore_for_file: type=lint
library openapi.api;

import 'package:collection/collection.dart';
BARREL

# Add part directives for all generated model files
for f in "$DIR"/lib/model/*.dart; do
  [ -f "$f" ] || continue
  echo "part 'model/$(basename "$f")';" >> "$DIR/lib/demo_contracts.dart"
done

# Add utility functions needed by generated model parts
cat >> "$DIR/lib/demo_contracts.dart" << 'UTILS'

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
UTILS
