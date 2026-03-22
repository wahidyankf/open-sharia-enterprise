/// Lightweight Gherkin parser for BDD tests in VM mode.
///
/// Reads `.feature` files from disk, parses Feature/Background/Scenario/step
/// lines, and wraps them in flutter_test's [group]/[test] primitives.
///
/// Usage:
/// ```dart
/// describeFeature(
///   '../../../../specs/apps/demo/fe/gherkin/health/health-status.feature',
///   (feature) {
///     feature.scenario('Health indicator shows the service is UP', (scenario) {
///       scenario.when('the user opens the app', () async { /* action */ });
///       scenario.then('the health status indicator should display "UP"', () async {
///         /* assert */
///       });
///     });
///   },
/// );
/// ```
///
/// Background steps are automatically prepended to each scenario.
library;

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';

// ---------------------------------------------------------------------------
// Public API types
// ---------------------------------------------------------------------------

typedef StepFn = Future<void> Function();
typedef ScenarioCallback = void Function(ScenarioBuilder);
typedef FeatureCallback = void Function(FeatureBuilder);

/// Registers a single step (Given/When/Then/And/But).
///
/// The [description] must match the step text in the feature file
/// (case-insensitive, leading/trailing whitespace ignored).
class ScenarioBuilder {
  final List<_Step> _steps = [];

  void given(String description, StepFn fn) =>
      _steps.add(_Step(description, fn));

  void when(String description, StepFn fn) =>
      _steps.add(_Step(description, fn));

  void then(String description, StepFn fn) =>
      _steps.add(_Step(description, fn));

  void and(String description, StepFn fn) =>
      _steps.add(_Step(description, fn));

  void but(String description, StepFn fn) =>
      _steps.add(_Step(description, fn));
}

/// Registers scenarios for a feature file.
class FeatureBuilder {
  final List<_ParsedScenario> _parsed;
  final List<_ParsedStep> _backgroundSteps;

  FeatureBuilder._(this._parsed, this._backgroundSteps);

  /// Registers test implementation for the named scenario.
  ///
  /// Calls [flutter_test]'s [test] via [group].
  void scenario(String title, ScenarioCallback callback) {
    final matched = _parsed.where((s) => _normalize(s.title) == _normalize(title)).toList();
    if (matched.isEmpty) {
      // Register a failing test so the missing scenario is visible in output.
      test(title, () => fail('Scenario not found in feature file: "$title"'));
      return;
    }
    final parsedScenario = matched.first;
    final builder = ScenarioBuilder();
    callback(builder);

    test(parsedScenario.title, () async {
      final allSteps = [..._backgroundSteps, ...parsedScenario.steps];
      for (final featureStep in allSteps) {
        final impl = builder._steps.firstWhere(
          (s) => _normalize(s.description) == _normalize(featureStep.text),
          orElse: () => _Step(
            featureStep.text,
            () async => fail(
              'No step implementation for: "${featureStep.text}"',
            ),
          ),
        );
        await impl.fn();
      }
    });
  }
}

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

/// Parses [featurePath] and creates a [group] containing all scenarios.
///
/// [featurePath] is resolved relative to the calling test file location.
/// Use a relative path like `'../../../../specs/.../health.feature'`.
void describeFeature(String featurePath, FeatureCallback callback) {
  final content = _loadFeature(featurePath);
  final parsed = _parseFeature(content);

  group(parsed.title, () {
    final builder = FeatureBuilder._(parsed.scenarios, parsed.backgroundSteps);
    callback(builder);
  });
}

// ---------------------------------------------------------------------------
// Internal parsing
// ---------------------------------------------------------------------------

class _Step {
  final String description;
  final StepFn fn;

  _Step(this.description, this.fn);
}

class _ParsedStep {
  final String text;

  _ParsedStep(this.text);
}

class _ParsedScenario {
  final String title;
  final List<_ParsedStep> steps;

  _ParsedScenario(this.title, this.steps);
}

class _ParsedFeature {
  final String title;
  final List<_ParsedStep> backgroundSteps;
  final List<_ParsedScenario> scenarios;

  _ParsedFeature(this.title, this.backgroundSteps, this.scenarios);
}

/// Loads the feature file from disk relative to the test/ directory.
String _loadFeature(String featurePath) {
  // Resolve from the workspace root: the test runner cwd is the package dir.
  final file = File(featurePath);
  if (!file.existsSync()) {
    // Try relative to the package root (common when running via `flutter test`).
    final pkgRelative = File(
      '${Directory.current.path}/$featurePath',
    );
    if (pkgRelative.existsSync()) {
      return pkgRelative.readAsStringSync();
    }
    throw StateError(
      'Feature file not found: "$featurePath"\n'
      'Current directory: ${Directory.current.path}',
    );
  }
  return file.readAsStringSync();
}

/// Parses a Gherkin feature file into structured [_ParsedFeature].
_ParsedFeature _parseFeature(String content) {
  final lines = content.split('\n');

  var featureTitle = '';
  final backgroundSteps = <_ParsedStep>[];
  final scenarios = <_ParsedScenario>[];

  var inBackground = false;
  var currentScenarioTitle = '';
  var currentSteps = <_ParsedStep>[];
  var inScenario = false;

  for (final rawLine in lines) {
    final line = rawLine.trim();

    if (line.isEmpty || line.startsWith('#')) {
      continue;
    }

    if (line.startsWith('Feature:')) {
      featureTitle = line.substring('Feature:'.length).trim();
      inBackground = false;
      inScenario = false;
    } else if (line.startsWith('Background:')) {
      inBackground = true;
      inScenario = false;
      if (currentScenarioTitle.isNotEmpty) {
        scenarios.add(_ParsedScenario(currentScenarioTitle, currentSteps));
        currentSteps = [];
        currentScenarioTitle = '';
      }
    } else if (line.startsWith('Scenario:') ||
        line.startsWith('Scenario Outline:')) {
      // Save previous scenario.
      if (inScenario && currentScenarioTitle.isNotEmpty) {
        scenarios.add(_ParsedScenario(currentScenarioTitle, currentSteps));
        currentSteps = [];
      }
      inBackground = false;
      inScenario = true;
      final prefix =
          line.startsWith('Scenario Outline:') ? 'Scenario Outline:' : 'Scenario:';
      currentScenarioTitle = line.substring(prefix.length).trim();
      currentSteps = [];
    } else if (_isStepLine(line)) {
      final stepText = _stripStepKeyword(line);
      if (inBackground) {
        backgroundSteps.add(_ParsedStep(stepText));
      } else if (inScenario) {
        currentSteps.add(_ParsedStep(stepText));
      }
    }
    // Skip: As a / I want / So that / Examples: / table rows / @tags
  }

  // Flush last scenario.
  if (inScenario && currentScenarioTitle.isNotEmpty) {
    scenarios.add(_ParsedScenario(currentScenarioTitle, currentSteps));
  }

  return _ParsedFeature(featureTitle, backgroundSteps, scenarios);
}

const _stepKeywords = [
  'Given ',
  'When ',
  'Then ',
  'And ',
  'But ',
];

bool _isStepLine(String line) =>
    _stepKeywords.any((kw) => line.startsWith(kw));

String _stripStepKeyword(String line) {
  for (final kw in _stepKeywords) {
    if (line.startsWith(kw)) {
      return line.substring(kw.length).trim();
    }
  }
  return line.trim();
}

String _normalize(String s) => s.trim().toLowerCase();
