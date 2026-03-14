// Test helper utilities for BDD widget tests
import 'package:flutter_test/flutter_test.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:demo_fe_dart_flutter/main.dart';

Future<void> pumpApp(WidgetTester tester) async {
  await tester.pumpWidget(
    const ProviderScope(child: DemoFeApp()),
  );
  await tester.pumpAndSettle();
}
