import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:provider/provider.dart';
import 'package:organiclever_app/providers/hello_provider.dart';
import 'package:organiclever_app/screens/home_screen.dart';

void main() {
  testWidgets('HomeScreen displays Fetch Hello button initially',
      (WidgetTester tester) async {
    await tester.pumpWidget(
      MaterialApp(
        home: ChangeNotifierProvider(
          create: (_) => HelloProvider(),
          child: const HomeScreen(),
        ),
      ),
    );

    expect(find.text('Fetch Hello'), findsOneWidget);
  });

  testWidgets('HomeScreen displays app bar with title',
      (WidgetTester tester) async {
    await tester.pumpWidget(
      MaterialApp(
        home: ChangeNotifierProvider(
          create: (_) => HelloProvider(),
          child: const HomeScreen(),
        ),
      ),
    );

    expect(find.text('OrganicLever'), findsOneWidget);
    expect(find.byType(AppBar), findsOneWidget);
  });
}
