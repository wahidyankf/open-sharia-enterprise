import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:demo_fe_dart_flutter/core/router/app_router.dart';

void main() {
  // Create the root ProviderContainer so the router can read providers
  // (specifically isAuthenticatedProvider) before the widget tree exists.
  final container = ProviderContainer();
  appRouter = createAppRouter(container);

  runApp(
    UncontrolledProviderScope(container: container, child: const DemoFeApp()),
  );
}

class DemoFeApp extends StatelessWidget {
  const DemoFeApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp.router(
      title: 'Demo Frontend',
      theme: ThemeData(colorSchemeSeed: Colors.blue, useMaterial3: true),
      routerConfig: appRouter,
    );
  }
}
