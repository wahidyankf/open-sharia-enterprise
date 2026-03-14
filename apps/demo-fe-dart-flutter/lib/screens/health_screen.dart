/// Health status screen — displays backend availability.
///
/// Fetches GET /health (via direct Dio call since there is no health provider)
/// and renders a green "UP" card or a red error card. A link to the login page
/// is always visible at the bottom.
library;

import 'package:dio/dio.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:demo_fe_dart_flutter/core/api/api_client.dart';
import 'package:demo_fe_dart_flutter/core/models/models.dart';

// ---------------------------------------------------------------------------
// Provider
// ---------------------------------------------------------------------------

final _healthProvider = FutureProvider<HealthResponse>((ref) async {
  final response = await dio.get<Map<String, dynamic>>('/health');
  return HealthResponse.fromJson(response.data!);
});

// ---------------------------------------------------------------------------
// Screen widget
// ---------------------------------------------------------------------------

/// Displays the backend health status.
class HealthScreen extends ConsumerWidget {
  const HealthScreen({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final healthAsync = ref.watch(_healthProvider);

    return Scaffold(
      appBar: AppBar(title: const Text('System Health')),
      body: Padding(
        padding: const EdgeInsets.all(24),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.stretch,
          children: [
            healthAsync.when(
              loading: () => const _LoadingCard(),
              error: (error, _) => _ErrorCard(error: error),
              data: (health) => _HealthCard(health: health),
            ),
            const SizedBox(height: 24),
            _RefreshButton(onPressed: () => ref.invalidate(_healthProvider)),
            const SizedBox(height: 16),
            _LoginLink(),
          ],
        ),
      ),
    );
  }
}

// ---------------------------------------------------------------------------
// Sub-widgets
// ---------------------------------------------------------------------------

class _LoadingCard extends StatelessWidget {
  const _LoadingCard();

  @override
  Widget build(BuildContext context) {
    return const Card(
      child: Padding(
        padding: EdgeInsets.all(32),
        child: Column(
          children: [
            CircularProgressIndicator(),
            SizedBox(height: 16),
            Text('Checking backend health…'),
          ],
        ),
      ),
    );
  }
}

class _HealthCard extends StatelessWidget {
  const _HealthCard({required this.health});

  final HealthResponse health;

  @override
  Widget build(BuildContext context) {
    final isUp = health.status.toUpperCase() == 'UP';
    final colorScheme = Theme.of(context).colorScheme;
    final cardColor = isUp ? Colors.green.shade50 : Colors.red.shade50;
    final indicatorColor = isUp ? Colors.green.shade700 : Colors.red.shade700;
    final statusText = isUp ? 'UP' : health.status;
    final statusLabel = isUp ? 'Backend is healthy' : 'Backend is unavailable';

    return Semantics(
      label: 'Backend health status: $statusText',
      child: Card(
        color: cardColor,
        child: Padding(
          padding: const EdgeInsets.all(24),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Row(
                children: [
                  Icon(
                    isUp ? Icons.check_circle : Icons.error,
                    color: indicatorColor,
                    size: 32,
                  ),
                  const SizedBox(width: 12),
                  Text(
                    statusText,
                    style: Theme.of(context).textTheme.headlineMedium?.copyWith(
                      color: indicatorColor,
                      fontWeight: FontWeight.bold,
                    ),
                  ),
                ],
              ),
              const SizedBox(height: 8),
              Text(statusLabel, style: Theme.of(context).textTheme.bodyLarge),
              if (health.version != null) ...[
                const SizedBox(height: 8),
                Text(
                  'Version: ${health.version}',
                  style: Theme.of(context).textTheme.bodyMedium?.copyWith(
                    color: colorScheme.onSurfaceVariant,
                  ),
                ),
              ],
              if (health.timestamp != null) ...[
                const SizedBox(height: 4),
                Text(
                  'Checked at: ${health.timestamp}',
                  style: Theme.of(context).textTheme.bodySmall?.copyWith(
                    color: colorScheme.onSurfaceVariant,
                  ),
                ),
              ],
            ],
          ),
        ),
      ),
    );
  }
}

class _ErrorCard extends StatelessWidget {
  const _ErrorCard({required this.error});

  final Object error;

  @override
  Widget build(BuildContext context) {
    String message = 'Backend unavailable';
    if (error is DioException) {
      final dioErr = error as DioException;
      message = dioErr.message ?? message;
    }

    return Semantics(
      label: 'Error: $message',
      child: Card(
        color: Colors.red.shade50,
        child: Padding(
          padding: const EdgeInsets.all(24),
          child: Row(
            children: [
              Icon(Icons.error_outline, color: Colors.red.shade700, size: 32),
              const SizedBox(width: 12),
              Expanded(
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    Text(
                      'Backend unavailable',
                      style: Theme.of(context).textTheme.titleMedium?.copyWith(
                        color: Colors.red.shade700,
                        fontWeight: FontWeight.bold,
                      ),
                    ),
                    const SizedBox(height: 4),
                    Text(
                      message,
                      style: Theme.of(context).textTheme.bodyMedium,
                    ),
                  ],
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }
}

class _RefreshButton extends StatelessWidget {
  const _RefreshButton({required this.onPressed});

  final VoidCallback onPressed;

  @override
  Widget build(BuildContext context) {
    return OutlinedButton.icon(
      onPressed: onPressed,
      icon: const Icon(Icons.refresh),
      label: const Text('Refresh'),
    );
  }
}

class _LoginLink extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Center(
      child: TextButton(
        onPressed: () => context.go('/login'),
        child: const Text('Go to Login'),
      ),
    );
  }
}
