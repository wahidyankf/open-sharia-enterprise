/// Profile screen — view and edit the current user's account.
///
/// Displays username, email, and status. Provides forms for updating the
/// display name and changing the password, and a confirmation-guarded
/// deactivate-account action. Wrapped in [AppShell] so navigation is
/// available.
library;

import 'package:dio/dio.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:demo_fe_dart_flutter/core/models/models.dart';
import 'package:demo_fe_dart_flutter/core/providers/user_provider.dart';
import 'package:demo_fe_dart_flutter/widgets/app_shell.dart';

class ProfileScreen extends ConsumerWidget {
  const ProfileScreen({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final userAsync = ref.watch(currentUserProvider);

    return AppShell(
      child: userAsync.when(
        loading: () => const Center(child: CircularProgressIndicator()),
        error: (e, _) => Center(child: Text('Error loading profile: $e')),
        data: (user) => _ProfileContent(user: user),
      ),
    );
  }
}

// ---------------------------------------------------------------------------
// Profile content
// ---------------------------------------------------------------------------

class _ProfileContent extends ConsumerWidget {
  const _ProfileContent({required this.user});

  final User user;

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    return ListView(
      padding: const EdgeInsets.all(24),
      children: [
        Text('Profile', style: Theme.of(context).textTheme.headlineMedium),
        const SizedBox(height: 24),
        _InfoCard(user: user),
        const SizedBox(height: 16),
        _DisplayNameForm(currentDisplayName: user.displayName),
        const SizedBox(height: 16),
        const _ChangePasswordForm(),
        const SizedBox(height: 16),
        _DeactivateSection(username: user.username),
      ],
    );
  }
}

// ---------------------------------------------------------------------------
// Info card
// ---------------------------------------------------------------------------

class _InfoCard extends StatelessWidget {
  const _InfoCard({required this.user});

  final User user;

  @override
  Widget build(BuildContext context) {
    final statusColor = _statusColor(user.status);

    return Card(
      child: Padding(
        padding: const EdgeInsets.all(16),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              'Account Information',
              style: Theme.of(context).textTheme.titleMedium,
            ),
            const Divider(),
            _InfoRow(label: 'Username', value: user.username),
            _InfoRow(label: 'Email', value: user.email),
            _InfoRow(
              label: 'Display Name',
              value: user.displayName.isNotEmpty ? user.displayName : '—',
            ),
            _InfoRow(label: 'Role', value: user.role),
            Row(
              children: [
                const SizedBox(width: 120, child: Text('Status')),
                Semantics(
                  label: 'Account status: ${user.status}',
                  child: Container(
                    padding: const EdgeInsets.symmetric(
                      horizontal: 10,
                      vertical: 4,
                    ),
                    decoration: BoxDecoration(
                      color: statusColor.withAlpha(30),
                      borderRadius: BorderRadius.circular(12),
                      border: Border.all(color: statusColor),
                    ),
                    child: Text(
                      user.status,
                      style: TextStyle(
                        color: statusColor,
                        fontWeight: FontWeight.w600,
                        fontSize: 12,
                      ),
                    ),
                  ),
                ),
              ],
            ),
          ],
        ),
      ),
    );
  }

  Color _statusColor(String status) => switch (status.toUpperCase()) {
    'ACTIVE' => Colors.green.shade700,
    'DISABLED' => Colors.red.shade700,
    'LOCKED' => Colors.purple.shade700,
    'INACTIVE' => Colors.orange.shade700,
    _ => Colors.grey.shade700,
  };
}

class _InfoRow extends StatelessWidget {
  const _InfoRow({required this.label, required this.value});

  final String label;
  final String value;

  @override
  Widget build(BuildContext context) {
    return Padding(
      padding: const EdgeInsets.symmetric(vertical: 6),
      child: Row(
        children: [
          SizedBox(
            width: 120,
            child: Text(
              label,
              style: const TextStyle(fontWeight: FontWeight.w500),
            ),
          ),
          Expanded(child: Text(value)),
        ],
      ),
    );
  }
}

// ---------------------------------------------------------------------------
// Display name form
// ---------------------------------------------------------------------------

class _DisplayNameForm extends ConsumerStatefulWidget {
  const _DisplayNameForm({required this.currentDisplayName});

  final String currentDisplayName;

  @override
  ConsumerState<_DisplayNameForm> createState() => _DisplayNameFormState();
}

class _DisplayNameFormState extends ConsumerState<_DisplayNameForm> {
  final _formKey = GlobalKey<FormState>();
  late final TextEditingController _controller;
  String? _message;
  bool _isError = false;

  @override
  void initState() {
    super.initState();
    _controller = TextEditingController(text: widget.currentDisplayName);
  }

  @override
  void dispose() {
    _controller.dispose();
    super.dispose();
  }

  Future<void> _save() async {
    if (!(_formKey.currentState?.validate() ?? false)) return;
    setState(() {
      _message = null;
      _isError = false;
    });
    try {
      await ref
          .read(userNotifierProvider.notifier)
          .updateProfile(_controller.text.trim());
      setState(() => _message = 'Display name updated.');
    } on DioException catch (e) {
      setState(() {
        _isError = true;
        _message =
            (e.response?.data as Map<String, dynamic>?)?['detail'] as String? ??
            'Failed to update display name.';
      });
    } catch (_) {
      setState(() {
        _isError = true;
        _message = 'An unexpected error occurred.';
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(16),
        child: Form(
          key: _formKey,
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                'Update Display Name',
                style: Theme.of(context).textTheme.titleMedium,
              ),
              const SizedBox(height: 12),
              if (_message != null) ...[
                Semantics(
                  label: _isError ? 'Error: $_message' : _message,
                  child: Text(
                    _message!,
                    style: TextStyle(
                      color: _isError
                          ? Theme.of(context).colorScheme.error
                          : Colors.green.shade700,
                    ),
                  ),
                ),
                const SizedBox(height: 8),
              ],
              TextFormField(
                controller: _controller,
                decoration: const InputDecoration(
                  labelText: 'Display Name',
                  border: OutlineInputBorder(),
                ),
                validator: (v) => (v == null || v.trim().isEmpty)
                    ? 'Display name is required'
                    : null,
              ),
              const SizedBox(height: 12),
              FilledButton(onPressed: _save, child: const Text('Save')),
            ],
          ),
        ),
      ),
    );
  }
}

// ---------------------------------------------------------------------------
// Change password form
// ---------------------------------------------------------------------------

class _ChangePasswordForm extends ConsumerStatefulWidget {
  const _ChangePasswordForm();

  @override
  ConsumerState<_ChangePasswordForm> createState() =>
      _ChangePasswordFormState();
}

class _ChangePasswordFormState extends ConsumerState<_ChangePasswordForm> {
  final _formKey = GlobalKey<FormState>();
  final _oldController = TextEditingController();
  final _newController = TextEditingController();
  bool _obscureOld = true;
  bool _obscureNew = true;
  String? _message;
  bool _isError = false;

  @override
  void dispose() {
    _oldController.dispose();
    _newController.dispose();
    super.dispose();
  }

  Future<void> _save() async {
    if (!(_formKey.currentState?.validate() ?? false)) return;
    setState(() {
      _message = null;
      _isError = false;
    });
    try {
      await ref
          .read(userNotifierProvider.notifier)
          .changePassword(
            oldPassword: _oldController.text,
            newPassword: _newController.text,
          );
      _oldController.clear();
      _newController.clear();
      setState(() => _message = 'Password changed successfully.');
    } on DioException catch (e) {
      setState(() {
        _isError = true;
        _message =
            (e.response?.data as Map<String, dynamic>?)?['detail'] as String? ??
            'Failed to change password.';
      });
    } catch (_) {
      setState(() {
        _isError = true;
        _message = 'An unexpected error occurred.';
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(16),
        child: Form(
          key: _formKey,
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                'Change Password',
                style: Theme.of(context).textTheme.titleMedium,
              ),
              const SizedBox(height: 12),
              if (_message != null) ...[
                Semantics(
                  label: _isError ? 'Error: $_message' : _message,
                  child: Text(
                    _message!,
                    style: TextStyle(
                      color: _isError
                          ? Theme.of(context).colorScheme.error
                          : Colors.green.shade700,
                    ),
                  ),
                ),
                const SizedBox(height: 8),
              ],
              TextFormField(
                controller: _oldController,
                obscureText: _obscureOld,
                decoration: InputDecoration(
                  labelText: 'Current Password',
                  border: const OutlineInputBorder(),
                  suffixIcon: IconButton(
                    tooltip: _obscureOld ? 'Show' : 'Hide',
                    icon: Icon(
                      _obscureOld
                          ? Icons.visibility_outlined
                          : Icons.visibility_off_outlined,
                    ),
                    onPressed: () => setState(() => _obscureOld = !_obscureOld),
                  ),
                ),
                validator: (v) => (v == null || v.isEmpty)
                    ? 'Current password is required'
                    : null,
              ),
              const SizedBox(height: 12),
              TextFormField(
                controller: _newController,
                obscureText: _obscureNew,
                decoration: InputDecoration(
                  labelText: 'New Password',
                  border: const OutlineInputBorder(),
                  suffixIcon: IconButton(
                    tooltip: _obscureNew ? 'Show' : 'Hide',
                    icon: Icon(
                      _obscureNew
                          ? Icons.visibility_outlined
                          : Icons.visibility_off_outlined,
                    ),
                    onPressed: () => setState(() => _obscureNew = !_obscureNew),
                  ),
                ),
                validator: (v) {
                  if (v == null || v.isEmpty) return 'New password is required';
                  if (v.length < 12) {
                    return 'Password must be at least 12 characters';
                  }
                  if (!v.contains(RegExp('[A-Z]'))) {
                    return 'Must contain at least one uppercase letter';
                  }
                  if (!v.contains(RegExp(r'[!@#$%^&*(),.?":{}|<>]'))) {
                    return 'Must contain at least one special character';
                  }
                  return null;
                },
              ),
              const SizedBox(height: 12),
              FilledButton(
                onPressed: _save,
                child: const Text('Change Password'),
              ),
            ],
          ),
        ),
      ),
    );
  }
}

// ---------------------------------------------------------------------------
// Deactivate account section
// ---------------------------------------------------------------------------

class _DeactivateSection extends ConsumerWidget {
  const _DeactivateSection({required this.username});

  final String username;

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    return Card(
      color: Colors.red.shade50,
      child: Padding(
        padding: const EdgeInsets.all(16),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              'Danger Zone',
              style: Theme.of(
                context,
              ).textTheme.titleMedium?.copyWith(color: Colors.red.shade700),
            ),
            const SizedBox(height: 8),
            const Text(
              'Deactivating your account is irreversible. You will be logged out immediately.',
            ),
            const SizedBox(height: 12),
            OutlinedButton(
              style: OutlinedButton.styleFrom(
                foregroundColor: Colors.red.shade700,
                side: BorderSide(color: Colors.red.shade700),
              ),
              onPressed: () => _confirmDeactivate(context, ref),
              child: const Text('Deactivate Account'),
            ),
          ],
        ),
      ),
    );
  }

  Future<void> _confirmDeactivate(BuildContext context, WidgetRef ref) async {
    final confirmed = await showDialog<bool>(
      context: context,
      builder: (ctx) => AlertDialog(
        title: const Text('Deactivate Account'),
        content: Text(
          'Are you sure you want to deactivate the account "$username"? '
          'This action cannot be undone.',
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.of(ctx).pop(false),
            child: const Text('Cancel'),
          ),
          FilledButton(
            style: FilledButton.styleFrom(backgroundColor: Colors.red.shade700),
            onPressed: () => Navigator.of(ctx).pop(true),
            child: const Text('Deactivate'),
          ),
        ],
      ),
    );

    if (confirmed == true) {
      await ref.read(userNotifierProvider.notifier).deactivateAccount();
      if (context.mounted) {
        context.go('/login');
      }
    }
  }
}
