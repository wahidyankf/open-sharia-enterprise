/// Admin screen — paginated user management.
///
/// Displays a searchable, paginated list of users with status badges.
/// Admins can disable (with reason dialog), enable, unlock, and trigger
/// password reset for any user. Wrapped in [AppShell].
library;

import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:demo_fe_dart_flutter/core/models/models.dart';
import 'package:demo_fe_dart_flutter/core/providers/admin_provider.dart';
import 'package:demo_fe_dart_flutter/widgets/app_shell.dart';

class AdminScreen extends ConsumerStatefulWidget {
  const AdminScreen({super.key});

  @override
  ConsumerState<AdminScreen> createState() => _AdminScreenState();
}

class _AdminScreenState extends ConsumerState<AdminScreen> {
  int _page = 1;
  static const int _pageSize = 20;
  String? _search;
  final _searchController = TextEditingController();

  @override
  void dispose() {
    _searchController.dispose();
    super.dispose();
  }

  AdminUsersParams get _params =>
      AdminUsersParams(page: _page, size: _pageSize, search: _search);

  @override
  Widget build(BuildContext context) {
    final usersAsync = ref.watch(adminUsersProvider(_params));

    return AppShell(
      child: Padding(
        padding: const EdgeInsets.all(16),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              'User Management',
              style: Theme.of(context).textTheme.headlineMedium,
            ),
            const SizedBox(height: 16),
            _SearchBar(
              controller: _searchController,
              onSearch: (query) {
                setState(() {
                  _search = query.isEmpty ? null : query;
                  _page = 1;
                });
              },
            ),
            const SizedBox(height: 16),
            Expanded(
              child: usersAsync.when(
                loading: () => const Center(child: CircularProgressIndicator()),
                error: (e, _) => Center(child: Text('Error loading users: $e')),
                data: (data) => Column(
                  children: [
                    Expanded(
                      child: _UserList(
                        users: data.users,
                        onAction: (user, action) =>
                            _handleAction(context, ref, user, action),
                      ),
                    ),
                    _Pagination(
                      page: _page,
                      total: data.total,
                      pageSize: _pageSize,
                      onPageChanged: (p) => setState(() => _page = p),
                    ),
                  ],
                ),
              ),
            ),
          ],
        ),
      ),
    );
  }

  Future<void> _handleAction(
    BuildContext context,
    WidgetRef ref,
    User user,
    _UserAction action,
  ) async {
    final notifier = ref.read(adminNotifierProvider.notifier);
    switch (action) {
      case _UserAction.disable:
        final reason = await _showDisableDialog(context, user.username);
        if (reason != null && reason.isNotEmpty) {
          await notifier.disableUser(user.id, reason);
        }
      case _UserAction.enable:
        await notifier.enableUser(user.id);
      case _UserAction.unlock:
        await notifier.unlockUser(user.id);
      case _UserAction.resetPassword:
        await notifier.forcePasswordReset(user.id);
        if (context.mounted) {
          _showResetTokenMessage(context, user.username);
        }
    }
  }

  Future<String?> _showDisableDialog(
    BuildContext context,
    String username,
  ) async {
    final controller = TextEditingController();
    final result = await showDialog<String>(
      context: context,
      builder: (ctx) => AlertDialog(
        title: Text('Disable User: $username'),
        content: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            const Text('Provide a reason for disabling this account:'),
            const SizedBox(height: 12),
            TextField(
              controller: controller,
              decoration: const InputDecoration(
                labelText: 'Reason',
                border: OutlineInputBorder(),
              ),
              maxLines: 3,
              autofocus: true,
            ),
          ],
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.of(ctx).pop(),
            child: const Text('Cancel'),
          ),
          FilledButton(
            onPressed: () => Navigator.of(ctx).pop(controller.text.trim()),
            child: const Text('Disable'),
          ),
        ],
      ),
    );
    controller.dispose();
    return result;
  }

  void _showResetTokenMessage(BuildContext context, String username) {
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Text('Password reset triggered for $username.'),
        action: SnackBarAction(label: 'Dismiss', onPressed: () {}),
      ),
    );
  }
}

// ---------------------------------------------------------------------------
// Enums
// ---------------------------------------------------------------------------

enum _UserAction { disable, enable, unlock, resetPassword }

// ---------------------------------------------------------------------------
// Search bar
// ---------------------------------------------------------------------------

class _SearchBar extends StatelessWidget {
  const _SearchBar({required this.controller, required this.onSearch});

  final TextEditingController controller;
  final ValueChanged<String> onSearch;

  @override
  Widget build(BuildContext context) {
    return TextField(
      controller: controller,
      decoration: InputDecoration(
        labelText: 'Search by email or username',
        prefixIcon: const Icon(Icons.search),
        border: const OutlineInputBorder(),
        suffixIcon: controller.text.isNotEmpty
            ? IconButton(
                tooltip: 'Clear search',
                icon: const Icon(Icons.clear),
                onPressed: () {
                  controller.clear();
                  onSearch('');
                },
              )
            : null,
      ),
      onSubmitted: onSearch,
    );
  }
}

// ---------------------------------------------------------------------------
// User list
// ---------------------------------------------------------------------------

class _UserList extends StatelessWidget {
  const _UserList({required this.users, required this.onAction});

  final List<User> users;
  final void Function(User, _UserAction) onAction;

  @override
  Widget build(BuildContext context) {
    if (users.isEmpty) {
      return const Center(child: Text('No users found.'));
    }

    return ListView.separated(
      itemCount: users.length,
      separatorBuilder: (_, __) => const Divider(height: 1),
      itemBuilder: (context, index) => _UserTile(
        user: users[index],
        onAction: (action) => onAction(users[index], action),
      ),
    );
  }
}

class _UserTile extends StatelessWidget {
  const _UserTile({required this.user, required this.onAction});

  final User user;
  final void Function(_UserAction) onAction;

  @override
  Widget build(BuildContext context) {
    return ListTile(
      title: Text(user.username),
      subtitle: Text(user.email),
      trailing: Row(
        mainAxisSize: MainAxisSize.min,
        children: [
          _StatusBadge(status: user.status),
          const SizedBox(width: 8),
          _ActionsMenu(user: user, onAction: onAction),
        ],
      ),
    );
  }
}

// ---------------------------------------------------------------------------
// Status badge
// ---------------------------------------------------------------------------

class _StatusBadge extends StatelessWidget {
  const _StatusBadge({required this.status});

  final String status;

  @override
  Widget build(BuildContext context) {
    final color = _statusColor(status);
    return Semantics(
      label: 'User status: $status',
      child: Container(
        padding: const EdgeInsets.symmetric(horizontal: 8, vertical: 3),
        decoration: BoxDecoration(
          color: color.withAlpha(30),
          borderRadius: BorderRadius.circular(10),
          border: Border.all(color: color),
        ),
        child: Text(
          status,
          style: TextStyle(
            color: color,
            fontWeight: FontWeight.w600,
            fontSize: 11,
          ),
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

// ---------------------------------------------------------------------------
// Actions menu
// ---------------------------------------------------------------------------

class _ActionsMenu extends StatelessWidget {
  const _ActionsMenu({required this.user, required this.onAction});

  final User user;
  final void Function(_UserAction) onAction;

  @override
  Widget build(BuildContext context) {
    final status = user.status.toUpperCase();
    return PopupMenuButton<_UserAction>(
      tooltip: 'User actions',
      onSelected: onAction,
      itemBuilder: (ctx) => [
        if (status == 'ACTIVE' || status == 'INACTIVE')
          const PopupMenuItem(
            value: _UserAction.disable,
            child: ListTile(
              leading: Icon(Icons.block),
              title: Text('Disable'),
              contentPadding: EdgeInsets.zero,
            ),
          ),
        if (status == 'DISABLED')
          const PopupMenuItem(
            value: _UserAction.enable,
            child: ListTile(
              leading: Icon(Icons.check_circle_outline),
              title: Text('Enable'),
              contentPadding: EdgeInsets.zero,
            ),
          ),
        if (status == 'LOCKED')
          const PopupMenuItem(
            value: _UserAction.unlock,
            child: ListTile(
              leading: Icon(Icons.lock_open_outlined),
              title: Text('Unlock'),
              contentPadding: EdgeInsets.zero,
            ),
          ),
        const PopupMenuItem(
          value: _UserAction.resetPassword,
          child: ListTile(
            leading: Icon(Icons.password),
            title: Text('Force Password Reset'),
            contentPadding: EdgeInsets.zero,
          ),
        ),
      ],
    );
  }
}

// ---------------------------------------------------------------------------
// Pagination
// ---------------------------------------------------------------------------

class _Pagination extends StatelessWidget {
  const _Pagination({
    required this.page,
    required this.total,
    required this.pageSize,
    required this.onPageChanged,
  });

  final int page;
  final int total;
  final int pageSize;
  final ValueChanged<int> onPageChanged;

  int get _totalPages => (total / pageSize).ceil().clamp(1, 9999);

  @override
  Widget build(BuildContext context) {
    return Padding(
      padding: const EdgeInsets.symmetric(vertical: 12),
      child: Row(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          IconButton(
            tooltip: 'Previous page',
            icon: const Icon(Icons.chevron_left),
            onPressed: page > 1 ? () => onPageChanged(page - 1) : null,
          ),
          Text('Page $page of $_totalPages'),
          IconButton(
            tooltip: 'Next page',
            icon: const Icon(Icons.chevron_right),
            onPressed: page < _totalPages
                ? () => onPageChanged(page + 1)
                : null,
          ),
        ],
      ),
    );
  }
}
