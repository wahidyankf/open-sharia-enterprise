/// Registration screen — username, email, password form.
///
/// Validates password complexity (12+ chars, ≥1 uppercase, ≥1 special char)
/// with real-time feedback. On success navigates to /login with a success
/// message. Handles 409 (duplicate username/email) and 400 (invalid data).
library;

import 'package:dio/dio.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:demo_fe_dart_flutter/core/providers/auth_provider.dart';

// ---------------------------------------------------------------------------
// Password complexity helpers
// ---------------------------------------------------------------------------

bool _hasMinLength(String v) => v.length >= 12;
bool _hasUppercase(String v) => v.contains(RegExp('[A-Z]'));
bool _hasSpecialChar(String v) => v.contains(RegExp(r'[!@#$%^&*(),.?":{}|<>]'));

// ---------------------------------------------------------------------------
// Screen widget
// ---------------------------------------------------------------------------

class RegisterScreen extends ConsumerStatefulWidget {
  const RegisterScreen({super.key});

  @override
  ConsumerState<RegisterScreen> createState() => _RegisterScreenState();
}

class _RegisterScreenState extends ConsumerState<RegisterScreen> {
  final _formKey = GlobalKey<FormState>();
  final _usernameController = TextEditingController();
  final _emailController = TextEditingController();
  final _passwordController = TextEditingController();

  bool _obscurePassword = true;
  bool _isLoading = false;
  String? _errorMessage;

  // Real-time password feedback flags.
  bool _minLength = false;
  bool _hasUpper = false;
  bool _hasSpecial = false;

  @override
  void initState() {
    super.initState();
    _passwordController.addListener(_onPasswordChanged);
  }

  void _onPasswordChanged() {
    final v = _passwordController.text;
    setState(() {
      _minLength = _hasMinLength(v);
      _hasUpper = _hasUppercase(v);
      _hasSpecial = _hasSpecialChar(v);
    });
  }

  @override
  void dispose() {
    _usernameController.dispose();
    _emailController.dispose();
    _passwordController.dispose();
    super.dispose();
  }

  Future<void> _submit() async {
    if (!(_formKey.currentState?.validate() ?? false)) return;

    setState(() {
      _isLoading = true;
      _errorMessage = null;
    });

    try {
      await ref
          .read(authProvider.notifier)
          .register(
            username: _usernameController.text.trim(),
            email: _emailController.text.trim(),
            password: _passwordController.text,
          );
      if (mounted) {
        // The auth provider logs the user in automatically after registration,
        // but per requirements we navigate to login with a success message.
        await ref.read(authProvider.notifier).logout();
        if (mounted) {
          context.go(
            '/login',
            extra: 'Account created successfully. Please sign in.',
          );
        }
      }
    } on DioException catch (e) {
      final status = e.response?.statusCode;
      setState(() {
        if (status == 409) {
          _errorMessage =
              'Username or email already in use. Please choose another.';
        } else if (status == 400) {
          final detail =
              (e.response?.data as Map<String, dynamic>?)?['detail'] as String?;
          _errorMessage = detail ?? 'Invalid registration data.';
        } else {
          _errorMessage = 'Registration failed. Please try again.';
        }
      });
    } catch (_) {
      setState(() => _errorMessage = 'An unexpected error occurred.');
    } finally {
      if (mounted) setState(() => _isLoading = false);
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: Center(
        child: SingleChildScrollView(
          padding: const EdgeInsets.all(24),
          child: ConstrainedBox(
            constraints: const BoxConstraints(maxWidth: 440),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.stretch,
              children: [
                Text(
                  'Create Account',
                  style: Theme.of(context).textTheme.headlineMedium,
                  textAlign: TextAlign.center,
                ),
                const SizedBox(height: 32),
                if (_errorMessage != null) ...[
                  Semantics(
                    label: 'Error: $_errorMessage',
                    child: Card(
                      color: Theme.of(context).colorScheme.errorContainer,
                      child: Padding(
                        padding: const EdgeInsets.all(12),
                        child: Text(
                          _errorMessage!,
                          style: TextStyle(
                            color: Theme.of(
                              context,
                            ).colorScheme.onErrorContainer,
                          ),
                        ),
                      ),
                    ),
                  ),
                  const SizedBox(height: 16),
                ],
                Form(
                  key: _formKey,
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.stretch,
                    children: [
                      TextFormField(
                        controller: _usernameController,
                        decoration: const InputDecoration(
                          labelText: 'Username',
                          prefixIcon: Icon(Icons.person_outlined),
                          border: OutlineInputBorder(),
                        ),
                        textInputAction: TextInputAction.next,
                        validator: (v) => (v == null || v.trim().isEmpty)
                            ? 'Username is required'
                            : null,
                      ),
                      const SizedBox(height: 16),
                      TextFormField(
                        controller: _emailController,
                        decoration: const InputDecoration(
                          labelText: 'Email',
                          prefixIcon: Icon(Icons.email_outlined),
                          border: OutlineInputBorder(),
                        ),
                        keyboardType: TextInputType.emailAddress,
                        autofillHints: const [AutofillHints.email],
                        textInputAction: TextInputAction.next,
                        validator: (v) {
                          if (v == null || v.trim().isEmpty) {
                            return 'Email is required';
                          }
                          if (!v.contains('@')) {
                            return 'Enter a valid email address';
                          }
                          return null;
                        },
                      ),
                      const SizedBox(height: 16),
                      TextFormField(
                        controller: _passwordController,
                        obscureText: _obscurePassword,
                        decoration: InputDecoration(
                          labelText: 'Password',
                          prefixIcon: const Icon(Icons.lock_outlined),
                          border: const OutlineInputBorder(),
                          suffixIcon: IconButton(
                            tooltip: _obscurePassword
                                ? 'Show password'
                                : 'Hide password',
                            icon: Icon(
                              _obscurePassword
                                  ? Icons.visibility_outlined
                                  : Icons.visibility_off_outlined,
                            ),
                            onPressed: () => setState(
                              () => _obscurePassword = !_obscurePassword,
                            ),
                          ),
                        ),
                        textInputAction: TextInputAction.done,
                        onFieldSubmitted: (_) => _submit(),
                        validator: (v) {
                          if (v == null || v.isEmpty) {
                            return 'Password is required';
                          }
                          if (!_hasMinLength(v)) {
                            return 'Password must be at least 12 characters';
                          }
                          if (!_hasUppercase(v)) {
                            return 'Password must contain at least one uppercase letter';
                          }
                          if (!_hasSpecialChar(v)) {
                            return 'Password must contain at least one special character';
                          }
                          return null;
                        },
                      ),
                      const SizedBox(height: 12),
                      _PasswordRequirements(
                        minLength: _minLength,
                        hasUpper: _hasUpper,
                        hasSpecial: _hasSpecial,
                      ),
                      const SizedBox(height: 24),
                      FilledButton(
                        onPressed: _isLoading ? null : _submit,
                        child: _isLoading
                            ? const SizedBox(
                                height: 20,
                                width: 20,
                                child: CircularProgressIndicator(
                                  strokeWidth: 2,
                                ),
                              )
                            : const Text('Create Account'),
                      ),
                    ],
                  ),
                ),
                const SizedBox(height: 16),
                Row(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    const Text('Already have an account?'),
                    TextButton(
                      onPressed: () => context.go('/login'),
                      child: const Text('Sign In'),
                    ),
                  ],
                ),
              ],
            ),
          ),
        ),
      ),
    );
  }
}

// ---------------------------------------------------------------------------
// Password requirements feedback widget
// ---------------------------------------------------------------------------

class _PasswordRequirements extends StatelessWidget {
  const _PasswordRequirements({
    required this.minLength,
    required this.hasUpper,
    required this.hasSpecial,
  });

  final bool minLength;
  final bool hasUpper;
  final bool hasSpecial;

  @override
  Widget build(BuildContext context) {
    return Semantics(
      label: 'Password requirements',
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            'Password requirements:',
            style: Theme.of(context).textTheme.bodySmall,
          ),
          const SizedBox(height: 4),
          _Requirement(met: minLength, label: 'At least 12 characters'),
          _Requirement(met: hasUpper, label: 'One uppercase letter (A-Z)'),
          _Requirement(
            met: hasSpecial,
            label: 'One special character (!@#\$%^&*…)',
          ),
        ],
      ),
    );
  }
}

class _Requirement extends StatelessWidget {
  const _Requirement({required this.met, required this.label});

  final bool met;
  final String label;

  @override
  Widget build(BuildContext context) {
    final color = met ? Colors.green.shade700 : Colors.grey.shade600;
    return Semantics(
      label: '${met ? 'Met' : 'Not met'}: $label',
      child: Row(
        children: [
          Icon(
            met ? Icons.check_circle_outline : Icons.radio_button_unchecked,
            size: 16,
            color: color,
          ),
          const SizedBox(width: 6),
          Text(
            label,
            style: Theme.of(
              context,
            ).textTheme.bodySmall?.copyWith(color: color),
          ),
        ],
      ),
    );
  }
}
