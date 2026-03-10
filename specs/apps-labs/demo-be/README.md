# Demo IAM API Specs

Platform-agnostic Gherkin acceptance specifications for a full IAM (Identity and Access Management)
service. These specs describe expected HTTP behavior without reference to any specific framework or
library, so they can be used to validate any implementation.

## What This Covers

The specifications cover all production IAM domains:

| Domain           | Description                                                    |
| ---------------- | -------------------------------------------------------------- |
| health           | Service liveness check                                         |
| authentication   | Password login, token refresh, logout, session management      |
| user-lifecycle   | Registration, profile, password change, deactivation, deletion |
| authorization    | Role and permission management, role assignment, enforcement   |
| organization     | Org creation, membership, invitations, org-level roles         |
| security         | Password policy, brute-force protection, account lockout       |
| mfa              | TOTP enrollment, MFA login flow, recovery codes                |
| token-management | JWT issuance, refresh rotation, revocation, introspection      |
| audit            | Login history, security event recording                        |
| admin            | User listing/search, account control, forced password reset    |
| api-keys         | API key lifecycle and rate-limit enforcement                   |

## Implementations

| Implementation | Language | Integration runner | E2E runner |
| -------------- | -------- | ------------------ | ---------- |
| demo-be        | TBD      | TBD                | TBD        |

Each new language implementation adds its own step definitions. The feature files here are the
single source of truth and must not contain language-specific concepts (framework names, library
paths, runtime-specific error formats).

## Feature File Organization

```
specs/apps-labs/demo-be/
├── health/
│   └── health-check.feature
├── authentication/
│   ├── password-login.feature
│   ├── token-refresh.feature
│   ├── logout.feature
│   └── session-management.feature
├── user-lifecycle/
│   ├── registration.feature
│   ├── profile-management.feature
│   ├── password-change.feature
│   ├── account-deactivation.feature
│   └── account-deletion.feature
├── authorization/
│   ├── role-management.feature
│   ├── permission-management.feature
│   ├── role-assignment.feature
│   └── permission-enforcement.feature
├── organization/
│   ├── organization-creation.feature
│   ├── organization-membership.feature
│   ├── organization-invitations.feature
│   └── organization-roles.feature
├── security/
│   ├── password-policy.feature
│   ├── brute-force-protection.feature
│   └── account-lockout.feature
├── mfa/
│   ├── totp-enrollment.feature
│   ├── mfa-login.feature
│   └── recovery-codes.feature
├── token-management/
│   ├── jwt-issuance.feature
│   ├── refresh-token-rotation.feature
│   ├── token-revocation.feature
│   └── token-introspection.feature
├── audit/
│   ├── login-history.feature
│   └── security-events.feature
├── admin/
│   ├── user-management.feature
│   ├── account-control.feature
│   └── force-password-reset.feature
└── api-keys/
    └── api-key-management.feature
```

**File naming**: `[domain-capability].feature` (kebab-case)

## Running Specs

TBD — depends on the chosen implementation language and framework.

## Adding a Feature File

1. Identify the bounded context (e.g., `authentication`, `user-lifecycle`)
2. Create the folder if it does not exist: `specs/apps-labs/demo-be/[context]/`
3. Create the `.feature` file: `[domain-capability].feature`
4. Open with `Feature:` then a user story block (`As a … / I want … / So that …`)
5. Use `Given the IAM API is running` as the first Background step
6. Use only HTTP-semantic steps — no framework or library names

## Related

- **BDD Standards**: [behavior-driven-development-bdd/](../../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/README.md)
