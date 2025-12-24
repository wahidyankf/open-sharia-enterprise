# Security Policy

## Supported Versions

We release security updates for the following versions:

| Version | Supported          |
| ------- | ------------------ |
| 1.x.x   | :white_check_mark: |
| < 1.0   | :x:                |

## Reporting a Vulnerability

**Please do not report security vulnerabilities through public GitHub issues.**

We take security seriously, especially as an enterprise platform with financial services. If you discover a security vulnerability, please report it responsibly.

### How to Report

Please report security vulnerabilities via email to: **wahidyankf@gmail.com**

### What to Include

Please include the following information in your report:

- **Description of the vulnerability**: What is the security issue?
- **Steps to reproduce**: How can we reproduce the vulnerability?
- **Potential impact**: What could an attacker do with this vulnerability?
- **Affected components**: Which parts of the system are affected?
- **Suggested fix** (if any): Do you have suggestions for fixing the issue?
- **Your contact information**: How can we reach you for follow-up?

### Response Timeline

You can expect:

- **Initial Response**: Within 48 hours of your report
- **Status Update**: Within 5 business days with assessment and timeline
- **Regular Updates**: We will keep you informed of progress toward a fix

### Security Response Process

1. **Acknowledgment**: We will acknowledge receipt of your report within 48 hours
2. **Investigation**: We will investigate and assess the severity of the vulnerability
3. **Fix Development**: We will develop and test a fix (timeline depends on severity)
4. **Security Release**: We will release a security update as a patch version
5. **Public Disclosure**: We will publicly disclose the vulnerability after the fix is available and users have had time to update

### Severity Assessment

We assess vulnerabilities using the following severity levels:

- **Critical**: Immediate threat to data integrity, confidentiality, or availability
- **High**: Significant security impact requiring urgent attention
- **Medium**: Security issue that should be addressed in next release
- **Low**: Minor security concern with limited impact

## Security Best Practices

As an enterprise platform with financial services, we follow these security principles:

- **Secure by Default**: Security features enabled out of the box
- **Principle of Least Privilege**: Components have minimal necessary permissions
- **Defense in Depth**: Multiple layers of security controls
- **Regular Security Audits**: Periodic review of code and infrastructure
- **Dependency Management**: Regular updates of dependencies with known vulnerabilities

## Security Updates

Security fixes are released as patch versions (e.g., 1.2.3 → 1.2.4).

**How to stay informed:**

- Watch this repository for releases
- Subscribe to GitHub release notifications
- Check the [CHANGELOG.md](./CHANGELOG.md) for security-related updates

## Scope

This security policy applies to:

- All code in this repository
- Official Docker images and deployments
- Official npm packages published under `@open-sharia-enterprise` scope

**Out of scope:**

- Vulnerabilities in dependencies (please report to the respective projects)
- Theoretical vulnerabilities without proof of concept
- Issues that require physical access to the server
- Social engineering attacks

## Recognition

We appreciate security researchers who help us maintain the security of our project. With your permission, we will acknowledge your contribution in:

- Security advisory (if applicable)
- CONTRIBUTORS.md file
- Release notes

Thank you for helping keep Open Sharia Enterprise secure!
