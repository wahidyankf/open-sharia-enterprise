---
title: "Security Practices"
date: 2026-02-03T00:00:00+07:00
draft: false
description: Implement defense-in-depth security with input validation, encryption, authentication, and protection against OWASP Top 10 vulnerabilities
weight: 10000009
tags: ["java", "security", "owasp", "cryptography", "input-validation", "authentication"]
---

## Understanding Java Security

Security is not a feature - it's a property of the entire system. Java applications face threats from injection attacks, broken authentication, sensitive data exposure, and more. Defense-in-depth requires layered security controls at every tier.

**Why security must be systematic:**

- **Attackers need one vulnerability**: Defenders must secure everything
- **Bugs become exploits**: Security flaws enable unauthorized access
- **Compliance requirements**: GDPR, HIPAA, PCI-DSS mandate security
- **Reputation damage**: Breaches destroy trust and business

This guide covers essential security patterns: input validation, cryptography, authentication, password management, and OWASP Top 10 mitigations.

## Input Validation - First Line of Defense

**Problem**: Trusting user input enables injection attacks (SQL injection, XSS, command injection), buffer overflows, and data corruption.

**Recognition signals:**

- Concatenating user input into SQL queries
- Passing user input to system commands
- Accepting any string length without bounds
- No type validation before processing
- Rendering user input in HTML without escaping

**Solution**: Validate all input at system boundaries with allowlist-based validation.

| Characteristic      | Unsafe Approach             | Secure Approach               |
| ------------------- | --------------------------- | ----------------------------- |
| Validation strategy | Blacklist (block known bad) | Allowlist (permit known good) |
| Location            | Business logic layer        | Entry point (boundary)        |
| SQL queries         | String concatenation        | Parameterized queries         |
| Command execution   | Direct string interpolation | Avoid or strictly validate    |
| Data binding        | Trust framework defaults    | Explicit validation rules     |

### SQL Injection Prevention

```java
// VULNERABLE: SQL Injection
public User findUser(String username) {
    String query = "SELECT * FROM users WHERE username = '" + username + "'";
    // ATTACK: username = "admin' OR '1'='1"
    // RESULT: SELECT * FROM users WHERE username = 'admin' OR '1'='1'
    return jdbcTemplate.queryForObject(query, userRowMapper);
}

// SECURE: Parameterized Queries
public User findUser(String username) {
    String query = "SELECT * FROM users WHERE username = ?";
    return jdbcTemplate.queryForObject(query, userRowMapper, username);
    // Parameter binding prevents SQL injection
}
```

### XSS Prevention

```java
// VULNERABLE: Stored XSS
public String displayComment(String userComment) {
    return "<div>" + userComment + "</div>";
    // ATTACK: userComment = "<script>alert('XSS')</script>"
}

// SECURE: HTML Escaping
import org.apache.commons.text.StringEscapeUtils;

public String displayComment(String userComment) {
    String escaped = StringEscapeUtils.escapeHtml4(userComment);
    return "<div>" + escaped + "</div>";
    // OUTPUT: <div>&lt;script&gt;alert('XSS')&lt;/script&gt;</div>
}
```

### Command Injection Prevention

```java
// VULNERABLE: Command Injection
public void processFile(String filename) {
    String command = "cat " + filename;
    Runtime.getRuntime().exec(command);  // DANGEROUS
    // ATTACK: filename = "file.txt; rm -rf /"
}

// SECURE: Avoid Shell Execution
public void processFile(Path filepath) {
    // VALIDATE: File exists and is within allowed directory
    if (!filepath.startsWith(ALLOWED_DIRECTORY)) {
        throw new SecurityException("Access denied");
    }
    // READ DIRECTLY: No shell involved
    String content = Files.readString(filepath);
}
```

### Input Validation Pattern

```java
public record UserInput(String username, String email, int age) {
    private static final Pattern USERNAME_PATTERN =
        Pattern.compile("^[a-zA-Z0-9_]{3,20}$");
    private static final Pattern EMAIL_PATTERN =
        Pattern.compile("^[^@]+@[^@]+\\.[^@]+$");

    public UserInput {  // COMPACT CONSTRUCTOR
        // ALLOWLIST: Only alphanumeric and underscore
        if (!USERNAME_PATTERN.matcher(username).matches()) {
            throw new IllegalArgumentException("Invalid username format");
        }
        // EMAIL VALIDATION
        if (!EMAIL_PATTERN.matcher(email).matches()) {
            throw new IllegalArgumentException("Invalid email format");
        }
        // RANGE CHECK
        if (age < 0 || age > 150) {
            throw new IllegalArgumentException("Invalid age");
        }
    }
}
```

**Validation principles:**

- **Validate at boundaries**: Entry points, not business logic
- **Allowlist over blacklist**: Define what's permitted
- **Type safety**: Convert strings to typed objects early
- **Length limits**: Prevent buffer-related issues
- **Encode outputs**: Escape based on context (HTML, SQL, JavaScript)

## Cryptography - Protecting Data at Rest and in Transit

**Problem**: Weak cryptography (DES, MD5, SHA-1, ECB mode) provides false sense of security. Custom crypto implementations have subtle flaws.

**Recognition signals:**

- Using DES, 3DES, or RC4 ciphers
- MD5 or SHA-1 for hashing
- ECB mode for block ciphers
- Hardcoded encryption keys
- Custom cryptographic algorithms
- Ignoring certificate validation in SSL/TLS

**Solution**: Use strong, modern algorithms with recommended configurations. Never implement custom crypto.

### Encryption Standards

| Purpose               | Weak/Deprecated | Strong/Modern          |
| --------------------- | --------------- | ---------------------- |
| Symmetric encryption  | DES, 3DES, RC4  | AES-256-GCM            |
| Asymmetric encryption | RSA < 2048 bits | RSA 2048+ bits, ECC    |
| Hashing               | MD5, SHA-1      | SHA-256, SHA-3         |
| Password hashing      | Plain hashing   | bcrypt, Argon2, PBKDF2 |
| Block cipher mode     | ECB             | GCM, CBC with HMAC     |

### AES-GCM Encryption

```java
import javax.crypto.*;
import javax.crypto.spec.*;
import java.security.SecureRandom;

public class SecureEncryption {
    private static final String ALGORITHM = "AES/GCM/NoPadding";
    private static final int GCM_IV_LENGTH = 12;  // 96 bits
    private static final int GCM_TAG_LENGTH = 128;  // 128 bits

    public record EncryptedData(byte[] ciphertext, byte[] iv) {}

    public static EncryptedData encrypt(byte[] plaintext, SecretKey key)
            throws GeneralSecurityException {
        // RANDOM IV: Never reuse IV with same key
        byte[] iv = new byte[GCM_IV_LENGTH];
        SecureRandom random = new SecureRandom();
        random.nextBytes(iv);

        Cipher cipher = Cipher.getInstance(ALGORITHM);
        GCMParameterSpec spec = new GCMParameterSpec(GCM_TAG_LENGTH, iv);
        cipher.init(Cipher.ENCRYPT_MODE, key, spec);

        byte[] ciphertext = cipher.doFinal(plaintext);
        return new EncryptedData(ciphertext, iv);  // RETURN IV with ciphertext
    }

    public static byte[] decrypt(EncryptedData data, SecretKey key)
            throws GeneralSecurityException {
        Cipher cipher = Cipher.getInstance(ALGORITHM);
        GCMParameterSpec spec = new GCMParameterSpec(GCM_TAG_LENGTH, data.iv());
        cipher.init(Cipher.DECRYPT_MODE, key, spec);

        return cipher.doFinal(data.ciphertext());
        // GCM verifies authenticity: throws exception if tampered
    }

    public static SecretKey generateKey() throws NoSuchAlgorithmException {
        KeyGenerator keyGen = KeyGenerator.getInstance("AES");
        keyGen.init(256);  // AES-256
        return keyGen.generateKey();
    }
}
```

**Critical requirements:**

- **Random IVs**: Never reuse initialization vectors
- **Authenticated encryption**: GCM provides confidentiality + integrity
- **Key management**: Store keys securely (key management service, HSM)
- **Key rotation**: Periodically rotate encryption keys

### SSL/TLS Best Practices

```java
// VULNERABLE: Disabling certificate validation
SSLContext sc = SSLContext.getInstance("TLS");
sc.init(null, trustAllCerts, new SecureRandom());  // DANGEROUS

// SECURE: Use default trust store
SSLContext sc = SSLContext.getInstance("TLSv1.3");
sc.init(null, null, null);  // Uses system's trusted CAs

HttpsURLConnection conn = (HttpsURLConnection) url.openConnection();
conn.setSSLSocketFactory(sc.getSocketFactory());
// VALIDATES: Certificate chain, hostname, expiration
```

**TLS configuration:**

- **Minimum TLS 1.2**: Disable SSL, TLS 1.0, TLS 1.1
- **Strong cipher suites**: Prefer AEAD ciphers (GCM)
- **Certificate validation**: Never disable hostname/certificate verification
- **Certificate pinning**: For mobile apps, pin leaf or CA certificates

## Authentication - Verifying Identity

**Problem**: Weak authentication allows unauthorized access. Broken session management enables session hijacking.

**Recognition signals:**

- Accepting weak passwords
- Storing passwords in plaintext or weakly hashed
- No multi-factor authentication
- Sessions never expire
- Session IDs in URLs
- No CSRF protection

**Solution**: Strong authentication mechanisms with defense-in-depth.

### Password Security

**Never store plaintext passwords**. Use adaptive hashing with salt.

```java
import at.favre.lib.crypto.bcrypt.BCrypt;

public class PasswordManager {
    private static final int BCRYPT_COST = 12;  // 2^12 iterations

    public static String hashPassword(String plaintext) {
        return BCrypt.withDefaults()
            .hashToString(BCRYPT_COST, plaintext.toCharArray());
        // SALT: Automatically generated and stored in hash
        // OUTPUT: $2a$12$randomsalt...hashedpassword
    }

    public static boolean verifyPassword(String plaintext, String hash) {
        BCrypt.Result result = BCrypt.verifyer()
            .verify(plaintext.toCharArray(), hash);
        return result.verified;
        // CONSTANT-TIME: Prevents timing attacks
    }
}
```

**Password requirements:**

| Characteristic   | Weak                | Strong                              |
| ---------------- | ------------------- | ----------------------------------- |
| Minimum length   | < 8 characters      | 12+ characters                      |
| Complexity       | Single type         | Mixed (upper, lower, digit, symbol) |
| Storage          | Plaintext/MD5/SHA-1 | bcrypt/Argon2/PBKDF2                |
| Reuse prevention | Not checked         | Check against breached passwords    |
| Rotation         | Forced monthly      | Only on compromise                  |

### Session Management

```java
// VULNERABLE: Predictable session IDs
String sessionId = username + "_" + System.currentTimeMillis();

// SECURE: Cryptographically random session IDs
import java.security.SecureRandom;

public class SessionManager {
    private static final SecureRandom random = new SecureRandom();
    private static final int TOKEN_LENGTH = 32;  // 256 bits

    public static String generateSessionToken() {
        byte[] token = new byte[TOKEN_LENGTH];
        random.nextBytes(token);
        return Base64.getUrlEncoder().withoutPadding().encodeToString(token);
        // OUTPUT: URL-safe, 256-bit random token
    }

    public static void setSessionCookie(HttpServletResponse response, String token) {
        Cookie cookie = new Cookie("SESSIONID", token);
        cookie.setHttpOnly(true);  // PREVENT: JavaScript access (XSS protection)
        cookie.setSecure(true);  // REQUIRE: HTTPS only
        cookie.setPath("/");
        cookie.setMaxAge(1800);  // 30 minutes
        cookie.setSameSite("Strict");  // CSRF protection
        response.addCookie(cookie);
    }
}
```

**Session security checklist:**

- ✓ Cryptographically random session IDs (256+ bits)
- ✓ HttpOnly cookie flag (prevents XSS theft)
- ✓ Secure cookie flag (HTTPS only)
- ✓ SameSite=Strict/Lax (CSRF protection)
- ✓ Session expiration (idle timeout + absolute timeout)
- ✓ Session fixation protection (regenerate ID after login)

### Multi-Factor Authentication

```java
import com.warrenstrange.googleauth.*;

public class TwoFactorAuth {
    private final GoogleAuthenticator gAuth = new GoogleAuthenticator();

    public String generateSecret() {
        GoogleAuthenticatorKey key = gAuth.createCredentials();
        return key.getKey();  // Share with user (QR code)
    }

    public boolean verifyCode(String secret, int code) {
        return gAuth.authorize(secret, code);
        // TOTP: Time-based One-Time Password
        // WINDOW: Accepts codes within ±30 seconds
    }
}
```

## OWASP Top 10 Mitigations

### 1. Injection Attacks

**Mitigation**:

- Parameterized queries (PreparedStatement)
- Input validation (allowlist)
- Least privilege database accounts
- ORM frameworks (with safe usage)

### 2. Broken Authentication

**Mitigation**:

- Strong password hashing (bcrypt, Argon2)
- Multi-factor authentication
- Secure session management
- Account lockout after failed attempts

### 3. Sensitive Data Exposure

**Mitigation**:

- Encrypt data at rest (AES-256-GCM)
- Encrypt data in transit (TLS 1.2+)
- Never log sensitive data (passwords, tokens)
- Secure key management

### 4. XML External Entities (XXE)

```java
// VULNERABLE: XXE Attack
DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
DocumentBuilder builder = factory.newDocumentBuilder();
Document doc = builder.parse(untrustedXML);  // DANGEROUS

// SECURE: Disable external entities
DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
factory.setFeature("http://xml.org/sax/features/external-general-entities", false);
factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
factory.setXIncludeAware(false);
factory.setExpandEntityReferences(false);
```

### 5. Broken Access Control

**Mitigation**:

- Deny by default
- Enforce access control on every request
- Disable directory listing
- Check permissions server-side (never client-side)

```java
public void deleteUser(String userId, User currentUser) {
    // AUTHORIZATION CHECK
    if (!currentUser.hasRole("ADMIN")) {
        throw new AccessDeniedException("Insufficient privileges");
    }
    // OWNERSHIP CHECK
    if (userId.equals(currentUser.getId()) && !currentUser.isSuperAdmin()) {
        throw new AccessDeniedException("Cannot delete own account");
    }
    userRepository.delete(userId);
}
```

### 6. Security Misconfiguration

**Mitigation**:

- Remove default accounts
- Disable unnecessary features
- Keep frameworks updated
- Use security headers (CSP, HSTS, X-Frame-Options)

```java
// Security Headers
response.setHeader("Content-Security-Policy", "default-src 'self'");
response.setHeader("X-Frame-Options", "DENY");
response.setHeader("X-Content-Type-Options", "nosniff");
response.setHeader("Strict-Transport-Security", "max-age=31536000; includeSubDomains");
```

### 7. Cross-Site Scripting (XSS)

**Mitigation**:

- Escape all user-generated content
- Content Security Policy headers
- HttpOnly cookies
- Modern frameworks with auto-escaping

### 8. Insecure Deserialization

```java
// VULNERABLE: Arbitrary code execution
ObjectInputStream ois = new ObjectInputStream(untrustedInput);
Object obj = ois.readObject();  // DANGEROUS: Can execute malicious code

// SECURE: Avoid Java serialization with untrusted data
// Alternative: Use JSON/XML with schema validation
ObjectMapper mapper = new ObjectMapper();
MyClass obj = mapper.readValue(jsonInput, MyClass.class);  // SAFE
```

### 9. Using Components with Known Vulnerabilities

**Mitigation**:

- Dependency scanning (OWASP Dependency-Check, Snyk)
- Keep dependencies updated
- Monitor security advisories
- Remove unused dependencies

```xml
<!-- Maven: Check for vulnerabilities -->
<plugin>
    <groupId>org.owasp</groupId>
    <artifactId>dependency-check-maven</artifactId>
    <executions>
        <execution>
            <goals><goal>check</goal></goals>
        </execution>
    </executions>
</plugin>
```

### 10. Insufficient Logging & Monitoring

**Mitigation**:

- Log authentication failures
- Log access control failures
- Log input validation failures
- Centralized logging
- Alert on suspicious patterns

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SecurityLogger {
    private static final Logger securityLog = LoggerFactory.getLogger("SECURITY");

    public static void logFailedLogin(String username, String ipAddress) {
        securityLog.warn("Failed login attempt: username={}, ip={}",
            username, ipAddress);
        // NEVER LOG: Passwords, tokens, sensitive data
    }

    public static void logAccessDenied(String userId, String resource) {
        securityLog.warn("Access denied: user={}, resource={}",
            userId, resource);
    }
}
```

## Security Checklist

### Input Validation

- [ ] All user input validated at entry points
- [ ] Parameterized queries for SQL
- [ ] HTML escaping for output
- [ ] Length limits enforced
- [ ] Type validation before processing

### Cryptography

- [ ] AES-256-GCM for symmetric encryption
- [ ] Random IVs for each encryption
- [ ] bcrypt/Argon2 for password hashing
- [ ] TLS 1.2+ for network communication
- [ ] Secure key management (not hardcoded)

### Authentication & Authorization

- [ ] Strong password requirements
- [ ] Multi-factor authentication for sensitive operations
- [ ] Secure session management
- [ ] Authorization checks on every request
- [ ] Account lockout after failed attempts

### Configuration

- [ ] Security headers configured
- [ ] Default accounts removed
- [ ] Error messages don't leak information
- [ ] Dependency vulnerabilities monitored
- [ ] Logging configured (excluding sensitive data)

## Conclusion

Java security requires defense-in-depth:

- **Input validation**: Never trust user input
- **Cryptography**: Use strong, standard algorithms
- **Authentication**: Verify identity with multiple factors
- **Authorization**: Check permissions on every operation
- **OWASP Top 10**: Understand and mitigate common vulnerabilities

Security is not optional. Integrate security practices throughout the development lifecycle: design review, secure coding, testing, and monitoring. Regular security audits and penetration testing identify weaknesses before attackers do.
