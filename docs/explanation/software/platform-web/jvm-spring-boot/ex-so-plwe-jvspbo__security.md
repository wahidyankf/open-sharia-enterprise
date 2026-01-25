---
title: "Spring Boot Security"
description: Spring Security integration and authentication
category: explanation
subcategory: platform-web
tags:
  - spring-boot
  - spring-security
  - oauth2
  - jwt
  - authentication
related:
  - ./ex-so-plwe-jvspbo__best-practices.md
last_updated: 2026-01-25
---

# Spring Boot Security

## Overview

Comprehensive guide to securing Spring Boot applications with Spring Security, OAuth2, and JWT authentication.

See [Best Practices - Security](./ex-so-plwe-jvspbo__best-practices.md#security-configuration) for detailed security patterns.

## Key Topics

- **SecurityFilterChain** - Security configuration
- **OAuth2/JWT** - Token-based authentication
- **Method Security** - @PreAuthorize annotations
- **CORS Configuration** - Cross-origin support
- **Password Encoding** - BCrypt hashing
- **CSRF Protection** - Token-based CSRF

## Security Configuration

```java
@Configuration
@EnableWebSecurity
public class SecurityConfig {

    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http
            .csrf(csrf -> csrf.ignoringRequestMatchers("/api/public/**"))
            .authorizeHttpRequests(auth -> auth
                .requestMatchers("/api/public/**").permitAll()
                .requestMatchers("/api/v1/zakat/**").hasRole("USER")
                .requestMatchers("/api/v1/admin/**").hasRole("ADMIN")
                .anyRequest().authenticated()
            )
            .oauth2ResourceServer(oauth2 -> oauth2.jwt(Customizer.withDefaults()))
            .sessionManagement(session -> session
                .sessionCreationPolicy(SessionCreationPolicy.STATELESS)
            );

        return http.build();
    }
}
```

---

**Last Updated**: 2026-01-25
