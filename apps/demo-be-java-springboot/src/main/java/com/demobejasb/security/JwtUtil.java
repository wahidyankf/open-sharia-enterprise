package com.demobejasb.security;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.Keys;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Date;
import java.util.Map;
import java.util.UUID;
import javax.crypto.SecretKey;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class JwtUtil {

    private final SecretKey signingKey;
    private final long expirationMs;
    private final String issuer;
    private final String secretBase64;

    public JwtUtil(
            @Value("${app.jwt.secret}") final String secret,
            @Value("${app.jwt.expiration-ms:86400000}") final long expirationMs,
            @Value("${app.jwt.issuer:demo-be}") final String issuer) {
        this.signingKey = Keys.hmacShaKeyFor(secret.getBytes(StandardCharsets.UTF_8));
        this.expirationMs = expirationMs;
        this.issuer = issuer;
        this.secretBase64 = Base64.getUrlEncoder().withoutPadding()
                .encodeToString(secret.getBytes(StandardCharsets.UTF_8));
    }

    public String generateAccessToken(final String username, final UUID userId) {
        return Jwts.builder()
                .subject(userId.toString())
                .issuer(issuer)
                .claim("username", username)
                .issuedAt(new Date())
                .expiration(new Date(System.currentTimeMillis() + expirationMs))
                .signWith(signingKey)
                .compact();
    }

    public String generateToken(final String username) {
        return Jwts.builder()
                .subject(username)
                .issuer(issuer)
                .issuedAt(new Date())
                .expiration(new Date(System.currentTimeMillis() + expirationMs))
                .signWith(signingKey)
                .compact();
    }

    public String generateRefreshToken() {
        return UUID.randomUUID().toString() + "-" + UUID.randomUUID().toString();
    }

    public String extractUsername(final String token) {
        Claims claims = parseClaims(token);
        Object username = claims.get("username");
        if (username != null) {
            return username.toString();
        }
        return claims.getSubject();
    }

    public Claims extractClaims(final String token) {
        return parseClaims(token);
    }

    public boolean isTokenValid(final String token) {
        try {
            parseClaims(token);
            return true;
        } catch (JwtException | IllegalArgumentException e) {
            return false;
        }
    }

    public Map<String, Object> getJwksKey() {
        return Map.of(
                "kty", "oct",
                "use", "sig",
                "alg", "HS256",
                "k", secretBase64);
    }

    public String getIssuer() {
        return issuer;
    }

    private Claims parseClaims(final String token) {
        return Jwts.parser()
                .verifyWith(signingKey)
                .build()
                .parseSignedClaims(token)
                .getPayload();
    }
}
