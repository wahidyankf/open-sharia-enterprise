package com.demobejavx.unit;

import com.auth0.jwt.exceptions.JWTVerificationException;
import com.demobejavx.auth.JwtService;
import com.demobejavx.domain.model.User;
import java.time.Instant;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

class JwtServiceTest {

    private JwtService jwtService;
    private User testUser;

    @BeforeEach
    void setUp() {
        jwtService = new JwtService("test-secret-32-chars-or-more-here!!");
        testUser = new User("1", "alice", "alice@example.com", "Alice",
                "hash", User.ROLE_USER, User.STATUS_ACTIVE, 0, Instant.now());
    }

    @Test
    void generateTokenPair_returnsNonNullTokens() {
        JwtService.TokenPair pair = jwtService.generateTokenPair(testUser);
        assertNotNull(pair.accessToken());
        assertNotNull(pair.refreshToken());
        assertNotNull(pair.accessJti());
        assertNotNull(pair.refreshJti());
    }

    @Test
    void validate_validAccessToken_returnsClaims() {
        JwtService.TokenPair pair = jwtService.generateTokenPair(testUser);
        JwtService.Claims claims = jwtService.validate(pair.accessToken());
        assertEquals("1", claims.subject());
        assertEquals("access", claims.type());
        assertEquals(User.ROLE_USER, claims.role());
    }

    @Test
    void validate_validRefreshToken_returnsClaims() {
        JwtService.TokenPair pair = jwtService.generateTokenPair(testUser);
        JwtService.Claims claims = jwtService.validate(pair.refreshToken());
        assertEquals("1", claims.subject());
        assertEquals("refresh", claims.type());
    }

    @Test
    void validate_expiredToken_throwsException() {
        String expiredToken = jwtService.generateExpiredRefreshToken(testUser);
        assertThrows(JWTVerificationException.class,
                () -> jwtService.validate(expiredToken));
    }

    @Test
    void validate_invalidToken_throwsException() {
        assertThrows(JWTVerificationException.class,
                () -> jwtService.validate("not.a.valid.token"));
    }

    @Test
    void getJwks_returnsJsonWithKeys() {
        String jwks = jwtService.getJwks();
        assertNotNull(jwks);
        org.junit.jupiter.api.Assertions.assertTrue(jwks.contains("\"keys\""));
        org.junit.jupiter.api.Assertions.assertTrue(jwks.contains("\"RSA\""));
    }

    @Test
    void decode_validToken_returnsClaims() {
        JwtService.TokenPair pair = jwtService.generateTokenPair(testUser);
        JwtService.Claims claims = jwtService.decode(pair.accessToken());
        assertEquals("1", claims.subject());
    }

    @Test
    void generateTokenPair_adminUser_hasAdminRole() {
        User admin = new User("2", "admin", "admin@example.com", "Admin",
                "hash", User.ROLE_ADMIN, User.STATUS_ACTIVE, 0, Instant.now());
        JwtService.TokenPair pair = jwtService.generateTokenPair(admin);
        JwtService.Claims claims = jwtService.validate(pair.accessToken());
        assertEquals(User.ROLE_ADMIN, claims.role());
    }
}
