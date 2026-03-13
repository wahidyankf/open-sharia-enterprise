package com.demobejavx.unit;

import com.demobejavx.auth.PasswordService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PasswordServiceTest {

    private PasswordService passwordService;

    @BeforeEach
    void setUp() {
        passwordService = new PasswordService();
    }

    @Test
    void hash_returnsNonNullHash() {
        String hash = passwordService.hash("Str0ng#Pass1");
        assertNotNull(hash);
    }

    @Test
    void hash_returnsHashDifferentFromPlaintext() {
        String plaintext = "Str0ng#Pass1";
        String hash = passwordService.hash(plaintext);
        assertNotEquals(plaintext, hash);
    }

    @Test
    void verify_correctPassword_returnsTrue() {
        String hash = passwordService.hash("Str0ng#Pass1");
        assertTrue(passwordService.verify("Str0ng#Pass1", hash));
    }

    @Test
    void verify_wrongPassword_returnsFalse() {
        String hash = passwordService.hash("Str0ng#Pass1");
        assertFalse(passwordService.verify("WrongPass#1", hash));
    }

    @Test
    void hash_samePasswordTwice_returnsDifferentHashes() {
        String hash1 = passwordService.hash("Str0ng#Pass1");
        String hash2 = passwordService.hash("Str0ng#Pass1");
        // BCrypt salts are random, so different hashes are expected
        assertNotEquals(hash1, hash2);
    }
}
