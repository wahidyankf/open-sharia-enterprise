package com.ademobejavx.unit;

import com.ademobejavx.domain.validation.UserValidator;
import com.ademobejavx.domain.validation.ValidationException;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class UserValidatorTest {

    @Test
    void validateEmail_validEmail_doesNotThrow() {
        assertDoesNotThrow(() -> UserValidator.validateEmail("user@example.com"));
    }

    @Test
    void validateEmail_invalidEmail_throwsValidationException() {
        ValidationException ex = assertThrows(ValidationException.class,
                () -> UserValidator.validateEmail("not-an-email"));
        assertEquals("email", ex.getField());
    }

    @Test
    void validateEmail_nullEmail_throwsValidationException() {
        ValidationException ex = assertThrows(ValidationException.class,
                () -> UserValidator.validateEmail(null));
        assertEquals("email", ex.getField());
    }

    @Test
    void validatePassword_strongPassword_doesNotThrow() {
        assertDoesNotThrow(() -> UserValidator.validatePassword("Str0ng#Pass1"));
    }

    @Test
    void validatePassword_emptyPassword_throwsValidationException() {
        ValidationException ex = assertThrows(ValidationException.class,
                () -> UserValidator.validatePassword(""));
        assertEquals("password", ex.getField());
    }

    @Test
    void validatePassword_tooShort_throwsValidationException() {
        ValidationException ex = assertThrows(ValidationException.class,
                () -> UserValidator.validatePassword("Short1!"));
        assertEquals("password", ex.getField());
    }

    @Test
    void validatePassword_noUppercase_throwsValidationException() {
        ValidationException ex = assertThrows(ValidationException.class,
                () -> UserValidator.validatePassword("str0ng#pass1longer"));
        assertEquals("password", ex.getField());
    }

    @Test
    void validatePassword_noSpecialChar_throwsValidationException() {
        ValidationException ex = assertThrows(ValidationException.class,
                () -> UserValidator.validatePassword("AllUpperCase1234"));
        assertEquals("password", ex.getField());
    }

    @Test
    void validateUsername_validUsername_doesNotThrow() {
        assertDoesNotThrow(() -> UserValidator.validateUsername("alice"));
    }

    @Test
    void validateUsername_emptyUsername_throwsValidationException() {
        ValidationException ex = assertThrows(ValidationException.class,
                () -> UserValidator.validateUsername(""));
        assertEquals("username", ex.getField());
    }

    @Test
    void validateRegistration_allValid_doesNotThrow() {
        assertDoesNotThrow(() -> UserValidator.validateRegistration(
                "alice", "alice@example.com", "Str0ng#Pass1"));
    }
}
