package com.demobejavx.domain.validation;

import java.util.regex.Pattern;

public final class UserValidator {

    private static final Pattern EMAIL_PATTERN =
            Pattern.compile("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$");
    private static final int MIN_PASSWORD_LENGTH = 12;

    private UserValidator() {
    }

    public static void validateUsername(String username) {
        if (username == null || username.isBlank()) {
            throw new ValidationException("username", "Username must not be empty");
        }
    }

    public static void validateEmail(String email) {
        if (email == null || !EMAIL_PATTERN.matcher(email).matches()) {
            throw new ValidationException("email", "Invalid email format");
        }
    }

    public static void validatePassword(String password) {
        if (password == null || password.isEmpty()) {
            throw new ValidationException("password", "Password must not be empty");
        }
        if (password.length() < MIN_PASSWORD_LENGTH) {
            throw new ValidationException("password",
                    "Password must be at least " + MIN_PASSWORD_LENGTH + " characters");
        }
        if (!password.chars().anyMatch(Character::isUpperCase)) {
            throw new ValidationException("password",
                    "Password must contain at least one uppercase letter");
        }
        if (!password.chars().anyMatch(c -> !Character.isLetterOrDigit(c))) {
            throw new ValidationException("password",
                    "Password must contain at least one special character");
        }
    }

    public static void validateRegistration(String username, String email, String password) {
        validateUsername(username);
        validateEmail(email);
        validatePassword(password);
    }
}
