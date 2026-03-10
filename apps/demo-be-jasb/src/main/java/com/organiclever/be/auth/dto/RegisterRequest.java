package com.organiclever.be.auth.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

public record RegisterRequest(
    @NotBlank
    @Size(min = 3, max = 50)
    @Pattern(
        regexp = "^[a-zA-Z0-9_]{3,50}$",
        message = "Username must contain only letters, digits, or underscores")
    String username,

    @NotBlank
    @Email(message = "Invalid email format")
    String email,

    @NotBlank
    @Size(min = 12, max = 128)
    @Pattern(
        regexp =
            "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[!@#$%^&*()_+\\-=\\[\\]{};':\"\\\\,.<>/?]).{12,128}$",
        message =
            "Password must be at least 12 characters and contain uppercase, lowercase, digit, and special character")
    String password) {}
