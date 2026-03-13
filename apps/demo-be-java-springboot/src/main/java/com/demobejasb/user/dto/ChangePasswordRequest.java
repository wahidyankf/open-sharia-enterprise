package com.demobejasb.user.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotBlank;

public record ChangePasswordRequest(
        @NotBlank @JsonProperty("old_password") String oldPassword,
        @NotBlank @JsonProperty("new_password") String newPassword) {}
