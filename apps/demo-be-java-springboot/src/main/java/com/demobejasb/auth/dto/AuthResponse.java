package com.demobejasb.auth.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public record AuthResponse(
    @JsonProperty("access_token") String accessToken,
    @JsonProperty("refresh_token") String refreshToken,
    @JsonProperty("token_type") String tokenType) {

    public static AuthResponse bearer(final String accessToken, final String refreshToken) {
        return new AuthResponse(accessToken, refreshToken, "Bearer");
    }
}
