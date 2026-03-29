package com.ademobejavx.domain.model;

import java.time.Instant;

public record TokenRevocation(
        String jti,
        String userId,
        Instant revokedAt) {
}
