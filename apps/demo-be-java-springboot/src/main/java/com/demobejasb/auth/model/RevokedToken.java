package com.demobejasb.auth.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.time.Instant;
import java.util.UUID;

@Entity
@Table(name = "revoked_tokens")
public class RevokedToken {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false, unique = true, length = 512)
    private String token;

    @Column(name = "revoked_at", nullable = false)
    private Instant revokedAt = Instant.now();

    @SuppressWarnings("NullAway")
    protected RevokedToken() {}

    @SuppressWarnings("NullAway")
    public RevokedToken(final String token) {
        this.token = token;
    }

    public UUID getId() {
        return id;
    }

    public String getToken() {
        return token;
    }

    public Instant getRevokedAt() {
        return revokedAt;
    }
}
