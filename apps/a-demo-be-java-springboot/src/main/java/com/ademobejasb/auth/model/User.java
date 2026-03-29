package com.ademobejasb.auth.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.time.Instant;
import java.util.UUID;
import org.jspecify.annotations.Nullable;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "users")
@EntityListeners(AuditingEntityListener.class)
public class User {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false, unique = true, length = 50)
    private String username;

    @Column(name = "password_hash", nullable = false)
    private String passwordHash;

    @Column(unique = true, length = 255)
    private @Nullable String email;

    @Column(name = "display_name", length = 255)
    private @Nullable String displayName;

    @Column(nullable = false, length = 20)
    private String role = "USER";

    @Column(nullable = false, length = 20)
    private String status = "ACTIVE";

    @Column(name = "failed_login_attempts", nullable = false)
    private int failedLoginAttempts = 0;

    @Column(name = "password_reset_token", length = 255)
    private @Nullable String passwordResetToken;

    @CreatedDate
    @Column(name = "created_at", nullable = false, updatable = false)
    private Instant createdAt;

    @CreatedBy
    @Column(name = "created_by", nullable = false, updatable = false, length = 255)
    private String createdBy;

    @LastModifiedDate
    @Column(name = "updated_at", nullable = false)
    private Instant updatedAt;

    @LastModifiedBy
    @Column(name = "updated_by", nullable = false, length = 255)
    private String updatedBy;

    @Column(name = "deleted_at")
    private @Nullable Instant deletedAt;

    @Column(name = "deleted_by", length = 255)
    private @Nullable String deletedBy;

    @SuppressWarnings("NullAway")
    protected User() {
        this.username = "";
        this.passwordHash = "";
        this.createdAt = Instant.EPOCH;
        this.createdBy = "";
        this.updatedAt = Instant.EPOCH;
        this.updatedBy = "";
    }

    @SuppressWarnings("NullAway")
    public User(final String username, final String email, final String passwordHash) {
        this.username = username;
        this.email = email;
        this.passwordHash = passwordHash;
        this.displayName = username;
    }

    public UUID getId() {
        return id;
    }

    public String getUsername() {
        return username;
    }

    public String getPasswordHash() {
        return passwordHash;
    }

    public void setPasswordHash(final String passwordHash) {
        this.passwordHash = passwordHash;
    }

    public @Nullable String getEmail() {
        return email;
    }

    public void setEmail(final @Nullable String email) {
        this.email = email;
    }

    public @Nullable String getDisplayName() {
        return displayName;
    }

    public void setDisplayName(final @Nullable String displayName) {
        this.displayName = displayName;
    }

    public String getRole() {
        return role;
    }

    public void setRole(final String role) {
        this.role = role;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(final String status) {
        this.status = status;
    }

    public int getFailedLoginAttempts() {
        return failedLoginAttempts;
    }

    public void setFailedLoginAttempts(final int failedLoginAttempts) {
        this.failedLoginAttempts = failedLoginAttempts;
    }

    public @Nullable String getPasswordResetToken() {
        return passwordResetToken;
    }

    public void setPasswordResetToken(final @Nullable String passwordResetToken) {
        this.passwordResetToken = passwordResetToken;
    }

    public Instant getCreatedAt() {
        return createdAt;
    }

    public String getCreatedBy() {
        return createdBy;
    }

    public Instant getUpdatedAt() {
        return updatedAt;
    }

    public String getUpdatedBy() {
        return updatedBy;
    }

    public @Nullable Instant getDeletedAt() {
        return deletedAt;
    }

    public @Nullable String getDeletedBy() {
        return deletedBy;
    }
}
