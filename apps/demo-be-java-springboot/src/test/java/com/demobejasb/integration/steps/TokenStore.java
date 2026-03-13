package com.demobejasb.integration.steps;

import java.util.UUID;
import org.jspecify.annotations.Nullable;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope("cucumber-glue")
public class TokenStore {

    @Nullable private String token;
    @Nullable private String refreshToken;
    @Nullable private String originalRefreshToken;
    @Nullable private String adminToken;
    @Nullable private UUID aliceId;
    @Nullable private UUID adminUserId;
    @Nullable private UUID expenseId;
    @Nullable private UUID attachmentId;
    @Nullable private UUID bobExpenseId;

    public void setToken(final String token) {
        this.token = token;
    }

    @Nullable
    public String getToken() {
        return token;
    }

    public void setRefreshToken(final String refreshToken) {
        this.refreshToken = refreshToken;
    }

    @Nullable
    public String getRefreshToken() {
        return refreshToken;
    }

    public void setOriginalRefreshToken(final String originalRefreshToken) {
        this.originalRefreshToken = originalRefreshToken;
    }

    @Nullable
    public String getOriginalRefreshToken() {
        return originalRefreshToken;
    }

    public void setAdminToken(final String adminToken) {
        this.adminToken = adminToken;
    }

    @Nullable
    public String getAdminToken() {
        return adminToken;
    }

    public void setAliceId(final UUID aliceId) {
        this.aliceId = aliceId;
    }

    @Nullable
    public UUID getAliceId() {
        return aliceId;
    }

    public void setAdminUserId(final UUID adminUserId) {
        this.adminUserId = adminUserId;
    }

    @Nullable
    public UUID getAdminUserId() {
        return adminUserId;
    }

    public void setExpenseId(final UUID expenseId) {
        this.expenseId = expenseId;
    }

    @Nullable
    public UUID getExpenseId() {
        return expenseId;
    }

    public void setAttachmentId(final UUID attachmentId) {
        this.attachmentId = attachmentId;
    }

    @Nullable
    public UUID getAttachmentId() {
        return attachmentId;
    }

    public void setBobExpenseId(final UUID bobExpenseId) {
        this.bobExpenseId = bobExpenseId;
    }

    @Nullable
    public UUID getBobExpenseId() {
        return bobExpenseId;
    }

    public void clear() {
        this.token = null;
        this.refreshToken = null;
        this.originalRefreshToken = null;
        this.adminToken = null;
        this.aliceId = null;
        this.adminUserId = null;
        this.expenseId = null;
        this.attachmentId = null;
        this.bobExpenseId = null;
    }
}
