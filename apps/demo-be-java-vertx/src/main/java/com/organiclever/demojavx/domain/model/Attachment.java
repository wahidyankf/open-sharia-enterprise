package com.organiclever.demojavx.domain.model;

import java.time.Instant;

public record Attachment(
        String id,
        String expenseId,
        String userId,
        String filename,
        String contentType,
        long size,
        byte[] data,
        Instant createdAt) {

    public Attachment withId(String newId) {
        return new Attachment(newId, expenseId, userId, filename, contentType, size, data,
                createdAt);
    }
}
