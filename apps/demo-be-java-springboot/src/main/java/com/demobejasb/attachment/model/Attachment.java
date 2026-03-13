package com.demobejasb.attachment.model;

import com.demobejasb.expense.model.Expense;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import java.time.Instant;
import java.util.UUID;

@Entity
@Table(name = "attachments")
public class Attachment {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "expense_id", nullable = false)
    private Expense expense;

    @Column(nullable = false, length = 255)
    private String filename;

    @Column(name = "content_type", nullable = false, length = 100)
    private String contentType;

    @Column(nullable = false)
    private long size;

    @Column(nullable = false, columnDefinition = "bytea")
    private byte[] data;

    @Column(name = "created_at", nullable = false)
    private Instant createdAt = Instant.now();

    @SuppressWarnings("NullAway")
    protected Attachment() {}

    @SuppressWarnings("NullAway")
    public Attachment(
            final Expense expense,
            final String filename,
            final String contentType,
            final long size,
            final byte[] data) {
        this.expense = expense;
        this.filename = filename;
        this.contentType = contentType;
        this.size = size;
        this.data = data;
    }

    public UUID getId() {
        return id;
    }

    public Expense getExpense() {
        return expense;
    }

    public String getFilename() {
        return filename;
    }

    public String getContentType() {
        return contentType;
    }

    public long getSize() {
        return size;
    }

    public byte[] getData() {
        return data;
    }

    public Instant getCreatedAt() {
        return createdAt;
    }
}
