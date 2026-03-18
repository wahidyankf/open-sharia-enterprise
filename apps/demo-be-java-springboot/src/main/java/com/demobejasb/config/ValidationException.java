package com.demobejasb.config;

/** Thrown when request data fails programmatic validation. */
public class ValidationException extends RuntimeException {
    private final String field;

    public ValidationException(final String message, final String field) {
        super(message);
        this.field = field;
    }

    public String getField() {
        return field;
    }
}
