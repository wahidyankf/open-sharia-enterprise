package com.ademobejavx.domain.validation;

public class DomainException extends RuntimeException {

    private final int statusCode;

    public DomainException(int statusCode, String message) {
        super(message);
        this.statusCode = statusCode;
    }

    public int getStatusCode() {
        return statusCode;
    }
}
