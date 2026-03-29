package com.ademobejasb.auth.service;

public class InvalidTokenException extends Exception {
    public InvalidTokenException(final String message) {
        super(message);
    }
}
