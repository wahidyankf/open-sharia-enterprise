package com.demobejasb.auth.service;

public class InvalidTokenException extends Exception {
    public InvalidTokenException(final String message) {
        super(message);
    }
}
