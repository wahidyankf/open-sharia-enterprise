package com.ademobejasb.auth.service;

public class InvalidCredentialsException extends Exception {
    public InvalidCredentialsException() {
        super("Invalid username or password");
    }

    public InvalidCredentialsException(final String message) {
        super(message);
    }
}
