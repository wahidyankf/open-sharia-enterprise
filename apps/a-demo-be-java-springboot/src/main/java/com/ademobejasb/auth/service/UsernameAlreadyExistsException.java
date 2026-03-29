package com.ademobejasb.auth.service;

public class UsernameAlreadyExistsException extends Exception {
    public UsernameAlreadyExistsException(final String username) {
        super("Username already exists: " + username);
    }
}
