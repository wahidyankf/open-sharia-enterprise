package com.ademobejasb.test.controller;

/** Thrown when a user referenced in a test-support request does not exist. */
public class UserNotFoundException extends RuntimeException {

    public UserNotFoundException(final String message) {
        super(message);
    }
}
