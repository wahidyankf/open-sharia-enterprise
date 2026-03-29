package com.aademobejasb.auth.service;

public class AccountNotActiveException extends Exception {
    public AccountNotActiveException(final String message) {
        super(message);
    }
}
