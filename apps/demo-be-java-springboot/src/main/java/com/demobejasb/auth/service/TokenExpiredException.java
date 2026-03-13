package com.demobejasb.auth.service;

public class TokenExpiredException extends InvalidTokenException {
    public TokenExpiredException(final String message) {
        super(message);
    }
}
