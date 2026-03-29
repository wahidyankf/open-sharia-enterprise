package com.ademobejavx.auth;

import org.mindrot.jbcrypt.BCrypt;

public class PasswordService {

    private static final int LOG_ROUNDS = 12;

    public String hash(String plaintext) {
        return BCrypt.hashpw(plaintext, BCrypt.gensalt(LOG_ROUNDS));
    }

    public boolean verify(String plaintext, String hashed) {
        return BCrypt.checkpw(plaintext, hashed);
    }
}
