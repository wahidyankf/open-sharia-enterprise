package com.demobektkt.auth

import org.mindrot.jbcrypt.BCrypt

class PasswordService {
  fun hash(plaintext: String): String = BCrypt.hashpw(plaintext, BCrypt.gensalt())

  fun verify(plaintext: String, hash: String): Boolean =
    runCatching { BCrypt.checkpw(plaintext, hash) }.getOrDefault(false)
}
