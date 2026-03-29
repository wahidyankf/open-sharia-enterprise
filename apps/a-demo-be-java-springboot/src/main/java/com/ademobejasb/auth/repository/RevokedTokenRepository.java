package com.aademobejasb.auth.repository;

import com.aademobejasb.auth.model.RevokedToken;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RevokedTokenRepository extends JpaRepository<RevokedToken, UUID> {
    boolean existsByJti(String jti);
}
