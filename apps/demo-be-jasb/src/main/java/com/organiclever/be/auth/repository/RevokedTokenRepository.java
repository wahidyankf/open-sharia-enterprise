package com.organiclever.be.auth.repository;

import com.organiclever.be.auth.model.RevokedToken;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RevokedTokenRepository extends JpaRepository<RevokedToken, UUID> {
    boolean existsByToken(String token);
}
