package com.ademobejavx.repository;

import com.ademobejavx.domain.model.TokenRevocation;
import io.vertx.core.Future;

public interface TokenRevocationRepository {

    Future<TokenRevocation> save(TokenRevocation revocation);

    Future<Boolean> isRevoked(String jti);

    Future<Void> deleteByUserId(String userId);
}
