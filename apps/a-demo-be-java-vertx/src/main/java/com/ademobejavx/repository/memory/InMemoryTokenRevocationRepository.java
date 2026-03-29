package com.ademobejavx.repository.memory;

import com.ademobejavx.domain.model.TokenRevocation;
import com.ademobejavx.repository.TokenRevocationRepository;
import io.vertx.core.Future;
import java.util.concurrent.ConcurrentHashMap;

public class InMemoryTokenRevocationRepository implements TokenRevocationRepository {

    private final ConcurrentHashMap<String, TokenRevocation> store = new ConcurrentHashMap<>();

    @Override
    public Future<TokenRevocation> save(TokenRevocation revocation) {
        store.put(revocation.jti(), revocation);
        return Future.succeededFuture(revocation);
    }

    @Override
    public Future<Boolean> isRevoked(String jti) {
        return Future.succeededFuture(store.containsKey(jti));
    }

    @Override
    public Future<Void> deleteByUserId(String userId) {
        store.values().removeIf(t -> t.userId().equals(userId));
        return Future.succeededFuture();
    }

    public void reset() {
        store.clear();
    }
}
