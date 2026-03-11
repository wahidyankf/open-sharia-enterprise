package com.organiclever.demojavx.repository.pg;

import com.organiclever.demojavx.domain.model.TokenRevocation;
import com.organiclever.demojavx.repository.TokenRevocationRepository;
import io.vertx.core.Future;
import io.vertx.pgclient.PgPool;
import java.util.List;

public class PgTokenRevocationRepository implements TokenRevocationRepository {

    private final PgPool pool;

    public PgTokenRevocationRepository(PgPool pool) {
        this.pool = pool;
    }

    @Override
    public Future<TokenRevocation> save(TokenRevocation revocation) {
        return Future.failedFuture("PG not implemented yet");
    }

    @Override
    public Future<Boolean> isRevoked(String jti) {
        return Future.failedFuture("PG not implemented yet");
    }

    public Future<List<TokenRevocation>> findByUserId(String userId) {
        return Future.failedFuture("PG not implemented yet");
    }

    @Override
    public Future<Void> deleteByUserId(String userId) {
        return Future.failedFuture("PG not implemented yet");
    }

    public PgPool getPool() {
        return pool;
    }
}
