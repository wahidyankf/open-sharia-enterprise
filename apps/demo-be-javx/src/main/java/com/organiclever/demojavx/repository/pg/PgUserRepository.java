package com.organiclever.demojavx.repository.pg;

import com.organiclever.demojavx.domain.model.User;
import com.organiclever.demojavx.repository.UserRepository;
import io.vertx.core.Future;
import io.vertx.pgclient.PgPool;
import java.util.List;
import java.util.Optional;
import org.jspecify.annotations.Nullable;

public class PgUserRepository implements UserRepository {

    private final PgPool pool;

    public PgUserRepository(PgPool pool) {
        this.pool = pool;
    }

    @Override
    public Future<User> save(User user) {
        return Future.failedFuture("PG not implemented yet");
    }

    @Override
    public Future<User> update(User user) {
        return Future.failedFuture("PG not implemented yet");
    }

    @Override
    public Future<Optional<User>> findById(String id) {
        return Future.failedFuture("PG not implemented yet");
    }

    @Override
    public Future<Optional<User>> findByUsername(String username) {
        return Future.failedFuture("PG not implemented yet");
    }

    @Override
    public Future<List<User>> findAll() {
        return Future.failedFuture("PG not implemented yet");
    }

    @Override
    public Future<List<User>> findByEmail(@Nullable String emailFilter) {
        return Future.failedFuture("PG not implemented yet");
    }

    @Override
    public Future<Boolean> existsByUsername(String username) {
        return Future.failedFuture("PG not implemented yet");
    }

    public PgPool getPool() {
        return pool;
    }
}
