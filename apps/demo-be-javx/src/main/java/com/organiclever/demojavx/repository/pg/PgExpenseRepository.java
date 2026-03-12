package com.organiclever.demojavx.repository.pg;

import com.organiclever.demojavx.domain.model.Expense;
import com.organiclever.demojavx.repository.ExpenseRepository;
import io.vertx.core.Future;
import io.vertx.sqlclient.Pool;
import java.util.List;
import java.util.Optional;

public class PgExpenseRepository implements ExpenseRepository {

    private final Pool pool;

    public PgExpenseRepository(Pool pool) {
        this.pool = pool;
    }

    @Override
    public Future<Expense> save(Expense expense) {
        return Future.failedFuture("PG not implemented yet");
    }

    @Override
    public Future<Expense> update(Expense expense) {
        return Future.failedFuture("PG not implemented yet");
    }

    @Override
    public Future<Optional<Expense>> findById(String id) {
        return Future.failedFuture("PG not implemented yet");
    }

    @Override
    public Future<List<Expense>> findByUserId(String userId) {
        return Future.failedFuture("PG not implemented yet");
    }

    @Override
    public Future<Boolean> deleteById(String id) {
        return Future.failedFuture("PG not implemented yet");
    }

    public Pool getPool() {
        return pool;
    }
}
