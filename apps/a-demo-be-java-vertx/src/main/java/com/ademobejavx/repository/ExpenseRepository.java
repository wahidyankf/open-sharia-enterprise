package com.ademobejavx.repository;

import com.ademobejavx.domain.model.Expense;
import io.vertx.core.Future;
import java.util.List;
import java.util.Optional;

public interface ExpenseRepository {

    Future<Expense> save(Expense expense);

    Future<Expense> update(Expense expense);

    Future<Optional<Expense>> findById(String id);

    Future<List<Expense>> findByUserId(String userId);

    Future<Boolean> deleteById(String id);
}
