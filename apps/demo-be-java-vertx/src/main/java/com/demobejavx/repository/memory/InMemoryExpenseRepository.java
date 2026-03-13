package com.demobejavx.repository.memory;

import com.demobejavx.domain.model.Expense;
import com.demobejavx.repository.ExpenseRepository;
import io.vertx.core.Future;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

public class InMemoryExpenseRepository implements ExpenseRepository {

    private final ConcurrentHashMap<String, Expense> store = new ConcurrentHashMap<>();
    private final AtomicLong idSequence = new AtomicLong(1);

    @Override
    public Future<Expense> save(Expense expense) {
        String id = String.valueOf(idSequence.getAndIncrement());
        Expense saved = expense.withId(id);
        store.put(id, saved);
        return Future.succeededFuture(saved);
    }

    @Override
    public Future<Expense> update(Expense expense) {
        store.put(expense.id(), expense);
        return Future.succeededFuture(expense);
    }

    @Override
    public Future<Optional<Expense>> findById(String id) {
        return Future.succeededFuture(Optional.ofNullable(store.get(id)));
    }

    @Override
    public Future<List<Expense>> findByUserId(String userId) {
        List<Expense> result = store.values().stream()
                .filter(e -> e.userId().equals(userId))
                .toList();
        return Future.succeededFuture(new ArrayList<>(result));
    }

    @Override
    public Future<Boolean> deleteById(String id) {
        boolean removed = store.remove(id) != null;
        return Future.succeededFuture(removed);
    }

    public void reset() {
        store.clear();
        idSequence.set(1);
    }
}
