package com.ademobejavx.repository.memory;

import com.ademobejavx.domain.model.User;
import com.ademobejavx.repository.UserRepository;
import io.vertx.core.Future;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import org.jspecify.annotations.Nullable;

public class InMemoryUserRepository implements UserRepository {

    private final ConcurrentHashMap<String, User> store = new ConcurrentHashMap<>();
    private final AtomicLong idSequence = new AtomicLong(1);

    @Override
    public Future<User> save(User user) {
        String id = String.valueOf(idSequence.getAndIncrement());
        User saved = user.withId(id);
        store.put(id, saved);
        return Future.succeededFuture(saved);
    }

    @Override
    public Future<User> update(User user) {
        store.put(user.id(), user);
        return Future.succeededFuture(user);
    }

    @Override
    public Future<Optional<User>> findById(String id) {
        return Future.succeededFuture(Optional.ofNullable(store.get(id)));
    }

    @Override
    public Future<Optional<User>> findByUsername(String username) {
        Optional<User> result = store.values().stream()
                .filter(u -> u.username().equals(username))
                .findFirst();
        return Future.succeededFuture(result);
    }

    @Override
    public Future<List<User>> findAll() {
        return Future.succeededFuture(new ArrayList<>(store.values()));
    }

    @Override
    public Future<List<User>> findByEmail(@Nullable String emailFilter) {
        List<User> result;
        if (emailFilter == null || emailFilter.isBlank()) {
            result = new ArrayList<>(store.values());
        } else {
            String filter = emailFilter.toLowerCase();
            result = store.values().stream()
                    .filter(u -> u.email().toLowerCase().contains(filter))
                    .toList();
        }
        return Future.succeededFuture(result);
    }

    @Override
    public Future<Boolean> existsByUsername(String username) {
        boolean exists = store.values().stream()
                .anyMatch(u -> u.username().equals(username));
        return Future.succeededFuture(exists);
    }

    public void reset() {
        store.clear();
        idSequence.set(1);
    }
}
