package com.organiclever.demojavx.repository;

import com.organiclever.demojavx.domain.model.User;
import io.vertx.core.Future;
import java.util.List;
import java.util.Optional;
import org.jspecify.annotations.Nullable;

public interface UserRepository {

    Future<User> save(User user);

    Future<User> update(User user);

    Future<Optional<User>> findById(String id);

    Future<Optional<User>> findByUsername(String username);

    Future<List<User>> findAll();

    Future<List<User>> findByEmail(@Nullable String emailFilter);

    Future<Boolean> existsByUsername(String username);
}
