package com.demobejavx.test;

import com.demobejavx.domain.model.User;
import com.demobejavx.repository.UserRepository;
import io.vertx.core.Future;
import io.vertx.sqlclient.Pool;

/**
 * PostgreSQL-backed implementation of {@link TestApiService}.
 *
 * <p>Uses the shared connection pool for raw DELETE statements (reset-db) and {@link
 * UserRepository} for user promotion.
 */
public class PgTestApiService implements TestApiService {

    private final Pool pool;
    private final UserRepository userRepo;

    public PgTestApiService(Pool pool, UserRepository userRepo) {
        this.pool = pool;
        this.userRepo = userRepo;
    }

    @Override
    public Future<Void> resetDb() {
        // Delete in FK-safe order: attachments → expenses → revoked_tokens → users
        return pool.query("DELETE FROM attachments").execute()
                .compose(ignored -> pool.query("DELETE FROM expenses").execute())
                .compose(ignored -> pool.query("DELETE FROM revoked_tokens").execute())
                .compose(ignored -> pool.query("DELETE FROM users").execute())
                .mapEmpty();
    }

    @Override
    public Future<Void> promoteAdmin(String username) {
        return userRepo.findByUsername(username)
                .compose(userOpt -> {
                    if (userOpt.isEmpty()) {
                        return Future.failedFuture(
                                new UserNotFoundException("User not found: " + username));
                    }
                    User updated = userOpt.get().withRole(User.ROLE_ADMIN);
                    return userRepo.update(updated);
                })
                .mapEmpty();
    }
}
