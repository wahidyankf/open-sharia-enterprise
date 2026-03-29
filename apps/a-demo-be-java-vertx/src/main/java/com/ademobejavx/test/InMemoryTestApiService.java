package com.ademobejavx.test;

import com.ademobejavx.domain.model.User;
import com.ademobejavx.repository.memory.InMemoryAttachmentRepository;
import com.ademobejavx.repository.memory.InMemoryExpenseRepository;
import com.ademobejavx.repository.memory.InMemoryTokenRevocationRepository;
import com.ademobejavx.repository.memory.InMemoryUserRepository;
import io.vertx.core.Future;

/**
 * In-memory implementation of {@link TestApiService}.
 *
 * <p>Resets all in-memory stores and promotes users via the in-memory user repository.
 */
public class InMemoryTestApiService implements TestApiService {

    private final InMemoryUserRepository userRepo;
    private final InMemoryExpenseRepository expenseRepo;
    private final InMemoryAttachmentRepository attachmentRepo;
    private final InMemoryTokenRevocationRepository revocationRepo;

    public InMemoryTestApiService(
            InMemoryUserRepository userRepo,
            InMemoryExpenseRepository expenseRepo,
            InMemoryAttachmentRepository attachmentRepo,
            InMemoryTokenRevocationRepository revocationRepo) {
        this.userRepo = userRepo;
        this.expenseRepo = expenseRepo;
        this.attachmentRepo = attachmentRepo;
        this.revocationRepo = revocationRepo;
    }

    @Override
    public Future<Void> resetDb() {
        attachmentRepo.reset();
        expenseRepo.reset();
        revocationRepo.reset();
        userRepo.reset();
        return Future.succeededFuture();
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
