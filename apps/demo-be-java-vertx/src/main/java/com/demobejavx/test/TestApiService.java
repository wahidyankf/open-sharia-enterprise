package com.demobejavx.test;

import io.vertx.core.Future;

/**
 * Test-only service interface for database reset and user promotion.
 *
 * <p>Only used when {@code ENABLE_TEST_API=true}. These operations must never be exposed in
 * production.
 */
public interface TestApiService {

    /**
     * Deletes all user-created data from the data store.
     *
     * <p>Deletion order respects foreign-key constraints: attachments → expenses → revoked_tokens →
     * users.
     *
     * @return a future that completes when all data has been deleted
     */
    Future<Void> resetDb();

    /**
     * Promotes an existing user to the {@code ADMIN} role.
     *
     * @param username the username of the user to promote
     * @return a future that completes when the user has been promoted, or fails with {@link
     *     UserNotFoundException} if the user does not exist
     */
    Future<Void> promoteAdmin(String username);

    /** Thrown when a user is not found during promotion. */
    class UserNotFoundException extends RuntimeException {
        public UserNotFoundException(String message) {
            super(message);
        }
    }
}
