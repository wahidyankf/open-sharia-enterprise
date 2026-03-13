package com.organiclever.be.integration.steps;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * Truncates all application tables between Cucumber scenarios to provide a clean database state.
 * Cascades on attachments → expenses → users ordering to satisfy foreign key constraints.
 *
 * <p>Active only under the {@code integration-test} profile, which uses a real PostgreSQL
 * database. Under other test profiles, database cleanup is handled differently or not needed.
 */
@Component
@Profile("integration-test")
public class DatabaseCleaner {

    @PersistenceContext
    private EntityManager entityManager;

    /**
     * Deletes all rows from every application table. Called in the {@code @Before} hook of
     * {@link CommonSteps} before each scenario so each scenario starts with an empty database.
     *
     * <p>The TRUNCATE order respects foreign key dependencies:
     * <ol>
     *   <li>attachments — references expenses</li>
     *   <li>revoked_tokens — standalone (references nothing via FK)</li>
     *   <li>refresh_tokens — references users</li>
     *   <li>expenses — references users</li>
     *   <li>users — base table</li>
     * </ol>
     */
    @Transactional
    public void truncateAll() {
        entityManager.createNativeQuery(
            "TRUNCATE TABLE attachments, revoked_tokens, refresh_tokens, expenses, users"
                + " RESTART IDENTITY CASCADE"
        ).executeUpdate();
        entityManager.flush();
        entityManager.clear();
    }
}
