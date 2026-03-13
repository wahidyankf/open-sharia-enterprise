package com.organiclever.demojavx.unit;

import com.organiclever.demojavx.auth.JwtService;
import com.organiclever.demojavx.auth.PasswordService;
import com.organiclever.demojavx.repository.memory.InMemoryAttachmentRepository;
import com.organiclever.demojavx.repository.memory.InMemoryExpenseRepository;
import com.organiclever.demojavx.repository.memory.InMemoryTokenRevocationRepository;
import com.organiclever.demojavx.repository.memory.InMemoryUserRepository;
import com.organiclever.demojavx.support.DirectCallService;

/**
 * Singleton factory used by unit tests. It creates a {@link DirectCallService}
 * backed by in-memory repositories — no PostgreSQL connection required.
 *
 * <p>Between Cucumber scenarios {@link #reset()} discards all in-memory state
 * so each scenario starts with a clean slate.
 */
public final class UnitFactory {

    private static DirectCallService service;
    private static JwtService jwtService;

    private static InMemoryUserRepository userRepo;
    private static InMemoryExpenseRepository expenseRepo;
    private static InMemoryAttachmentRepository attachmentRepo;
    private static InMemoryTokenRevocationRepository revocationRepo;

    private UnitFactory() {
    }

    public static synchronized void deploy() {
        if (service != null) {
            return;
        }
        jwtService = new JwtService("test-secret-32-chars-or-more-here!!");
        PasswordService passwordService = new PasswordService();

        userRepo = new InMemoryUserRepository();
        expenseRepo = new InMemoryExpenseRepository();
        attachmentRepo = new InMemoryAttachmentRepository();
        revocationRepo = new InMemoryTokenRevocationRepository();

        service = new DirectCallService(userRepo, expenseRepo, attachmentRepo, revocationRepo,
                jwtService, passwordService);
    }

    public static DirectCallService getService() {
        return service;
    }

    public static JwtService getJwtService() {
        return jwtService;
    }

    /**
     * Resets all in-memory repositories to an empty state. Called before each
     * Cucumber scenario to guarantee full isolation between scenarios.
     */
    public static synchronized void reset() {
        userRepo.reset();
        expenseRepo.reset();
        attachmentRepo.reset();
        revocationRepo.reset();
    }

    public static synchronized void close() {
        service = null;
        jwtService = null;
        userRepo = null;
        expenseRepo = null;
        attachmentRepo = null;
        revocationRepo = null;
    }
}
