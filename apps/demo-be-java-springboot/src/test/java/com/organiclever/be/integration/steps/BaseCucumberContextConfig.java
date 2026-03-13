package com.organiclever.be.integration.steps;

/**
 * Abstract base for Cucumber context configs. Not annotated with @CucumberContextConfiguration —
 * only concrete subclasses are.
 *
 * <p>Integration tests use the real Spring Boot application context backed by a real PostgreSQL
 * database. Step definitions call service methods directly — no HTTP layer is involved. Data
 * isolation is achieved by truncating all tables before each scenario via {@link DatabaseCleaner}.
 */
public abstract class BaseCucumberContextConfig {}
