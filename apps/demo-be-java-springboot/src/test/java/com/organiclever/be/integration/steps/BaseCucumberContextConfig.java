package com.organiclever.be.integration.steps;

import org.springframework.context.annotation.Import;

/**
 * Abstract base for Cucumber context configs. Not annotated with @CucumberContextConfiguration —
 * only concrete subclasses are.
 *
 * Imports MockMvcConfig (MockMvc with Spring Security). Uses the real Spring Boot application
 * context backed by a real PostgreSQL database. Data isolation is achieved by truncating all
 * tables before each scenario via {@link DatabaseCleaner}.
 */
@Import(MockMvcConfig.class)
public abstract class BaseCucumberContextConfig {}
