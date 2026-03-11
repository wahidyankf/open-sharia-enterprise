package com.organiclever.be.integration.steps;

import org.springframework.context.annotation.Import;

/**
 * Abstract base for Cucumber context configs. Not annotated with @CucumberContextConfiguration —
 * only concrete subclasses are.
 *
 * Imports MockMvcConfig (MockMvc with Spring Security) and TestcontainersConfig (PostgreSQL
 * container via @ServiceConnection).
 */
@Import({MockMvcConfig.class, TestcontainersConfig.class})
public abstract class BaseCucumberContextConfig {}
