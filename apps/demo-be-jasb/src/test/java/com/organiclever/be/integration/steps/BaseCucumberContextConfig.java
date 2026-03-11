package com.organiclever.be.integration.steps;

import org.springframework.context.annotation.Import;

/**
 * Abstract base for Cucumber context configs. Not annotated with @CucumberContextConfiguration —
 * only concrete subclasses are.
 *
 * Imports MockMvcConfig (MockMvc with Spring Security) and MockRepositoriesConfig (in-memory mocked
 * repositories — no real database).
 */
@Import({MockMvcConfig.class, MockRepositoriesConfig.class})
public abstract class BaseCucumberContextConfig {}
