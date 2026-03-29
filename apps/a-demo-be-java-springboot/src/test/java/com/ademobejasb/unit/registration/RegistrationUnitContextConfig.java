package com.aademobejasb.unit.registration;

import com.aademobejasb.unit.steps.BaseUnitCucumberContextConfig;
import com.aademobejasb.unit.steps.UnitTestApplication;
import io.cucumber.spring.CucumberContextConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

/**
 * Cucumber context configuration for the Registration unit test suite. Uses a minimal Spring
 * context (no web layer, no database) with service beans wired to in-memory repositories.
 */
@CucumberContextConfiguration
@SpringBootTest(
        classes = UnitTestApplication.class,
        webEnvironment = SpringBootTest.WebEnvironment.NONE)
@ActiveProfiles("unit-test")
public class RegistrationUnitContextConfig extends BaseUnitCucumberContextConfig {}
