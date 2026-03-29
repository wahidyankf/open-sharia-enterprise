package com.ademobejasb.unit.security;

import com.ademobejasb.unit.steps.BaseUnitCucumberContextConfig;
import com.ademobejasb.unit.steps.UnitTestApplication;
import io.cucumber.spring.CucumberContextConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

/**
 * Cucumber context configuration for the Security unit test suite.
 */
@CucumberContextConfiguration
@SpringBootTest(
        classes = UnitTestApplication.class,
        webEnvironment = SpringBootTest.WebEnvironment.NONE)
@ActiveProfiles("unit-test")
public class SecurityUnitContextConfig extends BaseUnitCucumberContextConfig {}
