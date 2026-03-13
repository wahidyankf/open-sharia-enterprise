package com.demobejasb.unit.token_lifecycle;

import com.demobejasb.unit.steps.BaseUnitCucumberContextConfig;
import com.demobejasb.unit.steps.UnitTestApplication;
import io.cucumber.spring.CucumberContextConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

/**
 * Cucumber context configuration for the TokenLifecycle unit test suite.
 */
@CucumberContextConfiguration
@SpringBootTest(
        classes = UnitTestApplication.class,
        webEnvironment = SpringBootTest.WebEnvironment.NONE)
@ActiveProfiles("unit-test")
public class TokenLifecycleUnitContextConfig extends BaseUnitCucumberContextConfig {}
