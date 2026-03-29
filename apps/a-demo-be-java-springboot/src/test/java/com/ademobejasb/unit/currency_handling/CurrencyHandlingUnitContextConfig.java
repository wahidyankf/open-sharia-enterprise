package com.ademobejasb.unit.currency_handling;

import com.ademobejasb.unit.steps.BaseUnitCucumberContextConfig;
import com.ademobejasb.unit.steps.UnitTestApplication;
import io.cucumber.spring.CucumberContextConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

/**
 * Cucumber context configuration for the CurrencyHandling unit test suite.
 */
@CucumberContextConfiguration
@SpringBootTest(
        classes = UnitTestApplication.class,
        webEnvironment = SpringBootTest.WebEnvironment.NONE)
@ActiveProfiles("unit-test")
public class CurrencyHandlingUnitContextConfig extends BaseUnitCucumberContextConfig {}
