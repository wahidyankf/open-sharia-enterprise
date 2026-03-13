package com.organiclever.demojavx.unit;

import org.junit.platform.suite.api.ConfigurationParameter;
import org.junit.platform.suite.api.IncludeEngines;
import org.junit.platform.suite.api.SelectPackages;
import org.junit.platform.suite.api.Suite;

import static io.cucumber.junit.platform.engine.Constants.GLUE_PROPERTY_NAME;
import static io.cucumber.junit.platform.engine.Constants.PLUGIN_PROPERTY_NAME;

/**
 * Unit-level Cucumber test runner. Runs all Gherkin scenarios against
 * an in-process Vert.x HTTP server backed by in-memory repositories.
 * Identical in behaviour to the integration-level CucumberIT but lives
 * under the {@code unit} package so it is picked up by the default
 * Maven Surefire configuration (no profile required).
 */
@Suite
@IncludeEngines("cucumber")
@SelectPackages({
    "health",
    "authentication",
    "user-lifecycle",
    "security",
    "token-management",
    "admin",
    "expenses"
})
@ConfigurationParameter(key = GLUE_PROPERTY_NAME,
        value = "com.organiclever.demojavx.unit.steps")
@ConfigurationParameter(key = PLUGIN_PROPERTY_NAME,
        value = "pretty,json:target/cucumber-reports/unit-cucumber.json")
public class UnitCucumberTest {
}
