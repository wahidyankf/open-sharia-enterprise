package com.organiclever.demojavx.integration;

import org.junit.platform.suite.api.ConfigurationParameter;
import org.junit.platform.suite.api.IncludeEngines;
import org.junit.platform.suite.api.SelectPackages;
import org.junit.platform.suite.api.Suite;

import static io.cucumber.junit.platform.engine.Constants.GLUE_PROPERTY_NAME;
import static io.cucumber.junit.platform.engine.Constants.PLUGIN_PROPERTY_NAME;

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
        value = "com.organiclever.demojavx.integration.steps")
@ConfigurationParameter(key = PLUGIN_PROPERTY_NAME,
        value = "pretty,json:target/cucumber-reports/cucumber.json")
public class CucumberIT {
}
