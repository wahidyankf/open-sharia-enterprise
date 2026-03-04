package com.organiclever.be.integration;

import io.cucumber.junit.platform.engine.Constants;
import org.junit.platform.suite.api.ConfigurationParameter;
import org.junit.platform.suite.api.IncludeEngines;
import org.junit.platform.suite.api.SelectClasspathResource;
import org.junit.platform.suite.api.Suite;

@Suite
@IncludeEngines("cucumber")
@SelectClasspathResource(".")
@ConfigurationParameter(
    key = Constants.GLUE_PROPERTY_NAME,
    value = "com.organiclever.be.integration"
)
@ConfigurationParameter(
    key = Constants.PLUGIN_PROPERTY_NAME,
    value = "pretty, junit:target/surefire-reports/cucumber-integration.xml"
)
public class CucumberIntegrationTest {
}
