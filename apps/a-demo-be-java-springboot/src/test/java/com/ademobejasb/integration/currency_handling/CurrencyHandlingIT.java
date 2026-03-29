package com.ademobejasb.integration.currency_handling;

import org.junit.platform.suite.api.ConfigurationParameter;
import org.junit.platform.suite.api.IncludeEngines;
import org.junit.platform.suite.api.SelectClasspathResource;
import org.junit.platform.suite.api.Suite;

import static io.cucumber.junit.platform.engine.Constants.GLUE_PROPERTY_NAME;
import static io.cucumber.junit.platform.engine.Constants.PLUGIN_PROPERTY_NAME;

@Suite
@IncludeEngines("cucumber")
@SelectClasspathResource("expenses/currency-handling.feature")
@ConfigurationParameter(
    key = GLUE_PROPERTY_NAME,
    value = "com.ademobejasb.integration.currency_handling"
        + ",com.ademobejasb.integration.steps")
@ConfigurationParameter(
    key = PLUGIN_PROPERTY_NAME,
    value = "pretty,html:target/cucumber-reports/currency-handling.html")
public class CurrencyHandlingIT {}
