package com.ademobejasb.integration.admin;

import org.junit.platform.suite.api.ConfigurationParameter;
import org.junit.platform.suite.api.IncludeEngines;
import org.junit.platform.suite.api.SelectClasspathResource;
import org.junit.platform.suite.api.Suite;

import static io.cucumber.junit.platform.engine.Constants.GLUE_PROPERTY_NAME;
import static io.cucumber.junit.platform.engine.Constants.PLUGIN_PROPERTY_NAME;

@Suite
@IncludeEngines("cucumber")
@SelectClasspathResource("admin/admin.feature")
@ConfigurationParameter(
    key = GLUE_PROPERTY_NAME,
    value = "com.ademobejasb.integration.admin"
        + ",com.ademobejasb.integration.steps")
@ConfigurationParameter(
    key = PLUGIN_PROPERTY_NAME,
    value = "pretty,html:target/cucumber-reports/admin.html")
public class AdminIT {}
