package com.organiclever.demoktkt.integration

import io.cucumber.junit.platform.engine.Constants
import org.junit.platform.suite.api.ConfigurationParameter
import org.junit.platform.suite.api.IncludeEngines
import org.junit.platform.suite.api.SelectClasspathResource
import org.junit.platform.suite.api.Suite

@Suite
@IncludeEngines("cucumber")
@SelectClasspathResource("specs/apps/demo-be/gherkin")
@ConfigurationParameter(
  key = Constants.GLUE_PROPERTY_NAME,
  value = "com.organiclever.demoktkt.integration.steps",
)
@ConfigurationParameter(key = Constants.PLUGIN_PROPERTY_NAME, value = "pretty")
@ConfigurationParameter(key = Constants.PLUGIN_PUBLISH_QUIET_PROPERTY_NAME, value = "true")
class CucumberRunner
