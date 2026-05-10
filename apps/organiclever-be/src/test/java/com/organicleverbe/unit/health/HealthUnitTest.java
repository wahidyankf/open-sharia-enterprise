package com.organicleverbe.unit.health;

import static io.cucumber.junit.platform.engine.Constants.GLUE_PROPERTY_NAME;

import org.junit.platform.suite.api.ConfigurationParameter;
import org.junit.platform.suite.api.IncludeEngines;
import org.junit.platform.suite.api.SelectClasspathResource;
import org.junit.platform.suite.api.Suite;

@Suite
@IncludeEngines("cucumber")
@SelectClasspathResource("health")
@ConfigurationParameter(key = GLUE_PROPERTY_NAME, value = "com.organicleverbe.unit.health,com.organicleverbe.unit.steps")
public class HealthUnitTest {}
