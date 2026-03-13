package com.demobejasb.unit.expense_management;

import org.junit.platform.suite.api.ConfigurationParameter;
import org.junit.platform.suite.api.IncludeEngines;
import org.junit.platform.suite.api.SelectClasspathResource;
import org.junit.platform.suite.api.Suite;

import static io.cucumber.junit.platform.engine.Constants.GLUE_PROPERTY_NAME;
import static io.cucumber.junit.platform.engine.Constants.PLUGIN_PROPERTY_NAME;

/**
 * Unit test runner for the ExpenseManagement feature.
 */
@Suite
@IncludeEngines("cucumber")
@SelectClasspathResource("expenses/expense-management.feature")
@ConfigurationParameter(
        key = GLUE_PROPERTY_NAME,
        value = "com.demobejasb.unit.expense_management"
                + ",com.demobejasb.unit.steps")
@ConfigurationParameter(
        key = PLUGIN_PROPERTY_NAME,
        value = "pretty,html:target/cucumber-reports/unit-expense-management.html")
public class ExpenseManagementUnitTest {}
