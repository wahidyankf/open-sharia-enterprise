package com.demobejasb.unit.expense_management;

import com.demobejasb.unit.steps.BaseUnitCucumberContextConfig;
import com.demobejasb.unit.steps.UnitTestApplication;
import io.cucumber.spring.CucumberContextConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

/**
 * Cucumber context configuration for the ExpenseManagement unit test suite.
 */
@CucumberContextConfiguration
@SpringBootTest(
        classes = UnitTestApplication.class,
        webEnvironment = SpringBootTest.WebEnvironment.NONE)
@ActiveProfiles("unit-test")
public class ExpenseManagementUnitContextConfig extends BaseUnitCucumberContextConfig {}
