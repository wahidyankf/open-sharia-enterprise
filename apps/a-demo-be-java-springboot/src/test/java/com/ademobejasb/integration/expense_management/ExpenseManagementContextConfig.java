package com.ademobejasb.integration.expense_management;

import com.ademobejasb.integration.steps.BaseCucumberContextConfig;
import io.cucumber.spring.CucumberContextConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

@CucumberContextConfiguration
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE)
@ActiveProfiles("integration-test")
public class ExpenseManagementContextConfig extends BaseCucumberContextConfig {}
