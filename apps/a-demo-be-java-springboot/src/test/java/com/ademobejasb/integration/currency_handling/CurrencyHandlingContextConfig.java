package com.aademobejasb.integration.currency_handling;

import com.aademobejasb.integration.steps.BaseCucumberContextConfig;
import io.cucumber.spring.CucumberContextConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

@CucumberContextConfiguration
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE)
@ActiveProfiles("integration-test")
public class CurrencyHandlingContextConfig extends BaseCucumberContextConfig {}
