package com.organiclever.be.integration.currency_handling;

import com.organiclever.be.integration.steps.BaseCucumberContextConfig;
import io.cucumber.spring.CucumberContextConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;

@CucumberContextConfiguration
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.MOCK)
@ActiveProfiles("test")
@TestPropertySource(
    properties = {
        "spring.datasource.url="
            + "jdbc:h2:mem:testdb_currency_handling;DB_CLOSE_DELAY=-1;"
            + "MODE=PostgreSQL;DATABASE_TO_UPPER=false"
    })
public class CurrencyHandlingContextConfig extends BaseCucumberContextConfig {}
