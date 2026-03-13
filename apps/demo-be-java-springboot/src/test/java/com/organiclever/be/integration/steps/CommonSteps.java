package com.organiclever.be.integration.steps;

import com.organiclever.be.integration.ResponseStore;
import io.cucumber.java.After;
import io.cucumber.java.Before;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

@Scope("cucumber-glue")
public class CommonSteps {

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    @Autowired
    private DatabaseCleaner databaseCleaner;

    @Before
    public void beginScenario() {
        responseStore.clear();
        tokenStore.clear();
        databaseCleaner.truncateAll();
    }

    @After
    public void cleanupScenario() {
        databaseCleaner.truncateAll();
    }

    @Given("the OrganicLever API is running")
    public void theOrganicLeverApiIsRunning() {
        // No-op: Spring Boot context is always ready when scenarios execute.
    }

    @Given("the API is running")
    public void theApiIsRunning() {
        // No-op: Spring Boot context is always ready when scenarios execute.
    }

    @Then("the response status code should be {int}")
    public void theResponseStatusCodeShouldBe(final int expectedStatusCode) throws Exception {
        MockMvcResultMatchers.status()
            .is(expectedStatusCode)
            .match(responseStore.getResult());
    }
}
