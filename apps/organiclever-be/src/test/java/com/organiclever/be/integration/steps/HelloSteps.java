package com.organiclever.be.integration.steps;

import com.organiclever.be.integration.ResponseStore;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;

public class HelloSteps {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ResponseStore responseStore;

    @When("^a client sends GET /api/v1/hello$")
    public void aClientSendsGetHello() throws Exception {
        responseStore.setResult(mockMvc.perform(get("/api/v1/hello")).andReturn());
    }

    @When("^a client sends GET /api/v1/hello with an Origin header of (.+)$")
    public void aClientSendsGetHelloWithOrigin(final String origin) throws Exception {
        responseStore.setResult(
            mockMvc.perform(get("/api/v1/hello").header(HttpHeaders.ORIGIN, origin)).andReturn()
        );
    }

    @Then("^the response body should be \\{\"message\":\"world!\"\\}$")
    public void theResponseBodyShouldBeHelloWorld() throws Exception {
        MockMvcResultMatchers.jsonPath("$.message").value("world!")
            .match(responseStore.getResult());
    }

    @Then("^the response Content-Type should be application/json$")
    public void theResponseContentTypeShouldBeJson() throws Exception {
        MockMvcResultMatchers.content()
            .contentTypeCompatibleWith("application/json")
            .match(responseStore.getResult());
    }

    @Then("the response should include an Access-Control-Allow-Origin header permitting the request")
    public void theResponseShouldIncludeAcaoHeader() {
        final String acao = responseStore.getResult()
            .getResponse()
            .getHeader("Access-Control-Allow-Origin");
        assertThat(acao).isNotBlank();
    }
}
