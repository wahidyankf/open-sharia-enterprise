package com.demobejasb.integration.unit_handling;

import com.demobejasb.integration.ResponseStore;
import com.demobejasb.integration.steps.ExpenseStepHelper;
import com.demobejasb.integration.steps.TokenStore;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import java.util.Map;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;

import static org.assertj.core.api.Assertions.assertThat;

@Scope("cucumber-glue")
public class UnitHandlingSteps {

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    @Autowired
    private ExpenseStepHelper expenseHelper;

    @Given("^alice has created an expense with body (.*)$")
    public void aliceHasCreatedAnExpenseWithBody(final String body) {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        UUID id = expenseHelper.createExpenseOrFail(token, body);
        tokenStore.setExpenseId(id);
    }

    @When("^alice sends GET /api/v1/expenses/\\{expenseId\\}$")
    public void aliceSendsGetExpenseById() {
        UUID expenseId = tokenStore.getExpenseId();
        if (expenseId == null) {
            throw new IllegalStateException("Expense ID not stored");
        }
        expenseHelper.getExpenseById(expenseId);
    }

    @Then("the response body should contain \"quantity\" equal to {double}")
    public void theResponseBodyShouldContainQuantityEqual(final double quantity) {
        Map<String, Object> body = responseStore.getBodyAsMap();
        assertThat(body).containsKey("quantity");
        Object value = body.get("quantity");
        double actual = value instanceof Number n ? n.doubleValue() : Double.parseDouble(String.valueOf(value));
        assertThat(actual).isEqualTo(quantity);
    }

    @When("^alice sends POST /api/v1/expenses with body (.*)$")
    public void aliceSendsPostExpensesWithBody(final String body) {
        expenseHelper.createExpenseForCurrentUser(body, true);
    }
}
