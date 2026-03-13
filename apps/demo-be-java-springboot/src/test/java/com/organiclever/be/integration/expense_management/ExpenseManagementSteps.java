package com.organiclever.be.integration.expense_management;

import com.organiclever.be.integration.steps.AuthSteps;
import com.organiclever.be.integration.steps.ExpenseStepHelper;
import com.organiclever.be.integration.steps.TokenStore;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.When;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;

@Scope("cucumber-glue")
public class ExpenseManagementSteps {

    @Autowired
    private ExpenseStepHelper expenseHelper;

    @Autowired
    private TokenStore tokenStore;

    @Autowired
    private AuthSteps authSteps;

    @When("^alice sends POST /api/v1/expenses with body \\{ \"amount\": \"10\\.50\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Lunch\", \"date\": \"2025-01-15\", \"type\": \"expense\" \\}$")
    public void aliceCreatesExpenseEntry() {
        String body = "{\"amount\":\"10.50\",\"currency\":\"USD\",\"category\":\"food\",\"description\":\"Lunch\",\"date\":\"2025-01-15\",\"type\":\"expense\"}";
        expenseHelper.createExpenseForCurrentUser(body, true);
    }

    @When("^alice sends POST /api/v1/expenses with body \\{ \"amount\": \"3000\\.00\", \"currency\": \"USD\", \"category\": \"salary\", \"description\": \"Monthly salary\", \"date\": \"2025-01-31\", \"type\": \"income\" \\}$")
    public void aliceCreatesIncomeEntry() {
        String body = "{\"amount\":\"3000.00\",\"currency\":\"USD\",\"category\":\"salary\",\"description\":\"Monthly salary\",\"date\":\"2025-01-31\",\"type\":\"income\"}";
        expenseHelper.createExpenseForCurrentUser(body, true);
    }

    @Given("^alice has created an entry with body (.*)$")
    public void aliceHasCreatedAnEntryWithBody(final String body) {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        UUID id = expenseHelper.createExpenseOrFail(token, body);
        tokenStore.setExpenseId(id);
    }

    @Given("alice has created 3 entries")
    public void aliceHasCreated3Entries() {
        String base = "{\"amount\":\"5.00\",\"currency\":\"USD\",\"category\":\"misc\",\"description\":\"Entry\",\"date\":\"2025-01-01\",\"type\":\"expense\"}";
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        for (int i = 0; i < 3; i++) {
            expenseHelper.createExpenseOrFail(token, base);
        }
    }

    @When("^alice sends GET /api/v1/expenses/\\{expenseId\\}$")
    public void aliceSendsGetExpenseById() {
        UUID expenseId = tokenStore.getExpenseId();
        if (expenseId == null) {
            throw new IllegalStateException("Expense ID not stored");
        }
        expenseHelper.getExpenseById(expenseId);
    }

    @When("^alice sends GET /api/v1/expenses$")
    public void aliceSendsGetExpenses() {
        expenseHelper.listExpenses();
    }

    @When("^alice sends PUT /api/v1/expenses/\\{expenseId\\} with body \\{ \"amount\": \"12\\.00\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Updated breakfast\", \"date\": \"2025-01-10\", \"type\": \"expense\" \\}$")
    public void aliceUpdatesExpense() {
        UUID expenseId = tokenStore.getExpenseId();
        if (expenseId == null) {
            throw new IllegalStateException("Expense ID not stored");
        }
        String body = "{\"amount\":\"12.00\",\"currency\":\"USD\",\"category\":\"food\",\"description\":\"Updated breakfast\",\"date\":\"2025-01-10\",\"type\":\"expense\"}";
        expenseHelper.updateExpense(expenseId, body);
    }

    @When("^alice sends DELETE /api/v1/expenses/\\{expenseId\\}$")
    public void aliceDeletesExpense() {
        UUID expenseId = tokenStore.getExpenseId();
        if (expenseId == null) {
            throw new IllegalStateException("Expense ID not stored");
        }
        expenseHelper.deleteExpense(expenseId);
    }

    @When("^the client sends POST /api/v1/expenses with body \\{ \"amount\": \"10\\.00\", \"currency\": \"USD\", \"category\": \"food\", \"description\": \"Coffee\", \"date\": \"2025-01-01\", \"type\": \"expense\" \\}$")
    public void unauthenticatedClientCreatesExpense() {
        // No token stored → should return 401
        String body = "{\"amount\":\"10.00\",\"currency\":\"USD\",\"category\":\"food\",\"description\":\"Coffee\",\"date\":\"2025-01-01\",\"type\":\"expense\"}";
        expenseHelper.createExpenseForCurrentUser(body, false);
    }
}
