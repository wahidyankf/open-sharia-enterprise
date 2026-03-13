package com.organiclever.demoktkt.integration.steps

import io.cucumber.java.en.Given
import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import org.junit.jupiter.api.Assertions.assertTrue

class ExpenseSteps {

    private fun createExpense(username: String, body: Map<String, String?>): String {
        val token = TestWorld.accessTokens[username] ?: error("$username has no access token")
        val amount = body["amount"] ?: error("amount missing")
        val currency = body["currency"] ?: error("currency missing")
        val category = body["category"] ?: error("category missing")
        val description = body["description"] ?: error("description missing")
        val date = body["date"] ?: error("date missing")
        val type = body["type"] ?: error("type missing")
        val quantity = body["quantity"]?.toDoubleOrNull()
        val unit = body["unit"]
        val (status, respBody) =
            ServiceDispatcher.createExpense(
                token,
                amount,
                currency,
                category,
                description,
                date,
                type,
                quantity,
                unit,
            )
        assertTrue(status == 201, "Expected 201 creating expense, got $status. Body: $respBody")
        return JsonHelper.getString(respBody, "id") ?: error("No id in expense response: $respBody")
    }

    @Suppress("NestedBlockDepth")
    private fun loginAndEnsureToken(username: String, passwords: List<String>) {
        if (!TestWorld.accessTokens.containsKey(username)) {
            for (pwd in passwords) {
                val (status, body) = ServiceDispatcher.login(username, pwd)
                if (status == 200) {
                    JsonHelper.getString(body, "access_token")?.let {
                        TestWorld.accessTokens[username] = it
                    }
                    JsonHelper.getString(body, "refresh_token")?.let {
                        TestWorld.refreshTokens[username] = it
                    }
                    break
                }
            }
        }
    }

    @Given(
        "alice has created an entry with body \\{ {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string} \\}"
    )
    fun aliceHasCreatedEntryWithBody(
        k1: String,
        v1: String,
        k2: String,
        v2: String,
        k3: String,
        v3: String,
        k4: String,
        v4: String,
        k5: String,
        v5: String,
        k6: String,
        v6: String,
    ) {
        val body = mapOf(k1 to v1, k2 to v2, k3 to v3, k4 to v4, k5 to v5, k6 to v6)
        val id = createExpense("alice", body)
        TestWorld.expenseIds["alice:last"] = id
    }

    @Given(
        "alice has created an expense with body \\{ {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string} \\}"
    )
    fun aliceHasCreatedExpenseWithSixFields(
        k1: String,
        v1: String,
        k2: String,
        v2: String,
        k3: String,
        v3: String,
        k4: String,
        v4: String,
        k5: String,
        v5: String,
        k6: String,
        v6: String,
    ) {
        val body = mapOf(k1 to v1, k2 to v2, k3 to v3, k4 to v4, k5 to v5, k6 to v6)
        val id = createExpense("alice", body)
        TestWorld.expenseIds["alice:last"] = id
    }

    @Given(
        "^alice has created an entry with body \\{ \"amount\": \"([^\"]+)\", \"currency\": \"([^\"]+)\", \"category\": \"([^\"]+)\", \"description\": \"([^\"]+)\", \"date\": \"([^\"]+)\", \"type\": \"([^\"]+)\", \"quantity\": ([0-9.]+), \"unit\": \"([^\"]+)\" \\}$"
    )
    fun aliceHasCreatedEntryWithQuantityUnit(
        amount: String,
        currency: String,
        category: String,
        description: String,
        date: String,
        type: String,
        quantity: String,
        unit: String,
    ) {
        val body =
            mapOf(
                "amount" to amount,
                "currency" to currency,
                "category" to category,
                "description" to description,
                "date" to date,
                "type" to type,
                "quantity" to quantity,
                "unit" to unit,
            )
        val id = createExpense("alice", body)
        TestWorld.expenseIds["alice:last"] = id
    }

    @Given(
        "^alice has created an expense with body \\{ \"amount\": \"([^\"]+)\", \"currency\": \"([^\"]+)\", \"category\": \"([^\"]+)\", \"description\": \"([^\"]+)\", \"date\": \"([^\"]+)\", \"type\": \"([^\"]+)\", \"quantity\": ([0-9.]+), \"unit\": \"([^\"]+)\" \\}$"
    )
    fun aliceHasCreatedExpenseWithQuantityUnit(
        amount: String,
        currency: String,
        category: String,
        description: String,
        date: String,
        type: String,
        quantity: String,
        unit: String,
    ) {
        val body =
            mapOf(
                "amount" to amount,
                "currency" to currency,
                "category" to category,
                "description" to description,
                "date" to date,
                "type" to type,
                "quantity" to quantity,
                "unit" to unit,
            )
        val id = createExpense("alice", body)
        TestWorld.expenseIds["alice:last"] = id
    }

    @Given("alice has created 3 entries")
    fun aliceHasCreated3Entries() {
        val entries =
            listOf(
                mapOf(
                    "amount" to "10.00",
                    "currency" to "USD",
                    "category" to "food",
                    "description" to "Entry1",
                    "date" to "2025-01-01",
                    "type" to "expense",
                ),
                mapOf(
                    "amount" to "20.00",
                    "currency" to "USD",
                    "category" to "food",
                    "description" to "Entry2",
                    "date" to "2025-01-02",
                    "type" to "expense",
                ),
                mapOf(
                    "amount" to "30.00",
                    "currency" to "USD",
                    "category" to "food",
                    "description" to "Entry3",
                    "date" to "2025-01-03",
                    "type" to "expense",
                ),
            )
        entries.forEach { createExpense("alice", it) }
    }

    @When("alice sends GET \\/api\\/v1\\/expenses\\/\\{expenseId\\}")
    fun aliceSendsGetExpenseById() {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val expenseId = TestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
        val (status, body) = ServiceDispatcher.getExpenseById(token, expenseId)
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @When("alice sends GET \\/api\\/v1\\/expenses")
    fun aliceSendsGetExpenses() {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val (status, body) = ServiceDispatcher.listExpenses(token)
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @When("alice sends GET \\/api\\/v1\\/expenses\\/summary")
    fun aliceSendsGetExpensesSummary() {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val (status, body) = ServiceDispatcher.expenseSummary(token)
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @When(
        "^alice sends PUT /api/v1/expenses/\\{expenseId\\} with body \\{ \"amount\": \"([^\"]+)\", \"currency\": \"([^\"]+)\", \"category\": \"([^\"]+)\", \"description\": \"([^\"]+)\", \"date\": \"([^\"]+)\", \"type\": \"([^\"]+)\" \\}$"
    )
    fun aliceSendsPutExpenseWithBody(
        amount: String,
        currency: String,
        category: String,
        description: String,
        date: String,
        type: String,
    ) {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val expenseId = TestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
        val (status, body) =
            ServiceDispatcher.updateExpense(
                token,
                expenseId,
                amount,
                currency,
                category,
                description,
                date,
                type,
            )
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @When("alice sends DELETE \\/api\\/v1\\/expenses\\/\\{expenseId\\}")
    fun aliceSendsDeleteExpense() {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val expenseId = TestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
        val (status, body) = ServiceDispatcher.deleteExpense(token, expenseId)
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @When(
        "^alice sends POST /api/v1/expenses with body \\{ \"amount\": \"([^\"]+)\", \"currency\": \"([^\"]+)\", \"category\": \"([^\"]+)\", \"description\": \"([^\"]+)\", \"date\": \"([^\"]+)\", \"type\": \"([^\"]+)\" \\}$"
    )
    fun aliceSendsPostExpensesWithBody(
        amount: String,
        currency: String,
        category: String,
        description: String,
        date: String,
        type: String,
    ) {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val (status, body) =
            ServiceDispatcher.createExpense(token, amount, currency, category, description, date, type)
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
        if (status == 201) {
            JsonHelper.getString(body, "id")?.let { TestWorld.expenseIds["alice:last"] = it }
        }
    }

    @When(
        "^alice sends POST /api/v1/expenses with body \\{ \"amount\": \"([^\"]+)\", \"currency\": \"([^\"]+)\", \"category\": \"([^\"]+)\", \"description\": \"([^\"]+)\", \"date\": \"([^\"]+)\", \"type\": \"([^\"]+)\", \"quantity\": ([0-9.]+), \"unit\": \"([^\"]+)\" \\}$"
    )
    fun aliceSendsPostExpensesWithBodyAndUnit(
        amount: String,
        currency: String,
        category: String,
        description: String,
        date: String,
        type: String,
        quantity: String,
        unit: String,
    ) {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val (status, body) =
            ServiceDispatcher.createExpense(
                token,
                amount,
                currency,
                category,
                description,
                date,
                type,
                quantity.toDoubleOrNull(),
                unit,
            )
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
        if (status == 201) {
            JsonHelper.getString(body, "id")?.let { TestWorld.expenseIds["alice:last"] = it }
        }
    }

    @Then("the response body should contain {string} total equal to {string}")
    fun theResponseBodyShouldContainCurrencyTotalEqualTo(currency: String, total: String) {
        val body = TestWorld.lastResponseBody
        assertTrue(
            body.contains(currency) && body.contains(total),
            "Expected currency '$currency' with total '$total' in: $body",
        )
    }

    @Then("^the response body should contain \"([^\"]+)\" equal to ([0-9]+(?:\\.[0-9]+)?)$")
    fun theResponseBodyShouldContainFieldEqualToNumber(field: String, expected: String) {
        val actual = JsonHelper.getNumber(TestWorld.lastResponseBody, field)
        val expectedDouble = expected.toDouble()
        val actualDouble = actual?.toDoubleOrNull()
        assertTrue(
            actualDouble != null && actualDouble == expectedDouble,
            "Field '$field' expected $expected but got $actual in: ${TestWorld.lastResponseBody}",
        )
    }
}
