package com.demobektkt.integration.steps

import io.cucumber.java.en.Given
import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue

class AttachmentSteps {

    private fun uploadFile(
        username: String,
        filename: String,
        contentType: String,
        expenseIdKey: String,
        fileContent: ByteArray = "test file content".toByteArray(),
    ): Pair<Int, String> {
        val token = TestWorld.accessTokens[username] ?: error("$username has no access token")
        val expenseId =
            TestWorld.expenseIds[expenseIdKey] ?: error("no expense id stored at $expenseIdKey")
        return ServiceDispatcher.uploadAttachment(token, expenseId, filename, contentType, fileContent)
    }

    @Given(
        "bob has created an entry with body \\{ {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string} \\}"
    )
    @Suppress("CyclomaticComplexMethod", "NestedBlockDepth")
    fun bobHasCreatedEntry(
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
        // Ensure bob is logged in
        val passwords = listOf("Str0ng#Pass2", "Str0ng#Pass1")
        if (!TestWorld.accessTokens.containsKey("bob")) {
            for (pwd in passwords) {
                val (status, body) = ServiceDispatcher.login("bob", pwd)
                if (status == 200) {
                    JsonHelper.getString(body, "accessToken")?.let {
                        TestWorld.accessTokens["bob"] = it
                    }
                    break
                }
            }
        }
        val token = TestWorld.accessTokens["bob"] ?: error("bob has no access token")
        val amount = v1.takeIf { k1 == "amount" }
            ?: v2.takeIf { k2 == "amount" }
            ?: v3.takeIf { k3 == "amount" }
            ?: v4.takeIf { k4 == "amount" }
            ?: v5.takeIf { k5 == "amount" }
            ?: v6.takeIf { k6 == "amount" }
            ?: error("amount key not found")
        val currency = v1.takeIf { k1 == "currency" }
            ?: v2.takeIf { k2 == "currency" }
            ?: v3.takeIf { k3 == "currency" }
            ?: v4.takeIf { k4 == "currency" }
            ?: v5.takeIf { k5 == "currency" }
            ?: v6.takeIf { k6 == "currency" }
            ?: error("currency key not found")
        val category = v1.takeIf { k1 == "category" }
            ?: v2.takeIf { k2 == "category" }
            ?: v3.takeIf { k3 == "category" }
            ?: v4.takeIf { k4 == "category" }
            ?: v5.takeIf { k5 == "category" }
            ?: v6.takeIf { k6 == "category" }
            ?: error("category key not found")
        val description = v1.takeIf { k1 == "description" }
            ?: v2.takeIf { k2 == "description" }
            ?: v3.takeIf { k3 == "description" }
            ?: v4.takeIf { k4 == "description" }
            ?: v5.takeIf { k5 == "description" }
            ?: v6.takeIf { k6 == "description" }
            ?: error("description key not found")
        val date = v1.takeIf { k1 == "date" }
            ?: v2.takeIf { k2 == "date" }
            ?: v3.takeIf { k3 == "date" }
            ?: v4.takeIf { k4 == "date" }
            ?: v5.takeIf { k5 == "date" }
            ?: v6.takeIf { k6 == "date" }
            ?: error("date key not found")
        val type = v1.takeIf { k1 == "type" }
            ?: v2.takeIf { k2 == "type" }
            ?: v3.takeIf { k3 == "type" }
            ?: v4.takeIf { k4 == "type" }
            ?: v5.takeIf { k5 == "type" }
            ?: v6.takeIf { k6 == "type" }
            ?: error("type key not found")

        val (status, respBody) =
            ServiceDispatcher.createExpense(
                token,
                amount,
                currency,
                category,
                description,
                date,
                type,
            )
        assertTrue(
            status == 201,
            "Expected 201 creating bob's expense, got $status. Body: $respBody",
        )
        JsonHelper.getString(respBody, "id")?.let { TestWorld.expenseIds["bob:last"] = it }
    }

    @Given("^alice has uploaded file \"([^\"]+)\" with content type \"([^\"]+)\" to the entry$")
    fun aliceHasUploadedFileToEntry(filename: String, contentType: String) {
        val (status, body) = uploadFile("alice", filename, contentType, "alice:last")
        assertTrue(status == 201, "Expected 201 uploading file, got $status. Body: $body")
        JsonHelper.getString(body, "id")?.let { TestWorld.attachmentIds["alice:last"] = it }
    }

    @When(
        "^alice uploads file \"([^\"]+)\" with content type \"([^\"]+)\" to POST /api/v1/expenses/\\{expenseId\\}/attachments$"
    )
    fun aliceUploadsFileToExpense(filename: String, contentType: String) {
        val (status, body) = uploadFile("alice", filename, contentType, "alice:last")
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
        if (status == 201) {
            JsonHelper.getString(body, "id")?.let { TestWorld.attachmentIds["alice:last"] = it }
        }
    }

    @When(
        "^alice uploads file \"([^\"]+)\" with content type \"([^\"]+)\" to POST /api/v1/expenses/\\{bobExpenseId\\}/attachments$"
    )
    fun aliceUploadsToBobsExpense(filename: String, contentType: String) {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val expenseId = TestWorld.expenseIds["bob:last"] ?: error("no bob expense id stored")
        val (status, body) =
            ServiceDispatcher.uploadAttachment(
                token,
                expenseId,
                filename,
                contentType,
                "test file content".toByteArray(),
            )
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @When(
        "alice uploads an oversized file to POST \\/api\\/v1\\/expenses\\/\\{expenseId\\}\\/attachments"
    )
    fun aliceUploadsOversizedFile() {
        val oversizedContent = ByteArray(11 * 1024 * 1024) { 'X'.code.toByte() } // 11 MB
        val (status, body) =
            uploadFile("alice", "large.jpg", "image/jpeg", "alice:last", oversizedContent)
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @When("alice sends GET \\/api\\/v1\\/expenses\\/\\{expenseId\\}\\/attachments")
    fun aliceSendsGetAttachments() {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val expenseId = TestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
        val (status, body) = ServiceDispatcher.listAttachments(token, expenseId)
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @When("^alice sends GET /api/v1/expenses/\\{bobExpenseId\\}/attachments$")
    fun aliceSendsGetBobsAttachments() {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val expenseId = TestWorld.expenseIds["bob:last"] ?: error("no bob expense id stored")
        val (status, body) = ServiceDispatcher.listAttachments(token, expenseId)
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @When(
        "alice sends DELETE \\/api\\/v1\\/expenses\\/\\{expenseId\\}\\/attachments\\/\\{attachmentId\\}"
    )
    fun aliceSendsDeleteAttachment() {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val expenseId = TestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
        val attachmentId =
            TestWorld.attachmentIds["alice:last"] ?: error("no attachment id stored")
        val (status, body) = ServiceDispatcher.deleteAttachment(token, expenseId, attachmentId)
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @When("^alice sends DELETE /api/v1/expenses/\\{bobExpenseId\\}/attachments/\\{attachmentId\\}$")
    fun aliceSendsDeleteAttachmentOnBobsExpense() {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val expenseId = TestWorld.expenseIds["bob:last"] ?: error("no bob expense id stored")
        val attachmentId =
            TestWorld.attachmentIds["alice:last"] ?: "00000000-0000-0000-0000-000000000000"
        val (status, body) = ServiceDispatcher.deleteAttachment(token, expenseId, attachmentId)
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @When(
        "^alice sends DELETE /api/v1/expenses/\\{expenseId\\}/attachments/\\{randomAttachmentId\\}$"
    )
    fun aliceSendsDeleteNonExistentAttachment() {
        val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
        val expenseId = TestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
        val randomId = "00000000-0000-0000-0000-000000000000"
        val (status, body) = ServiceDispatcher.deleteAttachment(token, expenseId, randomId)
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @Then("the response body should contain {int} items in the {string} array")
    fun theResponseBodyShouldContainItemsInArray(count: Int, arrayKey: String) {
        val actual = JsonHelper.getArraySize(TestWorld.lastResponseBody, arrayKey)
        assertEquals(
            count,
            actual,
            "Expected $count items in '$arrayKey' array in: ${TestWorld.lastResponseBody}",
        )
    }

    @Then("the response body should contain an attachment with {string} equal to {string}")
    fun theResponseBodyShouldContainAttachmentWithField(field: String, expected: String) {
        val body = TestWorld.lastResponseBody
        assertTrue(body.contains(expected), "Expected attachment with '$field'='$expected' in: $body")
    }
}
