package com.organiclever.demoktkt.integration.steps

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
    return HttpHelper.postMultipart(
      "/api/v1/expenses/$expenseId/attachments",
      filename,
      contentType,
      fileContent,
      token,
    )
  }

  @Given(
    "bob has created an entry with body \\{ {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string}, {string}: {string} \\}"
  )
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
    // Login bob
    val passwords = listOf("Str0ng#Pass2", "Str0ng#Pass1")
    for (pwd in passwords) {
      val (status, body) =
        HttpHelper.post("/api/v1/auth/login", """{"username":"bob","password":"$pwd"}""")
      if (status == 200) {
        JsonHelper.getString(body, "access_token")?.let { TestWorld.accessTokens["bob"] = it }
        break
      }
    }
    val body = """{"$k1":"$v1","$k2":"$v2","$k3":"$v3","$k4":"$v4","$k5":"$v5","$k6":"$v6"}"""
    val token = TestWorld.accessTokens["bob"] ?: error("bob has no access token")
    val (status, respBody) = HttpHelper.post("/api/v1/expenses", body, token)
    assertTrue(status == 201, "Expected 201 creating bob's expense, got $status. Body: $respBody")
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
      HttpHelper.postMultipart(
        "/api/v1/expenses/$expenseId/attachments",
        filename,
        contentType,
        "test file content".toByteArray(),
        token,
      )
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When(
    "alice uploads an oversized file to POST \\/api\\/v1\\/expenses\\/\\{expenseId\\}\\/attachments"
  )
  fun aliceUploadsOversizedFile() {
    val oversizedContent = ByteArray(11 * 1024 * 1024) { 'X'.code.toByte() } // 11MB
    val (status, body) =
      uploadFile("alice", "large.jpg", "image/jpeg", "alice:last", oversizedContent)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When("alice sends GET \\/api\\/v1\\/expenses\\/\\{expenseId\\}\\/attachments")
  fun aliceSendsGetAttachments() {
    val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val expenseId = TestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
    val (status, body) = HttpHelper.get("/api/v1/expenses/$expenseId/attachments", token)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When("^alice sends GET /api/v1/expenses/\\{bobExpenseId\\}/attachments$")
  fun aliceSendsGetBobsAttachments() {
    val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val expenseId = TestWorld.expenseIds["bob:last"] ?: error("no bob expense id stored")
    val (status, body) = HttpHelper.get("/api/v1/expenses/$expenseId/attachments", token)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When(
    "alice sends DELETE \\/api\\/v1\\/expenses\\/\\{expenseId\\}\\/attachments\\/\\{attachmentId\\}"
  )
  fun aliceSendsDeleteAttachment() {
    val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val expenseId = TestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
    val attachmentId = TestWorld.attachmentIds["alice:last"] ?: error("no attachment id stored")
    val (status, body) =
      HttpHelper.delete("/api/v1/expenses/$expenseId/attachments/$attachmentId", token)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When("^alice sends DELETE /api/v1/expenses/\\{bobExpenseId\\}/attachments/\\{attachmentId\\}$")
  fun aliceSendsDeleteAttachmentOnBobsExpense() {
    val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val expenseId = TestWorld.expenseIds["bob:last"] ?: error("no bob expense id stored")
    val attachmentId =
      TestWorld.attachmentIds["alice:last"] ?: "00000000-0000-0000-0000-000000000000"
    val (status, body) =
      HttpHelper.delete("/api/v1/expenses/$expenseId/attachments/$attachmentId", token)
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
    val (status, body) =
      HttpHelper.delete("/api/v1/expenses/$expenseId/attachments/$randomId", token)
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
