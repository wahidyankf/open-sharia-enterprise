package com.demobektkt.unit.steps

import io.cucumber.java.en.Given
import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue

class UnitAttachmentSteps {

  private fun uploadFile(
    username: String,
    filename: String,
    contentType: String,
    expenseIdKey: String,
    fileContent: ByteArray = "test file content".toByteArray(),
  ): Pair<Int, String> {
    val token = UnitTestWorld.accessTokens[username] ?: error("$username has no access token")
    val expenseId =
      UnitTestWorld.expenseIds[expenseIdKey] ?: error("no expense id stored at $expenseIdKey")
    return UnitServiceDispatcher.uploadAttachment(token, expenseId, filename, contentType, fileContent)
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
      val (status, body) = UnitServiceDispatcher.login("bob", pwd)
      if (status == 200) {
        UnitJsonHelper.getString(body, "accessToken")?.let {
          UnitTestWorld.accessTokens["bob"] = it
        }
        break
      }
    }
    val map = mapOf(k1 to v1, k2 to v2, k3 to v3, k4 to v4, k5 to v5, k6 to v6)
    val token = UnitTestWorld.accessTokens["bob"] ?: error("bob has no access token")
    val (status, respBody) =
      UnitServiceDispatcher.createExpense(
        token,
        map["amount"] ?: error("missing amount"),
        map["currency"] ?: error("missing currency"),
        map["category"] ?: error("missing category"),
        map["description"] ?: error("missing description"),
        map["date"] ?: error("missing date"),
        map["type"] ?: error("missing type"),
      )
    assertTrue(status == 201, "Expected 201 creating bob's expense, got $status. Body: $respBody")
    UnitJsonHelper.getString(respBody, "id")?.let { UnitTestWorld.expenseIds["bob:last"] = it }
  }

  @Given("^alice has uploaded file \"([^\"]+)\" with content type \"([^\"]+)\" to the entry$")
  fun aliceHasUploadedFileToEntry(filename: String, contentType: String) {
    val (status, body) = uploadFile("alice", filename, contentType, "alice:last")
    assertTrue(status == 201, "Expected 201 uploading file, got $status. Body: $body")
    UnitJsonHelper.getString(body, "id")?.let { UnitTestWorld.attachmentIds["alice:last"] = it }
  }

  @When(
    "^alice uploads file \"([^\"]+)\" with content type \"([^\"]+)\" to POST /api/v1/expenses/\\{expenseId\\}/attachments$"
  )
  fun aliceUploadsFileToExpense(filename: String, contentType: String) {
    val (status, body) = uploadFile("alice", filename, contentType, "alice:last")
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
    if (status == 201) {
      UnitJsonHelper.getString(body, "id")?.let { UnitTestWorld.attachmentIds["alice:last"] = it }
    }
  }

  @When(
    "^alice uploads file \"([^\"]+)\" with content type \"([^\"]+)\" to POST /api/v1/expenses/\\{bobExpenseId\\}/attachments$"
  )
  fun aliceUploadsToBobsExpense(filename: String, contentType: String) {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val expenseId = UnitTestWorld.expenseIds["bob:last"] ?: error("no bob expense id stored")
    val (status, body) =
      UnitServiceDispatcher.uploadAttachment(
        token,
        expenseId,
        filename,
        contentType,
        "test file content".toByteArray(),
      )
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When(
    "alice uploads an oversized file to POST \\/api\\/v1\\/expenses\\/\\{expenseId\\}\\/attachments"
  )
  fun aliceUploadsOversizedFile() {
    val oversizedContent = ByteArray(11 * 1024 * 1024) { 'X'.code.toByte() } // 11MB
    val (status, body) =
      uploadFile("alice", "large.jpg", "image/jpeg", "alice:last", oversizedContent)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When("alice sends GET \\/api\\/v1\\/expenses\\/\\{expenseId\\}\\/attachments")
  fun aliceSendsGetAttachments() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val expenseId = UnitTestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
    val (status, body) = UnitServiceDispatcher.listAttachments(token, expenseId)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When("^alice sends GET /api/v1/expenses/\\{bobExpenseId\\}/attachments$")
  fun aliceSendsGetBobsAttachments() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val expenseId = UnitTestWorld.expenseIds["bob:last"] ?: error("no bob expense id stored")
    val (status, body) = UnitServiceDispatcher.listAttachments(token, expenseId)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When(
    "alice sends DELETE \\/api\\/v1\\/expenses\\/\\{expenseId\\}\\/attachments\\/\\{attachmentId\\}"
  )
  fun aliceSendsDeleteAttachment() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val expenseId = UnitTestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
    val attachmentId = UnitTestWorld.attachmentIds["alice:last"] ?: error("no attachment id stored")
    val (status, body) = UnitServiceDispatcher.deleteAttachment(token, expenseId, attachmentId)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When("^alice sends DELETE /api/v1/expenses/\\{bobExpenseId\\}/attachments/\\{attachmentId\\}$")
  fun aliceSendsDeleteAttachmentOnBobsExpense() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val expenseId = UnitTestWorld.expenseIds["bob:last"] ?: error("no bob expense id stored")
    val attachmentId =
      UnitTestWorld.attachmentIds["alice:last"] ?: "00000000-0000-0000-0000-000000000000"
    val (status, body) = UnitServiceDispatcher.deleteAttachment(token, expenseId, attachmentId)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When(
    "^alice sends DELETE /api/v1/expenses/\\{expenseId\\}/attachments/\\{randomAttachmentId\\}$"
  )
  fun aliceSendsDeleteNonExistentAttachment() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val expenseId = UnitTestWorld.expenseIds["alice:last"] ?: error("no expense id stored")
    val randomId = "00000000-0000-0000-0000-000000000000"
    val (status, body) = UnitServiceDispatcher.deleteAttachment(token, expenseId, randomId)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @Then("the response body should contain {int} items in the {string} array")
  fun theResponseBodyShouldContainItemsInArray(count: Int, arrayKey: String) {
    val actual = UnitJsonHelper.getArraySize(UnitTestWorld.lastResponseBody, arrayKey)
    assertEquals(
      count,
      actual,
      "Expected $count items in '$arrayKey' array in: ${UnitTestWorld.lastResponseBody}",
    )
  }

  @Then("the response body should contain an attachment with {string} equal to {string}")
  fun theResponseBodyShouldContainAttachmentWithField(field: String, expected: String) {
    val body = UnitTestWorld.lastResponseBody
    assertTrue(body.contains(expected), "Expected attachment with '$field'='$expected' in: $body")
  }
}
