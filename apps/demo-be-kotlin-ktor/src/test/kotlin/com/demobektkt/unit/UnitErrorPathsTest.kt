package com.demobektkt.unit

import com.demobektkt.auth.PasswordService
import com.demobektkt.unit.steps.UnitJsonHelper
import com.demobektkt.unit.steps.UnitServiceDispatcher
import com.demobektkt.unit.steps.UnitTestWorld
import java.util.UUID
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestInstance

/**
 * Unit-level tests for error paths in service dispatching. These tests run against the
 * UnitServiceDispatcher with in-memory repositories and cover branches not exercised by Cucumber
 * scenarios.
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class UnitErrorPathsTest {

  private lateinit var aliceToken: String
  private lateinit var aliceExpenseId: String
  private lateinit var adminToken: String
  private lateinit var aliceUserId: String

  @BeforeAll
  fun setup() {
    UnitTestWorld.reset()

    // Register alice via ServiceDispatcher
    val username = "errtest${UUID.randomUUID().toString().take(6)}"
    val password = "Str0ng#Pass1"
    UnitServiceDispatcher.register(username, "$username@test.com", password)
    val (loginStatus, loginBody) = UnitServiceDispatcher.login(username, password)
    assertTrue(loginStatus == 200, "Login should succeed, got $loginStatus: $loginBody")
    aliceToken =
      UnitJsonHelper.getString(loginBody, "accessToken") ?: error("No token in: $loginBody")

    // Get user id from profile
    val (_, profileBody) = UnitServiceDispatcher.getProfile(aliceToken)
    aliceUserId =
      UnitJsonHelper.getString(profileBody, "id") ?: error("No id in profile: $profileBody")

    // Create one expense for alice
    val (createStatus, createBody) =
      UnitServiceDispatcher.createExpense(
        aliceToken,
        "10.00",
        "USD",
        "food",
        "Test",
        "2025-01-01",
        "expense",
      )
    assertTrue(createStatus == 201, "Create expense should succeed, got $createStatus: $createBody")
    aliceExpenseId = UnitJsonHelper.getString(createBody, "id") ?: error("No id in: $createBody")

    // Create admin user directly via repository
    val passwordService = PasswordService()
    val adminUsername = "admin${UUID.randomUUID().toString().take(6)}"
    val adminPassword = "Adm1n#Secure123"
    UnitTestWorld.userRepo.createAdmin(
      adminUsername,
      "$adminUsername@test.com",
      passwordService.hash(adminPassword),
    )
    val (adminLoginStatus, adminLoginBody) =
      UnitServiceDispatcher.login(adminUsername, adminPassword)
    assertTrue(
      adminLoginStatus == 200,
      "Admin login should succeed, got $adminLoginStatus: $adminLoginBody",
    )
    adminToken =
      UnitJsonHelper.getString(adminLoginBody, "accessToken")
        ?: error("No token in: $adminLoginBody")
  }

  // ---- ExpenseRoutes error paths ----

  @Test
  fun `get expense with invalid UUID returns 404`() {
    val (status, _) = UnitServiceDispatcher.getExpenseById(aliceToken, "not-a-uuid")
    assertEquals(404, status)
  }

  @Test
  fun `get non-existent expense returns 404`() {
    val (status, _) = UnitServiceDispatcher.getExpenseById(aliceToken, UUID.randomUUID().toString())
    assertEquals(404, status)
  }

  @Test
  fun `create expense with invalid type returns 400`() {
    val (status, body) =
      UnitServiceDispatcher.createExpense(
        aliceToken,
        "10.00",
        "USD",
        "food",
        "Test",
        "2025-01-01",
        "bad_type",
      )
    assertEquals(400, status)
    assertTrue(body.contains("type") || body.contains("Invalid"), "Expected type error in: $body")
  }

  @Test
  fun `create expense with invalid date returns 400`() {
    val (status, body) =
      UnitServiceDispatcher.createExpense(
        aliceToken,
        "10.00",
        "USD",
        "food",
        "Test",
        "not-a-date",
        "expense",
      )
    assertEquals(400, status)
    assertTrue(body.contains("date") || body.contains("Invalid"), "Expected date error in: $body")
  }

  @Test
  fun `update expense with invalid UUID in path returns 404`() {
    val (status, _) =
      UnitServiceDispatcher.updateExpense(
        aliceToken,
        "not-a-uuid",
        "10.00",
        "USD",
        "food",
        "Test",
        "2025-01-01",
        "expense",
      )
    assertEquals(404, status)
  }

  @Test
  fun `update non-existent expense returns 404`() {
    val (status, _) =
      UnitServiceDispatcher.updateExpense(
        aliceToken,
        UUID.randomUUID().toString(),
        "10.00",
        "USD",
        "food",
        "Test",
        "2025-01-01",
        "expense",
      )
    assertEquals(404, status)
  }

  @Test
  fun `update expense with invalid type returns 400`() {
    val (status, body) =
      UnitServiceDispatcher.updateExpense(
        aliceToken,
        aliceExpenseId,
        "10.00",
        "USD",
        "food",
        "Test",
        "2025-01-01",
        "bad_type",
      )
    assertEquals(400, status)
    assertTrue(body.contains("type") || body.contains("Invalid"), "Expected type error in: $body")
  }

  @Test
  fun `update expense with invalid date returns 400`() {
    val (status, body) =
      UnitServiceDispatcher.updateExpense(
        aliceToken,
        aliceExpenseId,
        "10.00",
        "USD",
        "food",
        "Test",
        "not-a-date",
        "expense",
      )
    assertEquals(400, status)
    assertTrue(body.contains("date") || body.contains("Invalid"), "Expected date error in: $body")
  }

  @Test
  fun `delete expense with invalid UUID in path returns 404`() {
    val (status, _) = UnitServiceDispatcher.deleteExpense(aliceToken, "not-a-uuid")
    assertEquals(404, status)
  }

  @Test
  fun `delete non-existent expense returns 404`() {
    val (status, _) = UnitServiceDispatcher.deleteExpense(aliceToken, UUID.randomUUID().toString())
    assertEquals(404, status)
  }

  // ---- AdminRoutes error paths ----

  @Test
  fun `non-admin user gets 403 on admin disable endpoint`() {
    val (status, _) =
      UnitServiceDispatcher.disableUser(aliceToken, UUID.randomUUID().toString(), "test")
    assertEquals(403, status)
  }

  @Test
  fun `non-admin user gets 403 on admin enable endpoint`() {
    val (status, _) = UnitServiceDispatcher.enableUser(aliceToken, UUID.randomUUID().toString())
    assertEquals(403, status)
  }

  @Test
  fun `non-admin user gets 403 on admin force-password-reset endpoint`() {
    val (status, _) =
      UnitServiceDispatcher.forcePasswordReset(aliceToken, UUID.randomUUID().toString())
    assertEquals(403, status)
  }

  @Test
  fun `admin can list users`() {
    val (status, body) = UnitServiceDispatcher.listUsers(adminToken)
    assertEquals(200, status)
    assertTrue(body.contains("content"), "Expected content in response: $body")
  }

  @Test
  fun `admin disable user with invalid UUID returns 404`() {
    val (status, _) = UnitServiceDispatcher.disableUser(adminToken, "not-a-uuid", "test")
    assertEquals(404, status)
  }

  @Test
  fun `admin disable non-existent user returns 404`() {
    val (status, _) =
      UnitServiceDispatcher.disableUser(adminToken, UUID.randomUUID().toString(), "not found")
    assertEquals(404, status)
  }

  @Test
  fun `admin disable existing user returns 200`() {
    val (status, body) = UnitServiceDispatcher.disableUser(adminToken, aliceUserId, "test disable")
    assertEquals(200, status)
    assertTrue(body.contains("DISABLED"), "Expected disabled status in: $body")
    // Re-enable alice so other tests can use her token
    UnitServiceDispatcher.enableUser(adminToken, aliceUserId)
  }

  @Test
  fun `admin enable user with invalid UUID returns 404`() {
    val (status, _) = UnitServiceDispatcher.enableUser(adminToken, "not-a-uuid")
    assertEquals(404, status)
  }

  @Test
  fun `admin enable non-existent user returns 404`() {
    val (status, _) = UnitServiceDispatcher.enableUser(adminToken, UUID.randomUUID().toString())
    assertEquals(404, status)
  }

  @Test
  fun `admin force-password-reset with invalid UUID returns 404`() {
    val (status, _) = UnitServiceDispatcher.forcePasswordReset(adminToken, "not-a-uuid")
    assertEquals(404, status)
  }

  @Test
  fun `admin force-password-reset non-existent user returns 404`() {
    val (status, _) =
      UnitServiceDispatcher.forcePasswordReset(adminToken, UUID.randomUUID().toString())
    assertEquals(404, status)
  }

  // ---- AttachmentRoutes error paths ----

  @Test
  fun `get attachments with invalid expense UUID returns 404`() {
    val (status, _) = UnitServiceDispatcher.listAttachments(aliceToken, "not-a-uuid")
    assertEquals(404, status)
  }

  @Test
  fun `get attachments for non-existent expense returns 404`() {
    val (status, _) =
      UnitServiceDispatcher.listAttachments(aliceToken, UUID.randomUUID().toString())
    assertEquals(404, status)
  }

  @Test
  fun `delete attachment with invalid attachment UUID returns 404`() {
    val (status, _) =
      UnitServiceDispatcher.deleteAttachment(aliceToken, aliceExpenseId, "not-a-uuid")
    assertEquals(404, status)
  }

  @Test
  fun `delete attachment with invalid expense UUID returns 404`() {
    val (status, _) =
      UnitServiceDispatcher.deleteAttachment(aliceToken, "not-a-uuid", UUID.randomUUID().toString())
    assertEquals(404, status)
  }

  // ---- ReportRoutes error paths ----

  @Test
  fun `report pl with invalid from date returns 400`() {
    val (status, _) = UnitServiceDispatcher.pl(aliceToken, "not-a-date", "2025-01-31", "USD")
    assertEquals(400, status)
  }

  @Test
  fun `report pl with invalid to date returns 400`() {
    val (status, _) = UnitServiceDispatcher.pl(aliceToken, "2025-01-01", "not-a-date", "USD")
    assertEquals(400, status)
  }

  @Test
  fun `report pl for IDR currency uses zero scale`() {
    // Creates an IDR expense and requests IDR report to cover the IDR scale branch
    UnitServiceDispatcher.createExpense(
      aliceToken,
      "150000",
      "IDR",
      "food",
      "Test IDR",
      "2025-06-01",
      "expense",
    )
    val (status, body) = UnitServiceDispatcher.pl(aliceToken, "2025-06-01", "2025-06-30", "IDR")
    assertEquals(200, status)
    assertTrue(body.contains("IDR"), "Expected IDR in response: $body")
  }

  // ---- AuthRoutes error paths ----

  @Test
  fun `logout without token succeeds with 200`() {
    val (status, body) = UnitServiceDispatcher.logout(null)
    assertEquals(200, status)
    assertTrue(body.contains("Logged out"), "Expected logged out message: $body")
  }

  @Test
  fun `logout with invalid token succeeds with 200`() {
    val (status, body) = UnitServiceDispatcher.logout("invalid.token.here")
    assertEquals(200, status)
    assertTrue(body.contains("Logged out"), "Expected logged out: $body")
  }
}
