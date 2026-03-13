package com.organiclever.demoktkt.unit

import com.organiclever.demoktkt.auth.PasswordService
import com.organiclever.demoktkt.unit.steps.UnitHttpHelper
import com.organiclever.demoktkt.unit.steps.UnitJsonHelper
import com.organiclever.demoktkt.unit.steps.UnitTestServer
import com.organiclever.demoktkt.unit.steps.UnitTestWorld
import java.util.UUID
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestInstance

/**
 * Unit-level tests for error paths in route handlers. These tests run against the embedded server
 * with in-memory repositories (UnitTestServer) and cover branches not exercised by Cucumber
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
    UnitTestServer.start()
    UnitTestWorld.reset()

    // Register alice via HTTP
    val username = "errtest${UUID.randomUUID().toString().take(6)}"
    val password = "Str0ng#Pass1"
    UnitHttpHelper.post(
      "/api/v1/auth/register",
      """{"username":"$username","email":"$username@test.com","password":"$password"}""",
    )
    val (loginStatus, loginBody) =
      UnitHttpHelper.post(
        "/api/v1/auth/login",
        """{"username":"$username","password":"$password"}""",
      )
    assertTrue(loginStatus == 200, "Login should succeed, got $loginStatus: $loginBody")
    aliceToken =
      UnitJsonHelper.getString(loginBody, "access_token") ?: error("No token in: $loginBody")
    aliceUserId =
      UnitJsonHelper.getString(loginBody, "id")
        ?: run {
          // Get user id from profile
          val (_, profileBody) = UnitHttpHelper.get("/api/v1/users/me", aliceToken)
          UnitJsonHelper.getString(profileBody, "id") ?: error("No id in profile: $profileBody")
        }

    // Create one expense for alice
    val (createStatus, createBody) =
      UnitHttpHelper.post(
        "/api/v1/expenses",
        """{"amount":"10.00","currency":"USD","category":"food","description":"Test","date":"2025-01-01","type":"expense"}""",
        aliceToken,
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
      UnitHttpHelper.post(
        "/api/v1/auth/login",
        """{"username":"$adminUsername","password":"$adminPassword"}""",
      )
    assertTrue(
      adminLoginStatus == 200,
      "Admin login should succeed, got $adminLoginStatus: $adminLoginBody",
    )
    adminToken =
      UnitJsonHelper.getString(adminLoginBody, "access_token")
        ?: error("No token in: $adminLoginBody")

    // Resolve alice's user id from profile
    val (_, profileBody) = UnitHttpHelper.get("/api/v1/users/me", aliceToken)
    aliceUserId =
      UnitJsonHelper.getString(profileBody, "id") ?: error("No id in profile: $profileBody")
  }

  // ---- ExpenseRoutes error paths ----

  @Test
  fun `get expense with invalid UUID returns 404`() {
    val (status, _) = UnitHttpHelper.get("/api/v1/expenses/not-a-uuid", aliceToken)
    assertEquals(404, status)
  }

  @Test
  fun `get non-existent expense returns 404`() {
    val (status, _) = UnitHttpHelper.get("/api/v1/expenses/${UUID.randomUUID()}", aliceToken)
    assertEquals(404, status)
  }

  @Test
  fun `create expense with invalid type returns 400`() {
    val (status, body) =
      UnitHttpHelper.post(
        "/api/v1/expenses",
        """{"amount":"10.00","currency":"USD","category":"food","description":"Test","date":"2025-01-01","type":"bad_type"}""",
        aliceToken,
      )
    assertEquals(400, status)
    assertTrue(body.contains("type") || body.contains("Invalid"), "Expected type error in: $body")
  }

  @Test
  fun `create expense with invalid date returns 400`() {
    val (status, body) =
      UnitHttpHelper.post(
        "/api/v1/expenses",
        """{"amount":"10.00","currency":"USD","category":"food","description":"Test","date":"not-a-date","type":"expense"}""",
        aliceToken,
      )
    assertEquals(400, status)
    assertTrue(body.contains("date") || body.contains("Invalid"), "Expected date error in: $body")
  }

  @Test
  fun `update expense with invalid UUID in path returns 404`() {
    val (status, _) =
      UnitHttpHelper.put(
        "/api/v1/expenses/not-a-uuid",
        """{"amount":"10.00","currency":"USD","category":"food","description":"Test","date":"2025-01-01","type":"expense"}""",
        aliceToken,
      )
    assertEquals(404, status)
  }

  @Test
  fun `update non-existent expense returns 404`() {
    val (status, _) =
      UnitHttpHelper.put(
        "/api/v1/expenses/${UUID.randomUUID()}",
        """{"amount":"10.00","currency":"USD","category":"food","description":"Test","date":"2025-01-01","type":"expense"}""",
        aliceToken,
      )
    assertEquals(404, status)
  }

  @Test
  fun `update expense with invalid type returns 400`() {
    val (status, body) =
      UnitHttpHelper.put(
        "/api/v1/expenses/$aliceExpenseId",
        """{"amount":"10.00","currency":"USD","category":"food","description":"Test","date":"2025-01-01","type":"bad_type"}""",
        aliceToken,
      )
    assertEquals(400, status)
    assertTrue(body.contains("type") || body.contains("Invalid"), "Expected type error in: $body")
  }

  @Test
  fun `update expense with invalid date returns 400`() {
    val (status, body) =
      UnitHttpHelper.put(
        "/api/v1/expenses/$aliceExpenseId",
        """{"amount":"10.00","currency":"USD","category":"food","description":"Test","date":"not-a-date","type":"expense"}""",
        aliceToken,
      )
    assertEquals(400, status)
    assertTrue(body.contains("date") || body.contains("Invalid"), "Expected date error in: $body")
  }

  @Test
  fun `delete expense with invalid UUID in path returns 404`() {
    val (status, _) = UnitHttpHelper.delete("/api/v1/expenses/not-a-uuid", aliceToken)
    assertEquals(404, status)
  }

  @Test
  fun `delete non-existent expense returns 404`() {
    val (status, _) = UnitHttpHelper.delete("/api/v1/expenses/${UUID.randomUUID()}", aliceToken)
    assertEquals(404, status)
  }

  // ---- AdminRoutes error paths ----

  @Test
  fun `non-admin user gets 403 on admin disable endpoint`() {
    val (status, _) =
      UnitHttpHelper.post(
        "/api/v1/admin/users/${UUID.randomUUID()}/disable",
        """{"reason":"test"}""",
        aliceToken,
      )
    assertEquals(403, status)
  }

  @Test
  fun `non-admin user gets 403 on admin enable endpoint`() {
    val (status, _) =
      UnitHttpHelper.post("/api/v1/admin/users/${UUID.randomUUID()}/enable", "", aliceToken)
    assertEquals(403, status)
  }

  @Test
  fun `non-admin user gets 403 on admin force-password-reset endpoint`() {
    val (status, _) =
      UnitHttpHelper.post(
        "/api/v1/admin/users/${UUID.randomUUID()}/force-password-reset",
        "",
        aliceToken,
      )
    assertEquals(403, status)
  }

  @Test
  fun `admin can list users`() {
    val (status, body) = UnitHttpHelper.get("/api/v1/admin/users", adminToken)
    assertEquals(200, status)
    assertTrue(body.contains("data"), "Expected data in response: $body")
  }

  @Test
  fun `admin disable user with invalid UUID returns 404`() {
    val (status, _) =
      UnitHttpHelper.post(
        "/api/v1/admin/users/not-a-uuid/disable",
        """{"reason":"test"}""",
        adminToken,
      )
    assertEquals(404, status)
  }

  @Test
  fun `admin disable non-existent user returns 404`() {
    val (status, _) =
      UnitHttpHelper.post(
        "/api/v1/admin/users/${UUID.randomUUID()}/disable",
        """{"reason":"not found"}""",
        adminToken,
      )
    assertEquals(404, status)
  }

  @Test
  fun `admin disable existing user returns 200`() {
    val (status, body) =
      UnitHttpHelper.post(
        "/api/v1/admin/users/$aliceUserId/disable",
        """{"reason":"test disable"}""",
        adminToken,
      )
    assertEquals(200, status)
    assertTrue(body.contains("DISABLED"), "Expected disabled status in: $body")
    // Re-enable alice so other tests can use her token
    UnitHttpHelper.post("/api/v1/admin/users/$aliceUserId/enable", "", adminToken)
  }

  @Test
  fun `admin enable user with invalid UUID returns 404`() {
    val (status, _) = UnitHttpHelper.post("/api/v1/admin/users/not-a-uuid/enable", "", adminToken)
    assertEquals(404, status)
  }

  @Test
  fun `admin enable non-existent user returns 404`() {
    val (status, _) =
      UnitHttpHelper.post("/api/v1/admin/users/${UUID.randomUUID()}/enable", "", adminToken)
    assertEquals(404, status)
  }

  @Test
  fun `admin force-password-reset with invalid UUID returns 404`() {
    val (status, _) =
      UnitHttpHelper.post("/api/v1/admin/users/not-a-uuid/force-password-reset", "", adminToken)
    assertEquals(404, status)
  }

  @Test
  fun `admin force-password-reset non-existent user returns 404`() {
    val (status, _) =
      UnitHttpHelper.post(
        "/api/v1/admin/users/${UUID.randomUUID()}/force-password-reset",
        "",
        adminToken,
      )
    assertEquals(404, status)
  }

  // ---- AttachmentRoutes error paths ----

  @Test
  fun `get attachments with invalid expense UUID returns 404`() {
    val (status, _) = UnitHttpHelper.get("/api/v1/expenses/not-a-uuid/attachments", aliceToken)
    assertEquals(404, status)
  }

  @Test
  fun `get attachments for non-existent expense returns 404`() {
    val (status, _) =
      UnitHttpHelper.get("/api/v1/expenses/${UUID.randomUUID()}/attachments", aliceToken)
    assertEquals(404, status)
  }

  @Test
  fun `delete attachment with invalid attachment UUID returns 404`() {
    val (status, _) =
      UnitHttpHelper.delete("/api/v1/expenses/$aliceExpenseId/attachments/not-a-uuid", aliceToken)
    assertEquals(404, status)
  }

  @Test
  fun `delete attachment with invalid expense UUID returns 404`() {
    val (status, _) =
      UnitHttpHelper.delete(
        "/api/v1/expenses/not-a-uuid/attachments/${UUID.randomUUID()}",
        aliceToken,
      )
    assertEquals(404, status)
  }

  // ---- ReportRoutes error paths ----

  @Test
  fun `report pl without from parameter returns 400`() {
    val (status, _) =
      UnitHttpHelper.get("/api/v1/reports/pl?to=2025-01-31&currency=USD", aliceToken)
    assertEquals(400, status)
  }

  @Test
  fun `report pl without to parameter returns 400`() {
    val (status, _) =
      UnitHttpHelper.get("/api/v1/reports/pl?from=2025-01-01&currency=USD", aliceToken)
    assertEquals(400, status)
  }

  @Test
  fun `report pl without currency parameter returns 400`() {
    val (status, _) =
      UnitHttpHelper.get("/api/v1/reports/pl?from=2025-01-01&to=2025-01-31", aliceToken)
    assertEquals(400, status)
  }

  @Test
  fun `report pl with invalid from date returns 400`() {
    val (status, _) =
      UnitHttpHelper.get(
        "/api/v1/reports/pl?from=not-a-date&to=2025-01-31&currency=USD",
        aliceToken,
      )
    assertEquals(400, status)
  }

  @Test
  fun `report pl with invalid to date returns 400`() {
    val (status, _) =
      UnitHttpHelper.get(
        "/api/v1/reports/pl?from=2025-01-01&to=not-a-date&currency=USD",
        aliceToken,
      )
    assertEquals(400, status)
  }

  @Test
  fun `report pl for IDR currency uses zero scale`() {
    // Creates an IDR expense and requests IDR report to cover the IDR scale branch
    UnitHttpHelper.post(
      "/api/v1/expenses",
      """{"amount":"150000","currency":"IDR","category":"food","description":"Test IDR","date":"2025-06-01","type":"expense"}""",
      aliceToken,
    )
    val (status, body) =
      UnitHttpHelper.get(
        "/api/v1/reports/pl?from=2025-06-01&to=2025-06-30&currency=IDR",
        aliceToken,
      )
    assertEquals(200, status)
    assertTrue(body.contains("IDR"), "Expected IDR in response: $body")
  }

  // ---- AuthRoutes error paths ----

  @Test
  fun `logout without Authorization header succeeds with 200`() {
    val (status, body) = UnitHttpHelper.post("/api/v1/auth/logout", "", null)
    assertEquals(200, status)
    assertTrue(body.contains("Logged out"), "Expected logged out message: $body")
  }

  @Test
  fun `logout with invalid token succeeds with 200`() {
    val fakeAuthHeader = "Bearer invalid.token.here"
    val request =
      java.net.http.HttpRequest.newBuilder()
        .uri(java.net.URI.create("${UnitTestWorld.baseUrl()}/api/v1/auth/logout"))
        .header("Authorization", fakeAuthHeader)
        .header("Content-Type", "application/json")
        .header("Accept", "application/json")
        .POST(java.net.http.HttpRequest.BodyPublishers.ofString(""))
        .build()
    val client = java.net.http.HttpClient.newHttpClient()
    val response = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString())
    assertEquals(200, response.statusCode())
    assertTrue(response.body().contains("Logged out"), "Expected logged out: ${response.body()}")
  }

  // ---- StatusPages error paths ----

  @Test
  fun `malformed JSON body returns 500 via generic exception handler`() {
    val (status, body) = UnitHttpHelper.post("/api/v1/auth/register", "not valid json {{{", null)
    assertEquals(500, status)
    assertTrue(body.contains("Internal server error"), "Expected internal server error in: $body")
  }
}
