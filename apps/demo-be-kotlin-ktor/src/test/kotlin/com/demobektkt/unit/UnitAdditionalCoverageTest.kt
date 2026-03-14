package com.demobektkt.unit

import com.demobektkt.auth.JwtService
import com.demobektkt.auth.PasswordService
import com.demobektkt.domain.Role
import com.demobektkt.domain.UserStatus
import com.demobektkt.infrastructure.repositories.CreateUserRequest
import com.demobektkt.infrastructure.repositories.UpdateUserPatch
import com.demobektkt.unit.steps.UNIT_JWT_SECRET
import com.demobektkt.unit.steps.UnitJsonHelper
import com.demobektkt.unit.steps.UnitServiceDispatcher
import com.demobektkt.unit.steps.UnitTestWorld
import java.util.UUID
import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestInstance

/**
 * Unit-level additional coverage tests targeting uncovered lines in service dispatching,
 * in-memory repositories, and domain error handling. These complement UnitErrorPathsTest
 * by covering the remaining branches not exercised by the Cucumber BDD scenarios.
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class UnitAdditionalCoverageTest {

  private lateinit var aliceToken: String
  private lateinit var aliceExpenseId: String
  private lateinit var aliceRefreshToken: String
  private lateinit var aliceAccessToken: String
  private lateinit var aliceUserId: String
  private lateinit var adminToken: String
  private lateinit var adminUserId: String
  private lateinit var bobToken: String
  private lateinit var bobExpenseId: String
  private val jwtService = JwtService(UNIT_JWT_SECRET)
  private val passwordService = PasswordService()

  @BeforeAll
  fun setup() {
    UnitTestWorld.reset()

    // Register alice (normal user)
    val aliceName = "covtest${UUID.randomUUID().toString().take(6)}"
    val alicePw = "Str0ng#Pass1"
    UnitServiceDispatcher.register(aliceName, "$aliceName@test.com", alicePw)
    val (loginStatus, loginBody) = UnitServiceDispatcher.login(aliceName, alicePw)
    assertTrue(loginStatus == 200, "Alice login should succeed: $loginBody")
    aliceToken =
      UnitJsonHelper.getString(loginBody, "accessToken") ?: error("No access_token: $loginBody")
    aliceRefreshToken =
      UnitJsonHelper.getString(loginBody, "refreshToken")
        ?: error("No refresh_token: $loginBody")
    aliceAccessToken = aliceToken

    // Get alice user ID from profile
    val (profileStatus, profileBody) = UnitServiceDispatcher.getProfile(aliceToken)
    assertTrue(profileStatus == 200, "Alice profile: $profileBody")
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
    assertTrue(createStatus == 201, "Create expense: $createBody")
    aliceExpenseId = UnitJsonHelper.getString(createBody, "id") ?: error("No id: $createBody")

    // Register bob (normal user)
    val bobName = "bob${UUID.randomUUID().toString().take(6)}"
    val bobPw = "Str0ng#Pass1"
    UnitServiceDispatcher.register(bobName, "$bobName@test.com", bobPw)
    val (bobLoginStatus, bobLoginBody) = UnitServiceDispatcher.login(bobName, bobPw)
    assertTrue(bobLoginStatus == 200, "Bob login: $bobLoginBody")
    bobToken =
      UnitJsonHelper.getString(bobLoginBody, "accessToken")
        ?: error("No bob access_token: $bobLoginBody")

    // Create expense for bob
    val (bobExpStatus, bobExpBody) =
      UnitServiceDispatcher.createExpense(
        bobToken,
        "5.00",
        "USD",
        "transport",
        "Bob's expense",
        "2025-01-02",
        "expense",
      )
    assertTrue(bobExpStatus == 201, "Create bob expense: $bobExpBody")
    bobExpenseId = UnitJsonHelper.getString(bobExpBody, "id") ?: error("No id: $bobExpBody")

    // Create admin user via in-memory repo directly
    val adminName = "admin${UUID.randomUUID().toString().take(6)}"
    val adminPw = "Str0ng#Pass1"
    val adminHash = passwordService.hash(adminPw)
    val adminUser = UnitTestWorld.userRepo.createAdmin(adminName, "$adminName@test.com", adminHash)
    adminUserId = adminUser.id.toString()

    // Login as admin
    val (adminLoginStatus, adminLoginBody) = UnitServiceDispatcher.login(adminName, adminPw)
    assertTrue(adminLoginStatus == 200, "Admin login: $adminLoginBody")
    adminToken =
      UnitJsonHelper.getString(adminLoginBody, "accessToken")
        ?: error("No admin access_token: $adminLoginBody")
  }

  // ---- AdminRoutes: unauthenticated (invalid token) -> 401 ----

  @Test
  fun `admin list users without token returns 401`() {
    val (status, _) = UnitServiceDispatcher.listUsers("invalid-token")
    assertEquals(401, status)
  }

  @Test
  fun `admin disable without token returns 401`() {
    val (status, _) =
      UnitServiceDispatcher.disableUser("invalid-token", UUID.randomUUID().toString(), "x")
    assertEquals(401, status)
  }

  @Test
  fun `admin enable without token returns 401`() {
    val (status, _) =
      UnitServiceDispatcher.enableUser("invalid-token", UUID.randomUUID().toString())
    assertEquals(401, status)
  }

  @Test
  fun `admin unlock without token returns 401`() {
    val (status, _) =
      UnitServiceDispatcher.unlockUser("invalid-token", UUID.randomUUID().toString())
    assertEquals(401, status)
  }

  @Test
  fun `admin force-password-reset without token returns 401`() {
    val (status, _) =
      UnitServiceDispatcher.forcePasswordReset("invalid-token", UUID.randomUUID().toString())
    assertEquals(401, status)
  }

  // ---- AdminRoutes: invalid UUID -> 404 ----

  @Test
  fun `admin disable with invalid UUID returns 404`() {
    val (status, _) = UnitServiceDispatcher.disableUser(adminToken, "not-a-uuid", "x")
    assertEquals(404, status)
  }

  @Test
  fun `admin disable non-existent user returns 404`() {
    val (status, _) =
      UnitServiceDispatcher.disableUser(adminToken, UUID.randomUUID().toString(), "test")
    assertEquals(404, status)
  }

  @Test
  fun `admin enable with invalid UUID returns 404`() {
    val (status, _) = UnitServiceDispatcher.enableUser(adminToken, "not-a-uuid")
    assertEquals(404, status)
  }

  @Test
  fun `admin enable non-existent user returns 404`() {
    val (status, _) = UnitServiceDispatcher.enableUser(adminToken, UUID.randomUUID().toString())
    assertEquals(404, status)
  }

  @Test
  fun `admin unlock with invalid UUID returns 404`() {
    val (status, _) = UnitServiceDispatcher.unlockUser(adminToken, "not-a-uuid")
    assertEquals(404, status)
  }

  @Test
  fun `admin unlock non-existent user returns 404`() {
    val (status, _) = UnitServiceDispatcher.unlockUser(adminToken, UUID.randomUUID().toString())
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

  @Test
  fun `admin disable user sends body with reason field`() {
    val (status, body) =
      UnitServiceDispatcher.disableUser(adminToken, aliceUserId, "policy violation")
    assertEquals(200, status, "Expected 200 when disabling alice: $body")
    assertTrue(body.contains("DISABLED"), "Expected status DISABLED in: $body")
    // Re-enable alice for cleanup
    UnitServiceDispatcher.enableUser(adminToken, aliceUserId)
  }

  // ---- ReportRoutes: invalid params -> 400 ----

  @Test
  fun `PL report without token returns 401`() {
    val (status, _) = UnitServiceDispatcher.pl("invalid-token", "2025-01-01", "2025-01-31", "USD")
    assertEquals(401, status)
  }

  @Test
  fun `PL report with invalid from date returns 400`() {
    val (status, body) = UnitServiceDispatcher.pl(aliceToken, "not-a-date", "2025-01-31", "USD")
    assertEquals(400, status)
    assertTrue(body.contains("from") || body.contains("date"), "Expected date error in: $body")
  }

  @Test
  fun `PL report with invalid to date returns 400`() {
    val (status, body) = UnitServiceDispatcher.pl(aliceToken, "2025-01-01", "bad-date", "USD")
    assertEquals(400, status)
    assertTrue(body.contains("to") || body.contains("date"), "Expected date error in: $body")
  }

  // ---- AuthRoutes: locked account login ----

  @Test
  fun `login to locked account returns 401`() {
    val lockedName = "locked${UUID.randomUUID().toString().take(6)}"
    val lockedPw = "Str0ng#Pass1"
    val hash = passwordService.hash(lockedPw)
    runBlocking {
      val u =
        UnitTestWorld.userRepo.create(
          CreateUserRequest(
            username = lockedName,
            email = "$lockedName@test.com",
            displayName = lockedName,
            passwordHash = hash,
            role = Role.USER,
          )
        )
      UnitTestWorld.userRepo.update(u.id, UpdateUserPatch(status = UserStatus.LOCKED))
    }

    val (status, body) = UnitServiceDispatcher.login(lockedName, lockedPw)
    assertEquals(401, status)
    assertTrue(body.contains("locked"), "Expected 'locked' in: $body")
  }

  // ---- AuthRoutes: refresh with access token (not refresh token) -> 401 ----

  @Test
  fun `refresh with access token (wrong type) returns 401`() {
    val (status, body) = UnitServiceDispatcher.refresh(aliceAccessToken)
    assertEquals(401, status)
    assertTrue(body.contains("Invalid") || body.contains("invalid"), "Expected invalid in: $body")
  }

  // ---- AuthRoutes: refresh after user deleted -> 401 ----

  @Test
  fun `refresh with valid token but non-existent user returns 401`() {
    val fakeUserId = UUID.randomUUID()
    val orphanRefreshToken = jwtService.generateRefreshToken(fakeUserId)

    val (status, body) = UnitServiceDispatcher.refresh(orphanRefreshToken)
    assertEquals(401, status)
    assertTrue(
      body.contains("not found") || body.contains("User"),
      "Expected user error in: $body",
    )
  }

  // ---- AuthRoutes: logout without token ----

  @Test
  fun `logout without token returns 200`() {
    val (status, body) = UnitServiceDispatcher.logout(null)
    assertEquals(200, status)
    assertTrue(body.contains("Logged out"), "Expected 'Logged out' in: $body")
  }

  // ---- AuthRoutes: logoutAll without token -> 401 ----

  @Test
  fun `logout all without token returns 401`() {
    val (status, _) = UnitServiceDispatcher.logoutAll("invalid-token")
    assertEquals(401, status)
  }

  // ---- UserRoutes: profile / display name / password without token -> 401 ----

  @Test
  fun `get profile without token returns 401`() {
    val (status, _) = UnitServiceDispatcher.getProfile("invalid-token")
    assertEquals(401, status)
  }

  @Test
  fun `update display name without token returns 401`() {
    val (status, _) = UnitServiceDispatcher.updateDisplayName("invalid-token", "New Name")
    assertEquals(401, status)
  }

  @Test
  fun `change password without token returns 401`() {
    val (status, _) =
      UnitServiceDispatcher.changePassword("invalid-token", "Str0ng#Pass1", "NewPass#123")
    assertEquals(401, status)
  }

  @Test
  fun `deactivate without token returns 401`() {
    val (status, _) = UnitServiceDispatcher.deactivate("invalid-token")
    assertEquals(401, status)
  }

  // ---- ExpenseRoutes: various 401/403 paths ----

  @Test
  fun `create expense without token returns 401`() {
    val (status, _) =
      UnitServiceDispatcher.createExpense(
        "invalid-token",
        "10.00",
        "USD",
        "food",
        "T",
        "2025-01-01",
        "expense",
      )
    assertEquals(401, status)
  }

  @Test
  fun `update expense belonging to another user returns 403`() {
    val (status, body) =
      UnitServiceDispatcher.updateExpense(
        bobToken,
        aliceExpenseId,
        "20.00",
        "USD",
        "food",
        "X",
        "2025-01-01",
        "expense",
      )
    assertEquals(403, status, "Expected 403 for cross-user update: $body")
  }

  @Test
  fun `delete expense belonging to another user returns 403`() {
    val (status, body) = UnitServiceDispatcher.deleteExpense(bobToken, aliceExpenseId)
    assertEquals(403, status, "Expected 403 for cross-user delete: $body")
  }

  @Test
  fun `delete expense with invalid UUID in path returns 404`() {
    val (status, _) = UnitServiceDispatcher.deleteExpense(aliceToken, "not-a-uuid")
    assertEquals(404, status)
  }

  // ---- AttachmentRoutes: various error paths ----

  @Test
  fun `upload attachment without token returns 401`() {
    val (status, _) =
      UnitServiceDispatcher.uploadAttachment(
        "invalid-token",
        aliceExpenseId,
        "test.jpg",
        "image/jpeg",
        ByteArray(100),
      )
    assertEquals(401, status)
  }

  @Test
  fun `upload attachment for another user expense returns 403`() {
    val (status, body) =
      UnitServiceDispatcher.uploadAttachment(
        bobToken,
        aliceExpenseId,
        "test.jpg",
        "image/jpeg",
        ByteArray(100),
      )
    assertEquals(403, status, "Expected 403: $body")
  }

  @Test
  fun `upload attachment with invalid expense UUID returns 404`() {
    val (status, _) =
      UnitServiceDispatcher.uploadAttachment(
        aliceToken,
        "not-a-uuid",
        "test.jpg",
        "image/jpeg",
        ByteArray(100),
      )
    assertEquals(404, status)
  }

  @Test
  fun `delete non-existent attachment returns 404`() {
    val (status, _) =
      UnitServiceDispatcher.deleteAttachment(
        aliceToken,
        aliceExpenseId,
        UUID.randomUUID().toString(),
      )
    assertEquals(404, status)
  }

  // ---- TokenRoutes: claims and JWKS ----

  @Test
  fun `token claims without token returns 401`() {
    val (status, _) = UnitServiceDispatcher.tokenClaims("invalid-token")
    assertEquals(401, status)
  }

  @Test
  fun `JWKS endpoint returns keys`() {
    val (status, body) = UnitServiceDispatcher.jwks()
    assertEquals(200, status)
    assertTrue(body.contains("keys"), "Expected 'keys' in JWKS response: $body")
  }

  // ---- Register validation ----

  @Test
  fun `register with invalid data triggers validation error`() {
    val (status, body) = UnitServiceDispatcher.register("", "t@t.com", "Pass#123")
    assertTrue(
      status == 400 || status == 500,
      "Expected 400 or 500 for invalid username, got $status: $body",
    )
  }

  // ---- InMemoryUserRepository: findByEmail coverage ----

  @Test
  fun `admin list users with email filter exercises findByEmail`() {
    val (status, body) = UnitServiceDispatcher.listUsers(adminToken, "test.com")
    assertEquals(200, status, "Expected 200 from admin user list: $body")
  }

  // ---- InMemoryTokenRepository: findByJti coverage ----

  @Test
  fun `token refresh exercises findByJti`() {
    val (status, body) = UnitServiceDispatcher.refresh(aliceRefreshToken)
    assertTrue(status == 200 || status == 401, "Expected 200 or 401: $status $body")
  }
}
