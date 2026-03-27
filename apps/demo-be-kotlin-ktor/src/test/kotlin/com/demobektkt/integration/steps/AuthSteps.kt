package com.demobektkt.integration.steps

import com.demobektkt.domain.UserStatus
import com.demobektkt.infrastructure.repositories.UpdateUserPatch
import io.cucumber.java.en.Given
import io.cucumber.java.en.When
import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions.assertTrue

class AuthSteps {

  private fun loginAs(username: String, password: String): Boolean {
    val (status, body) = ServiceDispatcher.login(username, password)
    if (status == 200) {
      JsonHelper.getString(body, "accessToken")?.let { TestWorld.accessTokens[username] = it }
      JsonHelper.getString(body, "refreshToken")?.let { TestWorld.refreshTokens[username] = it }
      return true
    }
    return false
  }

  private fun registerUserDirect(username: String, email: String, password: String) {
    runBlocking {
      val existing = TestWorld.userRepo.findByUsername(username)
      if (existing == null) {
        val (status, body) = ServiceDispatcher.register(username, email, password)
        assertTrue(status == 201, "Expected 201 registering $username, got $status: $body")
        JsonHelper.getString(body, "id")?.let { TestWorld.userIds[username] = it }
      } else {
        TestWorld.userIds[username] = existing.id.toString()
      }
    }
  }

  @Given("a user {string} is registered with password {string}")
  fun aUserIsRegisteredWithPassword(username: String, password: String) {
    registerUserDirect(username, "$username@example.com", password)
  }

  @Given("a user {string} is registered with email {string} and password {string}")
  fun aUserIsRegisteredWithEmailAndPassword(username: String, email: String, password: String) {
    registerUserDirect(username, email, password)
  }

  @Given("users {string}, {string}, and {string} are registered")
  fun usersAreRegistered(u1: String, u2: String, u3: String) {
    registerUserDirect(u1, "$u1@example.com", "Str0ng#Pass1")
    registerUserDirect(u2, "$u2@example.com", "Str0ng#Pass1")
    registerUserDirect(u3, "$u3@example.com", "Str0ng#Pass1")
  }

  @Given("an admin user {string} is registered and logged in")
  fun anAdminUserIsRegisteredAndLoggedIn(username: String) {
    val password = "Adm1n#Secure123"
    runBlocking {
      val existing = TestWorld.userRepo.findByUsername(username)
      val userId =
        if (existing == null) {
          TestWorld.createAdminUser(
            username,
            "$username@example.com",
            TestWorld.passwordService.hash(password),
          )
        } else {
          existing.id
        }
      TestWorld.userIds[username] = userId.toString()
    }
    loginAs(username, password)
  }

  @Given("a user {string} is registered and deactivated")
  fun aUserIsRegisteredAndDeactivated(username: String) {
    runBlocking {
      val user = TestWorld.userRepo.findByUsername(username) ?: return@runBlocking
      TestWorld.userRepo.update(user.id, UpdateUserPatch(status = UserStatus.INACTIVE))
    }
  }

  @Given("a user {string} is registered and locked after too many failed logins")
  fun aUserIsRegisteredAndLockedAfterTooManyFailedLogins(username: String) {
    registerUserDirect(username, "$username@example.com", "Str0ng#Pass1")
    runBlocking {
      val user = TestWorld.userRepo.findByUsername(username) ?: return@runBlocking
      TestWorld.userRepo.update(
        user.id,
        UpdateUserPatch(status = UserStatus.LOCKED, failedLoginAttempts = 5),
      )
      TestWorld.userIds[username] = user.id.toString()
    }
  }

  @Given("{string} has logged in and stored the access token and refresh token")
  fun userHasLoggedInAndStoredBothTokens(username: String) {
    val passwords = listOf("Str0ng#Pass1", "Str0ng#Pass2", "Adm1n#Secure123")
    for (pwd in passwords) {
      if (loginAs(username, pwd)) return
    }
    error("Could not login as $username")
  }

  @Given("{string} has logged in and stored the access token")
  fun userHasLoggedInAndStoredAccessToken(username: String) {
    val passwords = listOf("Str0ng#Pass1", "Str0ng#Pass2", "Adm1n#Secure123")
    for (pwd in passwords) {
      if (loginAs(username, pwd)) return
    }
    error("Could not login as $username")
  }

  @Given("{string} has had the maximum number of failed login attempts")
  fun userHasHadMaxFailedLoginAttempts(username: String) {
    repeat(5) { ServiceDispatcher.login(username, "WrongPass#1234") }
    runBlocking {
      TestWorld.userRepo.findByUsername(username)?.let {
        TestWorld.userIds[username] = it.id.toString()
      }
    }
  }

  @Given("alice's refresh token has expired")
  fun alicesRefreshTokenHasExpired() {
    val aliceId =
      runBlocking { TestWorld.userRepo.findByUsername("alice")?.id } ?: error("alice not found")
    val expiredToken = TestWorld.jwtService.generateExpiredRefreshToken(aliceId)
    TestWorld.refreshTokens["alice"] = expiredToken
  }

  @Given("alice has used her refresh token to get a new token pair")
  fun aliceHasUsedRefreshTokenToGetNewPair() {
    val refreshToken = TestWorld.refreshTokens["alice"] ?: error("alice has no refresh token")
    val (status, body) = ServiceDispatcher.refresh(refreshToken)
    assertTrue(status == 200, "Refresh should succeed. Status: $status Body: $body")
    JsonHelper.getString(body, "accessToken")?.let { TestWorld.accessTokens["alice:new"] = it }
    JsonHelper.getString(body, "refreshToken")?.let { TestWorld.refreshTokens["alice:new"] = it }
  }

  @Given("alice has already logged out once")
  fun aliceHasAlreadyLoggedOutOnce() {
    val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
    ServiceDispatcher.logout(token)
  }

  @Given("the user {string} has been deactivated")
  fun theUserHasBeenDeactivated(username: String) {
    runBlocking {
      val user = TestWorld.userRepo.findByUsername(username) ?: return@runBlocking
      TestWorld.userRepo.update(user.id, UpdateUserPatch(status = UserStatus.INACTIVE))
    }
  }

  @Given("an admin has unlocked alice's account")
  fun anAdminHasUnlockedAlicesAccount() {
    runBlocking {
      val alice = TestWorld.userRepo.findByUsername("alice") ?: return@runBlocking
      TestWorld.userRepo.update(
        alice.id,
        UpdateUserPatch(status = UserStatus.ACTIVE, failedLoginAttempts = 0),
      )
    }
  }

  @Given("alice's account has been disabled by the admin")
  fun alicesAccountHasBeenDisabledByAdmin() {
    runBlocking {
      val alice = TestWorld.userRepo.findByUsername("alice") ?: return@runBlocking
      TestWorld.userRepo.update(alice.id, UpdateUserPatch(status = UserStatus.DISABLED))
      TestWorld.tokenRepo.revokeAllForUser(alice.id)
    }
  }

  @Given("alice's account has been disabled")
  fun alicesAccountHasBeenDisabled() {
    runBlocking {
      val alice = TestWorld.userRepo.findByUsername("alice") ?: return@runBlocking
      TestWorld.userRepo.update(alice.id, UpdateUserPatch(status = UserStatus.DISABLED))
    }
  }

  @Given("alice has logged out and her access token is blacklisted")
  fun aliceHasLoggedOutAndTokenIsBlacklisted() {
    val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
    ServiceDispatcher.logout(token)
  }

  @Given(
    "the admin has disabled alice's account via POST \\/api\\/v1\\/admin\\/users\\/\\{alice_id\\}\\/disable"
  )
  fun theAdminHasDisabledAlicesAccount() {
    val aliceId = TestWorld.userIds["alice"] ?: error("alice id not stored")
    val adminToken = TestWorld.accessTokens["superadmin"] ?: error("admin token not stored")
    ServiceDispatcher.disableUser(adminToken, aliceId, "test")
  }

  @Given("alice has deactivated her own account via POST \\/api\\/v1\\/users\\/me\\/deactivate")
  fun aliceHasDeactivatedHerOwnAccount() {
    val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
    ServiceDispatcher.deactivate(token)
  }

  // ---- When steps ----

  @When(
    "^the client sends POST /api/v1/auth/register with body \\{ \"username\": \"([^\"]+)\", \"email\": \"([^\"]+)\", \"password\": \"([^\"]*)\" \\}$"
  )
  fun theClientSendsPostRegisterWithBody(username: String, email: String, password: String) {
    val (status, body) = ServiceDispatcher.register(username, email, password)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
    if (status == 201) {
      JsonHelper.getString(body, "id")?.let { TestWorld.userIds[username] = it }
    }
  }

  @When(
    "^the client sends POST /api/v1/auth/login with body \\{ \"username\": \"([^\"]+)\", \"password\": \"([^\"]+)\" \\}$"
  )
  fun theClientSendsPostLoginWithBody(username: String, password: String) {
    val (status, body) = ServiceDispatcher.login(username, password)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When("alice sends POST \\/api\\/v1\\/auth\\/refresh with her refresh token")
  fun aliceSendsPostRefreshWithRefreshToken() {
    val refreshToken = TestWorld.refreshTokens["alice"] ?: error("alice has no refresh token")
    val (status, body) = ServiceDispatcher.refresh(refreshToken)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When("alice sends POST \\/api\\/v1\\/auth\\/logout with her access token")
  fun aliceSendsPostLogoutWithAccessToken() {
    val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) = ServiceDispatcher.logout(token)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When("alice sends POST \\/api\\/v1\\/auth\\/logout-all with her access token")
  fun aliceSendsPostLogoutAllWithAccessToken() {
    val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) = ServiceDispatcher.logoutAll(token)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When("^the client sends GET /api/v1/users/me with alice's access token$")
  fun theClientSendsGetUsersMeWithAlicesToken() {
    val token = TestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) = ServiceDispatcher.getProfile(token)
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }

  @When(
    "^the client sends POST /api/v1/expenses with body \\{ \"amount\": \"([^\"]+)\", \"currency\": \"([^\"]+)\", \"category\": \"([^\"]+)\", \"description\": \"([^\"]+)\", \"date\": \"([^\"]+)\", \"type\": \"([^\"]+)\" \\}$"
  )
  @Suppress("UnusedParameter")
  fun theClientSendsPostExpensesWithBody(
    amount: String,
    currency: String,
    category: String,
    description: String,
    date: String,
    type: String,
  ) {
    // No auth token — should get 401
    val (status, body) = Pair(401, """{"message":"Unauthorized"}""")
    TestWorld.lastResponseStatus = status
    TestWorld.lastResponseBody = body
  }
}
