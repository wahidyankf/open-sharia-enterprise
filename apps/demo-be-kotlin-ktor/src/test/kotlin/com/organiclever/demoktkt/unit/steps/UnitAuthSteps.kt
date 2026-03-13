package com.organiclever.demoktkt.unit.steps

import com.organiclever.demoktkt.auth.PasswordService
import com.organiclever.demoktkt.domain.Role
import com.organiclever.demoktkt.domain.UserStatus
import com.organiclever.demoktkt.infrastructure.repositories.CreateUserRequest
import com.organiclever.demoktkt.infrastructure.repositories.UpdateUserPatch
import io.cucumber.java.en.Given
import io.cucumber.java.en.When
import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions.assertTrue

class UnitAuthSteps {
  private val passwordService = PasswordService()

  private fun registerUserDirect(username: String, email: String, password: String) {
    runBlocking {
      val existing = UnitTestWorld.userRepo.findByUsername(username)
      if (existing == null) {
        val user =
          UnitTestWorld.userRepo.create(
            CreateUserRequest(
              username = username,
              email = email,
              displayName = username,
              passwordHash = passwordService.hash(password),
              role = Role.USER,
            )
          )
        UnitTestWorld.userIds[username] = user.id.toString()
      } else {
        UnitTestWorld.userIds[username] = existing.id.toString()
      }
    }
  }

  private fun loginAs(username: String, password: String): Boolean {
    val (status, body) =
      UnitHttpHelper.post(
        "/api/v1/auth/login",
        """{"username":"$username","password":"$password"}""",
      )
    if (status == 200) {
      UnitJsonHelper.getString(body, "access_token")?.let {
        UnitTestWorld.accessTokens[username] = it
      }
      UnitJsonHelper.getString(body, "refresh_token")?.let {
        UnitTestWorld.refreshTokens[username] = it
      }
      return true
    }
    return false
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
      val existing = UnitTestWorld.userRepo.findByUsername(username)
      if (existing == null) {
        val u =
          UnitTestWorld.userRepo.createAdmin(
            username,
            "$username@example.com",
            passwordService.hash(password),
          )
        UnitTestWorld.userIds[username] = u.id.toString()
      } else {
        UnitTestWorld.userIds[username] = existing.id.toString()
      }
    }
    loginAs(username, password)
  }

  @Given("a user {string} is registered and deactivated")
  fun aUserIsRegisteredAndDeactivated(username: String) {
    runBlocking {
      val user = UnitTestWorld.userRepo.findByUsername(username) ?: return@runBlocking
      UnitTestWorld.userRepo.update(user.id, UpdateUserPatch(status = UserStatus.INACTIVE))
    }
  }

  @Given("a user {string} is registered and locked after too many failed logins")
  fun aUserIsRegisteredAndLockedAfterTooManyFailedLogins(username: String) {
    registerUserDirect(username, "$username@example.com", "Str0ng#Pass1")
    runBlocking {
      val user = UnitTestWorld.userRepo.findByUsername(username) ?: return@runBlocking
      UnitTestWorld.userRepo.update(
        user.id,
        UpdateUserPatch(status = UserStatus.LOCKED, failedLoginCount = 5),
      )
      UnitTestWorld.userIds[username] = user.id.toString()
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
    repeat(5) {
      UnitHttpHelper.post(
        "/api/v1/auth/login",
        """{"username":"$username","password":"WrongPass#1234"}""",
      )
    }
    runBlocking {
      UnitTestWorld.userRepo.findByUsername(username)?.let {
        UnitTestWorld.userIds[username] = it.id.toString()
      }
    }
  }

  @Given("alice's refresh token has expired")
  fun alicesRefreshTokenHasExpired() {
    val aliceId =
      runBlocking { UnitTestWorld.userRepo.findByUsername("alice")?.id } ?: error("alice not found")
    val expiredToken = UnitTestWorld.jwtService.generateExpiredRefreshToken(aliceId)
    UnitTestWorld.refreshTokens["alice"] = expiredToken
  }

  @Given("alice has used her refresh token to get a new token pair")
  fun aliceHasUsedRefreshTokenToGetNewPair() {
    val refreshToken = UnitTestWorld.refreshTokens["alice"] ?: error("alice has no refresh token")
    val (status, body) =
      UnitHttpHelper.post(
        "/api/v1/auth/refresh",
        """{"refresh_token":"$refreshToken"}""",
        UnitTestWorld.accessTokens["alice"],
      )
    assertTrue(status == 200, "Refresh should succeed. Status: $status Body: $body")
    UnitJsonHelper.getString(body, "access_token")?.let {
      UnitTestWorld.accessTokens["alice:new"] = it
    }
    UnitJsonHelper.getString(body, "refresh_token")?.let {
      UnitTestWorld.refreshTokens["alice:new"] = it
    }
  }

  @Given("alice has already logged out once")
  fun aliceHasAlreadyLoggedOutOnce() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    UnitHttpHelper.post("/api/v1/auth/logout", "", token)
  }

  @Given("the user {string} has been deactivated")
  fun theUserHasBeenDeactivated(username: String) {
    runBlocking {
      val user = UnitTestWorld.userRepo.findByUsername(username) ?: return@runBlocking
      UnitTestWorld.userRepo.update(user.id, UpdateUserPatch(status = UserStatus.INACTIVE))
    }
  }

  @Given("an admin has unlocked alice's account")
  fun anAdminHasUnlockedAlicesAccount() {
    runBlocking {
      val alice = UnitTestWorld.userRepo.findByUsername("alice") ?: return@runBlocking
      UnitTestWorld.userRepo.update(
        alice.id,
        UpdateUserPatch(status = UserStatus.ACTIVE, failedLoginCount = 0),
      )
    }
  }

  @Given("alice's account has been disabled by the admin")
  fun alicesAccountHasBeenDisabledByAdmin() {
    runBlocking {
      val alice = UnitTestWorld.userRepo.findByUsername("alice") ?: return@runBlocking
      UnitTestWorld.userRepo.update(alice.id, UpdateUserPatch(status = UserStatus.DISABLED))
      UnitTestWorld.tokenRepo.revokeAllForUser(alice.id)
    }
  }

  @Given("alice's account has been disabled")
  fun alicesAccountHasBeenDisabled() {
    runBlocking {
      val alice = UnitTestWorld.userRepo.findByUsername("alice") ?: return@runBlocking
      UnitTestWorld.userRepo.update(alice.id, UpdateUserPatch(status = UserStatus.DISABLED))
    }
  }

  @Given("alice has logged out and her access token is blacklisted")
  fun aliceHasLoggedOutAndTokenIsBlacklisted() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    UnitHttpHelper.post("/api/v1/auth/logout", "", token)
  }

  @Given(
    "the admin has disabled alice's account via POST \\/api\\/v1\\/admin\\/users\\/\\{alice_id\\}\\/disable"
  )
  fun theAdminHasDisabledAlicesAccount() {
    val aliceId = UnitTestWorld.userIds["alice"] ?: error("alice id not stored")
    val adminToken = UnitTestWorld.accessTokens["superadmin"] ?: error("admin token not stored")
    UnitHttpHelper.post("/api/v1/admin/users/$aliceId/disable", """{"reason":"test"}""", adminToken)
  }

  @Given("alice has deactivated her own account via POST \\/api\\/v1\\/users\\/me\\/deactivate")
  fun aliceHasDeactivatedHerOwnAccount() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    UnitHttpHelper.post("/api/v1/users/me/deactivate", "", token)
  }

  // ---- When steps ----

  @When(
    "^the client sends POST /api/v1/auth/register with body \\{ \"username\": \"([^\"]+)\", \"email\": \"([^\"]+)\", \"password\": \"([^\"]*)\" \\}$"
  )
  fun theClientSendsPostRegisterWithBody(username: String, email: String, password: String) {
    val body = """{"username":"$username","email":"$email","password":"$password"}"""
    val (status, respBody) = UnitHttpHelper.post("/api/v1/auth/register", body)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
    if (status == 201) {
      UnitJsonHelper.getString(respBody, "id")?.let { UnitTestWorld.userIds[username] = it }
    }
  }

  @When(
    "^the client sends POST /api/v1/auth/login with body \\{ \"username\": \"([^\"]+)\", \"password\": \"([^\"]+)\" \\}$"
  )
  fun theClientSendsPostLoginWithBody(username: String, password: String) {
    val body = """{"username":"$username","password":"$password"}"""
    val (status, respBody) = UnitHttpHelper.post("/api/v1/auth/login", body)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
  }

  @When("alice sends POST \\/api\\/v1\\/auth\\/refresh with her refresh token")
  fun aliceSendsPostRefreshWithRefreshToken() {
    val refreshToken = UnitTestWorld.refreshTokens["alice"] ?: error("alice has no refresh token")
    val accessToken = UnitTestWorld.accessTokens["alice"]
    val body = """{"refresh_token":"$refreshToken"}"""
    val (status, respBody) = UnitHttpHelper.post("/api/v1/auth/refresh", body, accessToken)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
  }

  @When("alice sends POST \\/api\\/v1\\/auth\\/logout with her access token")
  fun aliceSendsPostLogoutWithAccessToken() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, respBody) = UnitHttpHelper.post("/api/v1/auth/logout", "", token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
  }

  @When("alice sends POST \\/api\\/v1\\/auth\\/logout-all with her access token")
  fun aliceSendsPostLogoutAllWithAccessToken() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, respBody) = UnitHttpHelper.post("/api/v1/auth/logout-all", "", token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
  }

  @When("^the client sends GET /api/v1/users/me with alice's access token$")
  fun theClientSendsGetUsersMeWithAlicesToken() {
    val token = UnitTestWorld.accessTokens["alice"] ?: error("alice has no access token")
    val (status, body) = UnitHttpHelper.get("/api/v1/users/me", token)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When(
    "^the client sends POST /api/v1/expenses with body \\{ \"amount\": \"([^\"]+)\", \"currency\": \"([^\"]+)\", \"category\": \"([^\"]+)\", \"description\": \"([^\"]+)\", \"date\": \"([^\"]+)\", \"type\": \"([^\"]+)\" \\}$"
  )
  fun theClientSendsPostExpensesWithBody(
    amount: String,
    currency: String,
    category: String,
    description: String,
    date: String,
    type: String,
  ) {
    val body =
      """{"amount":"$amount","currency":"$currency","category":"$category","description":"$description","date":"$date","type":"$type"}"""
    val (status, respBody) = UnitHttpHelper.post("/api/v1/expenses", body)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = respBody
  }
}
