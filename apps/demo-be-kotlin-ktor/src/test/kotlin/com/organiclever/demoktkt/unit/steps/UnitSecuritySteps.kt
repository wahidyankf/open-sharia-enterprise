package com.organiclever.demoktkt.unit.steps

import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions.assertEquals

class UnitSecuritySteps {

  @Then("alice's account status should be {string}")
  fun alicesAccountStatusShouldBe(expectedStatus: String) {
    val user =
      runBlocking { UnitTestWorld.userRepo.findByUsername("alice") } ?: error("alice not found")
    assertEquals(
      expectedStatus.lowercase(),
      user.status.name.lowercase(),
      "alice's account status should be $expectedStatus",
    )
  }

  @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/unlock$")
  fun theAdminSendsPostUnlockAlice() {
    val aliceId = UnitTestWorld.userIds["alice"] ?: error("alice id not stored")
    val adminToken = UnitTestWorld.accessTokens["superadmin"] ?: error("admin token not stored")
    val (status, body) = UnitHttpHelper.post("/api/v1/admin/users/$aliceId/unlock", "", adminToken)
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }
}
