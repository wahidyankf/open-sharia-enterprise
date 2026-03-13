package com.demobektkt.integration.steps

import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions.assertEquals

class SecuritySteps {

    @Then("alice's account status should be {string}")
    fun alicesAccountStatusShouldBe(expectedStatus: String) {
        val user =
            runBlocking { TestWorld.userRepo.findByUsername("alice") } ?: error("alice not found")
        assertEquals(
            expectedStatus.lowercase(),
            user.status.name.lowercase(),
            "alice's account status should be $expectedStatus",
        )
    }

    @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/unlock$")
    fun theAdminSendsPostUnlockAlice() {
        val aliceId = TestWorld.userIds["alice"] ?: error("alice id not stored")
        val adminToken = TestWorld.accessTokens["superadmin"] ?: error("admin token not stored")
        val (status, body) = ServiceDispatcher.unlockUser(adminToken, aliceId)
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }
}
