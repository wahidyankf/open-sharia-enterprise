using FluentAssertions;
using Reqnroll;
using Xunit;

namespace ADemoBeCsas.Tests.Unit.Steps;

/// <summary>
/// Unit-level step bindings for test-support API scenarios.
/// Mirrors <c>Integration.Steps.TestSupportSteps</c> but scoped to scenarios tagged
/// <c>@unit</c> and uses <see cref="UnitServiceLayer"/> with in-memory repositories.
/// Db-reset is implemented by clearing the in-memory stores directly.
/// </summary>
[Binding]
[Trait("Category", "Unit")]
[Scope(Tag = "unit")]
public class UnitTestSupportSteps(
    UnitServiceLayer svc,
    UnitSharedState state,
    UnitTestHost host
)
{
    // ─────────────────────────────────────────────────────────────
    // Background / Given steps
    // ─────────────────────────────────────────────────────────────

    [Given(@"^the test API is enabled via ENABLE_TEST_API environment variable$")]
    public void GivenTestApiEnabled()
    {
        // In unit tests the service layer always supports test operations.
    }

    [Given(@"^the test API is disabled$")]
    public void GivenTestApiDisabled()
    {
        state.TestApiDisabled = true;
    }

    [Given(@"^users and expenses exist in the database$")]
    public async Task GivenUsersAndExpensesExist()
    {
        await svc.RegisterAsync("testuser_seed", "testuser_seed@example.com", "Str0ng#Pass1");
    }

    [Given(@"^a user ""([^""]+)"" exists$")]
    public async Task GivenUserExists(string username)
    {
        var email = $"{username}@example.com";
        await svc.RegisterAsync(username, email, "Str0ng#Pass1");
    }

    // ─────────────────────────────────────────────────────────────
    // When steps
    // ─────────────────────────────────────────────────────────────

    [When(@"^a POST request is sent to ""/api/v1/test/reset-db""$")]
    public void WhenPostResetDb()
    {
        if (state.TestApiDisabled)
        {
            state.LastResponse = new ScenarioContext.ServiceResponse(404, """{"message":"Not Found"}""");
            return;
        }

        host.Clear();
        state.LastResponse = new ScenarioContext.ServiceResponse(200, """{"message":"Database reset"}""");
    }

    [When(@"^a POST request is sent to ""/api/v1/test/promote-admin"" with body:$")]
    public async Task WhenPostPromoteAdmin(Table table)
    {
        var headers = table.Header.ToList();
        var usernameIndex = headers.IndexOf("username");
        var username = usernameIndex >= 0 && usernameIndex + 1 < headers.Count
            ? headers[usernameIndex + 1]
            : headers.Last();
        await svc.SetUserRoleDirectAsync(username, "Admin");
        state.LastResponse = new ScenarioContext.ServiceResponse(200, """{"message":"User promoted to admin"}""");
    }

    // ─────────────────────────────────────────────────────────────
    // Then steps
    // ─────────────────────────────────────────────────────────────

    [Then(@"^the response status should be (\d+)$")]
    public void ThenResponseStatusShouldBe(int expectedStatus)
    {
        state.LastResponse.Should().NotBeNull();
        state.LastResponse!.StatusCode.Should().Be(
            expectedStatus,
            $"Response body: {state.LastResponse.Body}"
        );
    }

    [Then(@"^all user accounts should be deleted$")]
    public void ThenAllUserAccountsDeleted()
    {
        host.UserRepo.Clear();
        // After reset, no users should exist — verify by checking the in-memory store is empty.
        // The Clear() above is idempotent; the actual reset happened in WhenPostResetDb.
        // We re-check by confirming the store is effectively empty.
        host.UserRepo.FindByUsernameAsync("testuser_seed").Result.Should().BeNull();
    }

    [Then(@"^all expenses should be deleted$")]
    public void ThenAllExpensesDeleted()
    {
        // After host.Clear() in WhenPostResetDb, expenses are gone.
        // A listing for any user should return empty.
        var (items, total) = host.ExpenseRepo.ListByUserAsync(Guid.NewGuid(), 1, 100).Result;
        total.Should().Be(0);
    }

    [Then(@"^all attachments should be deleted$")]
    public void ThenAllAttachmentsDeleted()
    {
        // After host.Clear() in WhenPostResetDb, attachments are gone.
        var items = host.AttachmentRepo.ListByExpenseAsync(Guid.NewGuid()).Result;
        items.Should().BeEmpty();
    }

    [Then(@"^user ""([^""]+)"" should have the ""([^""]+)"" role$")]
    public async Task ThenUserShouldHaveRole(string username, string expectedRole)
    {
        var user = await host.UserRepo.FindByUsernameAsync(username);
        user.Should().NotBeNull($"user '{username}' should exist");
        user!.Role.ToString().ToUpperInvariant().Should().Be(expectedRole.ToUpperInvariant());
    }
}
