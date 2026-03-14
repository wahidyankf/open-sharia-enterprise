using DemoBeCsas.Infrastructure;
using DemoBeCsas.Tests.ScenarioContext;
using FluentAssertions;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using Reqnroll;
using Xunit;

namespace DemoBeCsas.Tests.Integration.Steps;

[Binding]
[Trait("Category", "Integration")]
public class TestSupportSteps(ServiceLayer svc, SharedState state, IntegrationTestHost host)
{
    // ─────────────────────────────────────────────────────────────
    // Background / Given steps
    // ─────────────────────────────────────────────────────────────

    [Given(@"^the test API is enabled via ENABLE_TEST_API environment variable$")]
    public void GivenTestApiEnabled()
    {
        // In unit/integration tests the service layer always supports test operations.
        // No runtime check is needed here.
    }

    [Given(@"^the test API is disabled$")]
    public void GivenTestApiDisabled()
    {
        // Mark in state that we should simulate disabled behaviour (404) rather
        // than actually invoking reset-db.
        state.TestApiDisabled = true;
    }

    [Given(@"^users and expenses exist in the database$")]
    public async Task GivenUsersAndExpensesExist()
    {
        // Register a user so there is at least one user row in the database.
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
    public async Task WhenPostResetDb()
    {
        if (state.TestApiDisabled)
        {
            state.LastResponse = new ServiceResponse(404, """{"message":"Not Found"}""");
            return;
        }

        await svc.CleanDatabaseAsync();
        state.LastResponse = new ServiceResponse(200, """{"message":"Database reset"}""");
    }

    [When(@"^a POST request is sent to ""/api/v1/test/promote-admin"" with body:$")]
    public async Task WhenPostPromoteAdmin(Table table)
    {
        // The Gherkin table "| username | alice |" is a single-row, two-column table.
        // Reqnroll treats it as a header-only table: Header = ["username", "alice"].
        // Read from the header directly since there is no data row.
        var headers = table.Header.ToList();
        var usernameIndex = headers.IndexOf("username");
        var username = usernameIndex >= 0 && usernameIndex + 1 < headers.Count
            ? headers[usernameIndex + 1]
            : headers.Last();
        await svc.SetUserRoleDirectAsync(username, "Admin");
        state.LastResponse = new ServiceResponse(200, """{"message":"User promoted to admin"}""");
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
    public async Task ThenAllUserAccountsDeleted()
    {
        using var scope = host.CreateScope();
        var db = scope.ServiceProvider.GetRequiredService<AppDbContext>();
        var count = await db.Users.CountAsync();
        count.Should().Be(0, "all user accounts should have been deleted by reset-db");
    }

    [Then(@"^all expenses should be deleted$")]
    public async Task ThenAllExpensesDeleted()
    {
        using var scope = host.CreateScope();
        var db = scope.ServiceProvider.GetRequiredService<AppDbContext>();
        var count = await db.Expenses.CountAsync();
        count.Should().Be(0, "all expenses should have been deleted by reset-db");
    }

    [Then(@"^all attachments should be deleted$")]
    public async Task ThenAllAttachmentsDeleted()
    {
        using var scope = host.CreateScope();
        var db = scope.ServiceProvider.GetRequiredService<AppDbContext>();
        var count = await db.Attachments.CountAsync();
        count.Should().Be(0, "all attachments should have been deleted by reset-db");
    }

    [Then(@"^user ""([^""]+)"" should have the ""([^""]+)"" role$")]
    public async Task ThenUserShouldHaveRole(string username, string expectedRole)
    {
        using var scope = host.CreateScope();
        var db = scope.ServiceProvider.GetRequiredService<AppDbContext>();
        var user = await db.Users.FirstOrDefaultAsync(u => u.Username == username);
        user.Should().NotBeNull($"user '{username}' should exist");
        user!.Role.ToString().ToUpperInvariant().Should().Be(expectedRole.ToUpperInvariant());
    }
}
