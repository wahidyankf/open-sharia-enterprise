using System.Text.Json;
using FluentAssertions;
using Reqnroll;
using Xunit;

namespace ADemoBeCsas.Tests.Unit.Steps;

/// <summary>
/// Unit-level step bindings for admin user management scenarios.
/// Mirrors <c>Integration.Steps.AdminSteps</c> but scoped to scenarios tagged
/// <c>@unit</c> and uses <see cref="UnitServiceLayer"/> with in-memory repositories.
/// </summary>
[Binding]
[Trait("Category", "Unit")]
[Scope(Tag = "unit")]
public class UnitAdminSteps(UnitServiceLayer svc, UnitSharedState state, UnitAuthSteps auth)
{
    // ─────────────────────────────────────────────────────────────
    // When steps
    // ─────────────────────────────────────────────────────────────

    [When(@"^the admin sends GET /api/v1/admin/users$")]
    public async Task WhenAdminListsUsers()
    {
        state.LastResponse = await svc.AdminListUsersAsync(auth._adminToken, page: 1, size: 20);
    }

    [When(@"^the admin sends GET /api/v1/admin/users\?search=alice@example\.com$")]
    public async Task WhenAdminSearchesByEmail()
    {
        state.LastResponse = await svc.AdminListUsersAsync(
            auth._adminToken,
            page: 1,
            size: 20,
            search: "alice@example.com"
        );
    }

    [When(@"^the admin sends POST /api/v1/admin/users/\{alice_id\}/disable with body \{ ""reason"": ""([^""]+)"" \}$")]
    public async Task WhenAdminDisablesAlice(string reason)
    {
        var aliceId = state.LastCreatedId ?? auth._aliceId;
        aliceId.Should().NotBeNull("alice's ID should be known");
        state.LastResponse = await svc.AdminDisableUserAsync(auth._adminToken, aliceId!.Value);
    }

    [When(@"^the admin sends POST /api/v1/admin/users/\{alice_id\}/enable$")]
    public async Task WhenAdminEnablesAlice()
    {
        var aliceId = state.LastCreatedId ?? auth._aliceId;
        aliceId.Should().NotBeNull("alice's ID should be known");
        state.LastResponse = await svc.AdminEnableUserAsync(auth._adminToken, aliceId!.Value);
    }

    [When(@"^the admin sends POST /api/v1/admin/users/\{alice_id\}/unlock$")]
    public async Task WhenAdminUnlocksAlice()
    {
        var aliceId = state.LastCreatedId ?? auth._aliceId;
        aliceId.Should().NotBeNull("alice's ID should be known");
        state.LastResponse = await svc.AdminUnlockUserAsync(auth._adminToken, aliceId!.Value);
    }

    [When(@"^the admin sends POST /api/v1/admin/users/\{alice_id\}/force-password-reset$")]
    public async Task WhenAdminForcePasswordReset()
    {
        var aliceId = state.LastCreatedId ?? auth._aliceId;
        aliceId.Should().NotBeNull("alice's ID should be known");
        state.LastResponse = await svc.AdminForcePasswordResetAsync(
            auth._adminToken,
            aliceId!.Value
        );
        if (state.LastResponse.IsSuccess)
        {
            using var doc = JsonDocument.Parse(state.LastResponse.Body);
            if (doc.RootElement.TryGetProperty("token", out var rt))
            {
                state.LastResetToken = rt.GetString();
            }
        }
    }

    // ─────────────────────────────────────────────────────────────
    // Then steps
    // ─────────────────────────────────────────────────────────────

    [Then(@"^the response body should contain at least one user with ""email"" equal to ""([^""]+)""$")]
    public void ThenResponseContainsUserWithEmail(string email)
    {
        state.LastResponse.Should().NotBeNull();
        var body = state.LastResponse!.Body;
        body.Should().Contain(email, $"Expected user with email '{email}' in: {body}");
    }
}
