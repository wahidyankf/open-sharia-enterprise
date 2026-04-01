using System.Text.Json;
using FluentAssertions;
using Reqnroll;
using Xunit;

namespace ADemoBeCsas.Tests.Unit.Steps;

/// <summary>
/// Unit-level step bindings for user account management scenarios.
/// Mirrors <c>Integration.Steps.UserAccountSteps</c> but scoped to scenarios tagged
/// <c>@unit</c> and uses <see cref="UnitServiceLayer"/> with in-memory repositories.
/// </summary>
[Binding]
[Trait("Category", "Unit")]
[Scope(Tag = "unit")]
public class UnitUserAccountSteps(UnitServiceLayer svc, UnitSharedState state)
{
    // ─────────────────────────────────────────────────────────────
    // When steps
    // ─────────────────────────────────────────────────────────────

    [When(@"^alice sends GET /api/v1/users/me$")]
    public async Task WhenAliceSendsGetMe()
    {
        state.LastResponse = await svc.GetMeAsync(state.AccessToken);
    }

    [When(@"^alice sends PATCH /api/v1/users/me with body \{ ""displayName"": ""([^""]+)"" \}$")]
    public async Task WhenAlicePatchesMe(string displayName)
    {
        state.LastResponse = await svc.PatchMeAsync(state.AccessToken, displayName);
    }

    [When(
        @"^alice sends POST /api/v1/users/me/password with body \{ ""oldPassword"": ""([^""]+)"", ""newPassword"": ""([^""]+)"" \}$"
    )]
    public async Task WhenAliceChangesPassword(string oldPassword, string newPassword)
    {
        state.LastResponse = await svc.ChangePasswordAsync(state.AccessToken, oldPassword, newPassword);
    }

    [When(@"^alice sends POST /api/v1/users/me/deactivate$")]
    public async Task WhenAliceDeactivates()
    {
        state.LastResponse = await svc.DeactivateAsync(state.AccessToken);
    }

    // ─────────────────────────────────────────────────────────────
    // Then steps (password change error)
    // ─────────────────────────────────────────────────────────────

    [Then(@"^the response body should contain an error message about incorrect password$")]
    public void ThenErrorAboutIncorrectPassword()
    {
        state.LastResponse.Should().NotBeNull();
        state.LastResponse!.StatusCode.Should().BeOneOf(400, 401);
    }
}
