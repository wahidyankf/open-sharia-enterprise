using System.IdentityModel.Tokens.Jwt;
using System.Text.Json;
using FluentAssertions;
using Reqnroll;
using Xunit;

namespace ADemoBeCsas.Tests.Unit.Steps;

/// <summary>
/// Unit-level step bindings for token management and JWKS scenarios.
/// Mirrors <c>Integration.Steps.TokenManagementSteps</c> but scoped to scenarios tagged
/// <c>@unit</c> and uses <see cref="UnitServiceLayer"/> with in-memory repositories.
/// </summary>
[Binding]
[Trait("Category", "Unit")]
[Scope(Tag = "unit")]
public class UnitTokenManagementSteps(UnitServiceLayer svc, UnitSharedState state)
{
    // ─────────────────────────────────────────────────────────────
    // When steps
    // ─────────────────────────────────────────────────────────────

    [When(@"^alice decodes her access token payload$")]
    public void WhenAliceDecodesToken()
    {
        state.AccessToken.Should().NotBeNull("access token should be stored");
    }

    [When(@"^the client sends GET /\.well-known/jwks\.json$")]
    public void WhenGetJwks()
    {
        state.LastResponse = svc.GetJwks();
    }

    // ─────────────────────────────────────────────────────────────
    // Then steps
    // ─────────────────────────────────────────────────────────────

    [Then(@"^the token should contain a non-null ""([^""]+)"" claim$")]
    public void ThenTokenContainsClaim(string claimName)
    {
        state.AccessToken.Should().NotBeNull("access token should be stored");
        var handler = new JwtSecurityTokenHandler();
        var jwt = handler.ReadJwtToken(state.AccessToken!);

        if (claimName == "sub")
        {
            jwt.Subject.Should().NotBeNullOrEmpty($"Token should contain '{claimName}' claim");
        }
        else if (claimName == "iss")
        {
            jwt.Claims.Should().NotBeEmpty("Token should have claims");
        }
        else
        {
            var claimType = claimName switch
            {
                "jti" => JwtRegisteredClaimNames.Jti,
                "iat" => JwtRegisteredClaimNames.Iat,
                _ => claimName,
            };
            var claim = jwt.Claims.FirstOrDefault(c => c.Type == claimType || c.Type == claimName);
            claim.Should().NotBeNull($"Token should contain '{claimName}' claim");
        }
    }

    [Then(@"^the response body should contain at least one key in the ""keys"" array$")]
    public void ThenJwksContainsKeys()
    {
        state.LastResponse.Should().NotBeNull();
        var body = state.LastResponse!.Body;
        using var doc = JsonDocument.Parse(body);
        doc.RootElement.TryGetProperty("keys", out var keys).Should().BeTrue($"'keys' not found in: {body}");
        keys.GetArrayLength().Should().BeGreaterThan(0, "JWKS should contain at least one key");
    }
}
