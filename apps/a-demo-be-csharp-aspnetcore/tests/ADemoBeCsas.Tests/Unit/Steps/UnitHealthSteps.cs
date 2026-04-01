using System.Text.Json;
using FluentAssertions;
using Reqnroll;
using Xunit;

namespace ADemoBeCsas.Tests.Unit.Steps;

/// <summary>
/// Unit-level step bindings for the Service Health Check feature.
/// Mirrors <c>Integration.Steps.HealthSteps</c> but scoped to scenarios tagged
/// <c>@unit</c> and uses <see cref="UnitServiceLayer"/> with no database dependency.
/// </summary>
[Binding]
[Trait("Category", "Unit")]
[Scope(Tag = "unit")]
public class UnitHealthSteps(UnitServiceLayer svc, UnitSharedState state)
{
    [When(@"^an operations engineer sends GET \/health$")]
    public void WhenGetHealth()
    {
        state.LastResponse = svc.HealthCheck();
    }

    [When(@"^an unauthenticated engineer sends GET \/health$")]
    public void WhenUnauthenticatedGetHealth()
    {
        state.LastResponse = svc.HealthCheck();
    }

    [Then("the health status should be {string}")]
    public void ThenHealthStatus(string expectedStatus)
    {
        state.LastResponse.Should().NotBeNull();
        var body = state.LastResponse!.Body;
        using var doc = JsonDocument.Parse(body);
        doc.RootElement.GetProperty("status").GetString().Should().Be(expectedStatus);
    }

    [Then("the response should not include detailed component health information")]
    public void ThenNoDetailedHealthInfo()
    {
        state.LastResponse.Should().NotBeNull();
        var body = state.LastResponse!.Body;
        body.Should().NotContain("components");
        body.Should().NotContain("details");
    }
}
