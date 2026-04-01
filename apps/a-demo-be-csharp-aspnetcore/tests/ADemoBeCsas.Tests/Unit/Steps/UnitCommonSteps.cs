using ADemoBeCsas.Tests.ScenarioContext;
using FluentAssertions;
using Reqnroll;
using Xunit;

namespace ADemoBeCsas.Tests.Unit.Steps;

/// <summary>
/// Unit-level step bindings for cross-cutting steps (status codes, API running).
/// Mirrors <c>Integration.Steps.CommonSteps</c> but uses <see cref="UnitServiceLayer"/>
/// (in-memory repositories) and is scoped to scenarios tagged <c>@unit</c>.
/// </summary>
[Binding]
[Trait("Category", "Unit")]
[Scope(Tag = "unit")]
public class UnitCommonSteps(UnitSharedState state, UnitTestHost host)
{
    [BeforeScenario]
    public void ResetInMemoryStores()
    {
        // Clear all in-memory stores between scenarios so each scenario starts clean.
        host.Clear();
    }

    [Given("the API is running")]
    public void GivenTheApiIsRunning()
    {
        // UnitServiceLayer is always ready — nothing to do.
    }

    [Then("the response status code should be {int}")]
    public void ThenStatusCode(int expectedCode)
    {
        state.LastResponse.Should().NotBeNull();
        state.LastResponse!.StatusCode.Should().Be(
            expectedCode,
            $"Response body: {state.LastResponse.Body}"
        );
    }

    [Then("the response status code should be {int} or {int}")]
    public void ThenStatusCodeOneOf(int code1, int code2)
    {
        state.LastResponse.Should().NotBeNull();
        state.LastResponse!.StatusCode.Should().BeOneOf(code1, code2);
    }
}
