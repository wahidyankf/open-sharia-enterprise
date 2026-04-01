using Reqnroll;
using Reqnroll.BoDi;

namespace ADemoBeCsas.Tests;

/// <summary>
/// Reqnroll lifecycle hooks that configure the DI container before each scenario.
///
/// <para>
/// <b>Integration scenarios</b> (not tagged <c>@unit</c>): registers
/// <see cref="IntegrationTestHost"/> as <see cref="ITestHost"/> so that
/// <see cref="ServiceLayer"/> resolves correctly.
/// </para>
///
/// <para>
/// <b>Unit scenarios</b> (tagged <c>@unit</c>): registers
/// <see cref="Unit.UnitTestHost"/> as <see cref="ITestHost"/> and also registers
/// <see cref="Unit.UnitServiceLayer"/> and <see cref="Unit.UnitSharedState"/> so
/// that the <c>Unit.Steps.*</c> binding classes can be resolved by Reqnroll's DI.
/// </para>
/// </summary>
[Binding]
public sealed class ReqnrollHooks
{
    // A single shared IntegrationTestHost is reused across all non-unit scenarios
    // to avoid the cost of creating a new SQLite in-memory DB per scenario.
    // Its database is cleaned between scenarios by CommonSteps.CleanDatabase().
    private static readonly IntegrationTestHost SharedIntegrationHost = new();

    [BeforeScenario(Order = 0)]
    public void RegisterServiceLayer(IObjectContainer container, Reqnroll.ScenarioContext scenarioContext)
    {
        if (scenarioContext.ScenarioInfo.Tags.Contains("unit"))
        {
            // Unit scenario: use fresh in-memory host per scenario for full isolation.
            var unitHost = new Unit.UnitTestHost();
            container.RegisterInstanceAs<ITestHost>(unitHost);
            container.RegisterInstanceAs<Unit.UnitTestHost>(unitHost);
            container.RegisterInstanceAs<Unit.UnitServiceLayer>(new Unit.UnitServiceLayer(unitHost));
            container.RegisterInstanceAs<Unit.UnitSharedState>(new Unit.UnitSharedState());
        }
        else
        {
            // Integration scenario: use shared SQLite in-memory host.
            container.RegisterInstanceAs<ITestHost>(SharedIntegrationHost);
            container.RegisterInstanceAs<IntegrationTestHost>(SharedIntegrationHost);
        }
    }
}
