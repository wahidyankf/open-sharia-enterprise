using ADemoBeCsas.Tests.ScenarioContext;

namespace ADemoBeCsas.Tests.Unit;

/// <summary>
/// Scenario-scoped state container for unit-level BDD step bindings.
/// Structurally identical to <see cref="SharedState"/> but registered as a
/// separate type so Reqnroll's DI can maintain independent instances for unit
/// steps (<see cref="UnitSharedState"/>) and integration steps
/// (<see cref="SharedState"/>).
/// </summary>
public class UnitSharedState : SharedState
{
}
