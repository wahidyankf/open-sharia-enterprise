using Microsoft.Extensions.DependencyInjection;

namespace ADemoBeCsas.Tests;

/// <summary>
/// Abstracts the DI container used by <see cref="ServiceLayer"/> so that both
/// the integration-level host (SQLite or PostgreSQL) and the unit-level host
/// (pure in-memory dictionary repositories) can be substituted without changing
/// any service-layer logic.
/// </summary>
public interface ITestHost
{
    /// <summary>Creates a new DI scope for a single service operation.</summary>
    IServiceScope CreateScope();
}
