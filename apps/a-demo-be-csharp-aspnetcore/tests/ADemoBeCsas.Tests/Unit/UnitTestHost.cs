using ADemoBeCsas.Auth;
using ADemoBeCsas.Infrastructure;
using ADemoBeCsas.Infrastructure.Repositories;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;

namespace ADemoBeCsas.Tests.Unit;

/// <summary>
/// Builds and owns the DI service container for unit-level BDD tests.
/// No database is used — all repositories are backed by in-memory dictionaries.
/// This host is scenario-scoped: a fresh instance is created for each BDD
/// scenario so that scenarios are fully isolated from each other.
/// </summary>
public sealed class UnitTestHost : ITestHost, IDisposable
{
    private static string JwtSecret =>
        Environment.GetEnvironmentVariable("APP_JWT_SECRET")
        ?? "test-jwt-secret-at-least-32-chars-long!!";

    private readonly ServiceProvider _provider;

    /// <summary>
    /// Shared in-memory stores exposed so that step helpers can perform direct
    /// state manipulation (e.g., promoting a user to Admin) without going through
    /// the service layer.
    /// </summary>
    public readonly InMemoryUserRepository UserRepo;
    public readonly InMemoryExpenseRepository ExpenseRepo;
    public readonly InMemoryAttachmentRepository AttachmentRepo;
    public readonly InMemoryRevokedTokenRepository RevokedTokenRepo;

    public UnitTestHost()
    {
        var services = new ServiceCollection();

        // Configuration — expose APP_JWT_SECRET for JwtService
        var config = new ConfigurationBuilder()
            .AddInMemoryCollection(
                new Dictionary<string, string?> { ["APP_JWT_SECRET"] = JwtSecret }
            )
            .Build();
        services.AddSingleton<IConfiguration>(config);

        // In-memory repositories (singleton within this host instance so all
        // DI scopes see the same data, just like the SQLite shared-connection mode)
        UserRepo = new InMemoryUserRepository();
        ExpenseRepo = new InMemoryExpenseRepository();
        AttachmentRepo = new InMemoryAttachmentRepository();
        RevokedTokenRepo = new InMemoryRevokedTokenRepository();

        services.AddSingleton<IUserRepository>(UserRepo);
        services.AddSingleton<IExpenseRepository>(ExpenseRepo);
        services.AddSingleton<IAttachmentRepository>(AttachmentRepo);
        services.AddSingleton<IRevokedTokenRepository>(RevokedTokenRepo);

        // Auth services
        services.AddSingleton<IPasswordHasher, PasswordHasher>();
        services.AddSingleton<IJwtService, JwtService>();

        _provider = services.BuildServiceProvider();
    }

    /// <summary>Creates a new DI scope for a single service operation.</summary>
    public IServiceScope CreateScope() => _provider.CreateScope();

    /// <summary>
    /// Clears all in-memory stores. Call from a <c>[BeforeScenario]</c> hook to
    /// reset state between BDD scenarios.
    /// </summary>
    public void Clear()
    {
        UserRepo.Clear();
        ExpenseRepo.Clear();
        AttachmentRepo.Clear();
        RevokedTokenRepo.Clear();
    }

    public void Dispose() => _provider.Dispose();
}
