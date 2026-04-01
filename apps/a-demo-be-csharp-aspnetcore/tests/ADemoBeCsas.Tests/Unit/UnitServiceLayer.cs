using ADemoBeCsas.Domain;

namespace ADemoBeCsas.Tests.Unit;

/// <summary>
/// Thin wrapper around <see cref="ServiceLayer"/> that uses a <see cref="UnitTestHost"/>
/// (pure in-memory repositories, no database) as its backing store.
///
/// Overrides the direct database helpers (SetUserStatusDirectAsync etc.) so they
/// operate on the in-memory repositories rather than requiring an <c>AppDbContext</c>.
///
/// Reqnroll injects this as a distinct type from <see cref="ServiceLayer"/> so that
/// unit-level step binding classes can coexist in the same assembly alongside the
/// integration-level step bindings — Reqnroll's DI resolves each type independently.
/// </summary>
public sealed class UnitServiceLayer : ServiceLayer
{
    private readonly UnitTestHost _unitHost;

    public UnitServiceLayer(UnitTestHost host) : base(host)
    {
        _unitHost = host;
    }

    // ─────────────────────────────────────────────────────────────
    // Override direct-manipulation helpers to use in-memory repos
    // ─────────────────────────────────────────────────────────────

    public override async Task SetUserStatusDirectAsync(string username, string status)
    {
        var user = await _unitHost.UserRepo.FindByUsernameAsync(username);
        if (user is null)
        {
            return;
        }

        user.Status = Enum.Parse<UserStatus>(status, ignoreCase: true);
        user.UpdatedAt = DateTimeOffset.UtcNow;
        await _unitHost.UserRepo.UpdateAsync(user);
    }

    public override async Task SetUserRoleDirectAsync(string username, string role)
    {
        var user = await _unitHost.UserRepo.FindByUsernameAsync(username);
        if (user is null)
        {
            return;
        }

        user.Role = Enum.Parse<Role>(role, ignoreCase: true);
        user.UpdatedAt = DateTimeOffset.UtcNow;
        await _unitHost.UserRepo.UpdateAsync(user);
    }

    public override async Task<Guid> SetUserLockedDirectAsync(string username)
    {
        var user = await _unitHost.UserRepo.FindByUsernameAsync(username);
        if (user is null)
        {
            return Guid.Empty;
        }

        user.Status = UserStatus.Locked;
        user.FailedLoginAttempts = 5;
        user.UpdatedAt = DateTimeOffset.UtcNow;
        await _unitHost.UserRepo.UpdateAsync(user);
        return user.Id;
    }

    public override async Task SetUserFailedAttemptsDirectAsync(string username, int attempts)
    {
        var user = await _unitHost.UserRepo.FindByUsernameAsync(username);
        if (user is null)
        {
            return;
        }

        user.FailedLoginAttempts = attempts;
        user.UpdatedAt = DateTimeOffset.UtcNow;
        await _unitHost.UserRepo.UpdateAsync(user);
    }

    public override async Task<Guid> GetUserIdByUsernameAsync(string username)
    {
        var user = await _unitHost.UserRepo.FindByUsernameAsync(username);
        return user?.Id ?? Guid.Empty;
    }

    public override async Task<string?> GetUserStatusAsync(string username)
    {
        var user = await _unitHost.UserRepo.FindByUsernameAsync(username);
        return user?.Status.ToString();
    }
}
