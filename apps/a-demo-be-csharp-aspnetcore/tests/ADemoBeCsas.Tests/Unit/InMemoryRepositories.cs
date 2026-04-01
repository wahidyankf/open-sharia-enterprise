using System.Collections.Concurrent;
using ADemoBeCsas.Domain;
using ADemoBeCsas.Infrastructure.Models;
using ADemoBeCsas.Infrastructure.Repositories;

namespace ADemoBeCsas.Tests.Unit;

/// <summary>
/// Pure in-memory (dictionary-backed) implementations of all repository interfaces.
/// Used by the unit-level test host to eliminate any database dependency from
/// Gherkin BDD unit tests. No SQLite, no PostgreSQL, no I/O — all state lives in
/// <see cref="ConcurrentDictionary{TKey,TValue}"/> instances for the duration of a
/// test scenario.
/// </summary>

// ─────────────────────────────────────────────────────────────
// User repository
// ─────────────────────────────────────────────────────────────

public sealed class InMemoryUserRepository : IUserRepository
{
    private readonly ConcurrentDictionary<Guid, UserModel> _store = new();

    public Task<UserModel> CreateAsync(
        string username,
        string email,
        string passwordHash,
        string? displayName,
        Role role = Role.User,
        CancellationToken ct = default
    )
    {
        var now = DateTimeOffset.UtcNow;
        var user = new UserModel
        {
            Id = Guid.NewGuid(),
            Username = username,
            Email = email,
            PasswordHash = passwordHash,
            DisplayName = displayName ?? string.Empty,
            Status = UserStatus.Active,
            Role = role,
            FailedLoginAttempts = 0,
            CreatedAt = now,
            UpdatedAt = now,
        };
        _store[user.Id] = user;
        return Task.FromResult(user);
    }

    public Task<UserModel?> FindByUsernameAsync(string username, CancellationToken ct = default) =>
        Task.FromResult(_store.Values.FirstOrDefault(u => u.Username == username));

    public Task<UserModel?> FindByIdAsync(Guid userId, CancellationToken ct = default) =>
        Task.FromResult(_store.TryGetValue(userId, out var u) ? u : null);

    public Task<UserModel> UpdateAsync(UserModel user, CancellationToken ct = default)
    {
        user.UpdatedAt = DateTimeOffset.UtcNow;
        _store[user.Id] = user;
        return Task.FromResult(user);
    }

    public Task<(IReadOnlyList<UserModel> Items, int Total)> ListAsync(
        int page,
        int size,
        string? emailFilter = null,
        CancellationToken ct = default
    )
    {
        var query = _store.Values.AsEnumerable();
        if (!string.IsNullOrWhiteSpace(emailFilter))
        {
            query = query.Where(u => u.Email.Contains(emailFilter, StringComparison.OrdinalIgnoreCase));
        }

        var ordered = query.OrderBy(u => u.CreatedAt).ToList();
        var total = ordered.Count;
        var items = (IReadOnlyList<UserModel>)ordered
            .Skip((page - 1) * size)
            .Take(size)
            .ToList();
        return Task.FromResult((items, total));
    }

    /// <summary>Resets all user data — used in unit-level BeforeScenario cleanup.</summary>
    public void Clear() => _store.Clear();
}

// ─────────────────────────────────────────────────────────────
// Expense repository
// ─────────────────────────────────────────────────────────────

public sealed class InMemoryExpenseRepository : IExpenseRepository
{
    private readonly ConcurrentDictionary<Guid, ExpenseModel> _store = new();

    public Task<ExpenseModel> CreateAsync(
        Guid userId,
        string description,
        string category,
        decimal amount,
        string currency,
        ExpenseType type,
        double? quantity,
        string? unit,
        DateOnly date,
        CancellationToken ct = default
    )
    {
        var now = DateTimeOffset.UtcNow;
        var expense = new ExpenseModel
        {
            Id = Guid.NewGuid(),
            UserId = userId,
            Description = description,
            Category = category,
            Amount = amount,
            Currency = currency.ToUpperInvariant(),
            Type = type,
            Quantity = quantity,
            Unit = unit,
            Date = date,
            CreatedAt = now,
            UpdatedAt = now,
        };
        _store[expense.Id] = expense;
        return Task.FromResult(expense);
    }

    public Task<ExpenseModel?> FindByIdAsync(Guid expenseId, Guid userId, CancellationToken ct = default) =>
        Task.FromResult(
            _store.TryGetValue(expenseId, out var e) && e.UserId == userId ? e : null
        );

    public Task<(IReadOnlyList<ExpenseModel> Items, int Total)> ListByUserAsync(
        Guid userId,
        int page,
        int size,
        CancellationToken ct = default
    )
    {
        var items = _store.Values
            .Where(e => e.UserId == userId)
            .OrderByDescending(e => e.Date)
            .ToList();
        var total = items.Count;
        var page_ = (IReadOnlyList<ExpenseModel>)items
            .Skip((page - 1) * size)
            .Take(size)
            .ToList();
        return Task.FromResult((page_, total));
    }

    public Task<ExpenseModel> UpdateAsync(
        Guid expenseId,
        Guid userId,
        string description,
        string category,
        decimal amount,
        string currency,
        ExpenseType type,
        double? quantity,
        string? unit,
        DateOnly date,
        CancellationToken ct = default
    )
    {
        if (!_store.TryGetValue(expenseId, out var expense) || expense.UserId != userId)
        {
            throw new InvalidOperationException($"Expense {expenseId} not found for user {userId}");
        }

        expense.Description = description;
        expense.Category = category;
        expense.Amount = amount;
        expense.Currency = currency.ToUpperInvariant();
        expense.Type = type;
        expense.Quantity = quantity;
        expense.Unit = unit;
        expense.Date = date;
        expense.UpdatedAt = DateTimeOffset.UtcNow;
        _store[expenseId] = expense;
        return Task.FromResult(expense);
    }

    public Task DeleteAsync(Guid expenseId, Guid userId, CancellationToken ct = default)
    {
        _store.TryRemove(expenseId, out _);
        return Task.CompletedTask;
    }

    public Task<IReadOnlyList<CurrencySummary>> SummaryByCurrencyAsync(
        Guid userId,
        CancellationToken ct = default
    )
    {
        var summaries = (IReadOnlyList<CurrencySummary>)_store.Values
            .Where(e => e.UserId == userId)
            .GroupBy(e => e.Currency)
            .Select(g => new CurrencySummary(
                g.Key,
                g.Where(e => e.Type == ExpenseType.Income).Sum(e => e.Amount),
                g.Where(e => e.Type == ExpenseType.Expense).Sum(e => e.Amount)
            ))
            .ToList();
        return Task.FromResult(summaries);
    }

    public Task<IReadOnlyList<ExpenseModel>> ListByUserAndDateRangeAsync(
        Guid userId,
        DateOnly from,
        DateOnly to,
        string? currency,
        CancellationToken ct = default
    )
    {
        var query = _store.Values
            .Where(e => e.UserId == userId && e.Date >= from && e.Date <= to);
        if (!string.IsNullOrWhiteSpace(currency))
        {
            var upper = currency.ToUpperInvariant();
            query = query.Where(e => e.Currency == upper);
        }

        return Task.FromResult((IReadOnlyList<ExpenseModel>)query.ToList());
    }

    /// <summary>Resets all expense data — used in unit-level BeforeScenario cleanup.</summary>
    public void Clear() => _store.Clear();
}

// ─────────────────────────────────────────────────────────────
// Attachment repository
// ─────────────────────────────────────────────────────────────

public sealed class InMemoryAttachmentRepository : IAttachmentRepository
{
    private readonly ConcurrentDictionary<Guid, AttachmentModel> _store = new();

    public Task<AttachmentModel> CreateAsync(
        Guid expenseId,
        string filename,
        string contentType,
        long size,
        byte[] data,
        CancellationToken ct = default
    )
    {
        var attachment = new AttachmentModel
        {
            Id = Guid.NewGuid(),
            ExpenseId = expenseId,
            Filename = filename,
            ContentType = contentType,
            Size = size,
            Data = data,
            CreatedAt = DateTimeOffset.UtcNow,
        };
        _store[attachment.Id] = attachment;
        return Task.FromResult(attachment);
    }

    public Task<IReadOnlyList<AttachmentModel>> ListByExpenseAsync(
        Guid expenseId,
        CancellationToken ct = default
    )
    {
        var items = (IReadOnlyList<AttachmentModel>)_store.Values
            .Where(a => a.ExpenseId == expenseId)
            .OrderBy(a => a.CreatedAt)
            .ToList();
        return Task.FromResult(items);
    }

    public Task<AttachmentModel?> FindByIdAsync(Guid attachmentId, CancellationToken ct = default) =>
        Task.FromResult(_store.TryGetValue(attachmentId, out var a) ? a : null);

    public Task DeleteAsync(Guid attachmentId, CancellationToken ct = default)
    {
        _store.TryRemove(attachmentId, out _);
        return Task.CompletedTask;
    }

    /// <summary>Resets all attachment data — used in unit-level BeforeScenario cleanup.</summary>
    public void Clear() => _store.Clear();
}

// ─────────────────────────────────────────────────────────────
// Revoked token repository
// ─────────────────────────────────────────────────────────────

public sealed class InMemoryRevokedTokenRepository : IRevokedTokenRepository
{
    private readonly ConcurrentDictionary<string, (Guid UserId, DateTimeOffset RevokedAt)> _jtis = new();
    private readonly ConcurrentDictionary<Guid, DateTimeOffset> _userRevokedBefore = new();

    public Task RevokeAsync(string jti, Guid userId, CancellationToken ct = default)
    {
        _jtis.TryAdd(jti, (userId, DateTimeOffset.UtcNow));
        return Task.CompletedTask;
    }

    public Task<bool> IsRevokedAsync(string jti, CancellationToken ct = default) =>
        Task.FromResult(_jtis.ContainsKey(jti));

    public Task RevokeAllForUserAsync(Guid userId, DateTimeOffset revokedBefore, CancellationToken ct = default)
    {
        _userRevokedBefore[userId] = revokedBefore;
        return Task.CompletedTask;
    }

    public Task<DateTimeOffset?> GetUserRevokedBeforeAsync(Guid userId, CancellationToken ct = default) =>
        Task.FromResult(_userRevokedBefore.TryGetValue(userId, out var dt) ? (DateTimeOffset?)dt : null);

    /// <summary>Resets all revoked token data — used in unit-level BeforeScenario cleanup.</summary>
    public void Clear()
    {
        _jtis.Clear();
        _userRevokedBefore.Clear();
    }
}
