using ADemoBeCsas.Domain;
using ADemoBeCsas.Infrastructure;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;

namespace ADemoBeCsas.Endpoints;

public static class TestApiEndpoints
{
    public static IEndpointRouteBuilder MapTestApiEndpoints(this IEndpointRouteBuilder app)
    {
        app.MapPost("/api/v1/test/reset-db", ResetDbAsync);
        app.MapPost("/api/v1/test/promote-admin", PromoteAdminAsync);
        return app;
    }

    private static async Task<IResult> ResetDbAsync(AppDbContext db, CancellationToken ct)
    {
        await db.Attachments.ExecuteDeleteAsync(ct);
        await db.Expenses.ExecuteDeleteAsync(ct);
        await db.RevokedTokens.ExecuteDeleteAsync(ct);
        await db.Users.ExecuteDeleteAsync(ct);
        return Results.Ok(new { message = "Database reset successful" });
    }

    private static async Task<IResult> PromoteAdminAsync(
        [FromBody] PromoteAdminRequest req,
        AppDbContext db,
        CancellationToken ct
    )
    {
        if (req.Username is null)
        {
            return Results.BadRequest(new { message = "username is required" });
        }

        var user = await db.Users.FirstOrDefaultAsync(u => u.Username == req.Username, ct);
        if (user is null)
        {
            return Results.NotFound(new { message = $"User {req.Username} not found" });
        }

        user.Role = Role.Admin;
        user.UpdatedAt = DateTimeOffset.UtcNow;
        await db.SaveChangesAsync(ct);

        return Results.Ok(new { message = $"User {req.Username} promoted to ADMIN" });
    }

    private sealed record PromoteAdminRequest(string? Username);
}
