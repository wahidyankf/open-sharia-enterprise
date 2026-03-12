using DemoBeCsas.Domain;
using DemoBeCsas.Infrastructure.Repositories;
using Microsoft.AspNetCore.Mvc;

namespace DemoBeCsas.Endpoints;

public static class ReportEndpoints
{
    public static IEndpointRouteBuilder MapReportEndpoints(this IEndpointRouteBuilder app)
    {
        app.MapGet("/api/v1/reports/pl", GetPlReportAsync).RequireAuthorization();
        return app;
    }

    private static async Task<IResult> GetPlReportAsync(
        HttpContext ctx,
        [FromQuery] string? from,
        [FromQuery] string? to,
        [FromQuery] string? currency,
        IExpenseRepository expenseRepo,
        CancellationToken ct
    )
    {
        var userId = GetUserId(ctx);
        if (userId is null)
        {
            return Results.Unauthorized();
        }

        var fromDate =
            from is not null
                ? DateTimeOffset.Parse(
                    from + "T00:00:00Z",
                    System.Globalization.CultureInfo.InvariantCulture
                )
                : DateTimeOffset.MinValue;
        var toDate =
            to is not null
                ? DateTimeOffset.Parse(
                    to + "T23:59:59Z",
                    System.Globalization.CultureInfo.InvariantCulture
                )
                : DateTimeOffset.MaxValue;

        var expenses = await expenseRepo.ListByUserAndDateRangeAsync(
            userId.Value,
            fromDate,
            toDate,
            currency,
            ct
        );

        var incomeTotal = expenses.Where(e => e.Type == ExpenseType.Income).Sum(e => e.Amount);
        var expenseTotal = expenses.Where(e => e.Type == ExpenseType.Expense).Sum(e => e.Amount);

        // Build category-keyed breakdown dictionaries for income and expenses
        var incomeBreakdown = expenses
            .Where(e => e.Type == ExpenseType.Income)
            .GroupBy(e => e.Category)
            .ToDictionary(g => g.Key, g => FormatAmount(g.Sum(e => e.Amount), currency ?? "USD"));

        var expenseBreakdown = expenses
            .Where(e => e.Type == ExpenseType.Expense)
            .GroupBy(e => e.Category)
            .ToDictionary(g => g.Key, g => FormatAmount(g.Sum(e => e.Amount), currency ?? "USD"));

        return Results.Ok(
            new
            {
                income_total = FormatAmount(incomeTotal, currency ?? "USD"),
                expense_total = FormatAmount(expenseTotal, currency ?? "USD"),
                net = FormatAmount(incomeTotal - expenseTotal, currency ?? "USD"),
                income_breakdown = incomeBreakdown,
                expense_breakdown = expenseBreakdown,
            }
        );
    }

    private static string FormatAmount(decimal amount, string currency) =>
        currency == "IDR"
            ? Math.Round(amount, 0, MidpointRounding.AwayFromZero).ToString("F0")
            : amount.ToString("F2");

    private static Guid? GetUserId(HttpContext ctx)
    {
        var sub = ctx.User.FindFirst(
            System.IdentityModel.Tokens.Jwt.JwtRegisteredClaimNames.Sub
        )?.Value;
        return sub is not null && Guid.TryParse(sub, out var g) ? g : null;
    }
}
