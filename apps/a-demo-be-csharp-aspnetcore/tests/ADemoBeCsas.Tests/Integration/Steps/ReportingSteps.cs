using System.Text.Json;
using ADemoBeCsas.Tests.ScenarioContext;
using FluentAssertions;
using Reqnroll;
using Xunit;

namespace ADemoBeCsas.Tests.Integration.Steps;

[Binding]
[Trait("Category", "Integration")]
public class ReportingSteps(ServiceLayer svc, SharedState state)
{
    // ─────────────────────────────────────────────────────────────
    // When steps — each PL query variant
    // ─────────────────────────────────────────────────────────────

    [When(@"^alice sends GET /api/v1/reports/pl\?from=2025-01-01&to=2025-01-31&currency=USD$")]
    public async Task WhenAliceGetsPLJan()
    {
        await PerformGetPl("2025-01-01", "2025-01-31", "USD");
    }

    [When(@"^alice sends GET /api/v1/reports/pl\?from=2025-02-01&to=2025-02-28&currency=USD$")]
    public async Task WhenAliceGetsPLFeb()
    {
        await PerformGetPl("2025-02-01", "2025-02-28", "USD");
    }

    [When(@"^alice sends GET /api/v1/reports/pl\?from=2025-03-01&to=2025-03-31&currency=USD$")]
    public async Task WhenAliceGetsPLMar()
    {
        await PerformGetPl("2025-03-01", "2025-03-31", "USD");
    }

    [When(@"^alice sends GET /api/v1/reports/pl\?from=2025-04-01&to=2025-04-30&currency=USD$")]
    public async Task WhenAliceGetsPLApr()
    {
        await PerformGetPl("2025-04-01", "2025-04-30", "USD");
    }

    [When(@"^alice sends GET /api/v1/reports/pl\?from=2025-05-01&to=2025-05-31&currency=USD$")]
    public async Task WhenAliceGetsPLMay()
    {
        await PerformGetPl("2025-05-01", "2025-05-31", "USD");
    }

    [When(@"^alice sends GET /api/v1/reports/pl\?from=2099-01-01&to=2099-01-31&currency=USD$")]
    public async Task WhenAliceGetsPLFuture()
    {
        await PerformGetPl("2099-01-01", "2099-01-31", "USD");
    }

    // ─────────────────────────────────────────────────────────────
    // Then steps
    // ─────────────────────────────────────────────────────────────

    [Then(@"^the income breakdown should contain ""([^""]+)"" with amount ""([^""]+)""$")]
    public void ThenIncomeBreakdownContains(string category, string amount)
    {
        state.LastResponse.Should().NotBeNull();
        var body = state.LastResponse!.Body;
        using var doc = JsonDocument.Parse(body);
        doc.RootElement.TryGetProperty("incomeBreakdown", out var breakdown)
            .Should().BeTrue($"'incomeBreakdown' not found in: {body}");
        var entry = breakdown.EnumerateArray()
            .FirstOrDefault(el => el.TryGetProperty("category", out var cat) && cat.GetString() == category);
        entry.ValueKind.Should().NotBe(JsonValueKind.Undefined, $"Category '{category}' not found in incomeBreakdown of: {body}");
        var totalStr = entry.GetProperty("total").GetString()!;
        decimal.Parse(totalStr, System.Globalization.CultureInfo.InvariantCulture)
            .Should().Be(decimal.Parse(amount, System.Globalization.CultureInfo.InvariantCulture));
    }

    [Then(@"^the expense breakdown should contain ""([^""]+)"" with amount ""([^""]+)""$")]
    public void ThenExpenseBreakdownContains(string category, string amount)
    {
        state.LastResponse.Should().NotBeNull();
        var body = state.LastResponse!.Body;
        using var doc = JsonDocument.Parse(body);
        doc.RootElement.TryGetProperty("expenseBreakdown", out var breakdown)
            .Should().BeTrue($"'expenseBreakdown' not found in: {body}");
        var entry = breakdown.EnumerateArray()
            .FirstOrDefault(el => el.TryGetProperty("category", out var cat) && cat.GetString() == category);
        entry.ValueKind.Should().NotBe(JsonValueKind.Undefined, $"Category '{category}' not found in expenseBreakdown of: {body}");
        var totalStr = entry.GetProperty("total").GetString()!;
        decimal.Parse(totalStr, System.Globalization.CultureInfo.InvariantCulture)
            .Should().Be(decimal.Parse(amount, System.Globalization.CultureInfo.InvariantCulture));
    }

    // ─────────────────────────────────────────────────────────────
    // Helpers
    // ─────────────────────────────────────────────────────────────

    private async Task PerformGetPl(string from, string to, string currency)
    {
        state.LastResponse = await svc.GetPlReportAsync(state.AccessToken, from, to, currency);
    }
}
