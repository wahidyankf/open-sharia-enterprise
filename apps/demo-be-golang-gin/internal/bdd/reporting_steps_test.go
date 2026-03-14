package bdd_test

import (
	"fmt"
	"strings"

	"github.com/cucumber/godog"
)

func registerReportingSteps(sc *godog.ScenarioContext, ctx *scenarioCtx) {
	sc.Step(`^alice sends GET /api/v1/reports/pl\?from=([^&]+)&to=([^&]+)&currency=([^\s]+)$`, ctx.aliceSendsGetPLReport)
	sc.Step(`^the income breakdown should contain "([^"]*)" with amount "([^"]*)"$`, ctx.theIncomeBreakdownShouldContainCategory)
	sc.Step(`^the expense breakdown should contain "([^"]*)" with amount "([^"]*)"$`, ctx.theExpenseBreakdownShouldContainCategory)
}

// remapPLFieldName translates legacy snake_case field names from the shared Gherkin spec
// to the camelCase field names returned by the backend.
func remapPLFieldName(field string) string {
	switch field {
	case "income_total":
		return "totalIncome"
	case "expense_total":
		return "totalExpense"
	default:
		return field
	}
}

func (ctx *scenarioCtx) aliceSendsGetPLReport(from, to, currency string) error {
	// Map legacy from/to params to startDate/endDate.
	url := fmt.Sprintf("/api/v1/reports/pl?startDate=%s&endDate=%s&currency=%s", from, to, currency)
	resp, body := doRequest(ctx.Router, "GET", url, nil, ctx.AccessToken)
	ctx.LastResponse = resp
	ctx.LastBody = body
	return nil
}

func (ctx *scenarioCtx) theIncomeBreakdownShouldContainCategory(category, amount string) error {
	body := parseBody(ctx.LastBody)
	breakdown, ok := body["incomeBreakdown"]
	if !ok {
		return fmt.Errorf("response does not contain 'incomeBreakdown'; body: %s", string(ctx.LastBody))
	}
	items, ok := breakdown.([]interface{})
	if !ok {
		return fmt.Errorf("'incomeBreakdown' is not an array")
	}
	for _, item := range items {
		m, ok := item.(map[string]interface{})
		if !ok {
			continue
		}
		if fmt.Sprintf("%v", m["category"]) == category {
			got := strings.TrimRight(fmt.Sprintf("%v", m["total"]), "0")
			got = strings.TrimRight(got, ".")
			want := strings.TrimRight(amount, "0")
			want = strings.TrimRight(want, ".")
			if got == want || fmt.Sprintf("%v", m["total"]) == amount {
				return nil
			}
			return fmt.Errorf("incomeBreakdown category %q: expected amount %q, got %q", category, amount, fmt.Sprintf("%v", m["total"]))
		}
	}
	return fmt.Errorf("incomeBreakdown does not contain category %q", category)
}

func (ctx *scenarioCtx) theExpenseBreakdownShouldContainCategory(category, amount string) error {
	body := parseBody(ctx.LastBody)
	breakdown, ok := body["expenseBreakdown"]
	if !ok {
		return fmt.Errorf("response does not contain 'expenseBreakdown'; body: %s", string(ctx.LastBody))
	}
	items, ok := breakdown.([]interface{})
	if !ok {
		return fmt.Errorf("'expenseBreakdown' is not an array")
	}
	for _, item := range items {
		m, ok := item.(map[string]interface{})
		if !ok {
			continue
		}
		if fmt.Sprintf("%v", m["category"]) == category {
			if fmt.Sprintf("%v", m["total"]) == amount {
				return nil
			}
			return fmt.Errorf("expenseBreakdown category %q: expected amount %q, got %q", category, amount, fmt.Sprintf("%v", m["total"]))
		}
	}
	return fmt.Errorf("expenseBreakdown does not contain category %q", category)
}
