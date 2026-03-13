//go:build integration_pg

package integration_pg_test

import (
	"fmt"

	"github.com/gin-gonic/gin"
	"github.com/cucumber/godog"
)

func registerReportingSteps(sc *godog.ScenarioContext, ctx *scenarioCtx) {
	sc.Step(`^alice sends GET /api/v1/reports/pl\?from=([^&]+)&to=([^&]+)&currency=([^\s]+)$`, ctx.aliceSendsGetPLReport)
	sc.Step(`^the income breakdown should contain "([^"]*)" with amount "([^"]*)"$`, ctx.theIncomeBreakdownShouldContainCategory)
	sc.Step(`^the expense breakdown should contain "([^"]*)" with amount "([^"]*)"$`, ctx.theExpenseBreakdownShouldContainCategory)
}

func (ctx *scenarioCtx) aliceSendsGetPLReport(from, to, currency string) error {
	rawQuery := fmt.Sprintf("from=%s&to=%s&currency=%s", from, to, currency)
	c, w := buildGinContext("GET", "/api/v1/reports/pl?"+rawQuery, nil, ctx.AccessToken, gin.Params{}, ctx.JWTSvc)
	c.Request.URL.RawQuery = rawQuery
	ctx.Handler.PLReport(c)
	ctx.LastStatus = w.Code
	ctx.LastBody = readResponse(w)
	return nil
}

func (ctx *scenarioCtx) theIncomeBreakdownShouldContainCategory(category, amount string) error {
	breakdown, ok := ctx.LastBody["income_breakdown"]
	if !ok {
		return fmt.Errorf("response does not contain 'income_breakdown'; body: %v", ctx.LastBody)
	}
	breakdownMap, ok := breakdown.(map[string]interface{})
	if !ok {
		return fmt.Errorf("'income_breakdown' is not a map")
	}
	v, ok := breakdownMap[category]
	if !ok {
		return fmt.Errorf("income_breakdown does not contain category %q", category)
	}
	if fmt.Sprintf("%v", v) != amount {
		return fmt.Errorf("expected income_breakdown[%q] = %q, got %q", category, amount, fmt.Sprintf("%v", v))
	}
	return nil
}

func (ctx *scenarioCtx) theExpenseBreakdownShouldContainCategory(category, amount string) error {
	breakdown, ok := ctx.LastBody["expense_breakdown"]
	if !ok {
		return fmt.Errorf("response does not contain 'expense_breakdown'; body: %v", ctx.LastBody)
	}
	breakdownMap, ok := breakdown.(map[string]interface{})
	if !ok {
		return fmt.Errorf("'expense_breakdown' is not a map")
	}
	v, ok := breakdownMap[category]
	if !ok {
		return fmt.Errorf("expense_breakdown does not contain category %q", category)
	}
	if fmt.Sprintf("%v", v) != amount {
		return fmt.Errorf("expected expense_breakdown[%q] = %q, got %q", category, amount, fmt.Sprintf("%v", v))
	}
	return nil
}
