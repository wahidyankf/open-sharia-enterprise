//go:build integration_pg

package integration_pg_test

import (
	"fmt"

	"github.com/gin-gonic/gin"
	"github.com/cucumber/godog"
)

func registerCurrencySteps(sc *godog.ScenarioContext, ctx *scenarioCtx) {
	sc.Step(`^alice has created an expense with body \{ "amount": "([^"]*)", "currency": "([^"]*)", "category": "([^"]*)", "description": "([^"]*)", "date": "([^"]*)", "type": "([^"]*)" \}$`, ctx.aliceHasCreatedExpenseWithBody)
	sc.Step(`^alice sends GET /api/v1/expenses/summary$`, ctx.aliceSendsGetSummary)
	sc.Step(`^the response body should contain "([^"]*)" total equal to "([^"]*)"$`, ctx.theResponseBodyShouldContainCurrencyTotalEqualTo)
}

func (ctx *scenarioCtx) aliceHasCreatedExpenseWithBody(amount, currency, category, description, date, expType string) error {
	status, body := ctx.createExpense(amount, currency, category, description, date, expType, ctx.AccessToken)
	if status != 201 {
		return fmt.Errorf("create expense failed with %d: %v", status, body)
	}
	if id, ok := body["id"].(string); ok {
		ctx.ExpenseID = id
	}
	return nil
}

func (ctx *scenarioCtx) aliceSendsGetSummary() error {
	c, w := buildGinContext("GET", "/api/v1/expenses/summary", nil, ctx.AccessToken, gin.Params{}, ctx.JWTSvc)
	ctx.Handler.ExpenseSummary(c)
	ctx.LastStatus = w.Code
	ctx.LastBody = readResponse(w)
	return nil
}

func (ctx *scenarioCtx) theResponseBodyShouldContainCurrencyTotalEqualTo(currency, total string) error {
	v, ok := ctx.LastBody[currency]
	if !ok {
		return fmt.Errorf("response does not contain currency %q; body: %v", currency, ctx.LastBody)
	}
	if fmt.Sprintf("%v", v) != total {
		return fmt.Errorf("expected %q total = %q, got %q", currency, total, fmt.Sprintf("%v", v))
	}
	return nil
}
