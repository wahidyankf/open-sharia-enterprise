//go:build integration_pg

package integration_pg_test

import (
	"fmt"
	"strconv"

	"github.com/gin-gonic/gin"
	"github.com/cucumber/godog"
)

func registerUnitHandlingSteps(sc *godog.ScenarioContext, ctx *scenarioCtx) {
	sc.Step(`^alice has created an expense with body \{ "amount": "([^"]*)", "currency": "([^"]*)", "category": "([^"]*)", "description": "([^"]*)", "date": "([^"]*)", "type": "([^"]*)", "quantity": ([^,]+), "unit": "([^"]*)" \}$`, ctx.aliceHasCreatedExpenseWithUnit)
	sc.Step(`^alice sends POST /api/v1/expenses with body \{ "amount": "([^"]*)", "currency": "([^"]*)", "category": "([^"]*)", "description": "([^"]*)", "date": "([^"]*)", "type": "([^"]*)", "quantity": ([^,]+), "unit": "([^"]*)" \}$`, ctx.aliceSendsCreateExpenseWithUnit)
	sc.Step(`^the response body should contain "quantity" equal to ([^\s]+)$`, ctx.theResponseBodyShouldContainQuantityEqual)
}

// createExpenseWithUnit calls CreateExpense with quantity and unit fields.
func (ctx *scenarioCtx) createExpenseWithUnit(amount, currency, category, description, date, expType string, quantity float64, unit, token string) (int, map[string]interface{}) {
	body := map[string]interface{}{
		"amount": amount, "currency": currency, "category": category,
		"description": description, "date": date, "type": expType,
		"quantity": quantity, "unit": unit,
	}
	c, w := buildGinContext("POST", "/api/v1/expenses", body, token, gin.Params{}, ctx.JWTSvc)
	ctx.Handler.CreateExpense(c)
	return w.Code, readResponse(w)
}

func (ctx *scenarioCtx) aliceHasCreatedExpenseWithUnit(amount, currency, category, description, date, expType, quantityStr, unit string) error {
	quantity, err := strconv.ParseFloat(quantityStr, 64)
	if err != nil {
		return fmt.Errorf("invalid quantity %q: %w", quantityStr, err)
	}
	status, body := ctx.createExpenseWithUnit(amount, currency, category, description, date, expType, quantity, unit, ctx.AccessToken)
	if status != 201 {
		return fmt.Errorf("create expense failed with %d: %v", status, body)
	}
	if id, ok := body["id"].(string); ok {
		ctx.ExpenseID = id
	}
	return nil
}

func (ctx *scenarioCtx) aliceSendsCreateExpenseWithUnit(amount, currency, category, description, date, expType, quantityStr, unit string) error {
	quantity, err := strconv.ParseFloat(quantityStr, 64)
	if err != nil {
		return fmt.Errorf("invalid quantity %q: %w", quantityStr, err)
	}
	status, body := ctx.createExpenseWithUnit(amount, currency, category, description, date, expType, quantity, unit, ctx.AccessToken)
	ctx.LastStatus = status
	ctx.LastBody = body
	if status == 201 {
		if id, ok := body["id"].(string); ok {
			ctx.ExpenseID = id
		}
	}
	return nil
}

func (ctx *scenarioCtx) theResponseBodyShouldContainQuantityEqual(quantityStr string) error {
	expected, err := strconv.ParseFloat(quantityStr, 64)
	if err != nil {
		return fmt.Errorf("invalid expected quantity %q: %w", quantityStr, err)
	}
	v, ok := ctx.LastBody["quantity"]
	if !ok {
		return fmt.Errorf("response does not contain 'quantity' field; body: %v", ctx.LastBody)
	}
	actual, ok := v.(float64)
	if !ok {
		return fmt.Errorf("'quantity' is not a number: %v", v)
	}
	if actual != expected {
		return fmt.Errorf("expected quantity %v, got %v", expected, actual)
	}
	return nil
}
