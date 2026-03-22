package bdd_test

import (
	"fmt"

	"github.com/gin-gonic/gin"
	"github.com/cucumber/godog"
)

func registerExpenseSteps(sc *godog.ScenarioContext, ctx *scenarioCtx) {
	sc.Step(`^alice sends POST /api/v1/expenses with body \{ "amount": "([^"]*)", "currency": "([^"]*)", "category": "([^"]*)", "description": "([^"]*)", "date": "([^"]*)", "type": "([^"]*)" \}$`, ctx.aliceSendsCreateExpense)
	sc.Step(`^alice has created an entry with body \{ "amount": "([^"]*)", "currency": "([^"]*)", "category": "([^"]*)", "description": "([^"]*)", "date": "([^"]*)", "type": "([^"]*)" \}$`, ctx.aliceHasCreatedEntry)
	sc.Step(`^alice has created 3 entries$`, ctx.aliceHasCreated3Entries)
	sc.Step(`^alice sends GET /api/v1/expenses/\{expenseId\}$`, ctx.aliceSendsGetExpense)
	sc.Step(`^alice sends GET /api/v1/expenses$`, ctx.aliceSendsListExpenses)
	sc.Step(`^alice sends PUT /api/v1/expenses/\{expenseId\} with body \{ "amount": "([^"]*)", "currency": "([^"]*)", "category": "([^"]*)", "description": "([^"]*)", "date": "([^"]*)", "type": "([^"]*)" \}$`, ctx.aliceSendsPutExpense)
	sc.Step(`^alice sends DELETE /api/v1/expenses/\{expenseId\}$`, ctx.aliceSendsDeleteExpense)
	sc.Step(`^the client sends POST /api/v1/expenses with body \{ "amount": "([^"]*)", "currency": "([^"]*)", "category": "([^"]*)", "description": "([^"]*)", "date": "([^"]*)", "type": "([^"]*)" \}$`, ctx.unauthClientSendsCreateExpense)
}

// createExpense calls CreateExpense handler directly and captures the response.
func (ctx *scenarioCtx) createExpense(amount, currency, category, description, date, expType, token string) (int, map[string]interface{}) {
	body := map[string]interface{}{
		"amount": amount, "currency": currency, "category": category,
		"description": description, "date": date, "type": expType,
	}
	c, w := buildGinContext("POST", "/api/v1/expenses", body, token, gin.Params{}, ctx.JWTSvc)
	ctx.Handler.CreateExpense(c)
	return w.Code, readResponse(w)
}

func (ctx *scenarioCtx) aliceSendsCreateExpense(amount, currency, category, description, date, expType string) error {
	status, body := ctx.createExpense(amount, currency, category, description, date, expType, ctx.AccessToken)
	ctx.LastStatus = status
	ctx.LastBody = body
	if status == 201 {
		if id, ok := body["id"].(string); ok {
			ctx.ExpenseID = id
		}
	}
	return nil
}

func (ctx *scenarioCtx) aliceHasCreatedEntry(amount, currency, category, description, date, expType string) error {
	status, body := ctx.createExpense(amount, currency, category, description, date, expType, ctx.AccessToken)
	if status != 201 {
		return fmt.Errorf("create expense failed with %d: %v", status, body)
	}
	if id, ok := body["id"].(string); ok {
		ctx.ExpenseID = id
	}
	return nil
}

func (ctx *scenarioCtx) aliceHasCreated3Entries() error {
	entries := [][]string{
		{"10.00", "USD", "food", "Entry 1", "2025-01-01", "expense"},
		{"20.00", "USD", "food", "Entry 2", "2025-01-02", "expense"},
		{"30.00", "USD", "food", "Entry 3", "2025-01-03", "expense"},
	}
	for _, e := range entries {
		if err := ctx.aliceHasCreatedEntry(e[0], e[1], e[2], e[3], e[4], e[5]); err != nil {
			return err
		}
	}
	return nil
}

func (ctx *scenarioCtx) aliceSendsGetExpense() error {
	params := gin.Params{{Key: "id", Value: ctx.ExpenseID}}
	c, w := buildGinContext("GET", "/api/v1/expenses/"+ctx.ExpenseID, nil, ctx.AccessToken, params, ctx.JWTSvc)
	ctx.Handler.GetExpense(c)
	ctx.LastStatus = w.Code
	ctx.LastBody = readResponse(w)
	return nil
}

func (ctx *scenarioCtx) aliceSendsListExpenses() error {
	c, w := buildGinContext("GET", "/api/v1/expenses", nil, ctx.AccessToken, gin.Params{}, ctx.JWTSvc)
	c.Request.URL.RawQuery = "page=1&size=20"
	ctx.Handler.ListExpenses(c)
	ctx.LastStatus = w.Code
	ctx.LastBody = readResponse(w)
	return nil
}

func (ctx *scenarioCtx) aliceSendsPutExpense(amount, currency, category, description, date, expType string) error {
	body := map[string]interface{}{
		"amount": amount, "currency": currency, "category": category,
		"description": description, "date": date, "type": expType,
	}
	params := gin.Params{{Key: "id", Value: ctx.ExpenseID}}
	c, w := buildGinContext("PUT", "/api/v1/expenses/"+ctx.ExpenseID, body, ctx.AccessToken, params, ctx.JWTSvc)
	ctx.Handler.UpdateExpense(c)
	ctx.LastStatus = w.Code
	ctx.LastBody = readResponse(w)
	return nil
}

func (ctx *scenarioCtx) aliceSendsDeleteExpense() error {
	params := gin.Params{{Key: "id", Value: ctx.ExpenseID}}
	c, w := buildGinContext("DELETE", "/api/v1/expenses/"+ctx.ExpenseID, nil, ctx.AccessToken, params, ctx.JWTSvc)
	ctx.Handler.DeleteExpense(c)
	ctx.LastStatus = w.Code
	ctx.LastBody = readResponse(w)
	return nil
}

func (ctx *scenarioCtx) unauthClientSendsCreateExpense(amount, currency, category, description, date, expType string) error {
	// No token — the handler will fail to extract claims and return 401.
	status, body := ctx.createExpense(amount, currency, category, description, date, expType, "")
	ctx.LastStatus = status
	ctx.LastBody = body
	return nil
}
