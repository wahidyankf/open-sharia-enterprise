//go:build integration

package integration_test

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/cucumber/godog"

	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-golang-gin/internal/domain"
)

func registerAdminSteps(sc *godog.ScenarioContext, ctx *ScenarioCtx) {
	sc.Step(`^users "([^"]*)", "([^"]*)", and "([^"]*)" are registered$`, ctx.multipleUsersAreRegistered)
	sc.Step(`^the admin sends GET /api/v1/admin/users$`, ctx.theAdminSendsGetAdminUsers)
	sc.Step(`^the admin sends GET /api/v1/admin/users\?email=([^\s]*)$`, ctx.theAdminSendsGetAdminUsersWithEmail)
	sc.Step(`^the response body should contain at least one user with "([^"]*)" equal to "([^"]*)"$`, ctx.responseBodyContainsAtLeastOneUserWithField)
	sc.Step(`^"([^"]*)" has logged in and stored the access token$`, ctx.userHasLoggedInAndStoredAccessToken)
	sc.Step(`^the admin sends POST /api/v1/admin/users/\{alice_id\}/disable with body \{ "reason": "([^"]*)" \}$`, ctx.theAdminSendsDisableAlice)
	sc.Step(`^the admin sends POST /api/v1/admin/users/\{alice_id\}/enable$`, ctx.theAdminSendsEnableAlice)
	sc.Step(`^alice's account has been disabled by the admin$`, ctx.alicesAccountHasBeenDisabledByAdmin)
	sc.Step(`^alice's account has been disabled$`, ctx.alicesAccountHasBeenDisabledByAdmin)
	sc.Step(`^the admin sends POST /api/v1/admin/users/\{alice_id\}/force-password-reset$`, ctx.theAdminSendsForcePasswordReset)
	sc.Step(`^alice's account status should be "([^"]*)"$`, ctx.alicesAccountStatusShouldBe)
}

func (ctx *ScenarioCtx) multipleUsersAreRegistered(user1, user2, user3 string) error {
	for _, username := range []string{user1, user2, user3} {
		email := username + "@example.com"
		if err := ctx.aUserIsRegisteredWithEmailAndPassword(username, email, "Str0ng#Pass1"); err != nil {
			return err
		}
	}
	return nil
}

func (ctx *ScenarioCtx) theAdminSendsGetAdminUsers() error {
	resp, body := doRequest(ctx.Router, "GET", "/api/v1/admin/users", nil, ctx.AdminToken)
	ctx.LastResponse = resp
	ctx.LastBody = body
	return nil
}

func (ctx *ScenarioCtx) theAdminSendsGetAdminUsersWithEmail(email string) error {
	resp, body := doRequest(ctx.Router, "GET", "/api/v1/admin/users?email="+email, nil, ctx.AdminToken)
	ctx.LastResponse = resp
	ctx.LastBody = body
	return nil
}

func (ctx *ScenarioCtx) responseBodyContainsAtLeastOneUserWithField(field, value string) error {
	body := parseBody(ctx.LastBody)
	data, ok := body["data"]
	if !ok {
		return fmt.Errorf("response does not contain 'data' field; body: %s", string(ctx.LastBody))
	}
	users, ok := data.([]interface{})
	if !ok {
		return fmt.Errorf("'data' field is not an array")
	}
	for _, u := range users {
		uMap, ok := u.(map[string]interface{})
		if !ok {
			continue
		}
		if fmt.Sprintf("%v", uMap[field]) == value {
			return nil
		}
	}
	return fmt.Errorf("no user found with %q = %q in response", field, value)
}

func (ctx *ScenarioCtx) theAdminSendsDisableAlice(reason string) error {
	if ctx.AliceID == "" {
		user, err := ctx.Store.GetUserByUsername(context.Background(), "alice")
		if err != nil {
			return fmt.Errorf("alice not found: %w", err)
		}
		ctx.AliceID = user.ID
	}
	body := map[string]string{"reason": reason}
	resp, respBody := doRequest(ctx.Router, "POST", fmt.Sprintf("/api/v1/admin/users/%s/disable", ctx.AliceID), body, ctx.AdminToken)
	ctx.LastResponse = resp
	ctx.LastBody = respBody
	return nil
}

func (ctx *ScenarioCtx) theAdminSendsEnableAlice() error {
	if ctx.AliceID == "" {
		user, err := ctx.Store.GetUserByUsername(context.Background(), "alice")
		if err != nil {
			return fmt.Errorf("alice not found: %w", err)
		}
		ctx.AliceID = user.ID
	}
	resp, body := doRequest(ctx.Router, "POST", fmt.Sprintf("/api/v1/admin/users/%s/enable", ctx.AliceID), nil, ctx.AdminToken)
	ctx.LastResponse = resp
	ctx.LastBody = body
	return nil
}

func (ctx *ScenarioCtx) alicesAccountHasBeenDisabledByAdmin() error {
	if ctx.AliceID == "" {
		user, err := ctx.Store.GetUserByUsername(context.Background(), "alice")
		if err != nil {
			return fmt.Errorf("alice not found: %w", err)
		}
		ctx.AliceID = user.ID
	}
	// Directly set status via store.
	user, err := ctx.Store.GetUserByID(context.Background(), ctx.AliceID)
	if err != nil {
		return err
	}
	user.Status = domain.StatusDisabled
	return ctx.Store.UpdateUser(context.Background(), user)
}

func (ctx *ScenarioCtx) theAdminSendsForcePasswordReset() error {
	if ctx.AliceID == "" {
		user, err := ctx.Store.GetUserByUsername(context.Background(), "alice")
		if err != nil {
			return fmt.Errorf("alice not found: %w", err)
		}
		ctx.AliceID = user.ID
	}
	resp, body := doRequest(ctx.Router, "POST", fmt.Sprintf("/api/v1/admin/users/%s/force-password-reset", ctx.AliceID), nil, ctx.AdminToken)
	ctx.LastResponse = resp
	ctx.LastBody = body
	return nil
}

// Prevent unused import warning.
var _ = json.Marshal
