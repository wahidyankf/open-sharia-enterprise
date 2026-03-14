//go:build integration

package integration_test

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"mime/multipart"
	"net/http"
	"net/http/httptest"

	"github.com/cucumber/godog"

	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-golang-gin/internal/auth"
	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-golang-gin/internal/domain"
)

func registerAttachmentSteps(sc *godog.ScenarioContext, ctx *ScenarioCtx) {
	sc.Step(`^alice uploads file "([^"]*)" with content type "([^"]*)" to POST /api/v1/expenses/\{expenseId\}/attachments$`, ctx.aliceUploadsFileTo)
	sc.Step(`^alice uploads file "([^"]*)" with content type "([^"]*)" to POST /api/v1/expenses/\{bobExpenseId\}/attachments$`, ctx.aliceUploadsFileToBobExpense)
	sc.Step(`^alice has uploaded file "([^"]*)" with content type "([^"]*)" to the entry$`, ctx.aliceHasUploadedFileTo)
	sc.Step(`^alice sends GET /api/v1/expenses/\{expenseId\}/attachments$`, ctx.aliceSendsGetAttachments)
	sc.Step(`^alice sends GET /api/v1/expenses/\{bobExpenseId\}/attachments$`, ctx.aliceSendsGetAttachmentsForBobExpense)
	sc.Step(`^alice sends DELETE /api/v1/expenses/\{expenseId\}/attachments/\{attachmentId\}$`, ctx.aliceSendsDeleteAttachment)
	sc.Step(`^alice sends DELETE /api/v1/expenses/\{expenseId\}/attachments/\{randomAttachmentId\}$`, ctx.aliceSendsDeleteNonExistentAttachment)
	sc.Step(`^alice sends DELETE /api/v1/expenses/\{bobExpenseId\}/attachments/\{attachmentId\}$`, ctx.aliceSendsDeleteAttachmentOnBobExpense)
	sc.Step(`^alice uploads an oversized file to POST /api/v1/expenses/\{expenseId\}/attachments$`, ctx.aliceUploadsOversizedFile)
	sc.Step(`^the response body should contain 2 items in the "([^"]*)" array$`, ctx.theResponseBodyShouldContain2Items)
	sc.Step(`^the response body should contain an attachment with "([^"]*)" equal to "([^"]*)"$`, ctx.theResponseBodyShouldContainAttachmentWithField)
	sc.Step(`^a user "([^"]*)" is registered with email "([^"]*)" and password "([^"]*)"$`, ctx.aUserIsRegisteredWithEmailAndPassword)
	sc.Step(`^bob has created an entry with body \{ "amount": "([^"]*)", "currency": "([^"]*)", "category": "([^"]*)", "description": "([^"]*)", "date": "([^"]*)", "type": "([^"]*)" \}$`, ctx.bobHasCreatedEntry)
	sc.Step(`^alice uploads file "([^"]*)" with content type "([^"]*)" to the entry$`, ctx.aliceHasUploadedFileTo)
}

func uploadFile(router interface {
	ServeHTTP(http.ResponseWriter, *http.Request)
}, path, filename, contentType string, content []byte, token string) (*http.Response, []byte) {
	var buf bytes.Buffer
	writer := multipart.NewWriter(&buf)
	h := make(map[string][]string)
	h["Content-Disposition"] = []string{`form-data; name="file"; filename="` + filename + `"`}
	h["Content-Type"] = []string{contentType}
	part, _ := writer.CreatePart(h)
	_, _ = part.Write(content)
	_ = writer.Close()

	req := httptest.NewRequest(http.MethodPost, path, &buf)
	req.Header.Set("Content-Type", writer.FormDataContentType())
	if token != "" {
		req.Header.Set("Authorization", "Bearer "+token)
	}
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)
	resp := w.Result()
	body := w.Body.Bytes()
	return resp, body
}

func (ctx *ScenarioCtx) aliceUploadsFileTo(filename, contentType string) error {
	content := []byte("fake file content")
	path := fmt.Sprintf("/api/v1/expenses/%s/attachments", ctx.ExpenseID)
	resp, body := uploadFile(ctx.Router, path, filename, contentType, content, ctx.AccessToken)
	ctx.LastResponse = resp
	ctx.LastBody = body
	if resp.StatusCode == 201 {
		var parsed map[string]interface{}
		if err := json.Unmarshal(body, &parsed); err == nil {
			if id, ok := parsed["id"].(string); ok {
				ctx.AttachmentID = id
			}
		}
	}
	return nil
}

func (ctx *ScenarioCtx) aliceUploadsFileToBobExpense(filename, contentType string) error {
	content := []byte("fake file content")
	path := fmt.Sprintf("/api/v1/expenses/%s/attachments", ctx.BobExpenseID)
	resp, body := uploadFile(ctx.Router, path, filename, contentType, content, ctx.AccessToken)
	ctx.LastResponse = resp
	ctx.LastBody = body
	return nil
}

func (ctx *ScenarioCtx) aliceHasUploadedFileTo(filename, contentType string) error {
	content := []byte("fake file content")
	path := fmt.Sprintf("/api/v1/expenses/%s/attachments", ctx.ExpenseID)
	resp, body := uploadFile(ctx.Router, path, filename, contentType, content, ctx.AccessToken)
	if resp.StatusCode != 201 {
		return fmt.Errorf("upload failed with %d: %s", resp.StatusCode, string(body))
	}
	var parsed map[string]interface{}
	if err := json.Unmarshal(body, &parsed); err != nil {
		return err
	}
	if id, ok := parsed["id"].(string); ok {
		ctx.AttachmentID = id
	}
	return nil
}

func (ctx *ScenarioCtx) aliceSendsGetAttachments() error {
	path := fmt.Sprintf("/api/v1/expenses/%s/attachments", ctx.ExpenseID)
	resp, body := doRequest(ctx.Router, "GET", path, nil, ctx.AccessToken)
	ctx.LastResponse = resp
	ctx.LastBody = body
	return nil
}

func (ctx *ScenarioCtx) aliceSendsGetAttachmentsForBobExpense() error {
	path := fmt.Sprintf("/api/v1/expenses/%s/attachments", ctx.BobExpenseID)
	resp, body := doRequest(ctx.Router, "GET", path, nil, ctx.AccessToken)
	ctx.LastResponse = resp
	ctx.LastBody = body
	return nil
}

func (ctx *ScenarioCtx) aliceSendsDeleteAttachment() error {
	path := fmt.Sprintf("/api/v1/expenses/%s/attachments/%s", ctx.ExpenseID, ctx.AttachmentID)
	resp, body := doRequest(ctx.Router, "DELETE", path, nil, ctx.AccessToken)
	ctx.LastResponse = resp
	ctx.LastBody = body
	return nil
}

func (ctx *ScenarioCtx) aliceSendsDeleteNonExistentAttachment() error {
	path := fmt.Sprintf("/api/v1/expenses/%s/attachments/nonexistent-attachment-id", ctx.ExpenseID)
	resp, body := doRequest(ctx.Router, "DELETE", path, nil, ctx.AccessToken)
	ctx.LastResponse = resp
	ctx.LastBody = body
	return nil
}

func (ctx *ScenarioCtx) aliceSendsDeleteAttachmentOnBobExpense() error {
	path := fmt.Sprintf("/api/v1/expenses/%s/attachments/%s", ctx.BobExpenseID, ctx.AttachmentID)
	resp, body := doRequest(ctx.Router, "DELETE", path, nil, ctx.AccessToken)
	ctx.LastResponse = resp
	ctx.LastBody = body
	return nil
}

func (ctx *ScenarioCtx) aliceUploadsOversizedFile() error {
	// Create a file larger than 10 MB.
	content := make([]byte, domain.MaxAttachmentSize+1)
	path := fmt.Sprintf("/api/v1/expenses/%s/attachments", ctx.ExpenseID)
	resp, body := uploadFile(ctx.Router, path, "large.pdf", "application/pdf", content, ctx.AccessToken)
	ctx.LastResponse = resp
	ctx.LastBody = body
	return nil
}

func (ctx *ScenarioCtx) theResponseBodyShouldContain2Items(arrayField string) error {
	body := parseBody(ctx.LastBody)
	v, ok := body[arrayField]
	if !ok {
		return fmt.Errorf("response does not contain %q field; body: %s", arrayField, string(ctx.LastBody))
	}
	arr, ok := v.([]interface{})
	if !ok {
		return fmt.Errorf("field %q is not an array", arrayField)
	}
	if len(arr) != 2 {
		return fmt.Errorf("expected 2 items in %q, got %d", arrayField, len(arr))
	}
	return nil
}

func (ctx *ScenarioCtx) theResponseBodyShouldContainAttachmentWithField(field, value string) error {
	body := parseBody(ctx.LastBody)
	v, ok := body["attachments"]
	if !ok {
		return fmt.Errorf("response does not contain 'attachments' field; body: %s", string(ctx.LastBody))
	}
	attachments, ok := v.([]interface{})
	if !ok {
		return fmt.Errorf("'attachments' is not an array")
	}
	for _, a := range attachments {
		aMap, ok := a.(map[string]interface{})
		if !ok {
			continue
		}
		if fmt.Sprintf("%v", aMap[field]) == value {
			return nil
		}
	}
	return fmt.Errorf("no attachment found with %q = %q", field, value)
}

func (ctx *ScenarioCtx) bobHasCreatedEntry(amount, currency, category, description, date, expType string) error {
	// Login as bob and create an entry.
	loginBody := map[string]string{"username": "bob", "password": "Str0ng#Pass2"}
	resp, body := doRequest(ctx.Router, "POST", "/api/v1/auth/login", loginBody, "")
	if resp.StatusCode != 200 {
		return fmt.Errorf("bob login failed: %s", string(body))
	}
	var parsed map[string]interface{}
	if err := json.Unmarshal(body, &parsed); err != nil {
		return err
	}
	bobToken := parsed["accessToken"].(string)
	expBody := map[string]interface{}{
		"amount":      amount,
		"currency":    currency,
		"category":    category,
		"description": description,
		"date":        date,
		"type":        expType,
	}
	resp2, body2 := doRequest(ctx.Router, "POST", "/api/v1/expenses", expBody, bobToken)
	if resp2.StatusCode != 201 {
		return fmt.Errorf("bob expense creation failed: %s", string(body2))
	}
	var expParsed map[string]interface{}
	if err := json.Unmarshal(body2, &expParsed); err != nil {
		return err
	}
	if id, ok := expParsed["id"].(string); ok {
		ctx.BobExpenseID = id
	}
	return nil
}

// Ensure auth import is used.
var _ = auth.NewJWTService
var _ = domain.MaxAttachmentSize
var _ = context.Background
