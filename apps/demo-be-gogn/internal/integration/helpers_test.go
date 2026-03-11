//go:build integration

package integration_test

import (
	"bytes"
	"encoding/json"
	"io"
	"mime/multipart"
	"net/http"
	"net/http/httptest"

	"github.com/gin-gonic/gin"
)

func doRequest(r *gin.Engine, method, path string, body interface{}, token string) (*http.Response, []byte) {
	var bodyReader io.Reader
	if body != nil {
		b, _ := json.Marshal(body)
		bodyReader = bytes.NewReader(b)
	}
	req := httptest.NewRequest(method, path, bodyReader)
	req.Header.Set("Content-Type", "application/json")
	if token != "" {
		req.Header.Set("Authorization", "Bearer "+token)
	}
	w := httptest.NewRecorder()
	r.ServeHTTP(w, req)
	resp := w.Result()
	respBody, _ := io.ReadAll(resp.Body)
	return resp, respBody
}

func doMultipartRequest(r *gin.Engine, path string, fieldName, filename, contentType string, content []byte, token string) (*http.Response, []byte) {
	var buf bytes.Buffer
	writer := multipart.NewWriter(&buf)
	part, _ := writer.CreateFormFile(fieldName, filename)
	_, _ = part.Write(content)
	_ = writer.WriteField("content_type", contentType)
	writer.Close()

	req := httptest.NewRequest(http.MethodPost, path, &buf)
	req.Header.Set("Content-Type", writer.FormDataContentType())
	// Override individual part content type via a custom approach.
	// The multipart header Content-Type is set per-part.
	// We need to set it on the file part; use a raw approach:
	req2 := httptest.NewRequest(http.MethodPost, path, nil)
	_ = req2
	// Re-create with explicit Content-Type on file part.
	var buf2 bytes.Buffer
	writer2 := multipart.NewWriter(&buf2)
	h := make(map[string][]string)
	h["Content-Disposition"] = []string{`form-data; name="file"; filename="` + filename + `"`}
	h["Content-Type"] = []string{contentType}
	part2, _ := writer2.CreatePart(h)
	_, _ = part2.Write(content)
	_ = writer2.Close()

	req3 := httptest.NewRequest(http.MethodPost, path, &buf2)
	req3.Header.Set("Content-Type", writer2.FormDataContentType())
	if token != "" {
		req3.Header.Set("Authorization", "Bearer "+token)
	}
	w := httptest.NewRecorder()
	r.ServeHTTP(w, req3)
	resp := w.Result()
	respBody, _ := io.ReadAll(resp.Body)
	return resp, respBody
}

func parseBody(body []byte) map[string]interface{} {
	var result map[string]interface{}
	_ = json.Unmarshal(body, &result)
	return result
}
