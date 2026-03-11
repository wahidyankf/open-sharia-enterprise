package router_test

import (
	"net/http/httptest"
	"testing"

	"github.com/gin-gonic/gin"

	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-gogn/internal/auth"
	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-gogn/internal/router"
	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-gogn/internal/store"
)

func TestUnitNewRouter(t *testing.T) {
	gin.SetMode(gin.TestMode)
	ms := store.NewMemoryStore()
	jwtSvc := auth.NewJWTService("test-secret-at-least-32-chars-long")
	r := router.NewRouter(ms, jwtSvc)
	if r == nil {
		t.Fatal("expected non-nil router")
	}
	// Verify health route works.
	req := httptest.NewRequest("GET", "/health", nil)
	w := httptest.NewRecorder()
	r.ServeHTTP(w, req)
	if w.Code != 200 {
		t.Errorf("expected 200 for /health, got %d", w.Code)
	}
}
