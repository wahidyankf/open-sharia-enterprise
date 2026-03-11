// Package handler provides HTTP request handlers for the demo-be-gogn REST API,
// including authentication, expense management, attachments, and admin operations.
package handler

import (
	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-gogn/internal/auth"
	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-gogn/internal/store"
)

// Handler holds the dependencies for all HTTP handlers.
type Handler struct {
	store  store.Store
	jwtSvc *auth.JWTService
}

// New creates a new Handler.
func New(st store.Store, jwtSvc *auth.JWTService) *Handler {
	return &Handler{store: st, jwtSvc: jwtSvc}
}
