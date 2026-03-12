package handler

import (
	"net/http"

	"github.com/gin-gonic/gin"

	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-golang-gin/internal/auth"
)

// TokenClaims handles GET /api/v1/tokens/claims.
func (h *Handler) TokenClaims(c *gin.Context) {
	claimsVal, _ := c.Get(string(auth.ClaimsKey))
	claims, ok := claimsVal.(*auth.Claims)
	if !ok {
		c.JSON(http.StatusUnauthorized, gin.H{"message": "unauthorized"})
		return
	}
	c.JSON(http.StatusOK, gin.H{
		"sub":      claims.Subject,
		"iss":      claims.Issuer,
		"username": claims.Username,
		"role":     claims.Role,
		"jti":      claims.ID,
		"exp":      claims.ExpiresAt.Unix(),
		"iat":      claims.IssuedAt.Unix(),
	})
}

// JWKS handles GET /.well-known/jwks.json.
func (h *Handler) JWKS(c *gin.Context) {
	c.JSON(http.StatusOK, h.jwtSvc.JWKS())
}
