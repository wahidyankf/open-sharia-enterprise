package handler

import (
	"net/http"

	"github.com/gin-gonic/gin"
)

// Health handles GET /health.
func (h *Handler) Health(c *gin.Context) {
	c.JSON(http.StatusOK, gin.H{"status": "UP"})
}
