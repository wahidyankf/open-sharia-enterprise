package handler

import (
	"net/http"
	"strconv"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/google/uuid"

	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-gogn/internal/domain"
	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-gogn/internal/store"
)

// ListUsers handles GET /api/v1/admin/users.
func (h *Handler) ListUsers(c *gin.Context) {
	email := c.Query("email")
	pageStr := c.DefaultQuery("page", "1")
	sizeStr := c.DefaultQuery("size", "20")
	page, _ := strconv.Atoi(pageStr)
	size, _ := strconv.Atoi(sizeStr)
	q := store.ListUsersQuery{Email: email, Page: page, Size: size}
	users, total, err := h.store.ListUsers(c.Request.Context(), q)
	if err != nil {
		RespondError(c, err)
		return
	}
	var data []gin.H
	for _, u := range users {
		data = append(data, gin.H{
			"id":           u.ID,
			"username":     u.Username,
			"email":        u.Email,
			"display_name": u.DisplayName,
			"status":       u.Status,
			"role":         u.Role,
		})
	}
	if data == nil {
		data = []gin.H{}
	}
	c.JSON(http.StatusOK, gin.H{
		"data":  data,
		"total": total,
		"page":  page,
		"size":  size,
	})
}

// DisableUser handles POST /api/v1/admin/users/:id/disable.
func (h *Handler) DisableUser(c *gin.Context) {
	id := c.Param("id")
	user, err := h.store.GetUserByID(c.Request.Context(), id)
	if err != nil {
		RespondError(c, err)
		return
	}
	user.Status = domain.StatusDisabled
	user.UpdatedAt = time.Now()
	if err := h.store.UpdateUser(c.Request.Context(), user); err != nil {
		RespondError(c, err)
		return
	}
	// Revoke all tokens.
	if err := h.store.RevokeAllRefreshTokensForUser(c.Request.Context(), user.ID); err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"message": "internal server error"})
		return
	}
	c.JSON(http.StatusOK, gin.H{"message": "user disabled", "status": user.Status})
}

// EnableUser handles POST /api/v1/admin/users/:id/enable.
func (h *Handler) EnableUser(c *gin.Context) {
	id := c.Param("id")
	user, err := h.store.GetUserByID(c.Request.Context(), id)
	if err != nil {
		RespondError(c, err)
		return
	}
	user.Status = domain.StatusActive
	user.UpdatedAt = time.Now()
	if err := h.store.UpdateUser(c.Request.Context(), user); err != nil {
		RespondError(c, err)
		return
	}
	c.JSON(http.StatusOK, gin.H{"message": "user enabled", "status": user.Status})
}

// UnlockUser handles POST /api/v1/admin/users/:id/unlock.
func (h *Handler) UnlockUser(c *gin.Context) {
	id := c.Param("id")
	user, err := h.store.GetUserByID(c.Request.Context(), id)
	if err != nil {
		RespondError(c, err)
		return
	}
	user.Status = domain.StatusActive
	user.FailedAttempts = 0
	user.LockedAt = nil
	user.UpdatedAt = time.Now()
	if err := h.store.UpdateUser(c.Request.Context(), user); err != nil {
		RespondError(c, err)
		return
	}
	c.JSON(http.StatusOK, gin.H{"message": "user unlocked", "status": user.Status})
}

// ForcePasswordReset handles POST /api/v1/admin/users/:id/force-password-reset.
func (h *Handler) ForcePasswordReset(c *gin.Context) {
	id := c.Param("id")
	_, err := h.store.GetUserByID(c.Request.Context(), id)
	if err != nil {
		RespondError(c, err)
		return
	}
	resetToken := uuid.New().String()
	c.JSON(http.StatusOK, gin.H{"reset_token": resetToken})
}
