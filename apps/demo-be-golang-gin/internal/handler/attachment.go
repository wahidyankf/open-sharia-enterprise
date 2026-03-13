package handler

import (
	"fmt"
	"net/http"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/google/uuid"

	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-golang-gin/internal/auth"
	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-golang-gin/internal/domain"
)

// UploadAttachment handles POST /api/v1/expenses/:id/attachments.
func (h *Handler) UploadAttachment(c *gin.Context) {
	claimsVal, _ := c.Get(string(auth.ClaimsKey))
	claims, ok := claimsVal.(*auth.Claims)
	if !ok {
		c.JSON(http.StatusUnauthorized, gin.H{"message": "unauthorized"})
		return
	}
	expenseID := c.Param("id")
	expense, err := h.store.GetExpenseByID(c.Request.Context(), expenseID)
	if err != nil {
		RespondError(c, err)
		return
	}
	if expense.UserID != claims.Subject {
		c.JSON(http.StatusForbidden, gin.H{"message": "access denied"})
		return
	}
	fileHeader, err := c.FormFile("file")
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"message": "file is required"})
		return
	}
	contentType := fileHeader.Header.Get("Content-Type")
	if err := domain.ValidateMIMEType(contentType); err != nil {
		RespondError(c, err)
		return
	}
	if err := domain.ValidateFileSize(fileHeader.Size); err != nil {
		RespondError(c, err)
		return
	}
	attachmentID := uuid.New().String()
	attachment := &domain.Attachment{
		ID:          attachmentID,
		ExpenseID:   expenseID,
		Filename:    fileHeader.Filename,
		ContentType: contentType,
		Size:        fileHeader.Size,
		URL:         fmt.Sprintf("/files/%s/%s", expenseID, attachmentID),
		CreatedAt:   time.Now(),
	}
	if err := h.store.CreateAttachment(c.Request.Context(), attachment); err != nil {
		RespondError(c, err)
		return
	}
	c.JSON(http.StatusCreated, gin.H{
		"id":           attachment.ID,
		"expense_id":   attachment.ExpenseID,
		"filename":     attachment.Filename,
		"content_type": attachment.ContentType,
		"size":         attachment.Size,
		"url":          attachment.URL,
	})
}

// ListAttachments handles GET /api/v1/expenses/:id/attachments.
func (h *Handler) ListAttachments(c *gin.Context) {
	claimsVal, _ := c.Get(string(auth.ClaimsKey))
	claims, ok := claimsVal.(*auth.Claims)
	if !ok {
		c.JSON(http.StatusUnauthorized, gin.H{"message": "unauthorized"})
		return
	}
	expenseID := c.Param("id")
	expense, err := h.store.GetExpenseByID(c.Request.Context(), expenseID)
	if err != nil {
		RespondError(c, err)
		return
	}
	if expense.UserID != claims.Subject {
		c.JSON(http.StatusForbidden, gin.H{"message": "access denied"})
		return
	}
	attachments, err := h.store.ListAttachments(c.Request.Context(), expenseID)
	if err != nil {
		RespondError(c, err)
		return
	}
	var items []gin.H
	for _, a := range attachments {
		items = append(items, gin.H{
			"id":           a.ID,
			"expense_id":   a.ExpenseID,
			"filename":     a.Filename,
			"content_type": a.ContentType,
			"size":         a.Size,
			"url":          a.URL,
		})
	}
	if items == nil {
		items = []gin.H{}
	}
	c.JSON(http.StatusOK, gin.H{"attachments": items})
}

// DeleteAttachment handles DELETE /api/v1/expenses/:id/attachments/:aid.
func (h *Handler) DeleteAttachment(c *gin.Context) {
	claimsVal, _ := c.Get(string(auth.ClaimsKey))
	claims, ok := claimsVal.(*auth.Claims)
	if !ok {
		c.JSON(http.StatusUnauthorized, gin.H{"message": "unauthorized"})
		return
	}
	expenseID := c.Param("id")
	attachmentID := c.Param("aid")
	expense, err := h.store.GetExpenseByID(c.Request.Context(), expenseID)
	if err != nil {
		RespondError(c, err)
		return
	}
	if expense.UserID != claims.Subject {
		c.JSON(http.StatusForbidden, gin.H{"message": "access denied"})
		return
	}
	attachment, err := h.store.GetAttachmentByID(c.Request.Context(), attachmentID)
	if err != nil {
		RespondError(c, err)
		return
	}
	if attachment.ExpenseID != expenseID {
		c.JSON(http.StatusNotFound, gin.H{"message": "attachment not found"})
		return
	}
	if err := h.store.DeleteAttachment(c.Request.Context(), attachmentID); err != nil {
		RespondError(c, err)
		return
	}
	c.Status(http.StatusNoContent)
	c.Writer.WriteHeaderNow()
}
