package handler

import (
	"fmt"
	"net/http"
	"strconv"
	"strings"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/google/uuid"

	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-golang-gin/internal/auth"
	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-golang-gin/internal/domain"
	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-golang-gin/internal/store"
)

// ExpenseRequest is the request body for creating or updating an expense.
type ExpenseRequest struct {
	Amount      string   `json:"amount"`
	Currency    string   `json:"currency"`
	Category    string   `json:"category"`
	Description string   `json:"description"`
	Date        string   `json:"date"`
	Type        string   `json:"type"`
	Quantity    *float64 `json:"quantity,omitempty"`
	Unit        string   `json:"unit,omitempty"`
}

func formatAmountString(currency string, amount float64) string {
	upper := strings.ToUpper(currency)
	switch upper {
	case "IDR":
		return fmt.Sprintf("%.0f", amount)
	default:
		return fmt.Sprintf("%.2f", amount)
	}
}

func expenseToResponse(e *domain.Expense) gin.H {
	resp := gin.H{
		"id":          e.ID,
		"user_id":     e.UserID,
		"amount":      formatAmountString(e.Currency, e.Amount),
		"currency":    e.Currency,
		"category":    e.Category,
		"description": e.Description,
		"date":        e.Date,
		"type":        e.Type,
	}
	if e.Quantity != nil {
		resp["quantity"] = *e.Quantity
	}
	if e.Unit != "" {
		resp["unit"] = e.Unit
	}
	return resp
}

// CreateExpense handles POST /api/v1/expenses.
func (h *Handler) CreateExpense(c *gin.Context) {
	claimsVal, _ := c.Get(string(auth.ClaimsKey))
	claims, ok := claimsVal.(*auth.Claims)
	if !ok {
		c.JSON(http.StatusUnauthorized, gin.H{"message": "unauthorized"})
		return
	}
	var req ExpenseRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"message": "invalid request body"})
		return
	}
	if err := domain.ValidateCurrency(req.Currency); err != nil {
		RespondError(c, err)
		return
	}
	amount, err := strconv.ParseFloat(req.Amount, 64)
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"message": "invalid amount", "field": "amount"})
		return
	}
	if err := domain.ValidateAmount(req.Currency, amount); err != nil {
		RespondError(c, err)
		return
	}
	if err := domain.ValidateUnit(req.Unit); err != nil {
		RespondError(c, err)
		return
	}
	expense := &domain.Expense{
		ID:          uuid.New().String(),
		UserID:      claims.Subject,
		Amount:      amount,
		Currency:    strings.ToUpper(req.Currency),
		Category:    req.Category,
		Description: req.Description,
		Date:        req.Date,
		Type:        domain.EntryType(req.Type),
		Quantity:    req.Quantity,
		Unit:        req.Unit,
		CreatedAt:   time.Now(),
		UpdatedAt:   time.Now(),
	}
	if err := h.store.CreateExpense(c.Request.Context(), expense); err != nil {
		RespondError(c, err)
		return
	}
	c.JSON(http.StatusCreated, expenseToResponse(expense))
}

// GetExpense handles GET /api/v1/expenses/:id.
func (h *Handler) GetExpense(c *gin.Context) {
	claimsVal, _ := c.Get(string(auth.ClaimsKey))
	claims, ok := claimsVal.(*auth.Claims)
	if !ok {
		c.JSON(http.StatusUnauthorized, gin.H{"message": "unauthorized"})
		return
	}
	id := c.Param("id")
	expense, err := h.store.GetExpenseByID(c.Request.Context(), id)
	if err != nil {
		RespondError(c, err)
		return
	}
	if expense.UserID != claims.Subject {
		c.JSON(http.StatusForbidden, gin.H{"message": "access denied"})
		return
	}
	c.JSON(http.StatusOK, expenseToResponse(expense))
}

// ListExpenses handles GET /api/v1/expenses.
func (h *Handler) ListExpenses(c *gin.Context) {
	claimsVal, _ := c.Get(string(auth.ClaimsKey))
	claims, ok := claimsVal.(*auth.Claims)
	if !ok {
		c.JSON(http.StatusUnauthorized, gin.H{"message": "unauthorized"})
		return
	}
	pageStr := c.DefaultQuery("page", "1")
	sizeStr := c.DefaultQuery("size", "20")
	page, _ := strconv.Atoi(pageStr)
	size, _ := strconv.Atoi(sizeStr)
	q := store.ListExpensesQuery{UserID: claims.Subject, Page: page, Size: size}
	expenses, total, err := h.store.ListExpenses(c.Request.Context(), q)
	if err != nil {
		RespondError(c, err)
		return
	}
	var data []gin.H
	for _, e := range expenses {
		data = append(data, expenseToResponse(e))
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

// UpdateExpense handles PUT /api/v1/expenses/:id.
func (h *Handler) UpdateExpense(c *gin.Context) {
	claimsVal, _ := c.Get(string(auth.ClaimsKey))
	claims, ok := claimsVal.(*auth.Claims)
	if !ok {
		c.JSON(http.StatusUnauthorized, gin.H{"message": "unauthorized"})
		return
	}
	id := c.Param("id")
	expense, err := h.store.GetExpenseByID(c.Request.Context(), id)
	if err != nil {
		RespondError(c, err)
		return
	}
	if expense.UserID != claims.Subject {
		c.JSON(http.StatusForbidden, gin.H{"message": "access denied"})
		return
	}
	var req ExpenseRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"message": "invalid request body"})
		return
	}
	if err := domain.ValidateCurrency(req.Currency); err != nil {
		RespondError(c, err)
		return
	}
	amount, err := strconv.ParseFloat(req.Amount, 64)
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"message": "invalid amount", "field": "amount"})
		return
	}
	if err := domain.ValidateAmount(req.Currency, amount); err != nil {
		RespondError(c, err)
		return
	}
	if err := domain.ValidateUnit(req.Unit); err != nil {
		RespondError(c, err)
		return
	}
	expense.Amount = amount
	expense.Currency = strings.ToUpper(req.Currency)
	expense.Category = req.Category
	expense.Description = req.Description
	expense.Date = req.Date
	expense.Type = domain.EntryType(req.Type)
	expense.Quantity = req.Quantity
	expense.Unit = req.Unit
	expense.UpdatedAt = time.Now()
	if err := h.store.UpdateExpense(c.Request.Context(), expense); err != nil {
		RespondError(c, err)
		return
	}
	c.JSON(http.StatusOK, expenseToResponse(expense))
}

// DeleteExpense handles DELETE /api/v1/expenses/:id.
func (h *Handler) DeleteExpense(c *gin.Context) {
	claimsVal, _ := c.Get(string(auth.ClaimsKey))
	claims, ok := claimsVal.(*auth.Claims)
	if !ok {
		c.JSON(http.StatusUnauthorized, gin.H{"message": "unauthorized"})
		return
	}
	id := c.Param("id")
	expense, err := h.store.GetExpenseByID(c.Request.Context(), id)
	if err != nil {
		RespondError(c, err)
		return
	}
	if expense.UserID != claims.Subject {
		c.JSON(http.StatusForbidden, gin.H{"message": "access denied"})
		return
	}
	if err := h.store.DeleteExpense(c.Request.Context(), id); err != nil {
		RespondError(c, err)
		return
	}
	c.Status(http.StatusNoContent)
	c.Writer.WriteHeaderNow()
}

// ExpenseSummary handles GET /api/v1/expenses/summary.
func (h *Handler) ExpenseSummary(c *gin.Context) {
	claimsVal, _ := c.Get(string(auth.ClaimsKey))
	claims, ok := claimsVal.(*auth.Claims)
	if !ok {
		c.JSON(http.StatusUnauthorized, gin.H{"message": "unauthorized"})
		return
	}
	summaries, err := h.store.SumExpensesByCurrency(c.Request.Context(), claims.Subject)
	if err != nil {
		RespondError(c, err)
		return
	}
	result := make(map[string]string)
	for _, s := range summaries {
		result[s.Currency] = formatAmountString(s.Currency, s.Total)
	}
	c.JSON(http.StatusOK, result)
}
