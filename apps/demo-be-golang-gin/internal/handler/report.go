package handler

import (
	"fmt"
	"net/http"

	"github.com/gin-gonic/gin"

	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-golang-gin/internal/auth"
	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-golang-gin/internal/domain"
	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-golang-gin/internal/store"
)

// PLReport handles GET /api/v1/reports/pl.
func (h *Handler) PLReport(c *gin.Context) {
	claimsVal, _ := c.Get(string(auth.ClaimsKey))
	claims, ok := claimsVal.(*auth.Claims)
	if !ok {
		c.JSON(http.StatusUnauthorized, gin.H{"message": "unauthorized"})
		return
	}
	from := c.Query("from")
	to := c.Query("to")
	currency := c.Query("currency")
	if from == "" || to == "" || currency == "" {
		c.JSON(http.StatusBadRequest, gin.H{"message": "from, to, and currency are required"})
		return
	}
	if err := domain.ValidateCurrency(currency); err != nil {
		RespondError(c, err)
		return
	}
	q := store.PLReportQuery{
		UserID:   claims.Subject,
		From:     from,
		To:       to,
		Currency: currency,
	}
	report, err := h.store.PLReport(c.Request.Context(), q)
	if err != nil {
		RespondError(c, err)
		return
	}
	incomeBreakdown := make(map[string]string)
	for cat, amt := range report.IncomeBreakdown {
		incomeBreakdown[cat] = fmt.Sprintf("%.2f", amt)
	}
	expenseBreakdown := make(map[string]string)
	for cat, amt := range report.ExpenseBreakdown {
		expenseBreakdown[cat] = fmt.Sprintf("%.2f", amt)
	}
	c.JSON(http.StatusOK, gin.H{
		"income_total":      fmt.Sprintf("%.2f", report.IncomTotal),
		"expense_total":     fmt.Sprintf("%.2f", report.ExpenseTotal),
		"net":               fmt.Sprintf("%.2f", report.Net),
		"income_breakdown":  incomeBreakdown,
		"expense_breakdown": expenseBreakdown,
	})
}
