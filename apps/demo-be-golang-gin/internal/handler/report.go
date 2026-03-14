package handler

import (
	"fmt"
	"net/http"
	"strings"

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
	startDate := c.Query("startDate")
	endDate := c.Query("endDate")
	currency := c.Query("currency")
	if startDate == "" || endDate == "" || currency == "" {
		c.JSON(http.StatusBadRequest, gin.H{"message": "startDate, endDate, and currency are required"})
		return
	}
	if err := domain.ValidateCurrency(currency); err != nil {
		RespondError(c, err)
		return
	}
	q := store.PLReportQuery{
		UserID:   claims.Subject,
		From:     startDate,
		To:       endDate,
		Currency: currency,
	}
	report, err := h.store.PLReport(c.Request.Context(), q)
	if err != nil {
		RespondError(c, err)
		return
	}
	incomeBreakdown := make([]gin.H, 0, len(report.IncomeBreakdown))
	for cat, amt := range report.IncomeBreakdown {
		incomeBreakdown = append(incomeBreakdown, gin.H{"category": cat, "type": "income", "total": fmt.Sprintf("%.2f", amt)})
	}
	expenseBreakdown := make([]gin.H, 0, len(report.ExpenseBreakdown))
	for cat, amt := range report.ExpenseBreakdown {
		expenseBreakdown = append(expenseBreakdown, gin.H{"category": cat, "type": "expense", "total": fmt.Sprintf("%.2f", amt)})
	}
	c.JSON(http.StatusOK, gin.H{
		"totalIncome":      fmt.Sprintf("%.2f", report.IncomTotal),
		"totalExpense":     fmt.Sprintf("%.2f", report.ExpenseTotal),
		"net":              fmt.Sprintf("%.2f", report.Net),
		"incomeBreakdown":  incomeBreakdown,
		"expenseBreakdown": expenseBreakdown,
		"startDate":        startDate,
		"endDate":          endDate,
		"currency":         strings.ToUpper(currency),
	})
}
