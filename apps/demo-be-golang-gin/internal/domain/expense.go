package domain

import (
	"math"
	"strings"
	"time"
)

// EntryType represents whether an entry is income or expense.
type EntryType string

const (
	// EntryTypeExpense is an expense entry.
	EntryTypeExpense EntryType = "expense"
	// EntryTypeIncome is an income entry.
	EntryTypeIncome EntryType = "income"
)

// supportedCurrencies defines the allowed currencies and their decimal places.
var supportedCurrencies = map[string]int{
	"USD": 2,
	"IDR": 0,
}

// supportedUnits defines all valid units of measure.
var supportedUnits = map[string]bool{
	"liter":  true,
	"ml":     true,
	"kg":     true,
	"g":      true,
	"km":     true,
	"meter":  true,
	"gallon": true,
	"lb":     true,
	"oz":     true,
	"mile":   true,
	"piece":  true,
	"hour":   true,
}

// Expense represents a financial entry (income or expense).
type Expense struct {
	ID          string     `gorm:"primaryKey" json:"id"`
	UserID      string     `gorm:"not null;index" json:"user_id"`
	Amount      float64    `gorm:"not null" json:"amount"`
	Currency    string     `gorm:"not null" json:"currency"`
	Category    string     `gorm:"not null" json:"category"`
	Description string     `json:"description"`
	Date        string     `gorm:"not null" json:"date"`
	Type        EntryType  `gorm:"not null" json:"type"`
	Quantity    *float64   `json:"quantity,omitempty"`
	Unit        string     `json:"unit,omitempty"`
	CreatedAt   time.Time  `json:"created_at"`
	CreatedBy   string     `gorm:"default:system" json:"-"`
	UpdatedAt   time.Time  `json:"updated_at"`
	UpdatedBy   string     `gorm:"default:system" json:"-"`
	DeletedAt   *time.Time `json:"-"`
	DeletedBy   *string    `json:"-"`
}

// CurrencySummary holds the total amount for a given currency.
type CurrencySummary struct {
	Currency string
	Total    float64
}

// ExpenseCategoryBreakdown holds the total for a specific category and type.
type ExpenseCategoryBreakdown struct {
	Category string
	Type     string
	Total    float64
}

// ExpenseCurrencySummary holds income, expense, and category breakdown for a currency.
type ExpenseCurrencySummary struct {
	Currency     string
	TotalIncome  float64
	TotalExpense float64
	Net          float64
	Categories   []ExpenseCategoryBreakdown
}

// PLReport is the profit and loss report result.
type PLReport struct {
	IncomTotal       float64
	ExpenseTotal     float64
	Net              float64
	IncomeBreakdown  map[string]float64
	ExpenseBreakdown map[string]float64
}

// ValidateCurrency checks if the currency code is supported.
func ValidateCurrency(currency string) error {
	if len(currency) != 3 {
		return NewValidationError("invalid currency code: must be 3 characters", "currency")
	}
	upper := strings.ToUpper(currency)
	if _, ok := supportedCurrencies[upper]; !ok {
		return NewValidationError("unsupported currency: "+currency, "currency")
	}
	return nil
}

// ValidateAmount checks if the amount is valid for the given currency.
func ValidateAmount(currency string, amount float64) error {
	if amount < 0 {
		return NewValidationError("amount must not be negative", "amount")
	}
	upper := strings.ToUpper(currency)
	decimals, ok := supportedCurrencies[upper]
	if !ok {
		return NewValidationError("unsupported currency: "+currency, "currency")
	}
	switch decimals {
	case 0:
		if amount != math.Trunc(amount) {
			return NewValidationError(upper+" requires whole number amounts", "amount")
		}
	case 2:
		rounded := math.Round(amount*100) / 100
		if rounded != amount {
			return NewValidationError(upper+" requires at most 2 decimal places", "amount")
		}
	}
	return nil
}

// ValidateUnit checks if the unit is supported (empty string is allowed).
func ValidateUnit(unit string) error {
	if unit == "" {
		return nil
	}
	if !supportedUnits[strings.ToLower(unit)] {
		return NewValidationError("unsupported unit: "+unit, "unit")
	}
	return nil
}

// FormatAmount formats the amount according to currency precision rules.
func FormatAmount(currency string, amount float64) float64 {
	upper := strings.ToUpper(currency)
	decimals, ok := supportedCurrencies[upper]
	if !ok {
		return amount
	}
	switch decimals {
	case 0:
		return math.Trunc(amount)
	case 2:
		return math.Round(amount*100) / 100
	}
	return amount
}
