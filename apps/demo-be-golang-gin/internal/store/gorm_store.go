package store

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"math"
	"strings"
	"time"

	"github.com/pressly/goose/v3"
	"gorm.io/gorm"

	dbmigrations "github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-golang-gin/db"
	"github.com/wahidyankf/open-sharia-enterprise/apps/demo-be-golang-gin/internal/domain"
)

// GORMStore is the GORM-backed implementation of Store.
type GORMStore struct {
	db *gorm.DB
}

// NewGORMStore creates a new GORMStore with the given GORM database.
func NewGORMStore(db *gorm.DB) *GORMStore {
	return &GORMStore{db: db}
}

// Migrate runs goose SQL migrations from the embedded migration files.
// It selects the goose dialect based on the GORM dialector name so the same
// method works with both PostgreSQL (production) and SQLite (legacy tests).
func (s *GORMStore) Migrate() error {
	sqlDB, err := s.db.DB()
	if err != nil {
		return err
	}

	dialect := goose.DialectPostgres
	if s.db.Name() == "sqlite" {
		dialect = goose.DialectSQLite3
	}

	// The embed.FS has files under "migrations/" subdirectory — use fs.Sub to
	// give goose an FS rooted at the migrations directory.
	migrationsFS, err := fs.Sub(dbmigrations.EmbedMigrations, "migrations")
	if err != nil {
		return fmt.Errorf("failed to get migrations sub-FS: %w", err)
	}

	provider, err := goose.NewProvider(dialect, sqlDB, migrationsFS,
		goose.WithVerbose(false),
	)
	if err != nil {
		return err
	}
	_, err = provider.Up(context.Background())
	return err
}

// CreateUser stores a new user.
func (s *GORMStore) CreateUser(_ context.Context, u *domain.User) error {
	result := s.db.Create(u)
	if result.Error != nil {
		if strings.Contains(result.Error.Error(), "unique") || strings.Contains(result.Error.Error(), "UNIQUE") {
			return domain.NewConflictError("username already exists")
		}
		return result.Error
	}
	return nil
}

// GetUserByUsername retrieves a user by username.
func (s *GORMStore) GetUserByUsername(_ context.Context, username string) (*domain.User, error) {
	var u domain.User
	result := s.db.Where("username = ?", username).First(&u)
	if result.Error != nil {
		if errors.Is(result.Error, gorm.ErrRecordNotFound) {
			return nil, domain.NewNotFoundError("user not found")
		}
		return nil, result.Error
	}
	return &u, nil
}

// GetUserByID retrieves a user by ID.
func (s *GORMStore) GetUserByID(_ context.Context, id string) (*domain.User, error) {
	var u domain.User
	result := s.db.First(&u, "id = ?", id)
	if result.Error != nil {
		if errors.Is(result.Error, gorm.ErrRecordNotFound) {
			return nil, domain.NewNotFoundError("user not found")
		}
		return nil, result.Error
	}
	return &u, nil
}

// UpdateUser updates a stored user.
func (s *GORMStore) UpdateUser(_ context.Context, u *domain.User) error {
	return s.db.Save(u).Error
}

// ListUsers returns a paginated list of users, optionally filtered by email or search term.
func (s *GORMStore) ListUsers(_ context.Context, q ListUsersQuery) ([]*domain.User, int64, error) {
	var users []*domain.User
	var total int64
	db := s.db.Model(&domain.User{})
	if q.Search != "" {
		db = db.Where("email LIKE ? OR username LIKE ?", "%"+q.Search+"%", "%"+q.Search+"%")
	} else if q.Email != "" {
		db = db.Where("email LIKE ?", "%"+q.Email+"%")
	}
	if err := db.Count(&total).Error; err != nil {
		return nil, 0, err
	}
	page := q.Page
	if page < 0 {
		page = 0
	}
	size := q.Size
	if size < 1 {
		size = 20
	}
	offset := page * size
	if err := db.Offset(offset).Limit(size).Find(&users).Error; err != nil {
		return nil, 0, err
	}
	return users, total, nil
}

// SaveRefreshToken stores a refresh token.  When a record with the same
// token_str already exists (e.g. test helpers that back-date ExpiresAt),
// the existing row is updated in-place so the latest ExpiresAt and Revoked
// values are applied.
func (s *GORMStore) SaveRefreshToken(_ context.Context, t *domain.RefreshToken) error {
	// Try to create; on unique-constraint conflict for token_str, update the
	// expiry and revoked fields of the existing row.
	result := s.db.Where("token_str = ?", t.TokenStr).First(&domain.RefreshToken{})
	if result.Error == nil {
		// Row already exists — update its mutable fields.
		return s.db.Model(&domain.RefreshToken{}).
			Where("token_str = ?", t.TokenStr).
			Updates(map[string]interface{}{
				"expires_at": t.ExpiresAt,
				"revoked":    t.Revoked,
			}).Error
	}
	return s.db.Create(t).Error
}

// GetRefreshToken retrieves a refresh token by its string value.
func (s *GORMStore) GetRefreshToken(_ context.Context, tokenStr string) (*domain.RefreshToken, error) {
	var t domain.RefreshToken
	result := s.db.Where("token_str = ?", tokenStr).First(&t)
	if result.Error != nil {
		if errors.Is(result.Error, gorm.ErrRecordNotFound) {
			return nil, domain.NewUnauthorizedError("invalid or expired refresh token")
		}
		return nil, result.Error
	}
	return &t, nil
}

// RevokeRefreshToken marks a refresh token as revoked.
func (s *GORMStore) RevokeRefreshToken(_ context.Context, tokenStr string) error {
	return s.db.Model(&domain.RefreshToken{}).Where("token_str = ?", tokenStr).Update("revoked", true).Error
}

// RevokeAllRefreshTokensForUser revokes all refresh tokens for a user.
func (s *GORMStore) RevokeAllRefreshTokensForUser(_ context.Context, userID string) error {
	return s.db.Model(&domain.RefreshToken{}).Where("user_id = ?", userID).Update("revoked", true).Error
}

// BlacklistAccessToken adds an access token JTI to the revoked tokens table.
func (s *GORMStore) BlacklistAccessToken(_ context.Context, jti string, expiresAt time.Time) error {
	return s.db.Create(&domain.RevokedToken{JTI: jti, ExpiresAt: expiresAt}).Error
}

// IsAccessTokenBlacklisted checks if an access token JTI is in the revoked tokens table.
func (s *GORMStore) IsAccessTokenBlacklisted(_ context.Context, jti string) (bool, error) {
	var count int64
	err := s.db.Model(&domain.RevokedToken{}).Where("jti = ?", jti).Count(&count).Error
	return count > 0, err
}

// CreateExpense stores a new expense entry.
func (s *GORMStore) CreateExpense(_ context.Context, e *domain.Expense) error {
	return s.db.Create(e).Error
}

// GetExpenseByID retrieves an expense by ID.
func (s *GORMStore) GetExpenseByID(_ context.Context, id string) (*domain.Expense, error) {
	var e domain.Expense
	result := s.db.First(&e, "id = ?", id)
	if result.Error != nil {
		if errors.Is(result.Error, gorm.ErrRecordNotFound) {
			return nil, domain.NewNotFoundError("expense not found")
		}
		return nil, result.Error
	}
	return &e, nil
}

// ListExpenses returns a paginated list of expenses for the given user.
func (s *GORMStore) ListExpenses(_ context.Context, q ListExpensesQuery) ([]*domain.Expense, int64, error) {
	var expenses []*domain.Expense
	var total int64
	db := s.db.Model(&domain.Expense{}).Where("user_id = ?", q.UserID)
	if err := db.Count(&total).Error; err != nil {
		return nil, 0, err
	}
	page := q.Page
	if page < 0 {
		page = 0
	}
	size := q.Size
	if size < 1 {
		size = 20
	}
	offset := page * size
	if err := db.Offset(offset).Limit(size).Find(&expenses).Error; err != nil {
		return nil, 0, err
	}
	return expenses, total, nil
}

// UpdateExpense updates a stored expense.
func (s *GORMStore) UpdateExpense(_ context.Context, e *domain.Expense) error {
	return s.db.Save(e).Error
}

// DeleteExpense deletes an expense by ID.
func (s *GORMStore) DeleteExpense(_ context.Context, id string) error {
	return s.db.Delete(&domain.Expense{}, "id = ?", id).Error
}

// SumExpensesByCurrency aggregates expense totals by currency for a user.
func (s *GORMStore) SumExpensesByCurrency(_ context.Context, userID string) ([]domain.CurrencySummary, error) {
	type row struct {
		Currency string
		Total    float64
	}
	var rows []row
	err := s.db.Model(&domain.Expense{}).
		Select("currency, SUM(amount) as total").
		Where("user_id = ? AND type = ?", userID, domain.EntryTypeExpense).
		Group("currency").
		Scan(&rows).Error
	if err != nil {
		return nil, err
	}
	result := make([]domain.CurrencySummary, 0, len(rows))
	for _, r := range rows {
		result = append(result, domain.CurrencySummary{Currency: r.Currency, Total: r.Total})
	}
	return result, nil
}

// ExpenseSummaryByCurrency returns income/expense breakdown grouped by currency.
func (s *GORMStore) ExpenseSummaryByCurrency(_ context.Context, userID string) ([]domain.ExpenseCurrencySummary, error) {
	type row struct {
		Currency string
		Type     string
		Category string
		Total    float64
	}
	var rows []row
	err := s.db.Model(&domain.Expense{}).
		Select("currency, type, category, SUM(amount) as total").
		Where("user_id = ?", userID).
		Group("currency, type, category").
		Scan(&rows).Error
	if err != nil {
		return nil, err
	}
	type currencyData struct {
		totalIncome  float64
		totalExpense float64
		categories   []domain.ExpenseCategoryBreakdown
	}
	byCurrency := make(map[string]*currencyData)
	for _, r := range rows {
		if _, ok := byCurrency[r.Currency]; !ok {
			byCurrency[r.Currency] = &currencyData{}
		}
		d := byCurrency[r.Currency]
		switch domain.EntryType(r.Type) {
		case domain.EntryTypeIncome:
			d.totalIncome += r.Total
		case domain.EntryTypeExpense:
			d.totalExpense += r.Total
		}
		d.categories = append(d.categories, domain.ExpenseCategoryBreakdown{
			Category: r.Category,
			Type:     r.Type,
			Total:    r.Total,
		})
	}
	result := make([]domain.ExpenseCurrencySummary, 0, len(byCurrency))
	for currency, d := range byCurrency {
		result = append(result, domain.ExpenseCurrencySummary{
			Currency:     currency,
			TotalIncome:  d.totalIncome,
			TotalExpense: d.totalExpense,
			Net:          d.totalIncome - d.totalExpense,
			Categories:   d.categories,
		})
	}
	return result, nil
}

// CreateAttachment stores a new attachment.
func (s *GORMStore) CreateAttachment(_ context.Context, a *domain.Attachment) error {
	return s.db.Create(a).Error
}

// GetAttachmentByID retrieves an attachment by ID.
func (s *GORMStore) GetAttachmentByID(_ context.Context, id string) (*domain.Attachment, error) {
	var a domain.Attachment
	result := s.db.First(&a, "id = ?", id)
	if result.Error != nil {
		if errors.Is(result.Error, gorm.ErrRecordNotFound) {
			return nil, domain.NewNotFoundError("attachment not found")
		}
		return nil, result.Error
	}
	return &a, nil
}

// ListAttachments lists all attachments for a given expense.
func (s *GORMStore) ListAttachments(_ context.Context, expenseID string) ([]*domain.Attachment, error) {
	var attachments []*domain.Attachment
	err := s.db.Where("expense_id = ?", expenseID).Find(&attachments).Error
	return attachments, err
}

// DeleteAttachment deletes an attachment by ID.
func (s *GORMStore) DeleteAttachment(_ context.Context, id string) error {
	result := s.db.Delete(&domain.Attachment{}, "id = ?", id)
	if result.Error != nil {
		return result.Error
	}
	if result.RowsAffected == 0 {
		return domain.NewNotFoundError("attachment not found")
	}
	return nil
}

// PLReport generates a profit and loss report.
func (s *GORMStore) PLReport(_ context.Context, q PLReportQuery) (*domain.PLReport, error) {
	type row struct {
		Type     string
		Category string
		Total    float64
	}
	var rows []row
	err := s.db.Model(&domain.Expense{}).
		Select("type, category, SUM(amount) as total").
		Where("user_id = ? AND currency = ? AND date >= ? AND date <= ?", q.UserID, strings.ToUpper(q.Currency), q.From, q.To).
		Group("type, category").
		Scan(&rows).Error
	if err != nil {
		return nil, err
	}
	report := &domain.PLReport{
		IncomeBreakdown:  make(map[string]float64),
		ExpenseBreakdown: make(map[string]float64),
	}
	for _, r := range rows {
		switch domain.EntryType(r.Type) {
		case domain.EntryTypeIncome:
			report.IncomTotal += r.Total
			report.IncomeBreakdown[r.Category] = r.Total
		case domain.EntryTypeExpense:
			report.ExpenseTotal += r.Total
			report.ExpenseBreakdown[r.Category] = r.Total
		}
	}
	report.Net = report.IncomTotal - report.ExpenseTotal
	report.IncomTotal = math.Round(report.IncomTotal*100) / 100
	report.ExpenseTotal = math.Round(report.ExpenseTotal*100) / 100
	report.Net = math.Round(report.Net*100) / 100
	return report, nil
}

// ResetDB deletes all user-created data (for test use only).
// Deletions follow FK constraint order: attachments → expenses → refresh_tokens → revoked_tokens → users.
func (s *GORMStore) ResetDB(_ context.Context) error {
	if err := s.db.Session(&gorm.Session{AllowGlobalUpdate: true}).Delete(&domain.Attachment{}).Error; err != nil {
		return err
	}
	if err := s.db.Session(&gorm.Session{AllowGlobalUpdate: true}).Delete(&domain.Expense{}).Error; err != nil {
		return err
	}
	if err := s.db.Session(&gorm.Session{AllowGlobalUpdate: true}).Delete(&domain.RefreshToken{}).Error; err != nil {
		return err
	}
	if err := s.db.Session(&gorm.Session{AllowGlobalUpdate: true}).Delete(&domain.RevokedToken{}).Error; err != nil {
		return err
	}
	return s.db.Session(&gorm.Session{AllowGlobalUpdate: true}).Delete(&domain.User{}).Error
}

// PromoteToAdmin sets the role of the given username to "ADMIN" (for test use only).
func (s *GORMStore) PromoteToAdmin(_ context.Context, username string) error {
	var u domain.User
	result := s.db.Where("username = ?", username).First(&u)
	if result.Error != nil {
		if errors.Is(result.Error, gorm.ErrRecordNotFound) {
			return domain.NewNotFoundError("user not found")
		}
		return result.Error
	}
	u.Role = "ADMIN"
	return s.db.Save(&u).Error
}

// Ensure GORMStore implements Store at compile time.
var _ Store = (*GORMStore)(nil)
