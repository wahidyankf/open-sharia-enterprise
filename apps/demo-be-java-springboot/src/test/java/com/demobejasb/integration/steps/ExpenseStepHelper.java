package com.demobejasb.integration.steps;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.demobejasb.auth.model.User;
import com.demobejasb.auth.repository.UserRepository;
import com.demobejasb.contracts.Expense;
import com.demobejasb.contracts.ExpenseListResponse;
import com.demobejasb.expense.controller.ExpenseController;
import com.demobejasb.expense.repository.ExpenseRepository;
import com.demobejasb.integration.ResponseStore;
import com.demobejasb.security.JwtUtil;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.jspecify.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Component;

/**
 * Shared helper for expense-related step definitions. Encapsulates the service-level logic that
 * mirrors what the ExpenseController does, without using HTTP.
 */
@Component
@Scope("cucumber-glue")
public class ExpenseStepHelper {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Autowired
    private ExpenseRepository expenseRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    @Autowired
    private JwtUtil jwtUtil;

    /**
     * Simulates an unauthenticated POST to create an expense. Always returns 401 without
     * consulting the token store, because the scenario explicitly has no auth token.
     */
    public void createExpenseUnauthenticated(final String body) {
        responseStore.setResponse(401, Map.of("message", "Unauthorized"));
    }

    /**
     * Creates an expense for the currently authenticated user (identified by the stored JWT token).
     * Parses the JSON body, validates it, creates the entity and stores the result in ResponseStore.
     *
     * @param body JSON string matching CreateExpenseRequest schema
     * @param storeId if true, also stores the created expense ID in TokenStore
     */
    public void createExpenseForCurrentUser(final String body, final boolean storeId) {
        String token = tokenStore.getToken();
        if (token == null || !jwtUtil.isTokenValid(token)) {
            responseStore.setResponse(401, Map.of("message", "Unauthorized"));
            return;
        }
        String username = jwtUtil.extractUsername(token);
        User user = userRepository.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("User not found: " + username));

        ParsedExpense parsed = parseExpenseBody(body);
        if (parsed == null) {
            responseStore.setResponse(400, Map.of("message", "Invalid request body"));
            return;
        }
        if (!parsed.isValid()) {
            responseStore.setResponse(400, Map.of("message", "Validation failed"));
            return;
        }
        com.demobejasb.expense.model.Expense expense = new com.demobejasb.expense.model.Expense(
                user,
                parsed.amount,
                parsed.currency,
                parsed.category,
                parsed.description,
                parsed.date,
                parsed.type);
        expense.setQuantity(parsed.quantity);
        expense.setUnit(parsed.unit);
        com.demobejasb.expense.model.Expense saved = expenseRepository.save(expense);
        Expense resp = ExpenseController.buildExpenseResponse(saved);
        responseStore.setResponse(201, resp);
        if (storeId) {
            tokenStore.setExpenseId(saved.getId());
        }
    }

    /**
     * Creates an expense for the currently authenticated user (used in Given steps). Throws on
     * failure.
     */
    public UUID createExpenseOrFail(final String token, final String body) {
        String username = jwtUtil.extractUsername(token);
        User user = userRepository.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("User not found: " + username));
        ParsedExpense parsed = parseExpenseBody(body);
        if (parsed == null) {
            throw new RuntimeException("Could not parse expense body: " + body);
        }
        com.demobejasb.expense.model.Expense expense = new com.demobejasb.expense.model.Expense(
                user,
                parsed.amount,
                parsed.currency,
                parsed.category,
                parsed.description,
                parsed.date,
                parsed.type);
        expense.setQuantity(parsed.quantity);
        expense.setUnit(parsed.unit);
        return expenseRepository.save(expense).getId();
    }

    /**
     * Gets a single expense by ID for the current user.
     */
    public void getExpenseById(final UUID expenseId) {
        String token = tokenStore.getToken();
        if (token == null || !jwtUtil.isTokenValid(token)) {
            responseStore.setResponse(401, Map.of("message", "Unauthorized"));
            return;
        }
        String username = jwtUtil.extractUsername(token);
        User user = userRepository.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("User not found"));
        expenseRepository.findByIdAndUser(expenseId, user).ifPresentOrElse(
                e -> responseStore.setResponse(200, ExpenseController.buildExpenseResponse(e)),
                () -> responseStore.setResponse(404, Map.of("message", "Expense not found")));
    }

    /**
     * Lists all expenses for the current user.
     */
    public void listExpenses() {
        String token = tokenStore.getToken();
        if (token == null || !jwtUtil.isTokenValid(token)) {
            responseStore.setResponse(401, Map.of("message", "Unauthorized"));
            return;
        }
        String username = jwtUtil.extractUsername(token);
        User user = userRepository.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("User not found"));
        Page<com.demobejasb.expense.model.Expense> page = expenseRepository.findAllByUser(
                user, PageRequest.of(0, 20, Sort.by("createdAt").descending()));
        List<Expense> data = page.getContent().stream()
                .map(ExpenseController::buildExpenseResponse).toList();
        ExpenseListResponse response = new ExpenseListResponse();
        response.setContent(data);
        response.setTotalElements((int) page.getTotalElements());
        response.setTotalPages(page.getTotalPages());
        response.setPage(0);
        response.setSize(20);
        responseStore.setResponse(200, response);
    }

    /**
     * Updates an expense by ID for the current user.
     */
    public void updateExpense(final UUID expenseId, final String body) {
        String token = tokenStore.getToken();
        if (token == null || !jwtUtil.isTokenValid(token)) {
            responseStore.setResponse(401, Map.of("message", "Unauthorized"));
            return;
        }
        String username = jwtUtil.extractUsername(token);
        User user = userRepository.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("User not found"));
        ParsedExpense parsed = parseExpenseBody(body);
        if (parsed == null) {
            responseStore.setResponse(400, Map.of("message", "Invalid request body"));
            return;
        }
        expenseRepository.findByIdAndUser(expenseId, user).ifPresentOrElse(expense -> {
            expense.setAmount(parsed.amount);
            expense.setCurrency(parsed.currency);
            expense.setCategory(parsed.category);
            expense.setDescription(parsed.description);
            expense.setDate(parsed.date);
            expense.setType(parsed.type);
            expense.setQuantity(parsed.quantity);
            expense.setUnit(parsed.unit);
            expense.setUpdatedAt(Instant.now());
            com.demobejasb.expense.model.Expense saved = expenseRepository.save(expense);
            responseStore.setResponse(200, ExpenseController.buildExpenseResponse(saved));
        }, () -> responseStore.setResponse(404, Map.of("message", "Expense not found")));
    }

    /**
     * Deletes an expense by ID for the current user.
     */
    public void deleteExpense(final UUID expenseId) {
        String token = tokenStore.getToken();
        if (token == null || !jwtUtil.isTokenValid(token)) {
            responseStore.setResponse(401, Map.of("message", "Unauthorized"));
            return;
        }
        String username = jwtUtil.extractUsername(token);
        User user = userRepository.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("User not found"));
        expenseRepository.findByIdAndUser(expenseId, user).ifPresentOrElse(expense -> {
            expenseRepository.delete(expense);
            responseStore.setResponse(204);
        }, () -> responseStore.setResponse(404, Map.of("message", "Expense not found")));
    }

    /**
     * Gets the expense summary (currency totals) for the current user.
     */
    public void getExpenseSummary() {
        String token = tokenStore.getToken();
        if (token == null || !jwtUtil.isTokenValid(token)) {
            responseStore.setResponse(401, Map.of("message", "Unauthorized"));
            return;
        }
        String username = jwtUtil.extractUsername(token);
        User user = userRepository.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("User not found"));
        List<com.demobejasb.expense.model.Expense> all = expenseRepository
                .findAllByUser(user, PageRequest.of(0, Integer.MAX_VALUE, Sort.unsorted()))
                .getContent();
        java.util.Map<String, BigDecimal> totals = new java.util.HashMap<>();
        for (com.demobejasb.expense.model.Expense e : all) {
            if ("expense".equals(e.getType())) {
                totals.merge(e.getCurrency(), e.getAmount(), BigDecimal::add);
            }
        }
        java.util.Map<String, String> result = new java.util.LinkedHashMap<>();
        for (Map.Entry<String, BigDecimal> entry : totals.entrySet()) {
            result.put(entry.getKey(), formatAmount(entry.getValue(), entry.getKey()));
        }
        responseStore.setResponse(200, result);
    }

    /**
     * Parses a JSON body string into a ParsedExpense holder.
     */
    @Nullable
    private ParsedExpense parseExpenseBody(final String body) {
        try {
            Map<?, ?> map = MAPPER.readValue(body, Map.class);
            BigDecimal amount = new BigDecimal(String.valueOf(map.get("amount")));
            String currency = (String) map.get("currency");
            String category = (String) map.get("category");
            String description = (String) map.get("description");
            LocalDate date = LocalDate.parse((String) map.get("date"));
            String type = (String) map.get("type");
            BigDecimal quantity = map.containsKey("quantity") && map.get("quantity") != null
                    ? new BigDecimal(String.valueOf(map.get("quantity")))
                    : null;
            String unit = (String) map.get("unit");
            return new ParsedExpense(amount, currency, category, description, date, type,
                    quantity, unit);
        } catch (Exception e) {
            return null;
        }
    }

    private String formatAmount(final BigDecimal amount, final String currency) {
        if ("IDR".equals(currency)) {
            return amount.setScale(0, java.math.RoundingMode.HALF_UP).toPlainString();
        }
        return amount.setScale(2, java.math.RoundingMode.HALF_UP).toPlainString();
    }

    /** Simple data holder for parsed expense fields. */
    static class ParsedExpense {
        final BigDecimal amount;
        final String currency;
        final String category;
        final String description;
        final LocalDate date;
        final String type;
        final @Nullable BigDecimal quantity;
        final @Nullable String unit;

        ParsedExpense(
                final BigDecimal amount,
                final String currency,
                final String category,
                final String description,
                final LocalDate date,
                final String type,
                final @Nullable BigDecimal quantity,
                final @Nullable String unit) {
            this.amount = amount;
            this.currency = currency;
            this.category = category;
            this.description = description;
            this.date = date;
            this.type = type;
            this.quantity = quantity;
            this.unit = unit;
        }

        boolean isValid() {
            return amount != null && amount.compareTo(BigDecimal.ZERO) > 0
                    && currency != null && !currency.isBlank()
                    && category != null && !category.isBlank()
                    && description != null && !description.isBlank()
                    && date != null
                    && type != null && !type.isBlank();
        }
    }
}
