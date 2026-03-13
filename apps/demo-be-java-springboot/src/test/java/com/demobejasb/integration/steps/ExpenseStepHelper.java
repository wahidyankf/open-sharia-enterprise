package com.demobejasb.integration.steps;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.demobejasb.auth.model.User;
import com.demobejasb.auth.repository.UserRepository;
import com.demobejasb.expense.dto.ExpenseListResponse;
import com.demobejasb.expense.dto.ExpenseRequest;
import com.demobejasb.expense.dto.ExpenseResponse;
import com.demobejasb.expense.model.Expense;
import com.demobejasb.expense.repository.ExpenseRepository;
import com.demobejasb.integration.ResponseStore;
import com.demobejasb.security.JwtUtil;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
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

    private static final Validator BEAN_VALIDATOR =
            Validation.buildDefaultValidatorFactory().getValidator();
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
     * Creates an expense for the currently authenticated user (identified by the stored JWT token).
     * Parses the JSON body, validates it, creates the entity and stores the result in ResponseStore.
     *
     * @param body JSON string matching ExpenseRequest schema
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

        ExpenseRequest request = parseExpenseBody(body);
        if (request == null) {
            responseStore.setResponse(400, Map.of("message", "Invalid request body"));
            return;
        }
        var violations = BEAN_VALIDATOR.validate(request);
        if (!violations.isEmpty()) {
            String msg = violations.iterator().next().getMessage();
            responseStore.setResponse(400, Map.of("message", "Validation failed for field: " + msg));
            return;
        }
        Expense expense = new Expense(
                user,
                request.amount(),
                request.currency(),
                request.category(),
                request.description(),
                request.date(),
                request.type());
        expense.setQuantity(request.quantity());
        expense.setUnit(request.unit());
        Expense saved = expenseRepository.save(expense);
        ExpenseResponse resp = ExpenseResponse.from(saved);
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
        ExpenseRequest request = parseExpenseBody(body);
        if (request == null) {
            throw new RuntimeException("Could not parse expense body: " + body);
        }
        Expense expense = new Expense(
                user,
                request.amount(),
                request.currency(),
                request.category(),
                request.description(),
                request.date(),
                request.type());
        expense.setQuantity(request.quantity());
        expense.setUnit(request.unit());
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
                e -> responseStore.setResponse(200, ExpenseResponse.from(e)),
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
        Page<Expense> page = expenseRepository.findAllByUser(
                user, PageRequest.of(0, 20, Sort.by("createdAt").descending()));
        List<ExpenseResponse> data = page.getContent().stream().map(ExpenseResponse::from).toList();
        responseStore.setResponse(200, new ExpenseListResponse(data, page.getTotalElements(), 0));
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
        ExpenseRequest request = parseExpenseBody(body);
        if (request == null) {
            responseStore.setResponse(400, Map.of("message", "Invalid request body"));
            return;
        }
        expenseRepository.findByIdAndUser(expenseId, user).ifPresentOrElse(expense -> {
            expense.setAmount(request.amount());
            expense.setCurrency(request.currency());
            expense.setCategory(request.category());
            expense.setDescription(request.description());
            expense.setDate(request.date());
            expense.setType(request.type());
            expense.setQuantity(request.quantity());
            expense.setUnit(request.unit());
            expense.setUpdatedAt(Instant.now());
            Expense saved = expenseRepository.save(expense);
            responseStore.setResponse(200, ExpenseResponse.from(saved));
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
        List<Expense> all = expenseRepository
                .findAllByUser(user, PageRequest.of(0, Integer.MAX_VALUE, Sort.unsorted()))
                .getContent();
        java.util.Map<String, BigDecimal> totals = new java.util.HashMap<>();
        for (Expense e : all) {
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
     * Parses a JSON body string into an ExpenseRequest.
     */
    @Nullable
    private ExpenseRequest parseExpenseBody(final String body) {
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
            return new ExpenseRequest(amount, currency, category, description, date, type,
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
}
