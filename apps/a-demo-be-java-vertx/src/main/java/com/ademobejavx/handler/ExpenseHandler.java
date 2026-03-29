package com.ademobejavx.handler;

import com.ademobejavx.contracts.CreateExpenseRequest;
import com.ademobejavx.contracts.Expense;
import com.ademobejavx.contracts.ExpenseListResponse;
import com.ademobejavx.contracts.UpdateExpenseRequest;
import com.ademobejavx.domain.validation.DomainException;
import com.ademobejavx.domain.validation.ExpenseValidator;
import com.ademobejavx.domain.validation.ValidationException;
import com.ademobejavx.repository.ExpenseRepository;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.RoutingContext;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Instant;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ExpenseHandler implements Handler<RoutingContext> {

    private static final ObjectMapper MAPPER = new ObjectMapper()
            .registerModule(new JavaTimeModule())
            .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);

    private final ExpenseRepository expenseRepo;
    private final String action;

    public ExpenseHandler(String action, ExpenseRepository expenseRepo) {
        this.action = action;
        this.expenseRepo = expenseRepo;
    }

    @Override
    public void handle(RoutingContext ctx) {
        switch (action) {
            case "create" -> handleCreate(ctx);
            case "list" -> handleList(ctx);
            case "get" -> handleGet(ctx);
            case "update" -> handleUpdate(ctx);
            case "delete" -> handleDelete(ctx);
            case "summary" -> handleSummary(ctx);
            default -> ctx.fail(500);
        }
    }

    private void handleCreate(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();
        if (body == null) {
            ctx.fail(new ValidationException("body", "Body is null"));
            return;
        }
        String userId = ctx.get("userId");
        if (userId == null) {
            ctx.fail(400);
            return;
        }

        CreateExpenseRequest req;
        try {
            req = MAPPER.readValue(body.encode(), CreateExpenseRequest.class);
        } catch (Exception e) {
            ctx.fail(new ValidationException("body", "Invalid request body"));
            return;
        }

        String amountStr = req.getAmount() != null ? req.getAmount() : "";
        String currency = req.getCurrency() != null ? req.getCurrency().toUpperCase() : "";
        String category = req.getCategory() != null ? req.getCategory() : "";
        String description = req.getDescription() != null ? req.getDescription() : "";
        LocalDate parsedDate = req.getDate();
        String type = req.getType() != null ? req.getType().getValue().toLowerCase() : "";
        BigDecimal quantityVal = req.getQuantity();
        String unit = req.getUnit();

        BigDecimal normalizedAmount;
        com.ademobejavx.domain.model.Expense expense;
        try {
            ExpenseValidator.validateCurrency(currency);
            BigDecimal amount = new BigDecimal(amountStr);
            normalizedAmount = ExpenseValidator.validateAndNormalizeAmount(currency, amount);
            if (unit != null && !unit.isBlank()) {
                ExpenseValidator.validateUnit(unit);
            }
            if (parsedDate == null) {
                throw new ValidationException("date", "Date is required");
            }
            Double quantity = quantityVal != null ? quantityVal.doubleValue() : null;
            expense = new com.ademobejavx.domain.model.Expense(null, userId, type,
                    normalizedAmount, currency, category, description, parsedDate, quantity, unit,
                    Instant.now());
        } catch (ValidationException e) {
            ctx.fail(e);
            return;
        } catch (Exception e) {
            ctx.fail(new ValidationException("amount", "Invalid amount or date format"));
            return;
        }

        expenseRepo.save(expense)
                .onSuccess(saved -> {
                    Expense resp = buildContractExpense(saved);
                    AuthHandler.sendJson(ctx, 201, resp);
                })
                .onFailure(ctx::fail);
    }

    private void handleList(RoutingContext ctx) {
        String userId = ctx.get("userId");
        if (userId == null) {
            ctx.fail(400);
            return;
        }
        String pageParam = ctx.queryParam("page").stream().findFirst().orElse("1");
        String sizeParam = ctx.queryParam("size").stream().findFirst().orElse("20");

        int page = Math.max(1, parseInt(pageParam, 1));
        int size = Math.max(1, parseInt(sizeParam, 20));

        expenseRepo.findByUserId(userId)
                .onSuccess(expenses -> {
                    int total = expenses.size();
                    int totalPages = size > 0 ? (int) Math.ceil((double) total / size) : 0;
                    int start = (page - 1) * size;
                    List<com.ademobejavx.domain.model.Expense> pageExpenses = expenses.stream()
                            .skip(start)
                            .limit(size)
                            .toList();

                    List<Expense> content = new ArrayList<>();
                    for (com.ademobejavx.domain.model.Expense e : pageExpenses) {
                        content.add(buildContractExpense(e));
                    }

                    ExpenseListResponse resp = new ExpenseListResponse()
                            .content(content)
                            .totalElements(total)
                            .totalPages(totalPages)
                            .page(page)
                            .size(size);

                    AuthHandler.sendJson(ctx, 200, resp);
                })
                .onFailure(ctx::fail);
    }

    private void handleGet(RoutingContext ctx) {
        String userId = ctx.get("userId");
        String id = ctx.pathParam("id");

        if (userId == null || id == null) {
            ctx.fail(400);
            return;
        }
        expenseRepo.findById(id)
                .onSuccess(expOpt -> {
                    if (expOpt.isEmpty()) {
                        ctx.fail(404);
                        return;
                    }
                    com.ademobejavx.domain.model.Expense exp = expOpt.get();
                    if (!exp.userId().equals(userId)) {
                        ctx.fail(403);
                        return;
                    }
                    Expense resp = buildContractExpense(exp);
                    AuthHandler.sendJson(ctx, 200, resp);
                })
                .onFailure(ctx::fail);
    }

    private void handleUpdate(RoutingContext ctx) {
        String userId = ctx.get("userId");
        String id = ctx.pathParam("id");
        JsonObject body = ctx.body().asJsonObject();
        if (body == null) {
            ctx.fail(400);
            return;
        }
        if (userId == null || id == null) {
            ctx.fail(400);
            return;
        }

        UpdateExpenseRequest req;
        try {
            req = MAPPER.readValue(body.encode(), UpdateExpenseRequest.class);
        } catch (Exception e) {
            ctx.fail(400);
            return;
        }

        expenseRepo.findById(id)
                .compose(expOpt -> {
                    if (expOpt.isEmpty()) {
                        return Future.failedFuture(new DomainException(404, "Not found"));
                    }
                    com.ademobejavx.domain.model.Expense existing = expOpt.get();
                    if (!existing.userId().equals(userId)) {
                        return Future.failedFuture(new DomainException(403, "Forbidden"));
                    }

                    try {
                        String amountStr = req.getAmount() != null
                                ? req.getAmount()
                                : existing.amount().toPlainString();
                        String currency = req.getCurrency() != null
                                ? req.getCurrency()
                                : existing.currency();
                        String description = req.getDescription() != null
                                ? req.getDescription()
                                : existing.description();
                        String category = req.getCategory() != null
                                ? req.getCategory()
                                : existing.category();
                        LocalDate date = req.getDate() != null
                                ? req.getDate()
                                : existing.date();
                        String type = req.getType() != null
                                ? req.getType().getValue().toLowerCase()
                                : existing.type();

                        ExpenseValidator.validateCurrency(currency);
                        BigDecimal amount = new BigDecimal(amountStr);
                        BigDecimal normalizedAmount = ExpenseValidator.validateAndNormalizeAmount(
                                currency, amount);

                        com.ademobejavx.domain.model.Expense updated =
                                new com.ademobejavx.domain.model.Expense(existing.id(), userId,
                                        type, normalizedAmount, currency, category, description,
                                        date, existing.quantity(), existing.unit(),
                                        existing.createdAt());
                        return expenseRepo.update(updated);
                    } catch (ValidationException e) {
                        return Future.failedFuture(e);
                    }
                })
                .onSuccess(updated -> {
                    Expense resp = buildContractExpense(updated);
                    AuthHandler.sendJson(ctx, 200, resp);
                })
                .onFailure(ctx::fail);
    }

    private void handleDelete(RoutingContext ctx) {
        String userId = ctx.get("userId");
        String id = ctx.pathParam("id");

        if (userId == null || id == null) {
            ctx.fail(400);
            return;
        }
        expenseRepo.findById(id)
                .compose(expOpt -> {
                    if (expOpt.isEmpty()) {
                        return Future.failedFuture(new DomainException(404, "Not found"));
                    }
                    com.ademobejavx.domain.model.Expense exp = expOpt.get();
                    if (!exp.userId().equals(userId)) {
                        return Future.failedFuture(new DomainException(403, "Forbidden"));
                    }
                    return expenseRepo.deleteById(id);
                })
                .onSuccess(ignored -> ctx.response().setStatusCode(204).end())
                .onFailure(ctx::fail);
    }

    private void handleSummary(RoutingContext ctx) {
        String userId = ctx.get("userId");
        if (userId == null) {
            ctx.fail(400);
            return;
        }

        expenseRepo.findByUserId(userId)
                .onSuccess(expenses -> {
                    Map<String, BigDecimal> totals = new HashMap<>();
                    for (com.ademobejavx.domain.model.Expense e : expenses) {
                        if (!com.ademobejavx.domain.model.Expense.TYPE_INCOME.equals(e.type())) {
                            totals.merge(e.currency(), e.amount(), BigDecimal::add);
                        }
                    }

                    Map<String, String> result = new java.util.LinkedHashMap<>();
                    for (Map.Entry<String, BigDecimal> entry : totals.entrySet()) {
                        String currency = entry.getKey();
                        int scale = "IDR".equals(currency) ? 0 : 2;
                        BigDecimal rounded = entry.getValue().setScale(scale, RoundingMode.HALF_UP);
                        result.put(currency, rounded.toPlainString());
                    }

                    AuthHandler.sendJson(ctx, 200, result);
                })
                .onFailure(ctx::fail);
    }

    static Expense buildContractExpense(com.ademobejavx.domain.model.Expense expense) {
        Expense.TypeEnum typeEnum = com.ademobejavx.domain.model.Expense.TYPE_INCOME
                .equals(expense.type())
                ? Expense.TypeEnum.INCOME
                : Expense.TypeEnum.EXPENSE;

        Expense result = new Expense()
                .id(expense.id() != null ? expense.id() : "")
                .userId(expense.userId())
                .type(typeEnum)
                .amount(expense.amount().toPlainString())
                .currency(expense.currency())
                .category(expense.category())
                .description(expense.description())
                .date(expense.date());

        if (expense.quantity() != null) {
            result.quantity(BigDecimal.valueOf(expense.quantity()));
        }
        if (expense.unit() != null) {
            result.unit(expense.unit());
        }
        return result;
    }

    private int parseInt(String value, int defaultValue) {
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }
}
