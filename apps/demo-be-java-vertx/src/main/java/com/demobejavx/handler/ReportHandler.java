package com.demobejavx.handler;

import com.demobejavx.contracts.CategoryBreakdown;
import com.demobejavx.contracts.PLReport;
import com.demobejavx.domain.model.Expense;
import com.demobejavx.repository.ExpenseRepository;
import io.vertx.core.Handler;
import io.vertx.ext.web.RoutingContext;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ReportHandler implements Handler<RoutingContext> {

    private final ExpenseRepository expenseRepo;

    public ReportHandler(ExpenseRepository expenseRepo) {
        this.expenseRepo = expenseRepo;
    }

    @Override
    public void handle(RoutingContext ctx) {
        String userId = ctx.get("userId");
        if (userId == null) {
            ctx.fail(400);
            return;
        }
        String fromStr = ctx.queryParam("startDate").stream().findFirst()
                .orElseGet(() -> ctx.queryParam("from").stream().findFirst().orElse(""));
        String toStr = ctx.queryParam("endDate").stream().findFirst()
                .orElseGet(() -> ctx.queryParam("to").stream().findFirst().orElse(""));
        String currency = ctx.queryParam("currency").stream().findFirst().orElse("USD")
                .toUpperCase();

        LocalDate from;
        LocalDate to;
        try {
            from = LocalDate.parse(fromStr);
            to = LocalDate.parse(toStr);
        } catch (Exception e) {
            ctx.fail(400);
            return;
        }

        final LocalDate fromDate = from;
        final LocalDate toDate = to;
        final String filterCurrency = currency;

        expenseRepo.findByUserId(userId)
                .onSuccess(expenses -> {
                    List<Expense> filtered = expenses.stream()
                            .filter(e -> filterCurrency.equals(e.currency()))
                            .filter(e -> !e.date().isBefore(fromDate)
                                    && !e.date().isAfter(toDate))
                            .toList();

                    BigDecimal incomeTotal = BigDecimal.ZERO;
                    BigDecimal expenseTotal = BigDecimal.ZERO;
                    Map<String, BigDecimal> incomeByCategory = new HashMap<>();
                    Map<String, BigDecimal> expenseByCategory = new HashMap<>();

                    for (Expense e : filtered) {
                        if (Expense.TYPE_INCOME.equals(e.type())) {
                            incomeTotal = incomeTotal.add(e.amount());
                            incomeByCategory.merge(e.category(), e.amount(), BigDecimal::add);
                        } else {
                            expenseTotal = expenseTotal.add(e.amount());
                            expenseByCategory.merge(e.category(), e.amount(), BigDecimal::add);
                        }
                    }

                    BigDecimal net = incomeTotal.subtract(expenseTotal);
                    int scale = "IDR".equals(filterCurrency) ? 0 : 2;

                    List<CategoryBreakdown> incomeBreakdown = buildBreakdownList(
                            incomeByCategory, scale, "income");
                    List<CategoryBreakdown> expenseBreakdown = buildBreakdownList(
                            expenseByCategory, scale, "expense");

                    PLReport resp = new PLReport()
                            .startDate(fromDate)
                            .endDate(toDate)
                            .currency(filterCurrency)
                            .totalIncome(incomeTotal
                                    .setScale(scale, RoundingMode.HALF_UP).toPlainString())
                            .totalExpense(expenseTotal
                                    .setScale(scale, RoundingMode.HALF_UP).toPlainString())
                            .net(net.setScale(scale, RoundingMode.HALF_UP).toPlainString())
                            .incomeBreakdown(incomeBreakdown)
                            .expenseBreakdown(expenseBreakdown);

                    AuthHandler.sendJson(ctx, 200, resp);
                })
                .onFailure(ctx::fail);
    }

    private List<CategoryBreakdown> buildBreakdownList(
            Map<String, BigDecimal> map, int scale, String type) {
        List<CategoryBreakdown> result = new ArrayList<>();
        for (Map.Entry<String, BigDecimal> entry : map.entrySet()) {
            result.add(new CategoryBreakdown()
                    .category(entry.getKey())
                    .type(type)
                    .total(entry.getValue()
                            .setScale(scale, RoundingMode.HALF_UP).toPlainString()));
        }
        return result;
    }
}
