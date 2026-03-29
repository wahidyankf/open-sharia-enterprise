package com.ademobejasb.report.controller;

import com.ademobejasb.auth.model.User;
import com.ademobejasb.auth.repository.UserRepository;
import com.ademobejasb.contracts.CategoryBreakdown;
import com.ademobejasb.contracts.PLReport;
import com.ademobejasb.expense.model.Expense;
import com.ademobejasb.expense.repository.ExpenseRepository;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/reports")
public class ReportController {

    private final ExpenseRepository expenseRepository;
    private final UserRepository userRepository;

    public ReportController(
            final ExpenseRepository expenseRepository, final UserRepository userRepository) {
        this.expenseRepository = expenseRepository;
        this.userRepository = userRepository;
    }

    @GetMapping("/pl")
    public ResponseEntity<PLReport> profitAndLoss(
            @AuthenticationPrincipal final UserDetails userDetails,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) final LocalDate startDate,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) final LocalDate endDate,
            @RequestParam final String currency) {
        User user =
                userRepository
                        .findByUsername(userDetails.getUsername())
                        .orElseThrow(() -> new RuntimeException("User not found"));
        List<Expense> expenses =
                expenseRepository
                        .findAllByUser(user, PageRequest.of(0, Integer.MAX_VALUE, Sort.unsorted()))
                        .getContent()
                        .stream()
                        .filter(e -> e.getCurrency().equals(currency))
                        .filter(
                                e ->
                                        !e.getDate().isBefore(startDate)
                                                && !e.getDate().isAfter(endDate))
                        .toList();

        BigDecimal incomeTotal =
                expenses.stream()
                        .filter(e -> "income".equals(e.getType()))
                        .map(Expense::getAmount)
                        .reduce(BigDecimal.ZERO, BigDecimal::add);
        BigDecimal expenseTotal =
                expenses.stream()
                        .filter(e -> "expense".equals(e.getType()))
                        .map(Expense::getAmount)
                        .reduce(BigDecimal.ZERO, BigDecimal::add);
        BigDecimal net = incomeTotal.subtract(expenseTotal);

        List<CategoryBreakdown> incomeBreakdown =
                expenses.stream()
                        .filter(e -> "income".equals(e.getType()))
                        .collect(
                                Collectors.groupingBy(
                                        Expense::getCategory,
                                        Collectors.reducing(
                                                BigDecimal.ZERO,
                                                Expense::getAmount,
                                                BigDecimal::add)))
                        .entrySet()
                        .stream()
                        .map(
                                entry -> {
                                    CategoryBreakdown cb = new CategoryBreakdown();
                                    cb.setCategory(entry.getKey());
                                    cb.setType("income");
                                    cb.setTotal(format(entry.getValue(), currency));
                                    return cb;
                                })
                        .collect(Collectors.toList());

        List<CategoryBreakdown> expenseBreakdown =
                expenses.stream()
                        .filter(e -> "expense".equals(e.getType()))
                        .collect(
                                Collectors.groupingBy(
                                        Expense::getCategory,
                                        Collectors.reducing(
                                                BigDecimal.ZERO,
                                                Expense::getAmount,
                                                BigDecimal::add)))
                        .entrySet()
                        .stream()
                        .map(
                                entry -> {
                                    CategoryBreakdown cb = new CategoryBreakdown();
                                    cb.setCategory(entry.getKey());
                                    cb.setType("expense");
                                    cb.setTotal(format(entry.getValue(), currency));
                                    return cb;
                                })
                        .collect(Collectors.toList());

        PLReport report = new PLReport();
        report.setStartDate(startDate);
        report.setEndDate(endDate);
        report.setCurrency(currency);
        report.setTotalIncome(format(incomeTotal, currency));
        report.setTotalExpense(format(expenseTotal, currency));
        report.setNet(format(net, currency));
        report.setIncomeBreakdown(incomeBreakdown);
        report.setExpenseBreakdown(expenseBreakdown);
        return ResponseEntity.ok(report);
    }

    private String format(final BigDecimal amount, final String currency) {
        if ("IDR".equals(currency)) {
            return amount.setScale(0, RoundingMode.HALF_UP).toPlainString();
        }
        return amount.setScale(2, RoundingMode.HALF_UP).toPlainString();
    }
}
