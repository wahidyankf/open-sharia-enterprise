package com.demobejasb.expense.controller;

import com.demobejasb.auth.model.User;
import com.demobejasb.auth.repository.UserRepository;
import com.demobejasb.expense.dto.ExpenseListResponse;
import com.demobejasb.expense.dto.ExpenseRequest;
import com.demobejasb.expense.dto.ExpenseResponse;
import com.demobejasb.expense.model.Expense;
import com.demobejasb.expense.repository.ExpenseRepository;
import jakarta.validation.Valid;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Instant;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/expenses")
public class ExpenseController {

    private final ExpenseRepository expenseRepository;
    private final UserRepository userRepository;

    public ExpenseController(
            final ExpenseRepository expenseRepository, final UserRepository userRepository) {
        this.expenseRepository = expenseRepository;
        this.userRepository = userRepository;
    }

    @PostMapping
    public ResponseEntity<ExpenseResponse> create(
            @AuthenticationPrincipal final UserDetails userDetails,
            @Valid @RequestBody final ExpenseRequest request) {
        User user = getUser(userDetails);
        Expense expense =
                new Expense(
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
        return ResponseEntity.status(HttpStatus.CREATED).body(ExpenseResponse.from(saved));
    }

    @GetMapping("/{id}")
    public ResponseEntity<ExpenseResponse> getById(
            @AuthenticationPrincipal final UserDetails userDetails,
            @PathVariable final UUID id) {
        User user = getUser(userDetails);
        Expense expense =
                expenseRepository
                        .findByIdAndUser(id, user)
                        .orElseThrow(() -> new RuntimeException("Expense not found"));
        return ResponseEntity.ok(ExpenseResponse.from(expense));
    }

    @GetMapping
    public ResponseEntity<ExpenseListResponse> list(
            @AuthenticationPrincipal final UserDetails userDetails,
            @RequestParam(defaultValue = "0") final int page,
            @RequestParam(defaultValue = "20") final int size) {
        User user = getUser(userDetails);
        Page<Expense> expenses =
                expenseRepository.findAllByUser(
                        user, PageRequest.of(page, size, Sort.by("createdAt").descending()));
        List<ExpenseResponse> data = expenses.getContent().stream().map(ExpenseResponse::from).toList();
        return ResponseEntity.ok(new ExpenseListResponse(data, expenses.getTotalElements(), page));
    }

    @GetMapping("/summary")
    public ResponseEntity<Map<String, String>> summary(
            @AuthenticationPrincipal final UserDetails userDetails) {
        User user = getUser(userDetails);
        List<Expense> allExpenses =
                expenseRepository
                        .findAllByUser(user, PageRequest.of(0, Integer.MAX_VALUE, Sort.unsorted()))
                        .getContent();
        Map<String, BigDecimal> totals = new HashMap<>();
        for (Expense e : allExpenses) {
            if ("expense".equals(e.getType())) {
                totals.merge(e.getCurrency(), e.getAmount(), BigDecimal::add);
            }
        }
        Map<String, String> result = new LinkedHashMap<>();
        for (Map.Entry<String, BigDecimal> entry : totals.entrySet()) {
            result.put(entry.getKey(), formatAmount(entry.getValue(), entry.getKey()));
        }
        return ResponseEntity.ok(result);
    }

    @PutMapping("/{id}")
    public ResponseEntity<ExpenseResponse> update(
            @AuthenticationPrincipal final UserDetails userDetails,
            @PathVariable final UUID id,
            @Valid @RequestBody final ExpenseRequest request) {
        User user = getUser(userDetails);
        Expense expense =
                expenseRepository
                        .findByIdAndUser(id, user)
                        .orElseThrow(() -> new RuntimeException("Expense not found"));
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
        return ResponseEntity.ok(ExpenseResponse.from(saved));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(
            @AuthenticationPrincipal final UserDetails userDetails,
            @PathVariable final UUID id) {
        User user = getUser(userDetails);
        Expense expense =
                expenseRepository
                        .findByIdAndUser(id, user)
                        .orElseThrow(() -> new RuntimeException("Expense not found"));
        expenseRepository.delete(expense);
        return ResponseEntity.noContent().build();
    }

    private User getUser(final UserDetails userDetails) {
        return userRepository
                .findByUsername(userDetails.getUsername())
                .orElseThrow(() -> new RuntimeException("User not found"));
    }

    private String formatAmount(final BigDecimal amount, final String currency) {
        if ("IDR".equals(currency)) {
            return amount.setScale(0, RoundingMode.HALF_UP).toPlainString();
        }
        return amount.setScale(2, RoundingMode.HALF_UP).toPlainString();
    }
}
