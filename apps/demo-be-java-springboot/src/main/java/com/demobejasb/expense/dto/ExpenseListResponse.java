package com.demobejasb.expense.dto;

import java.util.List;

public record ExpenseListResponse(List<ExpenseResponse> content, long totalElements, int page) {}
