package com.organiclever.be.expense.dto;

import java.util.List;

public record ExpenseListResponse(List<ExpenseResponse> data, long total, int page) {}
