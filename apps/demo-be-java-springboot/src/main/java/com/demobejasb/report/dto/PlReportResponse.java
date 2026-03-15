package com.demobejasb.report.dto;

import java.util.List;
import java.util.Map;

public record PlReportResponse(
        String totalIncome,
        String totalExpense,
        String net,
        List<Map<String, String>> incomeBreakdown,
        List<Map<String, String>> expenseBreakdown) {}
