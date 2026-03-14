package com.demobejasb.report.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;

public record PlReportResponse(
        @JsonProperty("totalIncome") String incomeTotal,
        @JsonProperty("totalExpense") String expenseTotal,
        String net,
        @JsonProperty("income_breakdown") Map<String, String> incomeBreakdown,
        @JsonProperty("expense_breakdown") Map<String, String> expenseBreakdown) {}
