package com.organiclever.be.expense.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Positive;
import java.math.BigDecimal;
import java.time.LocalDate;
import org.jspecify.annotations.Nullable;

public record ExpenseRequest(
        @NotNull @Positive BigDecimal amount,
        @NotBlank
                @Pattern(regexp = "USD|IDR", message = "Currency must be USD or IDR")
                String currency,
        @NotBlank String category,
        @NotBlank String description,
        @NotNull LocalDate date,
        @NotBlank
                @Pattern(regexp = "expense|income", message = "Type must be expense or income")
                String type,
        @Nullable @Positive BigDecimal quantity,
        @Nullable
                @Pattern(
                        regexp = "liter|ml|kg|g|km|meter|gallon|lb|oz|mile|piece|hour",
                        message = "Unsupported unit")
                String unit) {}
