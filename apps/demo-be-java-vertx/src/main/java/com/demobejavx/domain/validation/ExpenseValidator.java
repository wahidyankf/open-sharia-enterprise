package com.demobejavx.domain.validation;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Set;

public final class ExpenseValidator {

    private static final Set<String> SUPPORTED_CURRENCIES = Set.of("USD", "IDR");
    private static final Set<String> SUPPORTED_UNITS = Set.of(
            "liter", "ml", "kg", "g", "km", "meter",
            "gallon", "lb", "oz", "mile",
            "piece", "hour"
    );

    private ExpenseValidator() {
    }

    public static BigDecimal validateAndNormalizeAmount(String currency, BigDecimal amount) {
        if (amount.compareTo(BigDecimal.ZERO) < 0) {
            throw new ValidationException("amount", "Amount must not be negative");
        }
        return switch (currency.toUpperCase()) {
            case "USD" -> {
                if (amount.scale() > 2) {
                    throw new ValidationException("amount",
                            "USD requires at most 2 decimal places");
                }
                yield amount.setScale(2, RoundingMode.UNNECESSARY);
            }
            case "IDR" -> {
                if (amount.scale() > 0) {
                    throw new ValidationException("amount",
                            "IDR requires 0 decimal places");
                }
                yield amount.setScale(0, RoundingMode.UNNECESSARY);
            }
            default -> throw new ValidationException("currency",
                    "Unsupported currency: " + currency);
        };
    }

    public static void validateCurrency(String currency) {
        if (currency == null || currency.length() != 3) {
            throw new ValidationException("currency", "Currency must be a 3-letter code");
        }
        if (!SUPPORTED_CURRENCIES.contains(currency.toUpperCase())) {
            throw new ValidationException("currency", "Unsupported currency: " + currency);
        }
    }

    public static void validateUnit(String unit) {
        if (!SUPPORTED_UNITS.contains(unit)) {
            throw new ValidationException("unit", "Unsupported unit: " + unit);
        }
    }

    public static boolean isSupportedAttachmentType(String contentType) {
        return "image/jpeg".equals(contentType)
                || "image/png".equals(contentType)
                || "application/pdf".equals(contentType);
    }
}
