package com.demobejavx.unit;

import com.demobejavx.domain.validation.ExpenseValidator;
import com.demobejavx.domain.validation.ValidationException;
import java.math.BigDecimal;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ExpenseValidatorTest {

    @Test
    void validateCurrency_usd_doesNotThrow() {
        assertDoesNotThrow(() -> ExpenseValidator.validateCurrency("USD"));
    }

    @Test
    void validateCurrency_idr_doesNotThrow() {
        assertDoesNotThrow(() -> ExpenseValidator.validateCurrency("IDR"));
    }

    @Test
    void validateCurrency_unsupported_throwsValidationException() {
        ValidationException ex = assertThrows(ValidationException.class,
                () -> ExpenseValidator.validateCurrency("EUR"));
        assertEquals("currency", ex.getField());
    }

    @Test
    void validateCurrency_malformed_throwsValidationException() {
        ValidationException ex = assertThrows(ValidationException.class,
                () -> ExpenseValidator.validateCurrency("US"));
        assertEquals("currency", ex.getField());
    }

    @Test
    void validateCurrency_null_throwsValidationException() {
        ValidationException ex = assertThrows(ValidationException.class,
                () -> ExpenseValidator.validateCurrency(null));
        assertEquals("currency", ex.getField());
    }

    @Test
    void validateAndNormalizeAmount_usdValidAmount_returnsWith2dp() {
        BigDecimal result = ExpenseValidator.validateAndNormalizeAmount("USD",
                new BigDecimal("10.50"));
        assertEquals(new BigDecimal("10.50"), result);
    }

    @Test
    void validateAndNormalizeAmount_idrValidAmount_returnsWithZeroDp() {
        BigDecimal result = ExpenseValidator.validateAndNormalizeAmount("IDR",
                new BigDecimal("150000"));
        assertEquals(new BigDecimal("150000"), result);
    }

    @Test
    void validateAndNormalizeAmount_negativeAmount_throwsValidationException() {
        ValidationException ex = assertThrows(ValidationException.class,
                () -> ExpenseValidator.validateAndNormalizeAmount("USD",
                        new BigDecimal("-10.00")));
        assertEquals("amount", ex.getField());
    }

    @Test
    void validateUnit_liter_doesNotThrow() {
        assertDoesNotThrow(() -> ExpenseValidator.validateUnit("liter"));
    }

    @Test
    void validateUnit_gallon_doesNotThrow() {
        assertDoesNotThrow(() -> ExpenseValidator.validateUnit("gallon"));
    }

    @Test
    void validateUnit_unsupported_throwsValidationException() {
        ValidationException ex = assertThrows(ValidationException.class,
                () -> ExpenseValidator.validateUnit("fathom"));
        assertEquals("unit", ex.getField());
    }

    @Test
    void isSupportedAttachmentType_jpeg_returnsTrue() {
        assertTrue(ExpenseValidator.isSupportedAttachmentType("image/jpeg"));
    }

    @Test
    void isSupportedAttachmentType_pdf_returnsTrue() {
        assertTrue(ExpenseValidator.isSupportedAttachmentType("application/pdf"));
    }

    @Test
    void isSupportedAttachmentType_unsupported_returnsFalse() {
        org.junit.jupiter.api.Assertions.assertFalse(
                ExpenseValidator.isSupportedAttachmentType("application/octet-stream"));
    }
}
