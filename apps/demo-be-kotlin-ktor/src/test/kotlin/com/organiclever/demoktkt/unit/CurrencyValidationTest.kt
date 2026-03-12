package com.organiclever.demoktkt.unit

import com.organiclever.demoktkt.domain.validateAmount
import com.organiclever.demoktkt.domain.validateCurrency
import com.organiclever.demoktkt.domain.validateUnit
import java.math.BigDecimal
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class CurrencyValidationTest {

  @Test
  fun `USD amount preserves 2 decimal places`() {
    val result = validateAmount("USD", BigDecimal("10.5"))
    assertTrue(result.isSuccess)
    assertEquals(2, result.getOrThrow().scale())
  }

  @Test
  fun `IDR amount requires whole number`() {
    val result = validateAmount("IDR", BigDecimal("150000"))
    assertTrue(result.isSuccess)
    assertEquals(0, result.getOrThrow().scale())
  }

  @Test
  fun `IDR with decimals fails`() {
    val result = validateAmount("IDR", BigDecimal("150000.50"))
    assertTrue(result.isFailure)
  }

  @Test
  fun `negative amount fails`() {
    val result = validateAmount("USD", BigDecimal("-10.00"))
    assertTrue(result.isFailure)
  }

  @Test
  fun `unsupported currency fails`() {
    val result = validateCurrency("EUR")
    assertTrue(result.isFailure)
  }

  @Test
  fun `malformed currency code fails`() {
    val result = validateCurrency("US")
    assertTrue(result.isFailure)
  }

  @Test
  fun `valid USD currency succeeds`() {
    val result = validateCurrency("USD")
    assertTrue(result.isSuccess)
  }

  @Test
  fun `valid IDR currency succeeds`() {
    val result = validateCurrency("IDR")
    assertTrue(result.isSuccess)
  }

  @Test
  fun `null unit is valid`() {
    val result = validateUnit(null)
    assertTrue(result.isSuccess)
  }

  @Test
  fun `valid unit liter succeeds`() {
    val result = validateUnit("liter")
    assertTrue(result.isSuccess)
  }

  @Test
  fun `unsupported unit fathom fails`() {
    val result = validateUnit("fathom")
    assertTrue(result.isFailure)
  }

  @Test
  fun `validateAmount with unsupported currency fails`() {
    // This path is reached when validateAmount is called directly with a bypassed currency
    val result = validateAmount("EUR", BigDecimal("10.00"))
    assertTrue(result.isFailure)
  }
}
