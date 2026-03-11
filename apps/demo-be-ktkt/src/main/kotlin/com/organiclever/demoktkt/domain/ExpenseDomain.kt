package com.organiclever.demoktkt.domain

import java.math.BigDecimal

private val SUPPORTED_CURRENCIES = setOf("USD", "IDR")
private val CURRENCY_CODE_REGEX = Regex("^[A-Z]{3}$")

private val SUPPORTED_UNITS =
  setOf(
    "liter",
    "ml",
    "kg",
    "g",
    "km",
    "meter",
    "gallon",
    "lb",
    "oz",
    "mile",
    "piece",
    "hour",
    "foot",
    "pound",
  )

fun validateCurrency(currency: String): Result<String> {
  if (!CURRENCY_CODE_REGEX.matches(currency)) {
    return Result.failure(
      DomainException(
        DomainError.ValidationError("currency", "Currency code must be a 3-letter ISO code")
      )
    )
  }
  if (currency !in SUPPORTED_CURRENCIES) {
    return Result.failure(
      DomainException(DomainError.ValidationError("currency", "Unsupported currency: $currency"))
    )
  }
  return Result.success(currency)
}

fun validateAmount(currency: String, amount: BigDecimal): Result<BigDecimal> {
  if (amount < BigDecimal.ZERO) {
    return Result.failure(
      DomainException(DomainError.ValidationError("amount", "Amount must not be negative"))
    )
  }
  return when (currency.uppercase()) {
    "USD" -> Result.success(amount.setScale(2, java.math.RoundingMode.HALF_UP))
    "IDR" -> {
      if (amount.stripTrailingZeros().scale() > 0) {
        Result.failure(
          DomainException(
            DomainError.ValidationError("amount", "IDR requires whole number amounts")
          )
        )
      } else {
        Result.success(amount.setScale(0, java.math.RoundingMode.HALF_UP))
      }
    }
    else ->
      Result.failure(
        DomainException(DomainError.ValidationError("currency", "Unsupported currency: $currency"))
      )
  }
}

fun validateUnit(unit: String?): Result<String?> {
  if (unit == null) return Result.success(null)
  if (unit !in SUPPORTED_UNITS) {
    return Result.failure(
      DomainException(DomainError.ValidationError("unit", "Unsupported unit: $unit"))
    )
  }
  return Result.success(unit)
}
