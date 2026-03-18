package com.demobektkt.routes

import com.demobektkt.contracts.Attachment as ContractAttachment
import com.demobektkt.contracts.AuthTokens
import com.demobektkt.contracts.CategoryBreakdown
import com.demobektkt.contracts.Expense as ContractExpense
import com.demobektkt.contracts.ExpenseListResponse
import com.demobektkt.contracts.PLReport
import com.demobektkt.contracts.PasswordResetResponse
import com.demobektkt.contracts.User as ContractUser
import com.demobektkt.contracts.UserListResponse
import com.demobektkt.domain.Attachment
import com.demobektkt.domain.EntryType
import com.demobektkt.domain.Expense
import com.demobektkt.domain.Page
import com.demobektkt.domain.User
import java.math.BigDecimal
import java.math.RoundingMode
import java.time.LocalDate

/** Convert [java.time.Instant] to [kotlin.time.Instant] for contract types. */
fun java.time.Instant.toContractInstant(): kotlin.time.Instant =
  kotlin.time.Instant.fromEpochMilliseconds(toEpochMilli())

/** Convert [LocalDate] to [kotlinx.datetime.LocalDate] for contract types. */
fun LocalDate.toContractLocalDate(): kotlinx.datetime.LocalDate =
  kotlinx.datetime.LocalDate(year, monthValue, dayOfMonth)

/** Format amount for serialization (IDR: 0 decimals, others: 2). */
private fun formatAmount(currency: String, amount: BigDecimal): String {
  val scale = if (currency.uppercase() == "IDR") 0 else 2
  return amount.setScale(scale, RoundingMode.HALF_UP).toPlainString()
}

/** Map domain [User] to [ContractUser] for HTTP responses. */
fun User.toContractUser(): ContractUser =
  ContractUser(
    id = id.toString(),
    username = username,
    email = email,
    displayName = displayName,
    status =
      when (status) {
        com.demobektkt.domain.UserStatus.ACTIVE -> ContractUser.Status.ACTIVE
        com.demobektkt.domain.UserStatus.INACTIVE -> ContractUser.Status.INACTIVE
        com.demobektkt.domain.UserStatus.DISABLED -> ContractUser.Status.DISABLED
        com.demobektkt.domain.UserStatus.LOCKED -> ContractUser.Status.LOCKED
      },
    roles = listOf(role.name.lowercase()),
    createdAt = createdAt.toContractInstant(),
    updatedAt = updatedAt.toContractInstant(),
  )

/** Map domain [Expense] to [ContractExpense] for HTTP responses. */
fun Expense.toContractExpense(): ContractExpense =
  ContractExpense(
    id = id.toString(),
    userId = userId.toString(),
    type =
      when (type) {
        EntryType.INCOME -> ContractExpense.Type.income
        EntryType.EXPENSE -> ContractExpense.Type.expense
      },
    amount = formatAmount(currency, amount),
    currency = currency,
    category = category,
    description = description,
    date = date.toContractLocalDate(),
    quantity = quantity,
    unit = unit,
    createdAt = createdAt.toContractInstant(),
    updatedAt = updatedAt.toContractInstant(),
  )

/** Attachment response with contract fields + extra url field for BDD spec compatibility. */
@kotlinx.serialization.Serializable
data class AttachmentWithUrl(
  val id: String,
  val filename: String,
  val contentType: String,
  @kotlinx.serialization.SerialName("size") val propertySize: Int,
  @kotlinx.serialization.Contextual val createdAt: kotlin.time.Instant,
  val url: String,
)

fun Attachment.toAttachmentWithUrl(expenseId: java.util.UUID): AttachmentWithUrl =
  AttachmentWithUrl(
    id = id.toString(),
    filename = filename,
    contentType = contentType,
    propertySize = sizeBytes.toInt(),
    createdAt = createdAt.toContractInstant(),
    url = "/api/v1/expenses/$expenseId/attachments/$id/download",
  )

/** Build an [AuthTokens] response from token strings. */
fun buildAuthTokens(accessToken: String, refreshToken: String): AuthTokens =
  AuthTokens(accessToken = accessToken, refreshToken = refreshToken, tokenType = "Bearer")

/** Build a [UserListResponse] from a paginated domain result. */
fun Page<User>.toContractUserListResponse(): UserListResponse {
  val totalPages = if (total == 0L) 1 else ((total + pageSize - 1) / pageSize).toInt()
  return UserListResponse(
    content = data.map { it.toContractUser() },
    totalElements = total.toInt(),
    totalPages = totalPages,
    page = page,
    propertySize = pageSize,
  )
}

/** Build an [ExpenseListResponse] from a paginated domain result. */
fun Page<Expense>.toContractExpenseListResponse(): ExpenseListResponse {
  val totalPages = if (total == 0L) 1 else ((total + pageSize - 1) / pageSize).toInt()
  return ExpenseListResponse(
    content = data.map { it.toContractExpense() },
    totalElements = total.toInt(),
    totalPages = totalPages,
    page = page,
    propertySize = pageSize,
  )
}

/** Build a [PLReport] from computed income/expense data. */
fun buildPLReport(
  currency: String,
  from: LocalDate,
  to: LocalDate,
  incomeEntries: List<Expense>,
  expenseEntries: List<Expense>,
): PLReport {
  val scale = if (currency == "IDR") 0 else 2
  val incomeTotal =
    incomeEntries
      .fold(BigDecimal.ZERO) { acc, e -> acc + e.amount }
      .setScale(scale, RoundingMode.HALF_UP)
  val expenseTotal =
    expenseEntries
      .fold(BigDecimal.ZERO) { acc, e -> acc + e.amount }
      .setScale(scale, RoundingMode.HALF_UP)
  val net = (incomeTotal - expenseTotal).setScale(scale, RoundingMode.HALF_UP)

  val incomeBreakdown =
    incomeEntries
      .groupBy { it.category }
      .map { (cat, list) ->
        CategoryBreakdown(
          category = cat,
          type = "income",
          total =
            list
              .fold(BigDecimal.ZERO) { acc, e -> acc + e.amount }
              .setScale(scale, RoundingMode.HALF_UP)
              .toPlainString(),
        )
      }
  val expenseBreakdown =
    expenseEntries
      .groupBy { it.category }
      .map { (cat, list) ->
        CategoryBreakdown(
          category = cat,
          type = "expense",
          total =
            list
              .fold(BigDecimal.ZERO) { acc, e -> acc + e.amount }
              .setScale(scale, RoundingMode.HALF_UP)
              .toPlainString(),
        )
      }

  return PLReport(
    startDate = from.toContractLocalDate(),
    endDate = to.toContractLocalDate(),
    currency = currency,
    totalIncome = incomeTotal.toPlainString(),
    totalExpense = expenseTotal.toPlainString(),
    net = net.toPlainString(),
    incomeBreakdown = incomeBreakdown,
    expenseBreakdown = expenseBreakdown,
  )
}

/** Build a [PasswordResetResponse] from a generated reset token. */
fun buildPasswordResetResponse(token: String): PasswordResetResponse = PasswordResetResponse(token)
