package com.organiclever.demoktkt.domain

private val ALLOWED_CONTENT_TYPES = setOf("image/jpeg", "image/png", "application/pdf")
const val MAX_FILE_SIZE_BYTES = 10L * 1024 * 1024 // 10 MB

fun validateContentType(contentType: String): Result<String> {
  val baseType = contentType.split(";").first().trim()
  if (baseType !in ALLOWED_CONTENT_TYPES) {
    return Result.failure(DomainException(DomainError.UnsupportedMediaType(baseType)))
  }
  return Result.success(baseType)
}

fun validateFileSize(sizeBytes: Long, limitBytes: Long = MAX_FILE_SIZE_BYTES): Result<Long> {
  if (sizeBytes > limitBytes) {
    return Result.failure(DomainException(DomainError.FileTooLarge(limitBytes)))
  }
  return Result.success(sizeBytes)
}
