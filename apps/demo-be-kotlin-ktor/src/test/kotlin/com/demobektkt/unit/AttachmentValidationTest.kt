package com.demobektkt.unit

import com.demobektkt.domain.MAX_FILE_SIZE_BYTES
import com.demobektkt.domain.validateContentType
import com.demobektkt.domain.validateFileSize
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class AttachmentValidationTest {

  @Test
  fun `image jpeg content type is valid`() {
    assertTrue(validateContentType("image/jpeg").isSuccess)
  }

  @Test
  fun `image png content type is valid`() {
    assertTrue(validateContentType("image/png").isSuccess)
  }

  @Test
  fun `application pdf content type is valid`() {
    assertTrue(validateContentType("application/pdf").isSuccess)
  }

  @Test
  fun `unsupported content type fails`() {
    assertTrue(validateContentType("application/octet-stream").isFailure)
  }

  @Test
  fun `content type with charset is handled`() {
    // Content-Type with charset parameter should be handled
    assertTrue(validateContentType("image/jpeg; charset=utf-8").isSuccess)
  }

  @Test
  fun `file within size limit succeeds`() {
    assertTrue(validateFileSize(1024).isSuccess)
  }

  @Test
  fun `file at exact size limit succeeds`() {
    assertTrue(validateFileSize(MAX_FILE_SIZE_BYTES).isSuccess)
  }

  @Test
  fun `file exceeding size limit fails`() {
    assertTrue(validateFileSize(MAX_FILE_SIZE_BYTES + 1).isFailure)
  }
}
