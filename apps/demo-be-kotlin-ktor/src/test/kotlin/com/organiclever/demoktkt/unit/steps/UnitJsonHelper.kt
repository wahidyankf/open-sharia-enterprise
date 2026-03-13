package com.organiclever.demoktkt.unit.steps

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNotNull
import org.junit.jupiter.api.Assertions.assertTrue

/** Minimal JSON parsing using string-based extraction for unit test step definitions. */
object UnitJsonHelper {
  fun getString(json: String, key: String): String? {
    val pattern = Regex("\"${key}\"\\s*:\\s*\"([^\"]*)\"")
    return pattern.find(json)?.groupValues?.get(1)
  }

  fun getNumber(json: String, key: String): String? {
    val pattern = Regex("\"${key}\"\\s*:\\s*([0-9.]+)")
    return pattern.find(json)?.groupValues?.get(1)
  }

  fun getBool(json: String, key: String): Boolean? {
    val pattern = Regex("\"${key}\"\\s*:\\s*(true|false)")
    return pattern.find(json)?.groupValues?.get(1)?.toBooleanStrictOrNull()
  }

  fun hasKey(json: String, key: String): Boolean {
    return json.contains("\"${key}\"")
  }

  fun isNull(json: String, key: String): Boolean {
    val pattern = Regex("\"${key}\"\\s*:\\s*null")
    return pattern.containsMatchIn(json)
  }

  fun getStringOrNumber(json: String, key: String): String? =
    getString(json, key) ?: getNumber(json, key)

  fun assertStringField(json: String, key: String, expected: String) {
    val actual = getString(json, key) ?: getNumber(json, key)
    assertNotNull(actual, "Expected field '$key' to exist in: $json")
    assertEquals(expected, actual, "Field '$key' mismatch")
  }

  fun assertNonNull(json: String, key: String) {
    assertTrue(hasKey(json, key) && !isNull(json, key), "Expected '$key' to be non-null in: $json")
  }

  fun assertNotPresent(json: String, key: String) {
    assertTrue(!hasKey(json, key), "Expected '$key' to NOT be present in: $json")
  }

  fun getArraySize(json: String, arrayKey: String): Int {
    val idx = json.indexOf("\"${arrayKey}\"")
    if (idx < 0) return 0
    val arrStart = json.indexOf("[", idx)
    if (arrStart < 0) return 0
    val arrEnd = json.lastIndexOf("]")
    if (arrEnd < 0) return 0
    val arrContent = json.substring(arrStart + 1, arrEnd).trim()
    if (arrContent.isEmpty()) return 0
    var depth = 0
    var count = 0
    var inStr = false
    var escape = false
    for (c in arrContent) {
      when {
        escape -> escape = false
        c == '\\' && inStr -> escape = true
        c == '"' -> inStr = !inStr
        !inStr && c == '{' -> {
          if (depth == 0) count++
          depth++
        }
        !inStr && c == '}' -> depth--
      }
    }
    return count
  }
}
