package com.organiclever.demoktkt.unit.steps

import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse

/** Simple Java HTTP client wrapper for unit-level Cucumber step definitions. */
object UnitHttpHelper {
  private val client: HttpClient =
    HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NEVER).build()

  fun get(path: String, authToken: String? = null): Pair<Int, String> {
    val request =
      HttpRequest.newBuilder()
        .uri(URI.create("${UnitTestWorld.baseUrl()}$path"))
        .apply { authToken?.let { header("Authorization", "Bearer $it") } }
        .header("Accept", "application/json")
        .GET()
        .build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    return Pair(response.statusCode(), response.body())
  }

  fun post(path: String, body: String, authToken: String? = null): Pair<Int, String> {
    val request =
      HttpRequest.newBuilder()
        .uri(URI.create("${UnitTestWorld.baseUrl()}$path"))
        .apply { authToken?.let { header("Authorization", "Bearer $it") } }
        .header("Content-Type", "application/json")
        .header("Accept", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(body))
        .build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    return Pair(response.statusCode(), response.body())
  }

  fun patch(path: String, body: String, authToken: String? = null): Pair<Int, String> {
    val request =
      HttpRequest.newBuilder()
        .uri(URI.create("${UnitTestWorld.baseUrl()}$path"))
        .apply { authToken?.let { header("Authorization", "Bearer $it") } }
        .header("Content-Type", "application/json")
        .header("Accept", "application/json")
        .method("PATCH", HttpRequest.BodyPublishers.ofString(body))
        .build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    return Pair(response.statusCode(), response.body())
  }

  fun put(path: String, body: String, authToken: String? = null): Pair<Int, String> {
    val request =
      HttpRequest.newBuilder()
        .uri(URI.create("${UnitTestWorld.baseUrl()}$path"))
        .apply { authToken?.let { header("Authorization", "Bearer $it") } }
        .header("Content-Type", "application/json")
        .header("Accept", "application/json")
        .PUT(HttpRequest.BodyPublishers.ofString(body))
        .build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    return Pair(response.statusCode(), response.body())
  }

  fun delete(path: String, authToken: String? = null): Pair<Int, String> {
    val request =
      HttpRequest.newBuilder()
        .uri(URI.create("${UnitTestWorld.baseUrl()}$path"))
        .apply { authToken?.let { header("Authorization", "Bearer $it") } }
        .header("Accept", "application/json")
        .DELETE()
        .build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    return Pair(response.statusCode(), response.body())
  }

  fun postMultipart(
    path: String,
    filename: String,
    contentType: String,
    fileContent: ByteArray,
    authToken: String? = null,
  ): Pair<Int, String> {
    val boundary = "----TestBoundary${System.currentTimeMillis()}"
    val bodyParts = buildMultipartBody(boundary, filename, contentType, fileContent)
    val request =
      HttpRequest.newBuilder()
        .uri(URI.create("${UnitTestWorld.baseUrl()}$path"))
        .apply { authToken?.let { header("Authorization", "Bearer $it") } }
        .header("Content-Type", "multipart/form-data; boundary=$boundary")
        .header("Accept", "application/json")
        .POST(HttpRequest.BodyPublishers.ofByteArray(bodyParts))
        .build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    return Pair(response.statusCode(), response.body())
  }

  private fun buildMultipartBody(
    boundary: String,
    filename: String,
    contentType: String,
    fileContent: ByteArray,
  ): ByteArray {
    val sb = StringBuilder()
    sb.append("--$boundary\r\n")
    sb.append("Content-Disposition: form-data; name=\"file\"; filename=\"$filename\"\r\n")
    sb.append("Content-Type: $contentType\r\n")
    sb.append("\r\n")
    val header = sb.toString().toByteArray()
    val footer = "\r\n--$boundary--\r\n".toByteArray()
    return header + fileContent + footer
  }
}
