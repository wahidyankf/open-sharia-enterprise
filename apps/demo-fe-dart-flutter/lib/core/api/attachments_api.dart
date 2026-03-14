/// Expense attachment API functions.
///
/// Wraps the `/api/v1/expenses/{expenseId}/attachments/*` endpoints.
library;

import 'package:dio/dio.dart';
import 'package:demo_fe_dart_flutter/core/api/api_client.dart';
import 'package:demo_fe_dart_flutter/core/models/models.dart';

/// Returns all attachments linked to [expenseId].
Future<List<Attachment>> listAttachments(String expenseId) async {
  final response = await dio.get<List<dynamic>>(
    '/api/v1/expenses/$expenseId/attachments',
  );
  return (response.data!)
      .map((e) => Attachment.fromJson(e as Map<String, dynamic>))
      .toList();
}

/// Uploads [fileBytes] as a new attachment on [expenseId].
///
/// [filename] is the original file name (used as the attachment label).
/// [contentType] is the MIME type, e.g. `'image/jpeg'` or `'application/pdf'`.
///
/// Returns the created [Attachment] record.
Future<Attachment> uploadAttachment(
  String expenseId, {
  required List<int> fileBytes,
  required String filename,
  required String contentType,
}) async {
  final formData = FormData.fromMap({
    'file': MultipartFile.fromBytes(
      fileBytes,
      filename: filename,
      contentType: DioMediaType.parse(contentType),
    ),
  });

  final response = await dio.post<Map<String, dynamic>>(
    '/api/v1/expenses/$expenseId/attachments',
    data: formData,
    options: Options(
      // Let Dio set the multipart boundary; remove the default JSON header.
      contentType: 'multipart/form-data',
    ),
  );
  return Attachment.fromJson(response.data!);
}

/// Deletes the attachment identified by [attachmentId] from [expenseId].
Future<void> deleteAttachment(String expenseId, String attachmentId) async {
  await dio.delete<void>(
    '/api/v1/expenses/$expenseId/attachments/$attachmentId',
  );
}
