import 'dart:async';
import 'dart:js_interop';
import 'dart:typed_data';

import 'package:dio/dio.dart';
import 'package:web/web.dart' hide FormData;

import '../models/attachment.dart';
import 'api_client.dart';

Future<List<Attachment>> listAttachments(String expenseId) async {
  final response = await apiClient.get<dynamic>(
    '/api/v1/expenses/$expenseId/attachments',
  );
  final data = response.data;
  // Backend returns { attachments: [...] } wrapper
  final List<dynamic> items;
  if (data is Map<String, dynamic> && data.containsKey('attachments')) {
    items = data['attachments'] as List<dynamic>;
  } else if (data is List<dynamic>) {
    items = data;
  } else {
    items = [];
  }
  return items
      .map((dynamic e) => Attachment.fromJson(e as Map<String, dynamic>))
      .toList();
}

Future<Attachment> uploadAttachment(String expenseId, File file) async {
  final completer = Completer<void>();
  final reader = FileReader();

  reader.addEventListener(
    'loadend',
    ((Event e) => completer.complete()).toJS,
  );

  reader.readAsArrayBuffer(file);
  await completer.future;

  final jsBuffer = reader.result as JSArrayBuffer;
  final bytes = Uint8List.view(jsBuffer.toDart);

  final formData = FormData.fromMap({
    'file': MultipartFile.fromBytes(
      bytes,
      filename: file.name,
    ),
  });

  final response = await apiClient.post<Map<String, dynamic>>(
    '/api/v1/expenses/$expenseId/attachments',
    data: formData,
  );
  return Attachment.fromJson(response.data!);
}

Future<void> deleteAttachment(String expenseId, String attachmentId) async {
  await apiClient.delete<void>(
    '/api/v1/expenses/$expenseId/attachments/$attachmentId',
  );
}
