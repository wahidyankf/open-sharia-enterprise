import 'package:flutter_test/flutter_test.dart';
import 'package:organiclever_app/models/hello_response.dart';

void main() {
  group('HelloResponse', () {
    test('fromJson creates valid object', () {
      final json = {'message': 'world!'};
      final response = HelloResponse.fromJson(json);
      expect(response.message, equals('world!'));
    });

    test('toJson creates valid map', () {
      final response = HelloResponse(message: 'test');
      final json = response.toJson();
      expect(json['message'], equals('test'));
    });
  });
}
