import 'package:json_annotation/json_annotation.dart';

part 'hello_response.g.dart';

@JsonSerializable()
class HelloResponse {
  final String message;

  HelloResponse({required this.message});

  factory HelloResponse.fromJson(Map<String, dynamic> json) =>
      _$HelloResponseFromJson(json);

  Map<String, dynamic> toJson() => _$HelloResponseToJson(this);
}
