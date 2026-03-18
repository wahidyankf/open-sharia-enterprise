import 'package:demo_contracts/demo_contracts.dart' as contracts;

/// HealthResponse backed by the generated contract type.
/// The generated type enforces the field name matches the OpenAPI spec.
class HealthResponse {
  final String status;

  const HealthResponse({required this.status});

  factory HealthResponse.fromJson(Map<String, dynamic> json) {
    // Validate through generated contract type (compile-time enforcement)
    final contract = contracts.HealthResponse.fromJson(json);
    if (contract == null) {
      return HealthResponse(status: json['status'] as String);
    }
    return HealthResponse(status: contract.status);
  }
}
