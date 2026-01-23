/**
 * Domain Service Template
 * Contains business logic that doesn't fit in entities.
 */

class DomainServiceName {
  constructor(private readonly dependency: DependencyType) {}

  performOperation(params: OperationParams): Result<OperationResult, Error> {
    // Business logic here
    return ok(result);
  }
}
