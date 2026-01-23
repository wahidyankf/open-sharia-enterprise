/**
 * Error Hierarchy Template
 * Domain-specific error types.
 */

export abstract class DomainError extends Error {
  constructor(message: string) {
    super(message);
    this.name = this.constructor.name;
    Error.captureStackTrace(this, this.constructor);
  }
}

export class ValidationError extends DomainError {
  constructor(
    public readonly field: string,
    message: string,
  ) {
    super(`Validation failed for ${field}: ${message}`);
  }
}

export class BusinessRuleError extends DomainError {
  constructor(message: string) {
    super(message);
  }
}

export class NotFoundError extends DomainError {
  constructor(entityName: string, id: string) {
    super(`${entityName} with id ${id} not found`);
  }
}

export class InfrastructureError extends DomainError {
  constructor(
    message: string,
    public readonly cause?: Error,
  ) {
    super(message);
  }
}
