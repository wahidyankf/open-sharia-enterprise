/**
 * Data Transfer Object Template
 * Used for API requests and responses.
 */

// Request DTO
export interface CreateEntityRequest {
  readonly propertyName: string;
  readonly amount: number;
}

// Response DTO
export interface EntityResponse {
  readonly id: string;
  readonly propertyName: string;
  readonly amount: number;
  readonly createdAt: string;
}

// Mapper
export class EntityMapper {
  static toResponse(entity: EntityName): EntityResponse {
    return {
      id: entity.id,
      propertyName: entity.propertyName,
      amount: entity.amount,
      createdAt: entity.createdAt.toISOString(),
    };
  }
}
