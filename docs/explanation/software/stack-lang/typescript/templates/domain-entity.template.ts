/**
 * Domain Entity Template
 *
 * Entities have unique identity and lifecycle.
 * Use this template for aggregates that change over time.
 */

type EntityId = string & { __brand: "EntityId" };

interface EntityProps {
  readonly id: EntityId;
  readonly createdAt: Date;
  readonly updatedAt: Date;
  // Add domain-specific properties
}

class EntityName {
  private constructor(private props: EntityProps) {}

  static create(params: Omit<EntityProps, "createdAt" | "updatedAt">): Result<EntityName, Error> {
    // Validation
    if (!params.id) {
      return err(new Error("ID is required"));
    }

    return ok(
      new EntityName({
        ...params,
        createdAt: new Date(),
        updatedAt: new Date(),
      }),
    );
  }

  get id(): EntityId {
    return this.props.id;
  }

  // Add domain methods here

  equals(other: EntityName): boolean {
    return this.id === other.id;
  }
}

// Helper types
type Result<T, E = Error> = { readonly ok: true; readonly value: T } | { readonly ok: false; readonly error: E };

function ok<T>(value: T): Result<T, never> {
  return { ok: true, value };
}

function err<E>(error: E): Result<never, E> {
  return { ok: false, error };
}
