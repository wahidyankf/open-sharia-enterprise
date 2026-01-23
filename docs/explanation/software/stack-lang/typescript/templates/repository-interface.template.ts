/**
 * Repository Interface Template
 * Abstracts persistence for aggregates.
 */

interface Repository<T, ID> {
  findById(id: ID): Promise<Result<T | null, Error>>;
  findAll(): Promise<Result<readonly T[], Error>>;
  save(entity: T): Promise<Result<void, Error>>;
  delete(id: ID): Promise<Result<void, Error>>;
}

interface EntityRepository extends Repository<EntityName, EntityId> {
  // Add domain-specific query methods
  findByProperty(value: string): Promise<Result<readonly EntityName[], Error>>;
}
