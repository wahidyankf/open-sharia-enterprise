/**
 * Aggregate Root Template
 * Aggregate root controls access to entities within aggregate boundary.
 */

class AggregateRootName {
  private constructor(private props: AggregateProps) {}

  static create(params: CreateParams): Result<AggregateRootName, Error> {
    // Validation and creation logic
    return ok(new AggregateRootName({ ...params }));
  }

  // Domain methods that maintain aggregate consistency

  get id() {
    return this.props.id;
  }
}
