/**
 * Value Object Template
 *
 * Value objects are immutable and defined by their attributes.
 * Use for domain concepts without identity.
 */

interface ValueObjectProps {
  readonly propertyName: string | number;
  // Add more properties
}

class ValueObjectName {
  private constructor(private readonly props: ValueObjectProps) {
    Object.freeze(this);
  }

  static create(props: ValueObjectProps): Result<ValueObjectName, Error> {
    // Validation
    if (!props.propertyName) {
      return err(new Error("Property is required"));
    }

    return ok(new ValueObjectName(props));
  }

  get propertyName() {
    return this.props.propertyName;
  }

  equals(other: ValueObjectName): boolean {
    return this.props.propertyName === other.props.propertyName;
  }
}
