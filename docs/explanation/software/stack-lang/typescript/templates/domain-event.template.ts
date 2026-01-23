/**
 * Domain Event Template
 * Events represent significant occurrences in the domain.
 */

type EventId = string & { __brand: "EventId" };

interface DomainEventBase {
  readonly eventId: EventId;
  readonly occurredAt: Date;
  readonly eventType: string;
}

interface SpecificEvent extends DomainEventBase {
  readonly eventType: "EventName";
  readonly aggregateId: string;
  // Add event-specific data
}

class EventFactory {
  static createEvent(params: EventParams): SpecificEvent {
    return {
      eventId: generateId() as EventId,
      occurredAt: new Date(),
      eventType: "EventName",
      ...params,
    };
  }
}
