# Demo Frontend C4 Diagrams

C4 architecture diagrams for the demo frontend application.

## Diagrams

| Level     | File           | What It Shows                                                                       |
| --------- | -------------- | ----------------------------------------------------------------------------------- |
| Context   | `context.md`   | The frontend system, the backend it consumes, and external actors                   |
| Container | `container.md` | Runtime containers: SPA, API Gateway/Backend, Database                              |
| Component | `component.md` | Internal structure of the SPA: pages, state management, API client, routing, UI kit |

## C4 Level Summary

- **Context** — answers: who uses the frontend and what systems does it interact with?
- **Container** — answers: what processes run and how does the frontend reach the backend?
- **Component** — answers: what are the logical building blocks inside the SPA?

## Related

- **Parent**: [demo-fe specs](../README.md)
- **Gherkin specs**: [gherkin/](../gherkin/README.md)
- **Backend C4**: [demo-be C4](../../demo-be/c4/README.md)
