# OSE Platform Web — `web` perspective Gherkin

UI-semantic Gherkin scenarios for `ose-web`. Step style: `clicks`, `types`, `sees`,
`navigates`. Background: `Given the app is running`. Consumed by `apps/ose-web-fe-e2e`
(Playwright).

Organized per bounded context (one folder per BC, matching
[`../../../ddd/bounded-contexts.yaml`](../../../ddd/bounded-contexts.yaml)).

## Coverage

| Bounded Context | Folder       | Features                                             | Scenarios |
| --------------- | ------------ | ---------------------------------------------------- | --------- |
| app-shell       | `app-shell/` | `accessibility`, `navigation`, `responsive`, `theme` | 4 files   |
| landing         | `landing/`   | `landing-page`                                       | 1 file    |

## File naming

`[domain-capability].feature` (kebab-case). Stay UI-semantic — no HTTP, no tRPC procedure
names.

## Adding a feature file

1. Identify the bounded context (e.g. `app-shell`, `content`).
2. Create the folder if it does not exist: `behavior/web/gherkin/<bc>/`.
3. Create the `.feature` file: `<domain-capability>.feature`.
4. Open with `Feature:` then a user story block (`As a … / I want … / So that …`).
5. Use `Given the app is running` as the first Background step.
6. Use only UI-semantic steps — no `sends GET/POST`, no procedure names.

## Related

- **Parent**: [`../../README.md`](../../README.md)
- **api perspective counterpart**: [`../../api/gherkin/`](../../api/gherkin/README.md)
- **DDD registry**: [`../../../ddd/bounded-contexts.yaml`](../../../ddd/bounded-contexts.yaml)
