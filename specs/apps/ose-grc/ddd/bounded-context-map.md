# Bounded Context Map — OSE GRC

Visual map of the four bounded contexts in OSE GRC and their strategic relationships.

## Context Map

```mermaid
%% Color Palette: Blue #0173B2 (supplier), Orange #DE8F05 (customer), Teal #029E73 (shared)
%% All colors meet WCAG AA contrast standards and are color-blind friendly.

graph TD
    RS["regulatory-source<br/>Supplier"]:::supplier
    IP["internal-policy<br/>Supplier"]:::supplier
    GA["gap-analysis<br/>Customer + Orchestrator"]:::customer
    AO["ai-orchestration<br/>Supplier"]:::supplier

    RS -->|customer-supplier| GA
    IP -->|customer-supplier| GA
    GA -->|customer-supplier| AO

    classDef supplier fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef customer fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

## Relationship Table

| Upstream (Supplier) | Downstream (Customer) | Pattern           | Notes                                   |
| ------------------- | --------------------- | ----------------- | --------------------------------------- |
| `regulatory-source` | `gap-analysis`        | customer-supplier | GA reads regulatory corpus              |
| `internal-policy`   | `gap-analysis`        | customer-supplier | GA reads internal policy corpus         |
| `ai-orchestration`  | `gap-analysis`        | customer-supplier | GA calls LLM via AI orchestration layer |

## Context Summaries

| Context             | One-line responsibility                                                          |
| ------------------- | -------------------------------------------------------------------------------- |
| `regulatory-source` | Ingests and stores regulator-published rule documents with provenance metadata   |
| `internal-policy`   | Ingests and stores company-internal documents (SOPs, manuals, procedures)        |
| `gap-analysis`      | Compares regulatory corpus against policy corpus; emits structured `GapItem`s    |
| `ai-orchestration`  | Wraps LLM calls (OpenRouter), prompt management, retry/backoff, token accounting |
