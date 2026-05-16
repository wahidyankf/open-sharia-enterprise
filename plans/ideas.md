# Ideas

Quick ideas and todos that haven't been formalized into plans yet.

When an idea is ready for implementation, create a proper plan folder in `backlog/` and remove it from this list.

## Ideas List

### Infrastructure

- Create IAM (Identity and Access Management) service/module for authentication and authorization

### Demo Apps

- Recheck all standards.

### Development Experience

- Standardize CIs
- .env backup scripts for rhino-cli
- simplify ayokoding-cli and ose-cli
- libraries update
- Split mermaid diagrams in `plans/done/2026-04-26__organiclever-ci-staging-split/tech-docs.md` to satisfy validator rules (surfaced 2026-04-26 by `rhino-cli-mermaid-fixes`): 7 label_too_long + 2 width_exceeded violations across blocks 0 (line 7) and 1 (line 40), plus 2 subgraph_density warnings on 7-child WF subgraphs. Follow-up to `2026-04-26__rhino-cli-mermaid-fixes`.

### Stack Update Deferrals (added 2026-05-16 by stack-update plan)

- Future plan: migrate `aws-sdk-go` v1 → v2 (currently transitive via `narqo/go-badge`; v1 EOL 2025-07-31; S3-crypto CVEs CVE-2020-8911/8912 only affect `s3crypto` codepaths which our CLIs do not use).
- Future plan: TypeScript 6.0 migration once TS 6.x has 60+ days of soak (eligible after ~2026-05-23).
- Future plan: ESLint 10 + react-hooks 7 migration once those versions have 60+ days of soak.
- Future plan: Zod 4.x migration (post-cutoff; eligible after 60-day soak).
- Future plan: lucide-react 1.x migration (post-cutoff; eligible after 60-day soak).
- Future plan: @xstate/react 6.x migration (post-cutoff; eligible after 60-day soak).
- Future plan: TailwindCSS 4.3.x migration (post-cutoff; eligible after 60-day soak).
- Future plan: @effect/platform 0.96.x + effect 4.x migration (post-cutoff; eligible after 60-day soak).
- Future plan: Storybook 10.3/10.4 adoption (post-cutoff; downgrade in this plan to 10.2.10 for CVE clearance).
- Future plan: Volta → mise migration (volta last release Dec 2024).
- Future plan: Microsoft Defender / dotnet 10.0.300 brew bottle availability (currently install via dotnet-install.sh to ~/.dotnet).
- Future plan: bump `vite` to 7.4+ across all consumers, then adopt `@vitejs/plugin-react 6.0.1` (this plan reverted plugin-react 6.0.1 → `^5.1.4` because plugin-react 6 requires vite's `./internal` subpath which is unavailable on the installed transitive vite 7.3.1). Caret retained pending the vite bump.
