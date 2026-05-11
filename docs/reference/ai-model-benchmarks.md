---
title: "AI Model Benchmarks Reference"
description: Cited benchmark scores for all AI models used in this project — primary source backing for tier assignments in model-selection.md
category: reference
tags:
  - ai-models
  - benchmarks
  - model-selection
created: 2026-04-19
---

# AI Model Benchmarks Reference

Canonical benchmark reference for all AI models used in this project. Last updated: 2026-05-07.

## Purpose and Scope

This document provides cited benchmark scores for every model used in `.claude/agents/` and
`.opencode/agents/`. Its purpose is to make tier assignments in
[AI Agent Model Selection Convention](../../repo-governance/development/agents/model-selection.md) **auditable and
defensible** — anyone reading a tier decision can follow the citation chain from claim to
primary source here.

All docs that cite benchmark numbers link to this file. This file links to primary sources.

**Scope**:

- **Claude models** — all currently active models, legacy models, and deprecated models available via the Anthropic API
- **OpenCode Go models** — all models available via the `opencode-go/` provider in the OpenCode flat-rate subscription

---

## Benchmark Definitions

### Quick Reference

| Benchmark          | What it measures                                                                          | Relevance to coding agents           | Official Leaderboard                                                                                            |
| ------------------ | ----------------------------------------------------------------------------------------- | ------------------------------------ | --------------------------------------------------------------------------------------------------------------- |
| SWE-bench Verified | Real GitHub issues resolved end-to-end; ~500 human-verified test cases                    | Primary signal for agentic code work | [swebench.com](https://www.swebench.com/verified.html)                                                          |
| SWE-bench Pro      | Harder variant — proprietary issues requiring deeper context and multi-file reasoning     | Secondary signal for complex tasks   | [Scale AI SEAL](https://labs.scale.com/leaderboard/swe_bench_pro_public)                                        |
| GPQA Diamond       | Expert-level science questions (chemistry, biology, physics) requiring graduate reasoning | Proxy for deep analytical capability | [Artificial Analysis](https://artificialanalysis.ai/evaluations/gpqa-diamond)                                   |
| AIME 2025          | Competition math problems; tests multi-step formal reasoning                              | Proxy for structured problem-solving | [MathArena](https://matharena.ai/) / [Artificial Analysis](https://artificialanalysis.ai/evaluations/aime-2025) |
| Terminal-Bench 2.0 | Autonomous agent navigation of real shell and system environments                         | Direct signal for CLI-native agents  | [tbench.ai](https://www.tbench.ai/leaderboard/terminal-bench/2.0)                                               |
| OSWorld-Verified   | Computer-use tasks in real GUI environments; multimodal agents                            | Relevant for computer-use agents     | [os-world.github.io](https://os-world.github.io/)                                                               |
| HumanEval          | Function synthesis from docstrings; largely saturated at frontier (90%+)                  | Less discriminative at top tier      | [Artificial Analysis](https://artificialanalysis.ai/evaluations/humaneval)                                      |
| CursorBench        | Real Cursor engineering sessions; multi-dimension scoring (private leaderboard)           | High ecological validity             | Private (Cursor internal)                                                                                       |
| ZClawBench         | Z.ai proprietary benchmark; methodology undisclosed                                       | Not independently verifiable         | Z.ai only                                                                                                       |

### SWE-bench Verified

**Official URL**: [swebench.com/verified.html](https://www.swebench.com/verified.html) (Princeton NLP Group)

**Where updated**: Live leaderboard at [swebench.com](https://www.swebench.com). Labs submit results to Princeton NLP. New entries appear as labs benchmark their models. The Verified variant uses ~500 human-validated tasks from a larger pool.

**Why we use it**: Closest publicly available proxy for real-world autonomous debugging and code modification. Tasks require the agent to understand an existing codebase, write a targeted patch, and pass a full automated test suite — directly simulating production coding agent workflows. Pass@1 on full test suites under a real agent scaffold is harder to game than function synthesis benchmarks.

**Known limitations**: Growing contamination risk as frontier models are trained on GitHub data (which includes SWE-bench solutions). Anthropic applies memorization-screen adjustments. Less discriminative at the top end now that scores exceed 85% — see SWE-bench Pro for harder discrimination. The Verified variant uses human-validated tasks, reducing noise versus the raw 2,294-task set.

### SWE-bench Pro

**Official URL**: [labs.scale.com/leaderboard/swe_bench_pro_public](https://labs.scale.com/leaderboard/swe_bench_pro_public) (Scale AI SEAL)

**Where updated**: Scale AI maintains the leaderboard and requires standardized scaffolding (250-turn limit, uncapped cost). Labs submit via a controlled evaluation protocol. Updated as labs request evaluation.

**Why we use it**: More discriminative than SWE-bench Verified at the frontier. 1,865 multi-language, multi-file tasks (avg 107 lines across 4.1 files) sourced from proprietary enterprise repositories — models cannot have seen training examples. The standardized scaffold removes scaffold-as-variable confounds present in SWE-bench Verified submissions.

**Known limitations**: Private test set; results depend on submitting through Scale AI's evaluation process. The gap between Verified and Pro scores (e.g., Claude Opus 4.7: 87.6% vs 64.3%) reflects both contamination/memorization in Verified and genuine task difficulty increase in Pro. Opus 4.7 has not yet been submitted to the standardized SEAL scaffold as of 2026-05-07.

### GPQA Diamond

**Official URL**: [artificialanalysis.ai/evaluations/gpqa-diamond](https://artificialanalysis.ai/evaluations/gpqa-diamond) | [epoch.ai/benchmarks/gpqa-diamond](https://epoch.ai/benchmarks/gpqa-diamond)

**Where updated**: Artificial Analysis and Epoch AI maintain live leaderboards. Original paper: [arXiv:2311.12022](https://arxiv.org/abs/2311.12022). New models appear as evaluators submit results; Anthropic publishes system card numbers.

**Why we use it**: Graduate-level chemistry/biology/physics questions validated by 20 PhD-level experts. Proxy for deep multi-step analytical reasoning — correlates with ability to reason about complex algorithms, debug obscure failures, and serve as an intelligent technical partner rather than a pattern-matcher.

**Known limitations**: Not directly a coding benchmark. The gap between "analytical reasoning" and "capable of multi-file tool use" can be large. Score varies significantly based on whether adaptive thinking/extended thinking is used (e.g., Sonnet 4.6: 74.1% standard vs 89.9% adaptive) — always note evaluation conditions when citing this score.

### AIME 2025

**Official URL**: [matharena.ai](https://matharena.ai/) | [artificialanalysis.ai/evaluations/aime-2025](https://artificialanalysis.ai/evaluations/aime-2025)

**Where updated**: MathArena and Artificial Analysis track live leaderboard entries. American Invitational Mathematics Examination problems are released annually; the 2025 set (30 problems: 15 AIME I + 15 AIME II) is the current standard.

**Why we use it**: Formal, multi-step competition math requiring structured problem decomposition. Scores above 90% indicate the model can chain complex logical steps — which correlates with agentic planning quality for multi-step coding tasks.

**Known limitations**: Known contamination risk — AIME problems circulate widely online. Anthropic explicitly flags this in the Opus 4.5 system card §2.2, and Opus 4.7 does not publish an AIME 2025 score likely for this reason. Treat any score near or above 95% with skepticism without contamination controls. AIME 2026 (used in some GLM and Kimi benchmarks) is not directly comparable to AIME 2025.

### Terminal-Bench 2.0

**Official URL**: [tbench.ai/leaderboard/terminal-bench/2.0](https://www.tbench.ai/leaderboard/terminal-bench/2.0)

**Where updated**: Laude Institute maintains the leaderboard. GitHub: [github.com/laude-institute/terminal-bench](https://github.com/laude-institute/terminal-bench). Evaluations submitted by labs and researchers; updated as new models are tested.

**Why we use it**: Requires agents to navigate a real terminal environment autonomously — shell, file systems, system administration. Direct signal for CLI-native agent capability; harder to game than code synthesis benchmarks because it requires real execution rather than text prediction.

**Known limitations**: Smaller task set than SWE-bench; agent scaffold choice may affect scores significantly. Less widely standardized than SWE-bench.

### OSWorld / OSWorld-Verified

**Official URL**: [os-world.github.io](https://os-world.github.io/) | [xlang.ai/blog/osworld-verified](https://xlang.ai/blog/osworld-verified)

**Where updated**: XLANG Lab / University of Hong Kong. Live leaderboard at the official site. OSWorld-Verified is a subset with more reliable task verification.

**Why we use it**: Key benchmark for computer-use capability — real GUI environments (desktop apps, web browsers, file managers). Assesses the vision + action loop for multimodal agents. Relevant for any agent that needs to interact with software interfaces rather than just code.

**Known limitations**: Requires running real OS environments; results can vary by screenshot quality and resolution. Scores can be sensitive to scaffold implementation.

### CursorBench

**Official blog**: [cursor.com/blog/cursorbench](https://cursor.com/blog/cursorbench)

**Where updated**: Cursor maintains this privately. No public leaderboard — scores are disclosed selectively via Anthropic and other lab announcements. Methodology is described in the blog post.

**Why we use it**: High ecological validity — tasks sourced from actual developer Cursor sessions (via Cursor Blame), not synthetic benchmarks. Multi-dimension scoring (correctness, quality, efficiency, interaction quality).

**Known limitations**: Private leaderboard — Cursor intentionally keeps detailed scores private to prevent benchmark gaming. Numbers cited (e.g., Opus 4.7: 70%) come from Anthropic's own announcements using their Cursor collaboration data. Cannot be independently verified or reproduced by third parties.

---

## Claude Models (Anthropic)

### Currently Active Models

The following three models are Anthropic's current recommended API models as of 2026-05-07.

**Source**: [Anthropic Models Overview](https://platform.claude.com/docs/en/about-claude/models/overview) (official API docs, accessed 2026-05-07)

| Feature                       | Claude Opus 4.7    | Claude Sonnet 4.6   | Claude Haiku 4.5            |
| ----------------------------- | ------------------ | ------------------- | --------------------------- |
| **API Model ID**              | `claude-opus-4-7`  | `claude-sonnet-4-6` | `claude-haiku-4-5-20251001` |
| **Alias**                     | `claude-opus-4-7`  | `claude-sonnet-4-6` | `claude-haiku-4-5`          |
| **Pricing (in/out MTok)**     | $5 / $25           | $3 / $15            | $1 / $5                     |
| **Context window**            | 1M tokens          | 1M tokens (beta)    | 200k tokens                 |
| **Max output**                | 128k tokens        | 64k tokens          | 64k tokens                  |
| **Reliable knowledge cutoff** | Jan 2026           | Aug 2025            | Feb 2025                    |
| **Training data cutoff**      | Jan 2026           | Jan 2026            | Jul 2025                    |
| **Extended thinking**         | No (adaptive only) | Yes                 | Yes                         |
| **Release date**              | 2026-04-16         | 2026-02-17          | 2025-10-15                  |

---

### Claude Opus 4.7

**Model ID**: `claude-opus-4-7` | **Alias**: `opus` (omit in agent frontmatter for budget-adaptive inherit)

**Primary sources**:

- [Anthropic Models Overview](https://platform.claude.com/docs/en/about-claude/models/overview) (official API docs, accessed 2026-05-07)
- [Introducing Claude Opus 4.7](https://www.anthropic.com/news/claude-opus-4-7) (Anthropic, 2026-04-16)
- [Claude Opus 4.7 What's New](https://platform.claude.com/docs/en/about-claude/models/whats-new-claude-4-7) (official API changelog)
- [Claude Opus 4.7 System Card](https://www.anthropic.com/claude-opus-4-7-system-card) (PDF; inaccessible to automated text extraction at research time — scores corroborated via launch post and third-party outlets)

| Benchmark                      | Score         | Vs Predecessor (Opus 4.6)   | Confidence        | Source                                                                       |
| ------------------------------ | ------------- | --------------------------- | ----------------- | ---------------------------------------------------------------------------- |
| SWE-bench Verified             | **87.6%**     | +6.8pp (vs 80.8%)           | `[Self-reported]` | Official launch post (2026-04-16); corroborated by VentureBeat, BenchLM.ai   |
| SWE-bench Pro                  | **64.3%**     | +10.9pp (vs 53.4%)          | `[Self-reported]` | Official launch post; Opus 4.7 not yet submitted to Scale AI SEAL scaffold   |
| GPQA Diamond                   | **94.2%**     | +2.9pp (vs 91.3%)           | `[Self-reported]` | Official launch post; multiple aggregators confirm                           |
| Terminal-Bench 2.0             | **69.4%**     | +4.0pp (vs 65.4%)           | `[Self-reported]` | Official launch post                                                         |
| OSWorld-Verified               | **78.0%**     | +5.3pp (vs 72.7%)           | `[Self-reported]` | Official launch post                                                         |
| CursorBench                    | **70%**       | +12pp (vs 58% for Opus 4.6) | `[Self-reported]` | Official launch post (Cursor partner data)                                   |
| Finance Agent v1.1             | **64.4%**     | +3.7pp (vs 60.7%)           | `[Self-reported]` | Official launch post                                                         |
| MCP-Atlas                      | **77.3%**     | —                           | `[Self-reported]` | Official launch post                                                         |
| BrowseComp                     | **79.3%**     | —                           | `[Self-reported]` | Official launch post                                                         |
| HLE (no tools)                 | **46.9%**     | —                           | `[Self-reported]` | Official launch post                                                         |
| HLE (with tools)               | **54.7%**     | —                           | `[Self-reported]` | Official launch post                                                         |
| CharXiv Reasoning (no tools)   | **82.1%**     | —                           | `[Self-reported]` | Official launch post                                                         |
| CharXiv Reasoning (with tools) | **91.0%**     | —                           | `[Self-reported]` | Official launch post                                                         |
| MMMLU                          | **91.5%**     | —                           | `[Self-reported]` | Official launch post                                                         |
| AIME 2025                      | Not published | —                           | —                 | Likely omitted due to contamination concerns (see Opus 4.5 system card §2.2) |

**Notable changes in Opus 4.7**:

- New tokenizer: up to 35% more tokens for same input vs prior models
- Sampling parameters (`temperature`, `top_p`, `top_k`) removed — setting them returns HTTP 400
- Extended thinking budgets removed — only `type: "adaptive"` supported
- High-resolution image support up to 2576px / 3.75MP

---

### Claude Sonnet 4.6

**Model ID**: `claude-sonnet-4-6` | **Alias**: `sonnet`

**Primary sources**:

- [Anthropic Models Overview](https://platform.claude.com/docs/en/about-claude/models/overview) (official API docs, accessed 2026-05-07)
- [Introducing Claude Sonnet 4.6](https://www.anthropic.com/news/claude-sonnet-4-6) (Anthropic, 2026-02-17)
- [Claude Sonnet 4.6 System Card](https://www.anthropic.com/claude-sonnet-4-6-system-card)

| Benchmark          | Score                                       | Conditions                                        | Confidence        | Source                                                 |
| ------------------ | ------------------------------------------- | ------------------------------------------------- | ----------------- | ------------------------------------------------------ |
| SWE-bench Verified | **79.6%** (80.2% with prompt mod)           | Official avg, adaptive thinking                   | `[Self-reported]` | Official launch post (2026-02-17)                      |
| GPQA Diamond       | **89.9%** (adaptive) / **74.1%** (standard) | 10-trial avg, adaptive+max effort / standard mode | `[Self-reported]` | System card Table 2.1.A / Morph aggregator             |
| AIME 2025          | **95.6%**                                   | 10-trial avg, adaptive thinking, max effort       | `[Self-reported]` | System card (confirmed via multiple secondary sources) |
| OSWorld-Verified   | **72.5%**                                   | —                                                 | `[Self-reported]` | Official launch post                                   |
| ARC-AGI-2          | **60.4%**                                   | —                                                 | `[Self-reported]` | NxCode citing official                                 |
| MCP-Atlas          | **61.3%**                                   | —                                                 | `[Self-reported]` | Official launch post                                   |
| Context window     | 1M tokens                                   | —                                                 | —                 | Official API docs (beta)                               |

**GPQA disambiguation**: Two values circulate — 89.9% is the system card adaptive-thinking 10-trial average; 74.1% is the standard (non-adaptive) evaluation. Both are accurate in their respective conditions. The 89.9% is the number Anthropic foregrounds in system card reporting.

**Contamination note**: Anthropic flags that AIME 2025 scores may be partially inflated due to contamination (Opus 4.5 system card §2.2). The 95.6% score should be treated with awareness of this caveat.

---

### Claude Haiku 4.5

**Model ID**: `claude-haiku-4-5-20251001` | **Alias**: `haiku`

**Primary sources**:

- [Anthropic Models Overview](https://platform.claude.com/docs/en/about-claude/models/overview) (official API docs, accessed 2026-05-07)
- [Introducing Claude Haiku 4.5](https://www.anthropic.com/news/claude-haiku-4-5) (Anthropic, 2025-10-15)
- [Claude Haiku 4.5 System Card](https://www.anthropic.com/claude-haiku-4-5-system-card) (PDF; binary, not text-extractable at research time)

| Benchmark            | Score         | Conditions                                            | Confidence             | Source                                                                       |
| -------------------- | ------------- | ----------------------------------------------------- | ---------------------- | ---------------------------------------------------------------------------- |
| SWE-bench Verified   | **73.3%**     | 50-trial avg, 128K think budget, no test-time compute | `[Verified]`           | Official launch post (verbatim quoted); consistent across all sources        |
| SWE-bench Pro (SEAL) | **39.5%**     | Standardized SEAL scaffold                            | `[Self-reported]`      | Morph citing official                                                        |
| OSWorld-Verified     | **50.7%**     | 10 runs, 128K thinking budget                         | `[Self-reported]`      | DataCamp citing official                                                     |
| MMMU                 | **73.2%**     | 10 runs, 128K thinking budget                         | `[Self-reported]`      | DataCamp citing official                                                     |
| GPQA Diamond         | **74.1%**     | Standard mode                                         | `[Needs Verification]` | Morph aggregator; 67.2% also circulates but 74.1% is more consistently cited |
| AIME 2025            | **80.7%**     | 10 runs, 128K thinking budget                         | `[Needs Verification]` | Aggregator paraphrase; 83.7% also circulates                                 |
| Context window       | 200k tokens   | —                                                     | —                      | Official API docs                                                            |
| Max output           | 64k tokens    | —                                                     | —                      | Official API docs                                                            |
| Speed                | ~89.6 tok/sec | —                                                     | —                      | Artificial Analysis measurement                                              |

**Haiku 3 retirement**: `claude-3-haiku` was retired 2026-04-19. All `haiku`-tier agents now resolve to `claude-haiku-4-5-20251001`.

**GPQA / AIME note**: Two conflicting values circulate for both scores. The Haiku 4.5 system card PDF was inaccessible to automated text extraction. The 74.1% GPQA and 80.7% AIME figures are the more consistently cited values across aggregators; 67.2% and 83.7% appear in fewer sources and may reflect earlier aggregator pulls or different evaluation conditions. Both remain `[Needs Verification]` until the system card PDF is confirmed.

---

### Legacy Models

These models remain available via the Anthropic API but are no longer the recommended current versions. Migration is encouraged.

**Source**: [Anthropic Models Overview](https://platform.claude.com/docs/en/about-claude/models/overview) (legacy section, accessed 2026-05-07)

| Model             | API ID                       | Pricing (in/out MTok) | Context | Max Out | Release    | SWE-bench Verified         | SWE-bench Pro (SEAL) | GPQA Diamond |
| ----------------- | ---------------------------- | --------------------- | ------- | ------- | ---------- | -------------------------- | -------------------- | ------------ |
| Claude Opus 4.6   | `claude-opus-4-6`            | $5 / $25              | 1M      | 128k    | 2026       | **80.8%**                  | **51.90%**           | **91.3%**    |
| Claude Sonnet 4.5 | `claude-sonnet-4-5-20250929` | $3 / $15              | 200k    | 64k     | 2025-09-29 | **77.2%** (82.0% parallel) | **43.60%**           | —            |
| Claude Opus 4.5   | `claude-opus-4-5-20251101`   | $5 / $25              | 200k    | 64k     | 2025-11-01 | **80.9%**                  | **45.89%**           | —            |
| Claude Opus 4.1   | `claude-opus-4-1-20250805`   | $15 / $75             | 200k    | 32k     | 2025-08-05 | —                          | **23.1%**            | **80.9%**    |

All legacy model scores `[Self-reported]` from respective official Anthropic announcements and system cards. SWE-bench Pro (SEAL) scores from Scale AI public leaderboard.

---

### Deprecated Models (Retiring June 15, 2026)

| Model             | API ID                                           | Pricing   | Context | Status                                    |
| ----------------- | ------------------------------------------------ | --------- | ------- | ----------------------------------------- |
| Claude Sonnet 4.0 | `claude-sonnet-4-20250514` / `claude-sonnet-4-0` | $3 / $15  | 200k    | DEPRECATED — migrate before June 15, 2026 |
| Claude Opus 4.0   | `claude-opus-4-20250514` / `claude-opus-4-0`     | $15 / $75 | 200k    | DEPRECATED — migrate before June 15, 2026 |

**Source**: [Anthropic Model Deprecations](https://platform.claude.com/docs/en/about-claude/model-deprecations)

---

### Preview Models (Not General API)

**Claude Mythos Preview** — Available only to ~50 invitation-only Project Glasswing partner organizations. NOT available via standard Claude API or Claude Code. Included here for reference only.

- **Bedrock ID**: `anthropic.claude-mythos-preview`
- **Access**: [Project Glasswing](https://www.anthropic.com/news/project-glasswing) (invitation-only)
- **Pricing**: $25 / $125 per MTok in/out (Glasswing participants)
- **Context**: 1M tokens input, 128k max output

| Benchmark          | Score | Confidence        |
| ------------------ | ----- | ----------------- |
| SWE-bench Verified | 93.9% | `[Self-reported]` |
| SWE-bench Pro      | 77.8% | `[Self-reported]` |
| GPQA Diamond       | 94.6% | `[Self-reported]` |
| Terminal-Bench 2.0 | 82.0% | `[Self-reported]` |
| OSWorld-Verified   | 79.6% | `[Self-reported]` |

---

## OpenCode Go Models (opencode-go/ provider)

OpenCode Go is a flat-rate subscription ($5 first month, then $10/month) providing access to
curated models from Chinese AI labs. All models use the `opencode-go/` provider prefix in
OpenCode configuration. Claude Code agents use Claude models — the `opencode-go/` models are
only active in the OpenCode runtime.

**Subscription model**: No per-token billing for subscribers. Rate limits (requests per 5-hour
window) serve as the effective capability signal — lower limit = heavier/more capable model,
higher limit = lighter/faster model.

**Current roster**: 12 models as of May 2026. The roster changes without a fixed cadence.
Check the live model list in the OpenCode TUI or at [opencode.ai/docs/go](https://opencode.ai/docs/go/).

**Source**: [OpenCode Go Docs](https://opencode.ai/docs/go/) (official, accessed 2026-05-07)

### Roster Overview

| opencode-go/ ID                 | Display Name      | Provider    | Req/5h | Capability Tier |
| ------------------------------- | ----------------- | ----------- | ------ | --------------- |
| `opencode-go/glm-5.1`           | GLM-5.1           | Z.ai        | 880    | Top             |
| `opencode-go/kimi-k2.6`         | Kimi K2.6         | Moonshot AI | 1,150  | Top             |
| `opencode-go/glm-5`             | GLM-5             | Z.ai        | 1,150  | High            |
| `opencode-go/mimo-v2.5-pro`     | MiMo-V2.5-Pro     | Xiaomi      | 1,290  | High            |
| `opencode-go/kimi-k2.5`         | Kimi K2.5         | Moonshot AI | 1,850  | High            |
| `opencode-go/mimo-v2.5`         | MiMo-V2.5         | Xiaomi      | 2,150  | Mid-high        |
| `opencode-go/deepseek-v4-pro`   | DeepSeek V4 Pro   | DeepSeek    | 3,450  | Mid-high        |
| `opencode-go/qwen3.6-plus`      | Qwen3.6 Plus      | Alibaba     | 3,300  | Mid-high        |
| `opencode-go/minimax-m2.7`      | MiniMax M2.7      | MiniMax     | 3,400  | Mid             |
| `opencode-go/minimax-m2.5`      | MiniMax M2.5      | MiniMax     | 6,300  | Mid             |
| `opencode-go/qwen3.5-plus`      | Qwen3.5 Plus      | Alibaba     | 10,200 | Mid             |
| `opencode-go/deepseek-v4-flash` | DeepSeek V4 Flash | DeepSeek    | 31,650 | Speed           |

---

### opencode-go/glm-5.1

**Provider**: Z.ai (formerly Zhipu AI) | **Release**: 2026-04-07

**Primary sources**:

- [Z.ai GLM-5.1 Docs](https://docs.z.ai/guides/llm/glm-5.1)
- [HuggingFace zai-org/GLM-5.1](https://huggingface.co/zai-org/GLM-5.1)

| Benchmark          | Score                    | Confidence        | Source                                                             |
| ------------------ | ------------------------ | ----------------- | ------------------------------------------------------------------ |
| SWE-bench Pro      | **58.4%**                | `[Self-reported]` | HF model card; reported as #1 on SWE-bench Pro at Apr 2026 release |
| SWE-bench Verified | Not separately published | —                 | GLM-5 baseline was 77.8%; GLM-5.1 improvement unconfirmed          |
| GPQA Diamond       | **86.2%**                | `[Self-reported]` | HF model card                                                      |
| AIME 2026 I        | **95.3%**                | `[Self-reported]` | HF model card (note: AIME 2026, not 2025)                          |
| HLE (with tools)   | **52.3%**                | `[Self-reported]` | HF model card                                                      |

**Architecture**: 754B total / 40B active (MoE) | **Context**: 200K tokens | **License**: MIT (open-weight)

---

### opencode-go/kimi-k2.6

**Provider**: Moonshot AI | **Release**: 2026-04-20

**Primary sources**:

- [Kimi K2.6 Tech Blog](https://www.kimi.com/blog/kimi-k2-6)
- [HuggingFace moonshotai/Kimi-K2.6](https://huggingface.co/moonshotai/Kimi-K2.6)

| Benchmark          | Score     | Confidence        | Source        |
| ------------------ | --------- | ----------------- | ------------- |
| SWE-bench Verified | **80.2%** | `[Self-reported]` | Official blog |
| SWE-bench Pro      | **58.6%** | `[Self-reported]` | Official blog |
| GPQA Diamond       | **90.5%** | `[Self-reported]` | Official blog |

**Context**: 262,144 tokens (256K) | **License**: Open-source | **Notable**: Supports agent swarms with up to 300 sub-agents and 4,000 coordination steps.

---

### opencode-go/glm-5

**Provider**: Z.ai | **Release**: 2026-02-17

**Primary sources**:

- [HuggingFace zai-org/GLM-5](https://huggingface.co/zai-org/GLM-5)
- [GLM-5 technical paper (arXiv:2602.15763)](https://arxiv.org/html/2602.15763v1)

| Benchmark              | Score     | Confidence        | Source                                              |
| ---------------------- | --------- | ----------------- | --------------------------------------------------- |
| SWE-bench Verified     | **77.8%** | `[Self-reported]` | HF model card; highest open-source score at release |
| GPQA Diamond           | **86.0%** | `[Self-reported]` | HF model card                                       |
| AIME 2026 I            | **92.7%** | `[Self-reported]` | HF model card (note: AIME 2026, not 2025)           |
| SWE-bench Multilingual | **73.3%** | `[Self-reported]` | HF model card                                       |
| Terminal-Bench 2.0     | **56.2%** | `[Self-reported]` | HF model card                                       |
| CyberGym               | **43.2%** | `[Self-reported]` | HF model card                                       |
| HLE (no tools)         | **30.5%** | `[Self-reported]` | HF model card                                       |

**Architecture**: 744B total / 40B active (MoE) | **Context**: 200K tokens | **License**: Open-weight

---

### opencode-go/mimo-v2.5-pro

**Provider**: Xiaomi (MiMo team) | **Release**: 2026-04-22

**Primary sources**:

- [MiMo-V2.5-Pro Official](https://mimo.xiaomi.com/mimo-v2-5-pro/)
- [HuggingFace XiaomiMiMo/MiMo-V2.5-Pro](https://huggingface.co/XiaomiMiMo/MiMo-V2.5-Pro)

| Benchmark                              | Score              | Confidence        | Source                                              |
| -------------------------------------- | ------------------ | ----------------- | --------------------------------------------------- |
| SWE-bench Pro                          | **57.2%**          | `[Self-reported]` | Official release; exceeds Claude Opus 4.6's 53.4%   |
| SWE-bench Verified                     | ~82%               | `[Unverified]`    | Third-party synthesis; primary source not confirmed |
| Terminal-Bench 2.0                     | **65.8%**          | `[Self-reported]` | HF model card                                       |
| Claw-Eval General                      | **62.1%**          | `[Self-reported]` | HF model card                                       |
| Artificial Analysis Intelligence Index | **54** (composite) | `[Verified]`      | Artificial Analysis                                 |

**Architecture**: 1.02T total / 42B active (MoE), multimodal | **Context**: 1M tokens | **License**: Open-weight | **Notable**: Supports 1,000+ sequential tool calls without coherence loss.

---

### opencode-go/kimi-k2.5

**Provider**: Moonshot AI | **Release**: Early 2026 (before April 20, 2026)

**Primary sources**:

- [Kimi K2.5 Tech Blog](https://www.kimi.com/blog/kimi-k2-5)
- [GitHub MoonshotAI/Kimi-K2.5](https://github.com/MoonshotAI/Kimi-K2.5)

| Benchmark          | Score     | Confidence        | Source                             |
| ------------------ | --------- | ----------------- | ---------------------------------- |
| SWE-bench Verified | **76.8%** | `[Self-reported]` | Official blog                      |
| SWE-bench Pro      | **50.7%** | `[Self-reported]` | Official blog                      |
| GPQA Diamond       | **87.6%** | `[Self-reported]` | Official blog                      |
| AIME 2025          | **96.1%** | `[Self-reported]` | Official blog (with thinking mode) |

**Context**: 256K tokens | **License**: Open-source

---

### opencode-go/mimo-v2.5

**Provider**: Xiaomi (MiMo team) | **Release**: 2026-04-22

**Primary sources**:

- [HuggingFace XiaomiMiMo/MiMo-V2.5](https://huggingface.co/XiaomiMiMo/MiMo-V2.5)

| Benchmark          | Score         | Confidence        | Source        |
| ------------------ | ------------- | ----------------- | ------------- |
| SWE-bench Pro      | **56.1%**     | `[Self-reported]` | HF model card |
| Terminal-Bench 2.0 | **65.8%**     | `[Self-reported]` | HF model card |
| Claw-Eval General  | **62.1%**     | `[Self-reported]` | HF model card |
| SWE-bench Verified | Not published | —                 | —             |

**Architecture**: 310B total / 15B active (MoE), multimodal | **Context**: 1M tokens (post-training extended) | **License**: Open-weight

---

### opencode-go/deepseek-v4-pro

**Provider**: DeepSeek | **Release**: 2026-04-24

**Primary sources**:

- [HuggingFace deepseek-ai/DeepSeek-V4-Pro](https://huggingface.co/deepseek-ai/DeepSeek-V4-Pro)
- [DeepSeek API Changelog](https://api-docs.deepseek.com/news/news260424)

| Benchmark          | Score         | Confidence             | Source        |
| ------------------ | ------------- | ---------------------- | ------------- |
| SWE-bench Verified | **80.6%**     | `[Self-reported]`      | HF model card |
| SWE-bench Pro      | **55.4%**     | `[Self-reported]`      | HF model card |
| GPQA Diamond       | **90.1%**     | `[Self-reported]`      | HF model card |
| MMLU-Pro           | **87.5%**     | `[Self-reported]`      | HF model card |
| Terminal-Bench 2.0 | **67.9%**     | `[Self-reported]`      | HF model card |
| AIME 2025          | Not published | `[Needs Verification]` | —             |

**Architecture**: 1.6T total / 49B active (MoE), FP4+FP8 | **Context**: 1M tokens | **License**: MIT (open-weight)

---

### opencode-go/qwen3.6-plus

**Provider**: Alibaba (Qwen team) | **Release**: 2026-03-31

**Primary sources**: Third-party aggregators (official Qwen primary source not directly confirmed at research time)

| Benchmark          | Score     | Confidence     | Source                                       |
| ------------------ | --------- | -------------- | -------------------------------------------- |
| SWE-bench Verified | **78.8%** | `[Unverified]` | Third-party aggregators                      |
| Terminal-Bench 2.0 | **61.6%** | `[Unverified]` | Third-party (leads Claude Opus 4.5 at 59.3%) |

**Context**: 1M tokens | **License**: Proprietary

---

### opencode-go/minimax-m2.7

**Provider**: MiniMax | **Release**: 2026-03-18

**Primary sources**:

- [MiniMax M2.7 Official](https://www.minimax.io/news/minimax-m27-en)
- [HuggingFace MiniMaxAI/MiniMax-M2.7](https://huggingface.co/MiniMaxAI/MiniMax-M2.7)

| Benchmark              | Score         | Confidence             | Source                                                           |
| ---------------------- | ------------- | ---------------------- | ---------------------------------------------------------------- |
| SWE-bench Verified     | ~78%          | `[Unverified]`         | Third-party estimate; official announcement did not publish      |
| SWE-bench Pro          | **56.22%**    | `[Self-reported]`      | Official announcement                                            |
| SWE-bench Multilingual | **76.5%**     | `[Self-reported]`      | Official announcement                                            |
| Multi-SWE-Bench        | **52.7%**     | `[Self-reported]`      | Official announcement                                            |
| VIBE-Pro               | **55.6%**     | `[Self-reported]`      | Official announcement                                            |
| Terminal-Bench 2.0     | **57.0%**     | `[Self-reported]`      | Official announcement                                            |
| GDPval-AA ELO          | **1495**      | `[Self-reported]`      | Official (highest open-source on office productivity at release) |
| GPQA Diamond           | Not published | `[Needs Verification]` | —                                                                |

**Architecture**: 230B total / 10B active (MoE) | **Context**: 200K tokens | **License**: Open-weight (non-commercial — commercial use requires separate agreement)

---

### opencode-go/minimax-m2.5

**Provider**: MiniMax | **Release**: 2026-02-12

**Primary sources**:

- [MiniMax M2.5 Official](https://www.minimax.io/news/minimax-m25)
- [HuggingFace MiniMaxAI/MiniMax-M2.5](https://huggingface.co/MiniMaxAI/MiniMax-M2.5)

| Benchmark          | Score     | Confidence        | Source                                               |
| ------------------ | --------- | ----------------- | ---------------------------------------------------- |
| SWE-bench Verified | **80.2%** | `[Self-reported]` | Official announcement; corroborated by HN discussion |
| Multi-SWE-Bench    | **51.3%** | `[Self-reported]` | Official announcement                                |
| BrowseComp         | **76.3%** | `[Self-reported]` | Official announcement (with context management)      |
| GPQA Diamond       | **85.2%** | `[Self-reported]` | HF Open LLM Leaderboard official submission          |

**Architecture**: 230B total / 10B active (MoE) | **Context**: 204,800 tokens | **License**: MIT (open-weight)

---

### opencode-go/qwen3.5-plus

**Provider**: Alibaba (Qwen team) | **Release**: February 2026

**Primary sources**: Third-party developer guides (official Qwen primary source not directly confirmed at research time)

| Benchmark          | Score     | Confidence     | Source                  |
| ------------------ | --------- | -------------- | ----------------------- |
| SWE-bench Verified | **76.4%** | `[Unverified]` | Third-party aggregators |

**Context**: 262K tokens native; extensible to ~1M | **License**: Proprietary

---

### opencode-go/deepseek-v4-flash

**Provider**: DeepSeek | **Release**: 2026-04-24

**Primary sources**:

- [HuggingFace deepseek-ai/DeepSeek-V4-Flash](https://huggingface.co/deepseek-ai/DeepSeek-V4-Flash)

| Benchmark          | Score         | Confidence             | Source                                       |
| ------------------ | ------------- | ---------------------- | -------------------------------------------- |
| SWE-bench Verified | **79.0%**     | `[Self-reported]`      | HF model card (1.6pp behind V4 Pro at 80.6%) |
| SWE-bench Pro      | Not published | `[Needs Verification]` | —                                            |
| GPQA Diamond       | Not published | `[Needs Verification]` | —                                            |

**Architecture**: 284B total / 13B active (MoE) | **Context**: 1M tokens | **License**: MIT (open-weight) | **Note**: Highest rate limit in entire roster (31,650 req/5h) — designed as the speed/throughput tier.

---

## Model Selection Mapping

Cross-reference with [AI Agent Model Selection Convention](../../repo-governance/development/agents/model-selection.md)
tier assignments. The Claude-to-OpenCode mapping reflects what `npm run sync:claude-to-opencode` produces
for the current agent frontmatter aliases.

| Claude Alias | Claude Model (2026)         | Pricing (in/out MTok) | SWE-bench Verified        | OpenCode Go ID             |
| ------------ | --------------------------- | --------------------- | ------------------------- | -------------------------- |
| `""` (omit)  | Inherits session model      | Inherits              | Inherits (87.6% or 79.6%) | `opencode-go/minimax-m2.7` |
| `sonnet`     | `claude-sonnet-4-6`         | $3 / $15              | 79.6% `[Self-reported]`   | `opencode-go/minimax-m2.7` |
| `haiku`      | `claude-haiku-4-5-20251001` | $1 / $5               | 73.3% `[Verified]`        | `opencode-go/glm-5`        |

**Note on OpenCode Go mapping**: The full opencode-go roster has 12 models as of May 2026. The
3-to-2 collapse (minimax-m2.7 for planning+execution, glm-5 for fast) reflects the mapping
encoded in `apps/rhino-cli/internal/agents/converter.go` at time of last sync. As the OpenCode
Go roster evolves, the converter may be updated to point to higher-capability models. See
`model-selection.md` for the authoritative mapping rationale.

---

## Model Capability Summary (Coding-Agents Lens)

| Model                 | SWE-bench Verified      | SWE-bench Pro            | Cost tier   | Notes                                                        |
| --------------------- | ----------------------- | ------------------------ | ----------- | ------------------------------------------------------------ |
| **Claude Opus 4.7**   | 87.6% `[Self-reported]` | 64.3% `[Self-reported]`  | Highest     | Budget-adaptive inherit — Max/Team Premium sessions          |
| **Claude Sonnet 4.6** | 79.6% `[Self-reported]` | —                        | Mid         | Budget-adaptive inherit — Pro/Standard; explicit sonnet-tier |
| **Claude Haiku 4.5**  | 73.3% `[Verified]`      | 39.5% `[Self-reported]`  | Lowest      | Explicit haiku-tier — deterministic/mechanical agents        |
| **GLM-5.1**           | ~77.8% (GLM-5 baseline) | 58.4% `[Self-reported]`  | OC Top      | opencode-go; #1 SWE-bench Pro at Apr 2026 release            |
| **Kimi K2.6**         | 80.2% `[Self-reported]` | 58.6% `[Self-reported]`  | OC Top      | opencode-go; strong coding and agent swarm support           |
| **GLM-5**             | 77.8% `[Self-reported]` | —                        | OC High     | opencode-go; base model for GLM-5.1                          |
| **MiMo-V2.5-Pro**     | ~82% `[Unverified]`     | 57.2% `[Self-reported]`  | OC High     | opencode-go; 1T+ param MoE, 1M context                       |
| **DeepSeek V4 Pro**   | 80.6% `[Self-reported]` | 55.4% `[Self-reported]`  | OC Mid-high | opencode-go; strong GPQA Diamond 90.1%                       |
| **Kimi K2.5**         | 76.8% `[Self-reported]` | 50.7% `[Self-reported]`  | OC High     | opencode-go; predecessor to K2.6                             |
| **MiniMax M2.5**      | 80.2% `[Self-reported]` | —                        | OC Mid      | opencode-go; predecessor to M2.7                             |
| **MiniMax M2.7**      | ~78% `[Unverified]`     | 56.22% `[Self-reported]` | OC Mid      | opencode-go; current sync target for claude opus+sonnet      |
| **DeepSeek V4 Flash** | 79.0% `[Self-reported]` | —                        | OC Speed    | opencode-go; 31,650 req/5h — highest throughput              |
| **Qwen3.6 Plus**      | 78.8% `[Unverified]`    | —                        | OC Mid-high | opencode-go; always-on CoT                                   |
| **MiMo-V2.5**         | —                       | 56.1% `[Self-reported]`  | OC Mid-high | opencode-go; lighter MiMo variant                            |
| **Qwen3.5 Plus**      | 76.4% `[Unverified]`    | —                        | OC Mid      | opencode-go; high-throughput option                          |

**OC = OpenCode Go subscription (flat-rate, no per-token billing)**

---

## Limitations and Caveats

1. **Claude system card PDFs inaccessible to automated extraction**: All three active model system card PDFs are confirmed to exist but return compressed binary that tools cannot text-extract. Benchmark scores for Opus 4.7, Sonnet 4.6, and Haiku 4.5 are corroborated via official launch posts and credible third-party aggregators, not direct PDF extraction.

2. **Haiku 4.5 GPQA / AIME discrepancy is unresolved**: Two values circulate for each. 74.1% GPQA and 80.7% AIME are the more consistently cited across aggregators; 67.2% and 83.7% appear in fewer sources. Both tagged `[Needs Verification]` — update when system card PDF is confirmed.

3. **Opus 4.7 AIME 2025 not published**: Anthropic did not include AIME 2025 in the Opus 4.7 launch post, likely due to contamination concerns documented in Opus 4.5 system card §2.2.

4. **All OpenCode Go model scores are self-reported by their respective labs**: No independent third-party replication of GLM, Kimi, MiMo, MiniMax, Qwen, or DeepSeek scores has been identified. Treat with appropriate skepticism compared to Claude scores corroborated by multiple outlets.

5. **MiniMax M2.7 SWE-bench Verified not officially published**: The ~78% figure is a third-party estimate. Only SWE-bench Pro (56.22%) is from the official announcement.

6. **GLM-5.1 SWE-bench Verified not separately published**: GLM-5.1 is a post-training upgrade to GLM-5. Only SWE-bench Pro (58.4%) is in the GLM-5.1 official benchmark table.

7. **Qwen3.5 Plus and Qwen3.6 Plus scores from third-party aggregators only**: Official Qwen primary source was not directly confirmed at research time. Scores tagged `[Unverified]`.

8. **CursorBench has no public leaderboard**: The 70% figure for Opus 4.7 is partner-reported via Anthropic's launch post. Cannot be independently reproduced.

9. **OpenCode Go roster changes without fixed cadence**: The 12-model roster above reflects May 7, 2026. New models may be added or retired without advance notice. Check the live list in the OpenCode TUI for the current roster.

10. **AIME 2026 ≠ AIME 2025**: GLM-5, GLM-5.1, and Kimi models report AIME 2026 scores. These are NOT directly comparable to Claude Sonnet 4.6's AIME 2025 score of 95.6%.

11. **Prices and context windows** are as of the access date shown. Check official API docs for current values before making cost comparisons.

12. **Accuracy as of**: 2026-05-07. Model versions, scores, pricing, and OpenCode Go roster change frequently. Re-verify when making tier assignment decisions more than 3 months from this date.

---

## Historical / Comparative References

These models are not current tier choices but are referenced in comparison or as platform fallbacks.

| Model             | SWE-bench Verified       | Context | Notes                                                            |
| ----------------- | ------------------------ | ------- | ---------------------------------------------------------------- |
| Claude Sonnet 4.5 | ~77.2% `[Self-reported]` | 200k    | Bedrock/Vertex platform fallback                                 |
| Claude Opus 4.6   | 80.8% `[Self-reported]`  | 1M      | Comparison baseline for GLM-5.1 SWE-bench Pro; SEAL shows 51.90% |

---

## Sources

1. Anthropic Models Overview — <https://platform.claude.com/docs/en/about-claude/models/overview> (accessed 2026-05-07)
2. Anthropic Model Deprecations — <https://platform.claude.com/docs/en/about-claude/model-deprecations>
3. Anthropic System Cards Index — <https://www.anthropic.com/system-cards>
4. Introducing Claude Opus 4.7 — <https://www.anthropic.com/news/claude-opus-4-7> (2026-04-16)
5. Claude Opus 4.7 What's New — <https://platform.claude.com/docs/en/about-claude/models/whats-new-claude-4-7>
6. Claude Opus 4.7 System Card — <https://www.anthropic.com/claude-opus-4-7-system-card>
7. Introducing Claude Sonnet 4.6 — <https://www.anthropic.com/news/claude-sonnet-4-6> (2026-02-17)
8. Claude Sonnet 4.6 System Card — <https://www.anthropic.com/claude-sonnet-4-6-system-card>
9. Introducing Claude Haiku 4.5 — <https://www.anthropic.com/news/claude-haiku-4-5> (2025-10-15)
10. Claude Haiku 4.5 System Card — <https://www.anthropic.com/claude-haiku-4-5-system-card>
11. Project Glasswing / Claude Mythos Preview — <https://www.anthropic.com/news/project-glasswing>
12. OpenCode Go Docs — <https://opencode.ai/docs/go/> (accessed 2026-05-07)
13. Z.ai GLM-5.1 Docs — <https://docs.z.ai/guides/llm/glm-5.1>
14. HuggingFace zai-org/GLM-5.1 — <https://huggingface.co/zai-org/GLM-5.1>
15. HuggingFace zai-org/GLM-5 — <https://huggingface.co/zai-org/GLM-5>
16. GLM-5 technical paper — <https://arxiv.org/html/2602.15763v1>
17. Kimi K2.6 Tech Blog — <https://www.kimi.com/blog/kimi-k2-6>
18. Kimi K2.5 Tech Blog — <https://www.kimi.com/blog/kimi-k2-5>
19. HuggingFace moonshotai/Kimi-K2.6 — <https://huggingface.co/moonshotai/Kimi-K2.6>
20. MiMo-V2.5-Pro Official — <https://mimo.xiaomi.com/mimo-v2-5-pro/>
21. HuggingFace XiaomiMiMo/MiMo-V2.5-Pro — <https://huggingface.co/XiaomiMiMo/MiMo-V2.5-Pro>
22. HuggingFace XiaomiMiMo/MiMo-V2.5 — <https://huggingface.co/XiaomiMiMo/MiMo-V2.5>
23. MiniMax M2.7 Official — <https://www.minimax.io/news/minimax-m27-en>
24. MiniMax M2.5 Official — <https://www.minimax.io/news/minimax-m25>
25. HuggingFace MiniMaxAI/MiniMax-M2.7 — <https://huggingface.co/MiniMaxAI/MiniMax-M2.7>
26. HuggingFace MiniMaxAI/MiniMax-M2.5 — <https://huggingface.co/MiniMaxAI/MiniMax-M2.5>
27. HuggingFace deepseek-ai/DeepSeek-V4-Pro — <https://huggingface.co/deepseek-ai/DeepSeek-V4-Pro>
28. HuggingFace deepseek-ai/DeepSeek-V4-Flash — <https://huggingface.co/deepseek-ai/DeepSeek-V4-Flash>
29. DeepSeek API Changelog — <https://api-docs.deepseek.com/news/news260424>
30. SWE-bench Verified Official Leaderboard — <https://www.swebench.com/verified.html>
31. Scale AI SWE-bench Pro Leaderboard — <https://labs.scale.com/leaderboard/swe_bench_pro_public>
32. GPQA Diamond — Artificial Analysis — <https://artificialanalysis.ai/evaluations/gpqa-diamond>
33. AIME 2025 — Artificial Analysis — <https://artificialanalysis.ai/evaluations/aime-2025>
34. MathArena — <https://matharena.ai/>
35. Terminal-Bench 2.0 Leaderboard — <https://www.tbench.ai/leaderboard/terminal-bench/2.0>
36. OSWorld Official — <https://os-world.github.io/>
37. CursorBench Blog — <https://cursor.com/blog/cursorbench>
38. Vellum Claude Opus 4.7 Benchmarks Explained — <https://www.vellum.ai/blog/claude-opus-4-7-benchmarks-explained>
39. DataCamp Claude Haiku 4.5 — <https://www.datacamp.com/blog/anthropic-claude-haiku-4-5>
40. NxCode Claude Sonnet 4.6 Complete Guide — <https://www.nxcode.io/resources/news/claude-sonnet-4-6-complete-guide-benchmarks-pricing-2026>
