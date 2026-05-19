---
title: "Beginner"
date: 2026-05-19T00:00:00+07:00
draft: false
weight: 10000001
description: "Examples 1-27: Pi Coding Agent fundamentals - installation, CLI, provider configuration, built-in tools, session management (0-40% coverage)"
tags: ["pi-coding-agent", "pi.dev", "beginner", "by-example", "tutorial", "ai-agent", "coding-agent"]
---

This tutorial provides 27 foundational examples covering Pi Coding Agent. Learn installation (Examples 1-4), provider configuration (Examples 5-12), session basics (Examples 13-18), built-in tools (Examples 19-23), and session management (Examples 24-27).

## Installation (Examples 1-4)

### Example 1: Installing Pi via the cross-platform script

Pi ships a one-line installer that detects your platform and drops the `pi` binary onto your PATH. The installer supports macOS, Linux, and Windows (PowerShell) out of the box.

```bash
curl -fsSL https://pi.dev/install.sh | sh
                                        # => Detects platform and runs the right installer
                                        # => Installs pi onto PATH (e.g. /usr/local/bin/pi)
                                        # => No sudo required when ~/.local/bin is on PATH

pi --version                            # => Prints the installed version (e.g. v0.75.3)
```

**Key Takeaway**: A single `curl | sh` installs Pi with no extra dependencies. The installer chooses the right binary for your platform.

**Why It Matters**: Pi is intentionally lightweight to install. There is no Docker image to pull, no language runtime to configure separately, and no system service to register. The installer drops one binary and exits — first-run cost is measured in seconds, which lowers the barrier to evaluating the tool against your existing workflow.

### Example 2: Installing Pi via npm

If you already manage Node.js tooling with npm, install Pi as a global package. This is convenient on developer machines that share Node-based CLIs.

```bash
npm install -g @earendil-works/pi-coding-agent
                                        # => Installs pi command globally
                                        # => Adds pi to $(npm bin -g)
                                        # => Requires Node.js (current LTS or newer recommended)

which pi                                # => Confirms install path
pi --version                            # => Confirms working binary
```

**Key Takeaway**: Pi is published as `@earendil-works/pi-coding-agent` on npm. `npm install -g` is interchangeable with the script installer.

**Why It Matters**: npm install lets you pin a specific Pi version per machine alongside other Node tooling. CI environments that already have Node installed do not need a second installer; one `npm install -g` keeps build dependencies uniform.

### Example 3: Installing Pi via pnpm

Teams standardised on pnpm can install Pi globally with the same package name. pnpm's content-addressable global store deduplicates Pi's dependencies with your other global tools.

```bash
pnpm add -g @earendil-works/pi-coding-agent
                                        # => Installs pi command into pnpm's global store
                                        # => Shares dependencies with other pnpm globals

pnpm list -g | grep pi-coding-agent     # => Confirms install
pi --version
```

**Key Takeaway**: pnpm install works identically to npm; pi command is the same regardless of package manager.

**Why It Matters**: Choice of package manager is a workflow preference, not a Pi requirement. Pi is published once and consumed by any Node-compatible package manager. This avoids the situation where adopting a tool forces team-wide changes to the package manager choice.

### Example 4: Verifying installation and checking platform support

After install, confirm Pi runs and prints version info. Pi prints platform details with `--debug` to help troubleshoot installer issues.

```bash
pi --version                            # => pi vX.Y.Z (semver)
pi --help                               # => Lists subcommands and flags

pi --debug --version                    # => Prints version + platform + Node version
                                        # => Useful when reporting issues on GitHub
```

**Key Takeaway**: `pi --version` is your first sanity check. `--debug` adds platform diagnostics for issue reports.

**Why It Matters**: Confirming the installed version separately from running a session catches `PATH` shadowing problems (an old binary higher on PATH). The `--debug` output collects everything the maintainers need to triage an issue, so reproducing problems is straightforward.

## Provider Configuration (Examples 5-12)

### Example 5: Configure Anthropic (Claude) provider

Pi keys are stored under `~/.config/pi/credentials.json` or whichever location your platform's XDG config dir resolves to. The CLI provides commands to set keys without hand-editing the file.

```bash
pi auth set anthropic                   # => Prompts for ANTHROPIC_API_KEY
                                        # => Writes the key into the credentials file
                                        # => File is created with 0600 permissions

pi auth list                            # => Shows configured providers (key is redacted)
```

**Key Takeaway**: `pi auth set <provider>` writes API keys into a protected credentials file with restrictive permissions.

**Why It Matters**: Hand-editing dotfiles is the dominant source of accidental credential leaks (e.g. file checked into a public dotfiles repo). The `auth set` command writes keys into a known path that ships out-of-the-box in `.gitignore`-style protections and uses 0600 permissions to prevent other local users from reading the file.

### Example 6: Configure OpenAI provider

OpenAI keys follow the same pattern. Pi reads them from credentials, but also honours the conventional `OPENAI_API_KEY` environment variable if set.

```bash
pi auth set openai                      # => Prompts for OPENAI_API_KEY
                                        # => Persists to credentials file

# Alternative: export an env var
export OPENAI_API_KEY="sk-..."          # => Pi prefers env var when present
                                        # => Useful for short-lived shells / CI
```

**Key Takeaway**: Env vars override credentials-file entries for the same provider — useful for CI runners and ephemeral shells.

**Why It Matters**: Layered configuration lets you keep a stable default in the credentials file and override per-shell when needed. CI pipelines drop in a `OPENAI_API_KEY` secret without touching disk; local development reuses the credentials file across many sessions.

### Example 7: Configure Google Gemini provider

Gemini keys come from the Google AI Studio dashboard or a Vertex AI service account, depending on whether you want consumer or Cloud-tier billing.

```bash
pi auth set google                      # => Prompts for GOOGLE_API_KEY
                                        # => Stores under credentials.providers.google.apiKey

pi auth list | grep google              # => Confirms google provider configured
```

**Key Takeaway**: Pi supports Gemini via direct API key or Vertex AI service account; the `google` provider key covers the simpler direct-API path.

**Why It Matters**: Google's developer-tier APIs use a single API key; Vertex AI uses service-account JSON for production workloads. Pi separates these so you can start with the free tier today and move to Vertex tomorrow without rewriting your config layout.

### Example 8: Configure Azure OpenAI provider

Azure OpenAI runs Anthropic and OpenAI models through Microsoft's compliance boundary. Configuration requires the endpoint URL, deployment name, and key.

```bash
pi auth set azure                       # => Prompts for AZURE_OPENAI_ENDPOINT,
                                        #    AZURE_OPENAI_API_KEY, deployment name

pi config get providers.azure           # => Prints (redacted) Azure config block
```

**Key Takeaway**: Azure requires three values: endpoint, key, and deployment. Pi captures all three in one prompt.

**Why It Matters**: Enterprise teams under compliance regimes (HIPAA, SOC 2, FedRAMP) frequently use Azure OpenAI rather than the public OpenAI API. Pi treating Azure as a first-class provider — not as an OpenAI subtype — keeps the configuration uniform regardless of which provider you adopt later.

### Example 9: Configure AWS Bedrock provider

Bedrock provides Anthropic, Mistral, Meta, and Amazon models through AWS IAM auth. Pi reads standard AWS credentials from `~/.aws/credentials` or env vars.

```bash
pi auth set bedrock                     # => Prompts for AWS region and profile
                                        # => Reuses standard AWS credential chain
                                        # => No separate key needed; IAM owns auth

aws sts get-caller-identity             # => Sanity check: confirms IAM identity
pi auth list | grep bedrock             # => Confirms bedrock configured
```

**Key Takeaway**: Bedrock auth uses the AWS credential chain — Pi does not duplicate AWS's key storage.

**Why It Matters**: AWS shops already manage credentials via `~/.aws/credentials`, IAM roles, or SSO. Pi reusing that chain means no second copy of long-lived credentials. IAM rotation policies apply to Pi automatically.

### Example 10: Configure Ollama (local models)

Ollama runs LLMs entirely on your machine. Pi talks to a running Ollama daemon at `http://localhost:11434` by default.

```bash
# Start Ollama (separately installed; brew install ollama on macOS)
ollama serve &                          # => Starts Ollama daemon
ollama pull llama3.1                    # => Downloads Llama 3.1 8B locally

pi auth set ollama                      # => Prompts for Ollama URL (default localhost:11434)
                                        # => No API key — local trust boundary
pi run -m ollama:llama3.1 "Hello"       # => Sends request to local Ollama
```

**Key Takeaway**: Ollama provides air-gapped LLM access; Pi connects to it over HTTP without an API key.

**Why It Matters**: Local models matter for offline development, privacy-sensitive prompts, and avoiding per-token costs during iteration. Pi treating Ollama as a peer of cloud providers means you can swap an Ollama model in mid-session via `/model` without restarting the agent or rewriting prompts.

### Example 11: Configure OpenRouter (200+ models)

OpenRouter is a routing layer in front of 200+ models from many providers. One key unlocks broad model access without configuring every upstream provider separately.

```bash
pi auth set openrouter                  # => Prompts for OPENROUTER_API_KEY
                                        # => Single key for 200+ models

pi run -m openrouter:anthropic/claude-3.5-sonnet "Hi"
                                        # => Model name uses upstream provider prefix
                                        # => OpenRouter handles the routing and billing
```

**Key Takeaway**: OpenRouter trades a small per-call fee for the convenience of one credential instead of many.

**Why It Matters**: Experimenting across providers is slow when every provider needs its own account, billing setup, and API key. OpenRouter is a one-credential gateway so you can A/B Claude vs GPT vs Gemini in a single Pi session and only later commit to direct accounts for the winning models.

### Example 12: List configured providers

After configuring multiple providers, list them to confirm which Pi can reach. The list command never prints raw secrets.

```bash
pi auth list                            # => Lists configured providers, e.g.:
                                        # =>   anthropic  configured
                                        # =>   openai     configured
                                        # =>   google     configured
                                        # =>   bedrock    configured (uses AWS chain)
                                        # =>   ollama     configured (http://localhost:11434)
                                        # =>   openrouter configured

pi auth list --verbose                  # => Adds model count per provider
```

**Key Takeaway**: `pi auth list` is the canonical inventory of working credentials; safe to share output for debugging.

**Why It Matters**: When a teammate asks "is your Pi set up right?" the list output is the answer that does not leak secrets. The verbose form adds model coverage so you can see which provider is missing a popular model before a long session begins.

## Session Basics (Examples 13-18)

### Example 13: Start an interactive session

The bare `pi` command starts an interactive TUI session in the current directory. Files in the working directory are available to the agent through built-in file tools.

```bash
cd ~/projects/my-repo                   # => Pi treats this as the session's root
pi                                      # => Launches interactive TUI
                                        # => Streams model output live
                                        # => Multiline input via Enter; submit via Esc-Enter
```

**Key Takeaway**: `pi` with no args is the canonical entry point — it picks up your shell's current directory as project context.

**Why It Matters**: Pi's "current directory is the project" convention matches the way developers actually work — you cd into a repo, then start a tool. Avoiding a separate `pi init` step or workspace selection screen means common path-from-shell-to-agent is short.

### Example 14: Select model for the session

Pass `-m provider:model` on the command line to pick the model for the session up front. This avoids paying for tokens against a default model that is not the one you want.

```bash
pi -m anthropic:claude-3.5-sonnet       # => Anthropic via direct API
pi -m openai:gpt-4o                     # => OpenAI via direct API
pi -m openrouter:google/gemini-1.5-pro  # => Gemini via OpenRouter
pi -m ollama:llama3.1                   # => Local Llama via Ollama
```

**Key Takeaway**: The `-m provider:model` flag is the most common Pi flag — set it once per session to lock in the model.

**Why It Matters**: Different tasks suit different models — refactoring with Claude, coding with GPT-4o, long-context reading with Gemini, offline edits with Llama. Naming the model on the command line keeps the choice explicit in your shell history, which makes session reproducibility trivial.

### Example 15: Switch model mid-session

Inside a session, the `/model` slash command switches the active model without losing conversation history.

```text
> /model openai:gpt-4o
Switched to openai:gpt-4o (was anthropic:claude-3.5-sonnet)

> /model openrouter:google/gemini-1.5-pro
Switched to openrouter:google/gemini-1.5-pro (was openai:gpt-4o)
```

**Key Takeaway**: `/model <provider>:<name>` swaps models inside an existing session; history is preserved and re-sent to the new model.

**Why It Matters**: When a long-context retrieval task fails on a small model, you can promote to a larger model mid-session without restarting. Conversely, when a small model would suffice for the next step, you demote and save token spend. The transcript stays continuous so the new model has all prior context.

### Example 16: View token and cost tracker

Pi tracks input/output tokens and rolling cost per session. Use `/cost` to see the running total.

```text
> /cost
Session: ~/projects/my-repo
Model:   anthropic:claude-3.5-sonnet
Input:   24,331 tokens ($0.073)
Output:   5,019 tokens ($0.075)
Total:               $0.148
```

**Key Takeaway**: `/cost` shows token and dollar totals for the current session, broken down by input/output and per provider.

**Why It Matters**: Cost visibility while you work shapes prompting behaviour — long, low-density prompts become visibly expensive, and you stop sending them. Per-session cost is also the right unit for justifying tool adoption: "this refactor cost me $0.30 in tokens" is a concrete number to put against the time saved.

### Example 17: Send a multi-line prompt

Pi accepts multi-line input by default. Press Enter for a new line; submit the prompt with `Esc-Enter` or `Ctrl-D`.

```text
> Please refactor the function `loadConfig` in
  src/config.ts to:
  - return a Result type
  - propagate validation errors via Either
  - keep the existing tests passing
[Esc-Enter to submit]
```

**Key Takeaway**: Multi-line is the default; the submit key is `Esc-Enter` (or `Ctrl-D`), not bare Enter.

**Why It Matters**: Most useful prompts are longer than a single line — Pi treats them as the common case. The submit shortcut is intentionally a chord so accidental Enters do not fire half-formed prompts at the model. This matters especially when prompts include code blocks pasted from elsewhere.

### Example 18: Exit a session

The `/exit` slash command (or Ctrl-D at the prompt) closes the session and writes the transcript to the session store.

```text
> /exit
Session saved: ~/.config/pi/sessions/2026-05-19T10-31Z-my-repo.jsonl
```

**Key Takeaway**: Sessions persist on exit; the path is printed so you can find the transcript later.

**Why It Matters**: Pi storing sessions by default means useful debugging chats are not lost when the terminal closes. Search and resume operations (Examples 24-25) all run against this on-disk store, so making it the default — not opt-in — keeps the productive path the easy one.

## Built-in Tools (Examples 19-23)

### Example 19: Read a file via the file tool

The built-in `file_read` tool is invoked automatically when you ask Pi about file contents. Pi shows the tool invocation inline.

```text
> What does src/auth.ts export?

[file_read src/auth.ts]
src/auth.ts exports:
- AuthService (class)
- loadCredentials (function)
- AuthError (class)
```

**Key Takeaway**: File reads happen via a structured tool call; the agent never silently slurps your filesystem without showing it.

**Why It Matters**: Transparent tool calls give you an audit trail of what the agent looked at. When an output looks wrong, the visible tool calls let you check whether the agent read the wrong file or the right file but misinterpreted it. Without visible tool calls, this debugging is impossible.

### Example 20: Edit a file via the file tool

The `file_edit` tool applies a structured diff to a file. Pi prints the diff before applying it; in default approval mode, you confirm.

```text
> Add a docstring to loadCredentials.

[file_edit src/auth.ts]
- export function loadCredentials(path: string) {
+ /** Reads credentials from disk; throws AuthError if file is unreadable. */
+ export function loadCredentials(path: string) {

Apply? [y/N]: y
Applied 1 hunk to src/auth.ts.
```

**Key Takeaway**: File edits are diff-shaped, previewable, and confirmable — you see the change before it lands on disk.

**Why It Matters**: Diff-shaped edits scale: a 200-line refactor reads the same as a 2-line one. The preview-then-apply flow stops the agent from silently rewriting your code, which is the single biggest concern developers have when handing edit authority to an AI agent.

### Example 21: Run a shell command via the shell tool

The `shell_run` tool executes commands in your session's working directory. By default Pi prompts for approval before running.

```text
> Run the test suite.

[shell_run]
$ npm test

Apply? [y/N]: y
> jest --coverage
PASS src/auth.test.ts
PASS src/config.test.ts
Tests: 18 passed
Coverage: 87.4%
```

**Key Takeaway**: Shell commands run via the `shell_run` tool with explicit approval; you never lose visibility into what the agent will execute.

**Why It Matters**: Shell access is the most dangerous tool — and the most useful. Pi defaulting to "ask first" rather than "auto-run" means a wrong command costs you a keystroke (decline approval), not a destroyed working tree. Auto-approve mode (Example 22) is opt-in.

### Example 22: Switch approval modes

Pi supports three approval modes: `manual` (default — ask for every tool call), `auto-safe` (auto-approve reads, ask for writes/shell), `auto` (auto-approve all tool calls).

```text
> /approval auto-safe
Approval mode: auto-safe (reads auto-approved; writes/shell ask)

> /approval auto
Approval mode: auto (all tool calls auto-approved)
                      [WARNING: Pi will edit files and run shell commands without confirmation]

> /approval manual
Approval mode: manual (every tool call asks)
```

**Key Takeaway**: `/approval <mode>` tunes the trust level; `auto-safe` is a useful midpoint when you want to read freely but stay cautious about writes.

**Why It Matters**: Different tasks need different trust. Browsing a codebase is fine in `auto-safe` — Pi reads many files and you do not want a popup per read. Performing a destructive refactor warrants `manual`. Coupling approval mode to task type — not to a global setting — keeps the right friction in the right places.

### Example 23: Use the `fetch` tool to read a URL

The `fetch` tool retrieves a URL and feeds it to the agent. Pi treats fetched content as untrusted input — see Advanced for injection-defense patterns.

```text
> Read https://example.com/api-spec.md and tell me what endpoints exist.

[fetch https://example.com/api-spec.md]
Status: 200 OK; 14,330 bytes

Endpoints found:
- GET    /v1/users
- POST   /v1/users
- DELETE /v1/users/:id
```

**Key Takeaway**: `fetch` is a built-in tool; Pi can read URLs without an external curl step but treats the response as untrusted text.

**Why It Matters**: Many tasks involve referencing an online spec, blog post, or RFC. A first-class `fetch` tool removes the awkward step of "copy URL, run curl, paste back" — but Pi's framing of fetched content as untrusted prevents the dominant indirect-prompt-injection attack (a hostile webpage including "ignore previous instructions" in its body).

## Session Management (Examples 24-27)

### Example 24: List saved sessions

Pi stores each session as a JSONL file. `pi session list` enumerates them with timestamps and project paths.

```bash
pi session list                         # => Lists recent sessions
                                        # =>   2026-05-19T10-31Z  my-repo
                                        # =>   2026-05-18T14-02Z  other-project
                                        # =>   2026-05-17T09-44Z  my-repo

pi session list --project my-repo       # => Filter by project
```

**Key Takeaway**: `pi session list` is the entry point to your session history; filter by project to focus on one repo.

**Why It Matters**: Sessions accumulate fast. Without a list-and-filter view, finding "that session where we figured out the migration plan" turns into grep-the-filesystem. The list command keeps the productive workflow ("resume the last session in this project") one command away.

### Example 25: Resume a session

`pi session resume <id>` loads a saved session and re-enters interactive mode with the full transcript preloaded.

```bash
pi session resume 2026-05-19T10-31Z-my-repo
                                        # => Loads transcript from disk
                                        # => Re-sends full history to the active model
                                        # => Drops you back into the TUI with cursor at end

> # Continue the conversation from where you left off
```

**Key Takeaway**: Resume = "load this transcript, replay context, give me the prompt again."

**Why It Matters**: Long-running work spans days. Without resume, every morning starts with "remember what we discussed yesterday about the auth refactor..." — wasted tokens and wasted attention. Resume keeps Pi's understanding of the project continuous across calendar time.

### Example 26: Branch a session at a previous turn

Sessions in Pi are trees, not lists. `/branch` forks the conversation at a previous turn so you can try an alternative approach without losing the original.

```text
> /branch 12
Branched at turn 12. Created branch 'b2' from current 'main'.

> # Now on branch b2 — original is still intact on 'main'

> /branches
* b2    (current; from turn 12)
  main  (4 turns past turn 12)
```

**Key Takeaway**: Sessions are trees. `/branch <turn>` lets you explore alternatives without destroying the original path.

**Why It Matters**: AI conversations frequently hit a fork where two approaches are worth exploring. In a linear chat, you pick one and lose the other. Pi's tree-structured sessions mean both branches stay reachable — you can compare results, revisit the rejected approach later, or merge insights from both.

### Example 27: Share a session via GitHub gist

`pi session share` uploads the session as a GitHub gist and prints the URL. The shared gist includes prompts, model responses, and tool calls.

```bash
pi session share 2026-05-19T10-31Z-my-repo
                                        # => Prompts for GitHub auth on first use
                                        # => Uploads JSONL + a rendered markdown view
                                        # => Prints: https://gist.github.com/.../<id>

pi session share <id> --private         # => Private gist (default is secret)
pi session share <id> --redact-paths    # => Strips absolute paths before upload
```

**Key Takeaway**: `pi session share` turns a local session into a shareable gist; `--redact-paths` prevents leaking filesystem layout.

**Why It Matters**: Asking a teammate "did I prompt this wrong?" used to mean screenshotting a chat. Sharing a Pi session preserves the full structure — every tool call, every model output — so the reviewer can read it the same way you saw it. The redaction flag prevents accidentally leaking project structure when sharing outside the team.

## Next Steps

You now understand Pi's installation, provider configuration, basic tool use, and session management. Continue with [Intermediate Examples 28-54](/en/learn/software-engineering/automation-tools/pi-coding-agent/by-example/intermediate) to learn skills, prompt templates, the four operating modes (interactive, print, RPC, SDK), session branching, and extension installation.
