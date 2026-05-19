---
title: "Advanced"
date: 2026-05-19T00:00:00+07:00
draft: false
weight: 10000003
description: "Examples 55-80: Pi Coding Agent advanced techniques - TypeScript extension authoring, custom tools, SDK embedding, security hardening, production patterns, multi-step workflows (75-95% coverage)"
tags:
  [
    "pi-coding-agent",
    "pi.dev",
    "advanced",
    "by-example",
    "tutorial",
    "ai-agent",
    "coding-agent",
    "typescript",
    "extensions",
  ]
---

This tutorial provides 26 advanced examples for Pi Coding Agent. Learn TypeScript extension authoring (Examples 55-60), custom tools (Examples 61-64), SDK embedding (Examples 65-68), security hardening (Examples 69-72), production patterns (Examples 73-76), and multi-step workflows (Examples 77-80).

## Extension Authoring (Examples 55-60)

### Example 55: Scaffold a new extension

Extensions are TypeScript packages that register tools, slash commands, keyboard shortcuts, event handlers, or TUI panels. The `pi extension new` command scaffolds a working starter.

```bash
pi extension new acme-pi-extension      # => Creates ./acme-pi-extension/
                                        # => Generates package.json with pi-agent-core dep
                                        # => Generates src/index.ts with example exports
                                        # => Generates tsconfig.json and a README

cd acme-pi-extension
ls                                      # => package.json, src/, tsconfig.json, README.md
cat src/index.ts                        # => See the registration shape (next example)
```

**Key Takeaway**: `pi extension new <name>` is the canonical starting point — never write the boilerplate by hand.

**Why It Matters**: The scaffolded layout is the supported shape Pi understands. Writing your own structure works for a single extension but creates friction when you publish to a registry or update Pi's runtime — the official scaffold tracks runtime changes for you.

### Example 56: Register a tool in an extension

A tool is a TypeScript function with a JSON schema describing its parameters. Pi's agent core invokes it during a conversation when the model requests it.

```typescript
// src/index.ts
import { defineExtension, defineTool } from "@earendil-works/pi-agent-core";
// => defineExtension is the entry point; defineTool registers a tool

export default defineExtension({
  name: "acme-pi-extension",
  // => The extension id used in pi.config.json and on the registry
  tools: [
    defineTool({
      name: "acme.greet",
      // => Tool name uses <extension>.<verb> convention
      description: "Greet a person by name.",
      // => The model reads this to decide when to call the tool
      parameters: {
        type: "object",
        properties: { name: { type: "string" } },
        required: ["name"],
      },
      // => JSON Schema; agent core validates inputs before invocation
      async run({ name }) {
        // => Handler runs in Node; can do anything Node can do
        return { content: `Hello, ${name}!` };
      },
    }),
  ],
});
```

**Key Takeaway**: A tool is a JSON Schema + an async handler; `defineTool` keeps both colocated and type-safe.

**Why It Matters**: Tools are how Pi reaches outside its built-in capabilities — a tool can hit your internal API, query your database, run your build, or anything else. The JSON Schema is the contract the model reads; getting that right is the difference between a tool the model uses correctly and one it ignores or misuses.

### Example 57: Register a slash command

Slash commands run user-driven actions inside a Pi session. They differ from tools in that the user — not the model — triggers them.

```typescript
import { defineExtension, defineSlashCommand } from "@earendil-works/pi-agent-core";

export default defineExtension({
  name: "acme-pi-extension",
  slashCommands: [
    defineSlashCommand({
      name: "stats",
      // => Available as /stats in the TUI
      description: "Show session statistics.",
      async run({ session }) {
        // => `session` object exposes the conversation state
        return {
          content: `Turns: ${session.turns.length}; Tokens: ${session.tokens}`,
        };
      },
    }),
  ],
});
```

**Key Takeaway**: `defineSlashCommand` lets users invoke an extension directly — typed `/stats` is the entry point.

**Why It Matters**: Some actions belong to the user, not the model. Resetting state, exporting a diagram, or running a smoke test are decisions the user makes, not the LLM. Putting them on slash commands keeps the user in control while still leveraging the extension runtime.

### Example 58: Register a keyboard shortcut

Keyboard shortcuts bind a key combination to an extension action. Use them for high-frequency operations where typing a slash command would slow you down.

```typescript
import { defineExtension, defineKeyboardShortcut } from "@earendil-works/pi-agent-core";

export default defineExtension({
  name: "acme-pi-extension",
  keyboardShortcuts: [
    defineKeyboardShortcut({
      key: "Ctrl+Shift+S",
      // => Pi's TUI binds this key combo to the action below
      description: "Save snapshot of current session.",
      async run({ session }) {
        await session.saveSnapshot(`snapshot-${Date.now()}.jsonl`);
        // => session.saveSnapshot writes the current state to disk
      },
    }),
  ],
});
```

**Key Takeaway**: Keyboard shortcuts wire frequent extension actions to a key chord; lower friction than `/slash` for repeat use.

**Why It Matters**: Friction matters when an action is invoked 50 times a day. A keystroke is faster than typing `/save-snapshot` — and faster than even reaching for a slash command list. For extensions used heavily, keyboard shortcuts cross the line from "useful" to "essential."

### Example 59: Register an event handler

Event handlers run in response to lifecycle events: session start, turn complete, tool failure, exit. Use them for logging, metrics, or auto-actions.

```typescript
import { defineExtension, defineEventHandler } from "@earendil-works/pi-agent-core";

export default defineExtension({
  name: "acme-pi-extension",
  eventHandlers: [
    defineEventHandler({
      event: "turn.complete",
      // => Fires after every conversation turn
      async run({ turn }) {
        console.error(`[acme] Turn ${turn.index} took ${turn.durationMs}ms`);
        // => Use stderr for diagnostics so stdout stays clean for JSON mode
      },
    }),
    defineEventHandler({
      event: "tool.failure",
      // => Fires when any tool errors
      async run({ tool, error }) {
        console.error(`[acme] Tool ${tool.name} failed: ${error.message}`);
      },
    }),
  ],
});
```

**Key Takeaway**: Event handlers observe session lifecycle without owning a UI surface.

**Why It Matters**: Many cross-cutting concerns — metrics, audit logs, drift detection, performance tracking — should not occupy slash-command or keyboard real estate. Event handlers are the right place because they run automatically and stay out of the user's way.

### Example 60: Register a TUI panel

TUI panels are extension-rendered widgets shown alongside the conversation. Use them for live information: build status, queue depth, server health, anything periodically refreshing.

```typescript
import { defineExtension, defineTuiPanel } from "@earendil-works/pi-agent-core";

export default defineExtension({
  name: "acme-pi-extension",
  tuiPanels: [
    defineTuiPanel({
      id: "queue-depth",
      // => Panel id; Pi reserves a slot in the TUI for it
      title: "Queue Depth",
      refreshMs: 5000,
      // => Pi calls render() every 5 seconds
      async render() {
        const depth = await fetch("http://localhost:9000/depth").then((r) => r.text());
        return { text: `Pending jobs: ${depth}` };
      },
    }),
  ],
});
```

**Key Takeaway**: TUI panels are first-class extension surfaces — they render alongside the conversation, not inside it.

**Why It Matters**: Some context belongs on screen continuously: are we still building? Is the queue draining? Pi giving extensions a panel slot means an answer to those questions is always visible — no slash command needed, no model token spent.

## Custom Tools (Examples 61-64)

### Example 61: Define a tool schema

JSON Schema is the contract the model reads. Be specific about types, enums, and required fields; the model uses these to decide when and how to call the tool.

```typescript
const greetSchema = {
  type: "object",
  properties: {
    name: { type: "string", description: "Person to greet" },
    formal: {
      type: "boolean",
      default: false,
      description: "Use formal greeting (Mr./Ms.)",
    },
    language: {
      type: "string",
      enum: ["en", "id", "ar"],
      default: "en",
      description: "Greeting language",
    },
  },
  required: ["name"],
  additionalProperties: false,
  // => additionalProperties: false prevents the model passing unknown fields
} as const;
```

**Key Takeaway**: A precise schema (enums, defaults, `additionalProperties: false`) makes the model call your tool correctly the first time.

**Why It Matters**: Models infer parameter usage almost entirely from the schema and description. A vague schema (`type: "object"`) yields unpredictable calls. A precise schema turns the model into a reliable invoker. The 30 minutes spent tightening a schema pays back across every future invocation.

### Example 62: Implement a tool handler

The handler receives validated input and returns a result that the model sees. Throwing surfaces as an error in the conversation; returning structured data lets the model continue cleanly.

```typescript
async function runGreet({ name, formal, language }: GreetInput) {
  if (!name.trim()) {
    // => Empty name — return error result, not throw
    return {
      error: { code: "INVALID_INPUT", message: "Name is required" },
    };
  }

  const titles = { en: "Mr./Ms.", id: "Bapak/Ibu", ar: "السيد" };
  // => Lookup table for formal title per language
  const prefix = formal ? titles[language] + " " : "";

  return {
    content: `${prefix}Hello, ${name}!`,
    // => Plain string content; the model reads this verbatim
  };
}
```

**Key Takeaway**: Return structured errors instead of throwing — the model reads errors and can adjust its next call.

**Why It Matters**: Tools that throw force Pi to surface an exception in the conversation, which the model often reads as "tool unusable; abandon plan." Tools that return structured errors stay usable: the model corrects its input and retries. The difference compounds over multi-step workflows where any tool failure could derail the chain.

### Example 63: Declare tool dependencies

Tools that depend on other tools or on configuration declare their needs up front. Pi enforces dependencies at extension load.

```typescript
defineTool({
  name: "acme.upload",
  description: "Upload a file to acme S3 bucket.",
  parameters: uploadSchema,
  dependencies: {
    config: ["acme.bucket", "acme.region"],
    // => Pi blocks extension load until these config keys exist
    tools: ["file_read"],
    // => Declares that acme.upload internally invokes file_read
  },
  async run({ path }, ctx) {
    const data = await ctx.tools.file_read({ path });
    // => ctx.tools is type-checked against `dependencies.tools`
    return ctx.api.upload(ctx.config.acme.bucket, data);
  },
});
```

**Key Takeaway**: `dependencies` makes implicit requirements explicit — easier to reason about, easier to test, easier to fail fast.

**Why It Matters**: Tools that quietly require config keys or other tools fail late — usually when the model first invokes them in production. Declaring dependencies up front moves the failure to extension load, where it is much easier to diagnose. The runtime contract also doubles as documentation.

### Example 64: Ship and install your extension

Publish the extension to npm or a private registry; users install via `pi extension install`. Local development uses `pi extension link`.

```bash
# Local development — link a working tree
cd acme-pi-extension
pi extension link                       # => Symlinks current dir into ~/.config/pi/extensions/
pi                                      # => Pi loads the extension immediately

# Publishing — push to npm
npm publish                             # => Pushes @your-org/acme-pi-extension

# Installing on another machine
pi extension install @your-org/acme-pi-extension
                                        # => Resolves from npm, installs into ~/.config/pi/extensions/

pi extension list                       # => Shows installed extensions
```

**Key Takeaway**: `pi extension link` for development, `pi extension install` for consumption — same loader, same lifecycle.

**Why It Matters**: A clean publish path matters most when an extension graduates from "useful on my machine" to "team standard." Treating extensions as npm packages means they get versioning, semver, and a familiar install command — no parallel registry to maintain, no bespoke distribution mechanism.

## SDK Embedding (Examples 65-68)

### Example 65: Use pi-agent-core directly

The `@earendil-works/pi-agent-core` package is the embeddable runtime. Use it when you want Pi's reasoning loop inside another Node application.

```typescript
import { createAgent } from "@earendil-works/pi-agent-core";

const agent = createAgent({
  model: "anthropic:claude-3.5-sonnet",
  // => Same model strings as the CLI
  tools: ["file_read", "file_edit", "shell_run"],
  // => Whitelist of built-in tools the embedded agent can use
});

const result = await agent.run("Summarize the diff between main and HEAD.");
console.log(result.content);
// => Embedded agent runs to completion and returns content
```

**Key Takeaway**: `createAgent` returns a fully functional Pi agent that you drive from your own Node code — no CLI, no TUI.

**Why It Matters**: Many teams want agent capability inside a tool they already ship — a CI bot, an internal dashboard, a build orchestrator. The SDK lets you reuse Pi's runtime instead of rebuilding the model-routing, tool-calling, and error-handling code yourself. Your product gets a battle-tested agent; Pi gets distribution.

### Example 66: Custom message handling

Hook into the agent's message stream for custom UI, logging, or transformation. The SDK exposes events for every model token, every tool call, and every result.

```typescript
const agent = createAgent({ model: "anthropic:claude-3.5-sonnet" });

agent.on("token", (token) => {
  // => Fires per streaming token from the model
  process.stdout.write(token);
});

agent.on("tool.call", ({ name, args }) => {
  // => Fires when the model invokes a tool
  console.error(`[tool ${name}]`, args);
});

agent.on("tool.result", ({ name, result }) => {
  // => Fires when a tool returns
  console.error(`[tool ${name} ←]`, result);
});

await agent.run("Refactor src/auth.ts to use Result types.");
```

**Key Takeaway**: The agent emits events; subscribe to whichever you need for your UI or logging layer.

**Why It Matters**: An embedded agent inside a web UI does not stream to a terminal — it streams to a websocket. The event API lets you route Pi's output to wherever your UI lives without forking the agent core. Same applies to centralised logging, audit trails, or replay tooling.

### Example 67: Embed Pi in an Express server

Wrap the agent in an HTTP endpoint to provide AI assistance over a network boundary.

```typescript
import express from "express";
import { createAgent } from "@earendil-works/pi-agent-core";

const app = express();
app.use(express.json());

app.post("/ask", async (req, res) => {
  const agent = createAgent({
    model: "anthropic:claude-3.5-sonnet",
    tools: ["file_read"],
    // => Restrict to safe tools for network-exposed agents
  });

  const result = await agent.run(req.body.prompt);
  res.json({ content: result.content });
});

app.listen(8080);
// => POST /ask {prompt: "..."} now invokes Pi from any HTTP client
```

**Key Takeaway**: A 20-line Express wrapper turns the embedded SDK into an HTTP service — useful for sharing one agent across a team.

**Why It Matters**: Network-shared agents make sense for cost (one API key, one billing record), policy (one tool allowlist for the whole team), and observability (one place to log). The simple embedding pattern shown here scales to a real internal service without changing the agent code itself.

### Example 68: Embed Pi in a CLI tool

Use the SDK inside your own CLI to provide context-aware help. The agent inherits the CLI's cwd as project root automatically.

```typescript
#!/usr/bin/env node
import { createAgent } from "@earendil-works/pi-agent-core";

const [, , subcommand, ...args] = process.argv;
// => e.g. mycli explain src/foo.ts

if (subcommand === "explain") {
  const agent = createAgent({
    model: "anthropic:claude-3.5-sonnet",
    tools: ["file_read"],
  });
  const result = await agent.run(`Explain this file in plain English: ${args[0]}`);
  console.log(result.content);
}
```

**Key Takeaway**: An embedded SDK agent inherits the CLI's cwd and env — no extra wiring needed.

**Why It Matters**: Existing CLI tools often gain AI features by spawning a separate agent CLI as a subprocess. That works but introduces serialization overhead and a second binary to install. Embedding via SDK ships one binary that talks to the agent in-process — simpler distribution, faster startup, cleaner UX.

## Security Hardening (Examples 69-72)

### Example 69: Tool allow/deny lists

Constrain which tools the agent may invoke per-session. Pi reads the list from configuration or the SDK options.

```bash
# Config-file form: ~/.config/pi/pi.config.json
{
  "tools": {
    "allow": ["file_read", "file_edit"],
    "deny":  ["shell_run", "fetch"]
  }
}
                                        # => Agent will refuse to invoke denied tools
                                        # => Allowed list is the positive whitelist
```

**Key Takeaway**: Explicit allow/deny lists make tool privilege a configuration decision, not a runtime gamble.

**Why It Matters**: Some sessions have no business running shell commands or fetching URLs. Locking those tools at the configuration layer means the model cannot use them even when asked. This is the right place to enforce policy — earlier than the approval prompt and harder to bypass.

### Example 70: Sandboxed shell execution

When you must allow shell, run commands inside a sandbox. Pi supports Docker-based sandboxing via the `shell.sandbox` config option.

```json
{
  "shell": {
    "sandbox": "docker",
    "image": "node:24-alpine",
    "readonlyMounts": ["./src"],
    "writableMounts": ["./.pi-scratch"],
    "network": "none"
  }
}
```

```text
                                        # => All shell_run calls now execute inside
                                        #    a fresh container with the listed mounts
                                        # => Network disabled by default — block egress
```

**Key Takeaway**: `shell.sandbox: "docker"` runs every shell command in a fresh container with explicit mounts — never on the host.

**Why It Matters**: Shell access is the highest-blast-radius tool. A misinterpreted instruction that runs `rm -rf` on the host is catastrophic; the same instruction inside a container is recoverable in seconds. Sandboxing turns shell from "ask before running" into "let the agent experiment freely" — a productivity gain disguised as a security control.

### Example 71: Redact secrets in agent output

Pi's `redactPatterns` config strips matched patterns before sending model output to the user (or to logs). Use it to keep secrets out of transcripts.

```json
{
  "redactPatterns": ["AKIA[A-Z0-9]{16}", "ghp_[A-Za-z0-9]{36}", "sk-[A-Za-z0-9]{20,}"]
}
```

```text
[user]   Run `cat .env`
[shell]  AWS_KEY=AKIAEXAMPLE1234567890
[model]  The .env contains AWS_KEY=[REDACTED] and ...
                                        # => Pattern matched; secret replaced before display
                                        # => Also applied to log output and shared gists
```

**Key Takeaway**: `redactPatterns` is a regex list applied to model output, logs, and shared sessions — defence in depth for accidental secret disclosure.

**Why It Matters**: Even careful prompting can produce a transcript containing a secret. Without redaction, that secret persists in saved sessions, gist shares, and audit logs forever. Pattern-based redaction catches the common shapes (AWS keys, GitHub tokens, OpenAI keys) automatically.

### Example 72: Defend against indirect prompt injection

When the agent reads untrusted content (a URL, a downloaded file, a third-party API response), wrap that content in an untrusted-input marker. Skills should be taught to ignore instructions found inside such markers.

```typescript
defineTool({
  name: "acme.fetch_external",
  parameters: { type: "object", properties: { url: { type: "string" } }, required: ["url"] },
  async run({ url }) {
    const body = await fetch(url).then((r) => r.text());
    return {
      content: `<untrusted-input source="${url}">\n${body}\n</untrusted-input>`,
      // => Marker makes the boundary explicit
      // => Agent skills are trained to treat instructions inside markers as data
    };
  },
});
```

**Key Takeaway**: Wrap untrusted tool output in `<untrusted-input>` markers so the agent treats instructions inside as data, not commands.

**Why It Matters**: Indirect prompt injection is the dominant agent-compromise vector through 2026. The attack hides instructions inside a webpage, PDF, or API response that the agent then reads. Marking content as untrusted at the tool boundary — and training skills to ignore in-marker instructions — neutralises the attack class without complicated content scanning.

## Production Patterns (Examples 73-76)

### Example 73: Pi inside CI

CI use cases want one-shot agent runs with predictable inputs and outputs. Pi's `print` mode plus a model-pinned config gives reproducible CI behaviour.

```yaml
# .github/workflows/pi-review.yml
- name: Pi code review
  run: |
    pi print -m anthropic:claude-3.5-sonnet \
             --tools file_read \
             --output json \
             "Review the diff for src/auth.ts and report any issues" \
             > pi-review.json
                                        # => Single one-shot run; deterministic tool set
                                        # => JSON output is parseable downstream
```

**Key Takeaway**: `pi print --output json` is the canonical CI pattern — one-shot, structured, scriptable.

**Why It Matters**: CI usage and interactive usage have different needs. Interactive wants a TUI, branching, mid-session model switching. CI wants a deterministic process that exits with a parseable result. Treating them as different modes — not as the same mode used differently — keeps both flows clean.

### Example 74: Pi behind a webhook

Use the SDK plus a webhook handler to make Pi respond to repository events, chat messages, or any HTTP-triggered workflow.

```typescript
import { createAgent } from "@earendil-works/pi-agent-core";

export async function POST(req: Request) {
  const { issueBody, repo } = await req.json();
  const agent = createAgent({
    model: "anthropic:claude-3.5-sonnet",
    tools: ["file_read"],
    cwd: `/var/repos/${repo}`,
  });

  const result = await agent.run(`Read the issue body and propose a fix as a unified diff:\n${issueBody}`);
  return Response.json({ proposedDiff: result.content });
}
```

**Key Takeaway**: Pi-as-webhook turns the agent into a service that responds to events — issues, PRs, chat — uniformly via HTTP.

**Why It Matters**: Many teams want "Pi reads our GitHub issues and proposes patches." The webhook pattern is the right shape: idempotent, observable, scalable. Treating each invocation as a one-shot run avoids the complexity of long-running stateful agents while still doing useful work.

### Example 75: Metrics and observability

Wrap the agent in an event handler that emits metrics. OpenTelemetry, Prometheus, or any custom system works.

```typescript
agent.on("turn.complete", ({ turn }) => {
  metrics.histogram("pi_turn_duration_ms", turn.durationMs);
  metrics.counter("pi_tokens_total").add(turn.tokens);
});

agent.on("tool.failure", ({ tool, error }) => {
  metrics.counter("pi_tool_failures", { tool: tool.name }).inc();
  log.error({ tool: tool.name, error: error.message }, "Pi tool failure");
});
```

**Key Takeaway**: Pi's event API is the observability hook — emit metrics for every turn, every tool call, every failure.

**Why It Matters**: Production agents need the same visibility as any other production service. Without metrics, debugging a slow agent or a high failure rate becomes guesswork. Wiring metrics into the event stream is a single setup step that pays back every incident later.

### Example 76: Multi-tenant config

Run Pi in a multi-tenant environment by pointing `PI_CONFIG_HOME` at a per-tenant directory. Each tenant gets isolated credentials, extensions, and sessions.

```bash
PI_CONFIG_HOME=/data/tenants/acme/pi   pi --version
PI_CONFIG_HOME=/data/tenants/widgets/pi pi --version
                                        # => Two independent Pi installs
                                        # => Same binary; distinct config + sessions
                                        # => Useful for SaaS / shared infrastructure
```

**Key Takeaway**: `PI_CONFIG_HOME` is the per-tenant isolation knob — no per-binary install required.

**Why It Matters**: SaaS providers running Pi on behalf of customers need strict tenant isolation. Environment-driven config home means a single Pi install serves many tenants without code changes; tenant data lives in distinct directories on disk and never crosses boundaries.

## Multi-step Workflows (Examples 77-80)

### Example 77: Chained tool flow

Some tasks need an explicit chain: read file → analyze → propose diff → apply. Express the chain inside a skill so the agent follows it predictably.

```markdown
<!-- skills/refactor-with-tests.md -->

When asked to refactor a file, follow this fixed sequence:

1. Use `file_read` on the file and on its existing test file.
2. Propose a refactor strategy in a single paragraph.
3. Wait for human approval before any `file_edit`.
4. After edit, use `shell_run` to invoke `npm test -- <file>`.
5. If tests fail, revert with `file_edit` and report the failure.
```

**Key Takeaway**: A skill enforcing a fixed tool order is the simplest reliable workflow — beats prompting "first do X, then Y" every time.

**Why It Matters**: One-off "remember to test" prompts are unreliable. The model forgets midway through a long task. A skill installed once is loaded into every relevant context and consulted by the agent automatically — reliability without per-prompt nagging.

### Example 78: Retry-on-failure pattern

Wrap a tool in retry logic via the event API. Re-running a failed tool with backoff handles transient errors (rate limits, network blips) without involving the model.

```typescript
agent.on("tool.failure", async ({ tool, args, error, retry }) => {
  if (error.code === "RATE_LIMIT" && tool.attempts < 3) {
    await sleep(2 ** tool.attempts * 1000);
    // => Exponential backoff
    return retry();
    // => Pi re-invokes the tool with same args
  }
});
```

**Key Takeaway**: The retry callback on `tool.failure` lets you handle transient errors out-of-band without the model wasting tokens reasoning about them.

**Why It Matters**: Rate-limit errors are mechanical, not semantic. The model should not waste reasoning capacity on them. Handling retries in the event handler keeps the model focused on the task and the agent resilient to API hiccups.

### Example 79: Agent → agent delegation

A parent agent can spawn child agents for sub-tasks. Each child runs its own conversation with its own tool allowlist.

```typescript
defineTool({
  name: "acme.delegate",
  description: "Spawn a sub-agent to handle a focused task.",
  parameters: {
    type: "object",
    properties: { task: { type: "string" }, model: { type: "string" } },
    required: ["task"],
  },
  async run({ task, model }) {
    const child = createAgent({
      model: model ?? "anthropic:claude-3.5-haiku",
      // => Use a cheaper model for sub-tasks by default
      tools: ["file_read"],
      // => Child has narrower tool access than parent
    });
    return child.run(task);
  },
});
```

**Key Takeaway**: A `delegate` tool that internally calls `createAgent` is the simplest fan-out primitive — the parent gets sub-results, the child stays isolated.

**Why It Matters**: Big tasks decompose. Letting the parent agent delegate a side-quest to a child — with a cheaper model and a smaller tool set — saves tokens, isolates risk (a misbehaving child cannot touch the parent's tools), and produces more focused outputs. The downside is one extra round-trip, which is almost always worth it for non-trivial sub-tasks.

### Example 80: End-to-end automation pattern

Combine extension authoring, embedded SDK, sandboxed shell, and event-driven retry into a single autonomous workflow — Pi as a self-driving incremental refactor agent.

```typescript
// scripts/autorefactor.ts
import { createAgent } from "@earendil-works/pi-agent-core";

const targets = ["src/auth.ts", "src/users.ts", "src/billing.ts"];
// => Files to refactor sequentially

for (const file of targets) {
  const agent = createAgent({
    model: "anthropic:claude-3.5-sonnet",
    tools: ["file_read", "file_edit", "shell_run"],
    shell: { sandbox: "docker", image: "node:24-alpine" },
    // => Containerised shell for safe `npm test` invocation
  });

  agent.on("tool.failure", ({ retry, tool }) => {
    if (tool.attempts < 2) return retry();
    // => Two retries before giving up on a single tool call
  });

  const result = await agent.run(
    `Refactor ${file} to use Result types. After edit, run npm test -- ${file}. If tests fail, revert.`,
  );

  console.log(`[${file}]`, result.content);
}
```

**Key Takeaway**: A few hundred lines of TypeScript turn Pi from "interactive coding agent" into "autonomous incremental refactor pipeline" — the same runtime, with extension hooks doing the heavy lifting.

**Why It Matters**: Pi's design pays off most at the autonomous-pipeline end of the spectrum. Every concern is a known hook: model choice, tool list, sandboxing, retry logic, observability. Assembling them into a custom pipeline is straightforward TypeScript, not a fight with the framework. This is the trade-off Pi made — minimal core, deep customisation — delivering on its promise.

## Next Steps

You now have a complete picture of Pi Coding Agent: installation and CLI basics (beginner), skills and multi-mode usage (intermediate), and extension authoring, SDK embedding, and production patterns (this section). Revisit specific examples as you build, and consider publishing your own extensions to share with other Pi users.

For deeper context on Pi's design philosophy and related tools, return to [Overview](/en/learn/software-engineering/automation-tools/pi-coding-agent/overview) or compare with [OpenClaw](/en/learn/software-engineering/automation-tools/openclaw) and [Hermes Agent](/en/learn/software-engineering/automation-tools/hermes-agent).
