# Apps Labs

## Purpose

The `apps-labs/` directory is for **experimental applications and proof-of-concepts (POCs)** that are **outside the Nx monorepo**. Use this space to evaluate framework ergonomics, test new programming languages, and prototype ideas before making production decisions.

## Key Characteristics

- **Not in Nx monorepo** - Independent build systems, no Nx overhead
- **Not for production** - Experimental only, never deployed to production
- **Temporary** - POCs can be deleted when experiments conclude
- **Stack independent** - Test any framework, language, or tooling
- **Low commitment** - Quick prototypes without monorepo integration concerns

## What Belongs Here

### Framework Evaluation

- Testing Next.js vs Remix vs SvelteKit
- Comparing frontend frameworks (React, Vue, Svelte, Solid)
- Evaluating backend frameworks (Express, Fastify, NestJS, Hono)

### Language Exploration

- Python microservices (FastAPI, Django)
- Go services (Gin, Fiber, Echo)
- Rust experiments (Actix, Axum, Rocket)
- Any other language/stack combination

### Technology POCs

- Database evaluations (Postgres vs MySQL vs MongoDB)
- Authentication approaches (OAuth, JWT, session-based)
- State management patterns
- API design explorations (REST, GraphQL, tRPC)

### Quick Prototypes

- UI/UX experiments
- Architecture pattern validation
- Performance testing setups
- Tool evaluation (build tools, testing frameworks)

## What Does NOT Belong Here

❌ **Production apps** → Use `apps/` instead (Nx monorepo)
❌ **Stable experimental Nx apps** → Use `apps/labs-*` naming in `apps/`
❌ **Reusable libraries** → Use `libs/` instead
❌ **Long-term maintained projects** → Graduate to `apps/`

## Naming Convention

No strict naming rules, but prefer descriptive names:

- `nextjs-vs-remix-comparison/` - Framework comparison
- `go-grpc-poc/` - Language/tech POC
- `python-fastapi-demo/` - Language exploration
- `react-query-evaluation/` - Library evaluation

## Lifecycle Management

### Creating an Experiment

1. Create a new directory: `apps-labs/[experiment-name]/`
2. Set up your stack independently (no Nx configuration needed)
3. Add a brief README explaining the experiment goal
4. Experiment freely!

### Graduating to Production

If an experiment proves valuable:

1. Move/recreate the app in `apps/` with Nx integration
2. Follow production naming: `[domain]-[type]` (e.g., `payment-api`)
3. Add proper `project.json` for Nx
4. Delete the original experiment from `apps-labs/`

### Cleanup Policy

**Delete experiments when:**

- Evaluation is complete (decision made)
- POC has been inactive for 3+ months
- Experiment failed/proved unviable
- Functionality has been reimplemented in `apps/`

**Keep a lightweight record:**

- Consider documenting learnings in `docs/explanation/` or `docs/journals/`
- Update `plans/ideas.md` if the experiment inspired new plans

## Structure Example

```
apps-labs/
├── README.md                          # This file
├── nextjs-vs-remix/                   # Framework comparison POC
│   ├── nextjs-demo/
│   │   ├── package.json
│   │   ├── app/
│   │   └── README.md
│   └── remix-demo/
│       ├── package.json
│       ├── app/
│       └── README.md
├── go-grpc-service/                   # Go gRPC POC
│   ├── go.mod
│   ├── main.go
│   └── README.md
└── python-ml-api/                     # Python ML API POC
    ├── requirements.txt
    ├── main.py
    └── README.md
```

## Relationship to Other Directories

| Directory     | Purpose                 | In Nx? | Production? |
| ------------- | ----------------------- | ------ | ----------- |
| `apps/`       | Production applications | ✅ Yes | ✅ Yes      |
| `apps/labs-*` | Experimental Nx apps    | ✅ Yes | ❌ No       |
| `apps-labs/`  | Standalone experiments  | ❌ No  | ❌ No       |
| `libs/`       | Reusable libraries      | ✅ Yes | ✅ Yes      |

## Getting Started

To create your first experiment:

```bash
# Create experiment directory
mkdir apps-labs/my-experiment

# Set up your stack (example: Node.js)
cd apps-labs/my-experiment
npm init -y

# Add a README explaining the goal
echo "# My Experiment\n\nGoal: Testing XYZ framework ergonomics" > README.md

# Start experimenting!
```

## Questions?

- **Where should stable apps go?** → `apps/` (Nx monorepo)
- **Can I use Nx in labs?** → No, use `apps/labs-*` in `apps/` instead
- **How long can experiments stay?** → No strict limit, but clean up completed/stale POCs
- **Can I share code between labs experiments?** → Yes, but consider if it should be a lib in `libs/`

Happy experimenting! 🧪
