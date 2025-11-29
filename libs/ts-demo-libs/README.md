# ts-demo-libs

## Description

Demo TypeScript library demonstrating the flat library structure in the Nx monorepo.

## Public API

This library exports a `greet` function:

```typescript
import { greet } from "@open-sharia-enterprise/ts-demo-libs";

const message = greet("World"); // Returns "Hello, World!"
```

## Build

Build the library:

```bash
nx build ts-demo-libs
```

## Test

Run tests:

```bash
nx test ts-demo-libs
```

## Usage

This library can be imported in any app using the path mapping configured in `tsconfig.base.json`:

```typescript
import { greet } from "@open-sharia-enterprise/ts-demo-libs";
```

The import resolves to `libs/ts-demo-libs/src/index.ts` which exports the public API.
