---
title: "Template: Framework Integration - XState"
description: Complete XState integration template for TypeScript/JavaScript applications with React hooks
tags:
  - explanation
  - software
  - architecture
  - finite-state-machine
  - fsm
  - template
  - xstate
  - typescript
  - react
  - islamic-finance
last_updated: 2026-01-21
---

# Template: Framework Integration - XState

## Purpose

Complete XState configuration for TypeScript/JavaScript + React applications.

## Prerequisites

- TypeScript 5+
- React 18+ (for React integration)
- XState 5.x

## Example: Donation Campaign FSM with XState + React

### 1. Installation

```bash
npm install xstate @xstate/react
```

### 2. Machine Definition

```typescript
import { createMachine, assign } from "xstate";

interface CampaignContext {
  goalAmount: number;
  totalRaised: number;
  donorCount: number;
  endDate: Date;
}

type CampaignEvent =
  | { type: "LAUNCH" }
  | { type: "DONATE"; amount: number; donorId: string }
  | { type: "GOAL_REACHED" }
  | { type: "COMPLETE" }
  | { type: "EXPIRE" }
  | { type: "CANCEL"; reason: string };

export const campaignMachine = createMachine(
  {
    id: "donation-campaign",
    initial: "planning",
    context: {
      goalAmount: 10000,
      totalRaised: 0,
      donorCount: 0,
      endDate: new Date(),
    } as CampaignContext,
    states: {
      planning: {
        on: {
          LAUNCH: {
            target: "active",
            guard: "isCampaignValid",
            actions: "publishCampaign",
          },
          CANCEL: "cancelled",
        },
      },
      active: {
        entry: ["notifyDonors", "startTimer"],
        exit: "stopTimer",
        on: {
          DONATE: {
            actions: "recordDonation",
          },
          GOAL_REACHED: {
            target: "funded",
            guard: "isGoalMet",
          },
          EXPIRE: "expired",
          CANCEL: "cancelled",
        },
      },
      funded: {
        entry: "notifySuccess",
        on: {
          COMPLETE: {
            target: "completed",
            guard: "areFundsDisbursed",
          },
        },
      },
      completed: {
        type: "final",
        entry: ["disburseFunds", "sendReceipts"],
      },
      expired: {
        type: "final",
        entry: "notifyExpiry",
      },
      cancelled: {
        type: "final",
        entry: "refundDonors",
      },
    },
  },
  {
    guards: {
      isCampaignValid: ({ context }) => {
        return context.goalAmount > 0 && context.endDate > new Date();
      },
      isGoalMet: ({ context }) => {
        return context.totalRaised >= context.goalAmount;
      },
      areFundsDisbursed: ({ context }) => {
        // Business logic for disbursement check
        return true;
      },
    },
    actions: {
      publishCampaign: () => {
        console.log("Campaign published");
      },
      recordDonation: assign({
        totalRaised: ({ context, event }) => {
          if (event.type === "DONATE") {
            return context.totalRaised + event.amount;
          }
          return context.totalRaised;
        },
        donorCount: ({ context, event }) => {
          if (event.type === "DONATE") {
            return context.donorCount + 1;
          }
          return context.donorCount;
        },
      }),
      notifyDonors: () => {
        console.log("Notifying donors");
      },
      startTimer: () => {
        console.log("Starting campaign timer");
      },
      stopTimer: () => {
        console.log("Stopping campaign timer");
      },
      notifySuccess: () => {
        console.log("Goal reached!");
      },
      disburseFunds: () => {
        console.log("Disbursing funds to beneficiary");
      },
      sendReceipts: () => {
        console.log("Sending donation receipts");
      },
      notifyExpiry: () => {
        console.log("Campaign expired");
      },
      refundDonors: () => {
        console.log("Initiating refunds");
      },
    },
  },
);
```

### 3. React Integration

```typescript
import { useMachine } from '@xstate/react';
import { campaignMachine } from './campaignMachine';

export function CampaignDashboard() {
  const [state, send] = useMachine(campaignMachine);

  const handleDonate = (amount: number) => {
    send({ type: 'DONATE', amount, donorId: 'user123' });

    // Check if goal reached
    if (state.context.totalRaised + amount >= state.context.goalAmount) {
      send({ type: 'GOAL_REACHED' });
    }
  };

  return (
    <div className="campaign-dashboard">
      <h1>Campaign Status: {state.value}</h1>

      <div className="progress">
        <p>Goal: ${state.context.goalAmount}</p>
        <p>Raised: ${state.context.totalRaised}</p>
        <p>Donors: {state.context.donorCount}</p>
        <progress
          value={state.context.totalRaised}
          max={state.context.goalAmount}
        />
      </div>

      <div className="actions">
        {state.matches('planning') && (
          <>
            <button onClick={() => send({ type: 'LAUNCH' })}>
              Launch Campaign
            </button>
            <button onClick={() => send({ type: 'CANCEL', reason: 'User cancelled' })}>
              Cancel
            </button>
          </>
        )}

        {state.matches('active') && (
          <>
            <button onClick={() => handleDonate(100)}>
              Donate $100
            </button>
            <button onClick={() => send({ type: 'CANCEL', reason: 'Admin stop' })}>
              Stop Campaign
            </button>
          </>
        )}

        {state.matches('funded') && (
          <button onClick={() => send({ type: 'COMPLETE' })}>
            Complete Campaign
          </button>
        )}

        {(state.matches('completed') || state.matches('expired') || state.matches('cancelled')) && (
          <p>Campaign finished</p>
        )}
      </div>
    </div>
  );
}
```

### 4. Persistence with Backend

```typescript
import { Actor, createActor } from "xstate";
import { campaignMachine } from "./campaignMachine";

class CampaignService {
  private actors: Map<string, Actor<typeof campaignMachine>> = new Map();

  async loadCampaign(campaignId: string) {
    // Load from backend
    const savedState = await fetch(`/api/campaigns/${campaignId}/state`).then((res) => res.json());

    // Create actor with persisted state
    const actor = createActor(campaignMachine, {
      snapshot: savedState,
    });

    // Subscribe to state changes for persistence
    actor.subscribe((state) => {
      this.persistState(campaignId, state);
    });

    actor.start();
    this.actors.set(campaignId, actor);

    return actor;
  }

  async persistState(campaignId: string, state: any) {
    await fetch(`/api/campaigns/${campaignId}/state`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(state),
    });
  }

  sendEvent(campaignId: string, event: CampaignEvent) {
    const actor = this.actors.get(campaignId);
    if (actor) {
      actor.send(event);
    }
  }
}

export const campaignService = new CampaignService();
```

### 5. Testing

```typescript
import { describe, it, expect } from "vitest";
import { createActor } from "xstate";
import { campaignMachine } from "./campaignMachine";

describe("Campaign State Machine", () => {
  it("should transition from planning to active on launch", () => {
    const actor = createActor(campaignMachine).start();

    expect(actor.getSnapshot().value).toBe("planning");

    actor.send({ type: "LAUNCH" });

    expect(actor.getSnapshot().value).toBe("active");
  });

  it("should update context on donation", () => {
    const actor = createActor(campaignMachine).start();

    actor.send({ type: "LAUNCH" });
    actor.send({ type: "DONATE", amount: 500, donorId: "donor1" });

    const snapshot = actor.getSnapshot();
    expect(snapshot.context.totalRaised).toBe(500);
    expect(snapshot.context.donorCount).toBe(1);
  });

  it("should transition to funded when goal met", () => {
    const actor = createActor(campaignMachine, {
      snapshot: {
        value: "active",
        context: {
          goalAmount: 1000,
          totalRaised: 1000,
          donorCount: 10,
          endDate: new Date(),
        },
      },
    }).start();

    actor.send({ type: "GOAL_REACHED" });

    expect(actor.getSnapshot().value).toBe("funded");
  });

  it("should prevent launch with invalid campaign", () => {
    const actor = createActor(campaignMachine, {
      snapshot: {
        value: "planning",
        context: {
          goalAmount: 0, // Invalid goal
          totalRaised: 0,
          donorCount: 0,
          endDate: new Date(),
        },
      },
    }).start();

    actor.send({ type: "LAUNCH" });

    // Should remain in planning due to guard failure
    expect(actor.getSnapshot().value).toBe("planning");
  });
});
```

---

## Key XState Concepts

1. **Machine definition**: Use `createMachine()` with config object
2. **Context**: Typed extended state using TypeScript
3. **Guards**: Boolean functions in `guards` config
4. **Actions**: Side effects in `actions` config, use `assign()` for context updates
5. **React integration**: Use `useMachine()` hook
6. **Persistence**: Subscribe to state changes, save/restore snapshots

## XState Advantages

- **Type safety**: Full TypeScript support
- **Visualization**: Built-in visualizer at https://stately.ai/viz
- **Debugging**: Time-travel debugging in browser dev tools
- **Hierarchical states**: Nested states via `states` within states
- **Parallel states**: Use `type: 'parallel'` for concurrent regions

## Common Pitfalls

- **Forgetting `assign()`**: Context updates require `assign()` action
- **Guard timing**: Guards evaluated before transition, actions after
- **Event payload**: Type events correctly for TypeScript inference

## Related Documentation

- XState Docs: https://stately.ai/docs/xstate
- [Declarative FSM Approaches](../ex-so-ar-fsm__10-declarative-and-dsl-approaches.md)
- [Testing FSMs](../ex-so-ar-fsm__12-testing-fsm-implementations.md)
