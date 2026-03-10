import { createBdd } from "playwright-bdd";

const { Then } = createBdd();

// Stubs — implement alongside production features

Then(
  "the response body should contain {string} equal to {float}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _field: string, _quantity: number) => {
    throw new Error("TODO: not implemented");
  },
);
