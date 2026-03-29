import { createBdd } from "playwright-bdd";
import { resetDatabase } from "@/utils/api-helpers.js";

const { Before } = createBdd();

// Reset DB before each scenario to ensure clean state
Before(async ({}) => {
  await resetDatabase();
});
