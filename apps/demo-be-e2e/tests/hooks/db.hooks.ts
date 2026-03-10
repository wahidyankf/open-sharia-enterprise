import { createBdd } from "playwright-bdd";
import { cleanupDatabase } from "../fixtures/db-cleanup";
import { clearResponse } from "../utils/response-store";
import { clearAll } from "../utils/token-store";

const { Before } = createBdd();

Before(async () => {
  await cleanupDatabase();
  clearAll();
  clearResponse();
});
