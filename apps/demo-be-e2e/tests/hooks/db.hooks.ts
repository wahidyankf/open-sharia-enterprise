import { createBdd } from "playwright-bdd";
import { cleanupDatabase } from "../fixtures/db-cleanup";
import { clearResponse } from "../utils/response-store";
import { clearToken } from "../utils/token-store";

const { Before } = createBdd();

Before(async () => {
  await cleanupDatabase();
  clearToken();
  clearResponse();
});
