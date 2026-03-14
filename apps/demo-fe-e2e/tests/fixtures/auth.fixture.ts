import { test as base } from "playwright-bdd";

// World object to carry shared state across steps within a scenario
export type World = {
  adminToken: string;
  userToken: string;
  userId: string;
  adminUserId: string;
  entryId: string;
  attachmentId: string;
};

export const test = base.extend<{ world: World }>({
  world: [
    async ({}, use) => {
      await use({
        adminToken: "",
        userToken: "",
        userId: "",
        adminUserId: "",
        entryId: "",
        attachmentId: "",
      });
    },
    { scope: "test" },
  ],
});
