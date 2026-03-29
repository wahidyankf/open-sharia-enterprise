import path from "node:path";
import { processAllIndexFiles } from "../server/content/index-generator";

const mode = process.argv.includes("--validate") ? "validate" : "generate";
const contentDir = path.resolve(process.cwd(), "content");

async function main() {
  const result = await processAllIndexFiles(contentDir, mode as "generate" | "validate");

  if (result.errors.length > 0) {
    console.error("Errors:");
    for (const err of result.errors) {
      console.error(`  ${err}`);
    }
  }

  if (mode === "validate" && result.changed.length > 0) {
    console.error(`${result.changed.length} _index.md file(s) are out of date:`);
    for (const f of result.changed) {
      console.error(`  ${f}`);
    }
    console.error('\nRun "npx tsx src/scripts/generate-indexes.ts" to fix.');
    process.exit(1);
  }

  if (mode === "generate" && result.changed.length > 0) {
    console.log(`Updated ${result.changed.length} _index.md file(s).`);
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
