#!/usr/bin/env node

const fs = require("fs");
const path = require("path");
const yaml = require("js-yaml");

const AGENTS_DIR = ".opencode/agent";
const SKILLS_DIR = ".opencode/skill";
const AGENTS_MD = "AGENTS.md";
const CLAUDE_MD = "CLAUDE.md";

const GLM_MODELS = ["zai/glm-4.7", "zai/glm-4.5-air", "zai/glm-4.7-flash", "zai/glm-4.7-plus", "inherit"];

const results = {
  passed: 0,
  failed: 0,
  warnings: 0,
  errors: [],
};

function logError(category, message) {
  results.errors.push({ category, message });
  results.failed++;
  console.error("❌ [" + category + "] " + message);
}

function logWarning(category, message) {
  results.errors.push({ category, message, type: "warning" });
  results.warnings++;
  console.warn("⚠️  [" + category + "] " + message);
}

function logSuccess(category, message) {
  results.passed++;
  console.log("✅ [" + category + "] " + message);
}

function validateAgentSchemas() {
  console.log("\n=== Validating Agent Schemas ===\n");

  if (!fs.existsSync(AGENTS_DIR)) {
    logError("AgentSchema", AGENTS_DIR + " directory does not exist");
    return;
  }

  const agentFiles = fs.readdirSync(AGENTS_DIR).filter((f) => f.endsWith(".md"));
  console.log("Found " + agentFiles.length + " agent files");

  for (const agentFile of agentFiles) {
    const filePath = path.join(AGENTS_DIR, agentFile);
    const content = fs.readFileSync(filePath, "utf8");
    const frontmatterMatch = content.match(/^---\n(.*?)\n---/s);

    if (!frontmatterMatch) {
      logError("AgentSchema", agentFile + ": No frontmatter found");
      continue;
    }

    let frontmatter;
    try {
      frontmatter = yaml.load(frontmatterMatch[1]);
    } catch (e) {
      logError("AgentSchema", agentFile + ": Invalid YAML frontmatter - " + e.message);
      continue;
    }

    if (!frontmatter.description) {
      logError("AgentSchema", agentFile + ": Missing description field");
    } else {
      logSuccess("AgentSchema", agentFile + ": Has description field");
    }

    if (!frontmatter.model) {
      logWarning("AgentSchema", agentFile + ": Missing model field (defaults to inherit)");
    } else {
      logSuccess("AgentSchema", agentFile + ": Has model field: " + frontmatter.model);
    }

    const CLAUDE_ALIASES = ["sonnet", "haiku", "opus"];
    if (CLAUDE_ALIASES.includes(frontmatter.model)) {
      logError("AgentSchema", agentFile + ": Uses Claude Code model alias instead of GLM name");
    }
  }
}

function validateModelConfiguration() {
  console.log("\n=== Validating Model Configuration ===\n");

  if (!fs.existsSync(AGENTS_DIR)) {
    logError("ModelConfig", AGENTS_DIR + " directory does not exist");
    return;
  }

  const agentFiles = fs.readdirSync(AGENTS_DIR).filter((f) => f.endsWith(".md"));

  for (const agentFile of agentFiles) {
    const filePath = path.join(AGENTS_DIR, agentFile);
    const content = fs.readFileSync(filePath, "utf8");
    const frontmatterMatch = content.match(/^---\n(.*?)\n---/s);

    if (!frontmatterMatch) continue;

    const frontmatter = yaml.load(frontmatterMatch[1]);
    const model = frontmatter.model || "inherit";

    if (!GLM_MODELS.includes(model)) {
      logError("ModelConfig", agentFile + ": Invalid model (not a GLM model)");
    } else {
      logSuccess("ModelConfig", agentFile + ": Uses valid GLM model: " + model);
    }
  }
}

function validateToolPermissions() {
  console.log("\n=== Validating Tool Permissions ===\n");

  if (!fs.existsSync(AGENTS_DIR)) {
    logError("ToolPermissions", AGENTS_DIR + " directory does not exist");
    return;
  }

  const agentFiles = fs.readdirSync(AGENTS_DIR).filter((f) => f.endsWith(".md"));

  for (const agentFile of agentFiles) {
    const filePath = path.join(AGENTS_DIR, agentFile);
    const content = fs.readFileSync(filePath, "utf8");
    const frontmatterMatch = content.match(/^---\n(.*?)\n---/s);

    if (!frontmatterMatch) continue;

    const frontmatter = yaml.load(frontmatterMatch[1]);
    const tools = frontmatter.tools || {};

    const VALID_TOOLS = ["read", "write", "edit", "bash", "grep", "glob", "webfetch", "websearch", "web-reader"];
    for (const tool of Object.keys(tools)) {
      if (!VALID_TOOLS.includes(tool)) {
        logError("ToolPermissions", agentFile + ": Invalid tool: " + tool);
      }
    }

    if (tools.bash === true || tools.edit === true || tools.write === true) {
      logSuccess("ToolPermissions", agentFile + ": Has full-access tools configured");
    } else {
      logSuccess("ToolPermissions", agentFile + ": Has read-only access configured");
    }
  }
}

function validateSkillsLocation() {
  console.log("\n=== Validating Skills Location ===\n");

  if (!fs.existsSync(SKILLS_DIR)) {
    logError("SkillsLocation", SKILLS_DIR + " directory does not exist");
    return;
  }

  logSuccess("SkillsLocation", SKILLS_DIR + " directory exists");

  const skillDirs = fs.readdirSync(SKILLS_DIR).filter((f) => fs.statSync(path.join(SKILLS_DIR, f)).isDirectory());
  console.log("Found " + skillDirs.length + " skill directories");

  for (const skillDir of skillDirs) {
    const skillFile = path.join(SKILLS_DIR, skillDir, "SKILL.md");
    if (!fs.existsSync(skillFile)) {
      logError("SkillsLocation", skillDir + ": Missing SKILL.md file");
    } else {
      logSuccess("SkillsLocation", skillDir + ": SKILL.md exists");
    }
  }
}

function validateSkillsFrontmatter() {
  console.log("\n=== Validating Skills Frontmatter ===\n");

  if (!fs.existsSync(SKILLS_DIR)) {
    logError("SkillsFrontmatter", SKILLS_DIR + " directory does not exist");
    return;
  }

  const skillDirs = fs.readdirSync(SKILLS_DIR).filter((f) => fs.statSync(path.join(SKILLS_DIR, f)).isDirectory());

  for (const skillDir of skillDirs) {
    const skillFile = path.join(SKILLS_DIR, skillDir, "SKILL.md");

    if (!fs.existsSync(skillFile)) continue;

    const content = fs.readFileSync(skillFile, "utf8");
    const frontmatterMatch = content.match(/^---\n(.*?)\n---/s);

    if (!frontmatterMatch) {
      logError("SkillsFrontmatter", skillDir + ": No frontmatter found");
      continue;
    }

    let frontmatter;
    try {
      frontmatter = yaml.load(frontmatterMatch[1]);
    } catch (e) {
      logError("SkillsFrontmatter", skillDir + ": Invalid YAML frontmatter - " + e.message);
      continue;
    }

    const CLAUDE_FIELDS = ["name", "model", "tags"];
    for (const field of CLAUDE_FIELDS) {
      if (frontmatter[field] !== undefined) {
        logError("SkillsFrontmatter", skillDir + ": Has Claude Code field (should be removed): " + field);
      }
    }

    if (!frontmatter.description) {
      logError("SkillsFrontmatter", skillDir + ": Missing description field");
    } else {
      logSuccess("SkillsFrontmatter", skillDir + ": Has description field");
    }
  }
}

function validateDocumentationCompleteness() {
  console.log("\n=== Validating Documentation Completeness ===\n");

  if (!fs.existsSync(AGENTS_MD)) {
    logError("Documentation", AGENTS_MD + " does not exist");
    return;
  }

  const content = fs.readFileSync(AGENTS_MD, "utf8");

  const REQUIRED_SECTIONS = [
    "# AI Agents",
    "## Agent Catalog",
    "## Agent Format",
    "## Agent Invocation",
    "## Skills",
    "## Maker-Checker-Fixer Workflow",
  ];

  for (const section of REQUIRED_SECTIONS) {
    if (content.includes(section)) {
      logSuccess("Documentation", "AGENTS.md has section: " + section);
    } else {
      logError("Documentation", "AGENTS.md missing section: " + section);
    }
  }

  if (fs.existsSync(CLAUDE_MD)) {
    logWarning("Documentation", CLAUDE_MD + " still exists (should be deleted in Phase 6)");
  }
}

function validateCleanup() {
  console.log("\n=== Validating Cleanup (No Claude Code Artifacts) ===\n");

  const CLAUDE_AGENTS_DIR = ".claude/agents";
  if (fs.existsSync(CLAUDE_AGENTS_DIR)) {
    logWarning("Cleanup", CLAUDE_AGENTS_DIR + " still exists (should be deleted in Phase 6)");
  } else {
    logSuccess("Cleanup", CLAUDE_AGENTS_DIR + " deleted");
  }

  const CLAUDE_SETTINGS = ".claude/settings.json";
  if (fs.existsSync(CLAUDE_SETTINGS)) {
    logWarning("Cleanup", CLAUDE_SETTINGS + " still exists (should be deleted in Phase 6)");
  } else {
    logSuccess("Cleanup", CLAUDE_SETTINGS + " deleted");
  }

  const CONVERSION_SCRIPTS = [
    "scripts/convert-agents-to-opencode.py",
    "scripts/validate-opencode-agents.py",
    "scripts/sync-claude-opencode.py",
  ];

  for (const script of CONVERSION_SCRIPTS) {
    if (fs.existsSync(script)) {
      logWarning("Cleanup", script + " still exists (should be deleted in Phase 6)");
    } else {
      logSuccess("Cleanup", script + " deleted");
    }
  }
}

function main() {
  console.log("========================================");
  console.log("Migration Validation Script");
  console.log("========================================\n");

  validateAgentSchemas();
  validateModelConfiguration();
  validateToolPermissions();
  validateSkillsLocation();
  validateSkillsFrontmatter();
  validateDocumentationCompleteness();
  validateCleanup();

  console.log("\n========================================");
  console.log("Validation Summary");
  console.log("========================================");
  console.log("✅ Passed: " + results.passed);
  console.log("❌ Failed: " + results.failed);
  console.log("⚠️  Warnings: " + results.warnings);
  console.log("\nTotal checks: " + (results.passed + results.failed + results.warnings));

  if (results.failed > 0) {
    console.log("\n❌ Validation FAILED - Fix errors before proceeding");
    process.exit(1);
  } else if (results.warnings > 0) {
    console.log("\n⚠️  Validation PASSED with warnings - Review and fix");
    process.exit(0);
  } else {
    console.log("\n✅ Validation PASSED - All checks successful");
    process.exit(0);
  }
}

if (require.main === module) {
  main();
}

module.exports = {
  validateAgentSchemas,
  validateModelConfiguration,
  validateToolPermissions,
  validateSkillsLocation,
  validateSkillsFrontmatter,
  validateDocumentationCompleteness,
  validateCleanup,
};
