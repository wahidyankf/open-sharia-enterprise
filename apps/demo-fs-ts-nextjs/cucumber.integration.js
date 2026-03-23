module.exports = {
  default: {
    paths: ["/specs/apps/demo/be/gherkin/**/*.feature"],
    requireModule: ["tsx/cjs"],
    require: ["test/integration/hooks.ts", "test/integration/world.ts", "test/integration/steps/**/*.ts"],
    format: ["progress", "rerun:@rerun.txt"],
    strict: true,
    publishQuiet: true,
  },
};
