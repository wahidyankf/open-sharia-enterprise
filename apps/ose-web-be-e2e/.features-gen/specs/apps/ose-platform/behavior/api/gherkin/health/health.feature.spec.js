// Generated from: ../../specs/apps/ose-platform/behavior/api/gherkin/health/health.feature
import { test } from "playwright-bdd";

test.describe('Health Check', () => {

  test.beforeEach('Background', async ({ Given }, testInfo) => { if (testInfo.error) return;
    await Given('the API is running'); 
  });
  
  test('Health endpoint returns ok status', async ({ When, Then, request }) => { 
    await When('the health endpoint is called', null, { request }); 
    await Then('the response contains status "ok"'); 
  });

});

// == technical section ==

test.use({
  $test: [({}, use) => use(test), { scope: 'test', box: true }],
  $uri: [({}, use) => use('../../specs/apps/ose-platform/behavior/api/gherkin/health/health.feature'), { scope: 'test', box: true }],
  $bddFileData: [({}, use) => use(bddFileData), { scope: "test", box: true }],
});

const bddFileData = [ // bdd-data-start
  {"pwTestLine":10,"pickleLine":9,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":7,"keywordType":"Context","textWithKeyword":"Given the API is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":11,"gherkinStepLine":10,"keywordType":"Action","textWithKeyword":"When the health endpoint is called","stepMatchArguments":[]},{"pwStepLine":12,"gherkinStepLine":11,"keywordType":"Outcome","textWithKeyword":"Then the response contains status \"ok\"","stepMatchArguments":[{"group":{"start":29,"value":"\"ok\"","children":[{"start":30,"value":"ok","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]}]},
]; // bdd-data-end