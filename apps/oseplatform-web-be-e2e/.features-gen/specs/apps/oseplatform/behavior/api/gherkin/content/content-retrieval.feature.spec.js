// Generated from: ../../specs/apps/oseplatform/behavior/api/gherkin/content/content-retrieval.feature
import { test } from "playwright-bdd";

test.describe('Content Retrieval', () => {

  test.beforeEach('Background', async ({ Given }, testInfo) => { if (testInfo.error) return;
    await Given('the API is running'); 
  });
  
  test('Retrieve a page by slug', async ({ Given, When, Then, And, request }) => { 
    await Given('the content repository contains a page with slug "about"'); 
    await When('the content service retrieves the page by slug "about"', null, { request }); 
    await Then('the response contains the page title'); 
    await And('the response contains rendered HTML content'); 
    await And('the response contains extracted headings'); 
  });

  test('List all update posts sorted by date', async ({ Given, When, Then, And, request }) => { 
    await Given('the content repository contains multiple update posts'); 
    await When('the content service lists all updates', null, { request }); 
    await Then('the updates are returned sorted by date descending'); 
    await And('each update contains title, date, summary, and tags'); 
  });

  test('Draft pages are excluded from listings', async ({ Given, When, Then, And, request }) => { 
    await Given('the content repository contains a draft page'); 
    await And('the SHOW_DRAFTS environment variable is not set'); 
    await When('the content service lists all updates', null, { request }); 
    await Then('the draft page is not included in the results'); 
  });

  test('Non-existent slug returns null', async ({ Given, When, Then, request }) => { 
    await Given('the content repository contains no page with slug "nonexistent"'); 
    await When('the content service retrieves the page by slug "nonexistent"', null, { request }); 
    await Then('the response is null'); 
  });

});

// == technical section ==

test.use({
  $test: [({}, use) => use(test), { scope: 'test', box: true }],
  $uri: [({}, use) => use('../../specs/apps/oseplatform/behavior/api/gherkin/content/content-retrieval.feature'), { scope: 'test', box: true }],
  $bddFileData: [({}, use) => use(bddFileData), { scope: "test", box: true }],
});

const bddFileData = [ // bdd-data-start
  {"pwTestLine":10,"pickleLine":9,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":7,"keywordType":"Context","textWithKeyword":"Given the API is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":11,"gherkinStepLine":10,"keywordType":"Context","textWithKeyword":"Given the content repository contains a page with slug \"about\"","stepMatchArguments":[{"group":{"start":49,"value":"\"about\"","children":[{"start":50,"value":"about","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":12,"gherkinStepLine":11,"keywordType":"Action","textWithKeyword":"When the content service retrieves the page by slug \"about\"","stepMatchArguments":[{"group":{"start":47,"value":"\"about\"","children":[{"start":48,"value":"about","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":13,"gherkinStepLine":12,"keywordType":"Outcome","textWithKeyword":"Then the response contains the page title","stepMatchArguments":[]},{"pwStepLine":14,"gherkinStepLine":13,"keywordType":"Outcome","textWithKeyword":"And the response contains rendered HTML content","stepMatchArguments":[]},{"pwStepLine":15,"gherkinStepLine":14,"keywordType":"Outcome","textWithKeyword":"And the response contains extracted headings","stepMatchArguments":[]}]},
  {"pwTestLine":18,"pickleLine":16,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":7,"keywordType":"Context","textWithKeyword":"Given the API is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":19,"gherkinStepLine":17,"keywordType":"Context","textWithKeyword":"Given the content repository contains multiple update posts","stepMatchArguments":[]},{"pwStepLine":20,"gherkinStepLine":18,"keywordType":"Action","textWithKeyword":"When the content service lists all updates","stepMatchArguments":[]},{"pwStepLine":21,"gherkinStepLine":19,"keywordType":"Outcome","textWithKeyword":"Then the updates are returned sorted by date descending","stepMatchArguments":[]},{"pwStepLine":22,"gherkinStepLine":20,"keywordType":"Outcome","textWithKeyword":"And each update contains title, date, summary, and tags","stepMatchArguments":[]}]},
  {"pwTestLine":25,"pickleLine":22,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":7,"keywordType":"Context","textWithKeyword":"Given the API is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":26,"gherkinStepLine":23,"keywordType":"Context","textWithKeyword":"Given the content repository contains a draft page","stepMatchArguments":[]},{"pwStepLine":27,"gherkinStepLine":24,"keywordType":"Context","textWithKeyword":"And the SHOW_DRAFTS environment variable is not set","stepMatchArguments":[]},{"pwStepLine":28,"gherkinStepLine":25,"keywordType":"Action","textWithKeyword":"When the content service lists all updates","stepMatchArguments":[]},{"pwStepLine":29,"gherkinStepLine":26,"keywordType":"Outcome","textWithKeyword":"Then the draft page is not included in the results","stepMatchArguments":[]}]},
  {"pwTestLine":32,"pickleLine":28,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":7,"keywordType":"Context","textWithKeyword":"Given the API is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":33,"gherkinStepLine":29,"keywordType":"Context","textWithKeyword":"Given the content repository contains no page with slug \"nonexistent\"","stepMatchArguments":[{"group":{"start":50,"value":"\"nonexistent\"","children":[{"start":51,"value":"nonexistent","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":34,"gherkinStepLine":30,"keywordType":"Action","textWithKeyword":"When the content service retrieves the page by slug \"nonexistent\"","stepMatchArguments":[{"group":{"start":47,"value":"\"nonexistent\"","children":[{"start":48,"value":"nonexistent","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":35,"gherkinStepLine":31,"keywordType":"Outcome","textWithKeyword":"Then the response is null","stepMatchArguments":[]}]},
]; // bdd-data-end