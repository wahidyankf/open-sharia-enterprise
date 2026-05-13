// Generated from: ../../specs/apps/ose-platform/behavior/api/gherkin/search/search.feature
import { test } from "playwright-bdd";

test.describe('Search', () => {

  test.beforeEach('Background', async ({ Given }, testInfo) => { if (testInfo.error) return;
    await Given('the API is running'); 
  });
  
  test('Search returns matching results', async ({ Given, When, Then, And, request }) => { 
    await Given('the search index contains pages about "enterprise" and "compliance"'); 
    await When('a search query "enterprise" is executed', null, { request }); 
    await Then('the results contain pages matching "enterprise"'); 
    await And('each result contains a title, slug, and excerpt'); 
  });

  test('Search with no matches returns empty results', async ({ Given, When, Then, request }) => { 
    await Given('the search index contains pages about "enterprise" and "compliance"'); 
    await When('a search query "nonexistent-term-xyz" is executed', null, { request }); 
    await Then('the results are empty'); 
  });

  test('Search results respect the limit parameter', async ({ Given, When, Then, request }) => { 
    await Given('the search index contains 5 pages matching "phase"'); 
    await When('a search query "phase" is executed with limit 2', null, { request }); 
    await Then('at most 2 results are returned'); 
  });

});

// == technical section ==

test.use({
  $test: [({}, use) => use(test), { scope: 'test', box: true }],
  $uri: [({}, use) => use('../../specs/apps/ose-platform/behavior/api/gherkin/search/search.feature'), { scope: 'test', box: true }],
  $bddFileData: [({}, use) => use(bddFileData), { scope: "test", box: true }],
});

const bddFileData = [ // bdd-data-start
  {"pwTestLine":10,"pickleLine":9,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":7,"keywordType":"Context","textWithKeyword":"Given the API is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":11,"gherkinStepLine":10,"keywordType":"Context","textWithKeyword":"Given the search index contains pages about \"enterprise\" and \"compliance\"","stepMatchArguments":[{"group":{"start":38,"value":"\"enterprise\"","children":[{"start":39,"value":"enterprise","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"},{"group":{"start":55,"value":"\"compliance\"","children":[{"start":56,"value":"compliance","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":12,"gherkinStepLine":11,"keywordType":"Action","textWithKeyword":"When a search query \"enterprise\" is executed","stepMatchArguments":[{"group":{"start":15,"value":"\"enterprise\"","children":[{"start":16,"value":"enterprise","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":13,"gherkinStepLine":12,"keywordType":"Outcome","textWithKeyword":"Then the results contain pages matching \"enterprise\"","stepMatchArguments":[{"group":{"start":35,"value":"\"enterprise\"","children":[{"start":36,"value":"enterprise","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":14,"gherkinStepLine":13,"keywordType":"Outcome","textWithKeyword":"And each result contains a title, slug, and excerpt","stepMatchArguments":[]}]},
  {"pwTestLine":17,"pickleLine":15,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":7,"keywordType":"Context","textWithKeyword":"Given the API is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":18,"gherkinStepLine":16,"keywordType":"Context","textWithKeyword":"Given the search index contains pages about \"enterprise\" and \"compliance\"","stepMatchArguments":[{"group":{"start":38,"value":"\"enterprise\"","children":[{"start":39,"value":"enterprise","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"},{"group":{"start":55,"value":"\"compliance\"","children":[{"start":56,"value":"compliance","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":19,"gherkinStepLine":17,"keywordType":"Action","textWithKeyword":"When a search query \"nonexistent-term-xyz\" is executed","stepMatchArguments":[{"group":{"start":15,"value":"\"nonexistent-term-xyz\"","children":[{"start":16,"value":"nonexistent-term-xyz","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":20,"gherkinStepLine":18,"keywordType":"Outcome","textWithKeyword":"Then the results are empty","stepMatchArguments":[]}]},
  {"pwTestLine":23,"pickleLine":20,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":7,"keywordType":"Context","textWithKeyword":"Given the API is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":24,"gherkinStepLine":21,"keywordType":"Context","textWithKeyword":"Given the search index contains 5 pages matching \"phase\"","stepMatchArguments":[{"group":{"start":26,"value":"5","children":[]},"parameterTypeName":"int"},{"group":{"start":43,"value":"\"phase\"","children":[{"start":44,"value":"phase","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":25,"gherkinStepLine":22,"keywordType":"Action","textWithKeyword":"When a search query \"phase\" is executed with limit 2","stepMatchArguments":[{"group":{"start":15,"value":"\"phase\"","children":[{"start":16,"value":"phase","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"},{"group":{"start":46,"value":"2","children":[]},"parameterTypeName":"int"}]},{"pwStepLine":26,"gherkinStepLine":23,"keywordType":"Outcome","textWithKeyword":"Then at most 2 results are returned","stepMatchArguments":[{"group":{"start":8,"value":"2","children":[]},"parameterTypeName":"int"}]}]},
]; // bdd-data-end