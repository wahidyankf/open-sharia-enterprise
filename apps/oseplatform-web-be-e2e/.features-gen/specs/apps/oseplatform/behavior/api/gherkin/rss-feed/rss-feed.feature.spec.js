// Generated from: ../../specs/apps/oseplatform/behavior/api/gherkin/rss-feed/rss-feed.feature
import { test } from "playwright-bdd";

test.describe('RSS Feed', () => {

  test.beforeEach('Background', async ({ Given }, testInfo) => { if (testInfo.error) return;
    await Given('the API is running'); 
  });
  
  test('RSS feed contains valid structure', async ({ Given, When, Then, And, request }) => { 
    await Given('the content repository contains update posts'); 
    await When('the RSS feed is generated', null, { request }); 
    await Then('the feed has a channel with title "OSE Platform Updates"'); 
    await And('the feed has a channel link to the site URL'); 
    await And('the feed contains item elements for each update'); 
  });

  test('RSS feed entries contain required fields', async ({ Given, When, Then, And, request }) => { 
    await Given('the content repository contains an update post with title "Phase 0 End" and date "2026-02-08"'); 
    await When('the RSS feed is generated', null, { request }); 
    await Then('the feed entry has the title "Phase 0 End"'); 
    await And('the feed entry has a publication date'); 
    await And('the feed entry has a link to the update page'); 
    await And('the feed entry has a description'); 
  });

});

// == technical section ==

test.use({
  $test: [({}, use) => use(test), { scope: 'test', box: true }],
  $uri: [({}, use) => use('../../specs/apps/oseplatform/behavior/api/gherkin/rss-feed/rss-feed.feature'), { scope: 'test', box: true }],
  $bddFileData: [({}, use) => use(bddFileData), { scope: "test", box: true }],
});

const bddFileData = [ // bdd-data-start
  {"pwTestLine":10,"pickleLine":9,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":7,"keywordType":"Context","textWithKeyword":"Given the API is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":11,"gherkinStepLine":10,"keywordType":"Context","textWithKeyword":"Given the content repository contains update posts","stepMatchArguments":[]},{"pwStepLine":12,"gherkinStepLine":11,"keywordType":"Action","textWithKeyword":"When the RSS feed is generated","stepMatchArguments":[]},{"pwStepLine":13,"gherkinStepLine":12,"keywordType":"Outcome","textWithKeyword":"Then the feed has a channel with title \"OSE Platform Updates\"","stepMatchArguments":[{"group":{"start":34,"value":"\"OSE Platform Updates\"","children":[{"start":35,"value":"OSE Platform Updates","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":14,"gherkinStepLine":13,"keywordType":"Outcome","textWithKeyword":"And the feed has a channel link to the site URL","stepMatchArguments":[]},{"pwStepLine":15,"gherkinStepLine":14,"keywordType":"Outcome","textWithKeyword":"And the feed contains item elements for each update","stepMatchArguments":[]}]},
  {"pwTestLine":18,"pickleLine":16,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":7,"keywordType":"Context","textWithKeyword":"Given the API is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":19,"gherkinStepLine":17,"keywordType":"Context","textWithKeyword":"Given the content repository contains an update post with title \"Phase 0 End\" and date \"2026-02-08\"","stepMatchArguments":[{"group":{"start":58,"value":"\"Phase 0 End\"","children":[{"start":59,"value":"Phase 0 End","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"},{"group":{"start":81,"value":"\"2026-02-08\"","children":[{"start":82,"value":"2026-02-08","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":20,"gherkinStepLine":18,"keywordType":"Action","textWithKeyword":"When the RSS feed is generated","stepMatchArguments":[]},{"pwStepLine":21,"gherkinStepLine":19,"keywordType":"Outcome","textWithKeyword":"Then the feed entry has the title \"Phase 0 End\"","stepMatchArguments":[{"group":{"start":29,"value":"\"Phase 0 End\"","children":[{"start":30,"value":"Phase 0 End","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":22,"gherkinStepLine":20,"keywordType":"Outcome","textWithKeyword":"And the feed entry has a publication date","stepMatchArguments":[]},{"pwStepLine":23,"gherkinStepLine":21,"keywordType":"Outcome","textWithKeyword":"And the feed entry has a link to the update page","stepMatchArguments":[]},{"pwStepLine":24,"gherkinStepLine":22,"keywordType":"Outcome","textWithKeyword":"And the feed entry has a description","stepMatchArguments":[]}]},
]; // bdd-data-end