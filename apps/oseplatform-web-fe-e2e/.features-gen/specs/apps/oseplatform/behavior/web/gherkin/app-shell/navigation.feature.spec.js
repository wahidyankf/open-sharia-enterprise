// Generated from: ../../specs/apps/oseplatform/behavior/web/gherkin/app-shell/navigation.feature
import { test } from "playwright-bdd";

test.describe('Navigation', () => {

  test.beforeEach('Background', async ({ Given }, testInfo) => { if (testInfo.error) return;
    await Given('the app is running'); 
  });
  
  test('Header contains navigation links', async ({ Given, Then, And, page }) => { 
    await Given('the header component is rendered', null, { page }); 
    await Then('the header contains a link to "Updates" at "/updates/"', null, { page }); 
    await And('the header contains a link to "About" at "/about/"', null, { page }); 
    await And('the header contains an external link to "Documentation"', null, { page }); 
    await And('the header contains an external link to "GitHub"', null, { page }); 
  });

  test('Breadcrumb shows ancestor hierarchy without current page', async ({ Given, Then, And, page }) => { 
    await Given('the about page is rendered with breadcrumbs', null, { page }); 
    await Then('the breadcrumb shows "Home" linking to "/"', null, { page }); 
    await And('the current page should not appear in the breadcrumb', null, { page }); 
    await And('all breadcrumb segments should be clickable links', null, { page }); 
    await And('breadcrumb text should wrap naturally without horizontal truncation', null, { page }); 
  });

  test('Previous and next navigation between updates', async ({ Given, Then, And, page }) => { 
    await Given('an update detail page is rendered with adjacent updates', null, { page }); 
    await Then('a "Previous" link is displayed with the previous update title', null, { page }); 
    await And('a "Next" link is displayed with the next update title', null, { page }); 
  });

});

// == technical section ==

test.use({
  $test: [({}, use) => use(test), { scope: 'test', box: true }],
  $uri: [({}, use) => use('../../specs/apps/oseplatform/behavior/web/gherkin/app-shell/navigation.feature'), { scope: 'test', box: true }],
  $bddFileData: [({}, use) => use(bddFileData), { scope: "test", box: true }],
});

const bddFileData = [ // bdd-data-start
  {"pwTestLine":10,"pickleLine":9,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":7,"keywordType":"Context","textWithKeyword":"Given the app is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":11,"gherkinStepLine":10,"keywordType":"Context","textWithKeyword":"Given the header component is rendered","stepMatchArguments":[]},{"pwStepLine":12,"gherkinStepLine":11,"keywordType":"Outcome","textWithKeyword":"Then the header contains a link to \"Updates\" at \"/updates/\"","stepMatchArguments":[{"group":{"start":30,"value":"\"Updates\"","children":[{"start":31,"value":"Updates","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"},{"group":{"start":43,"value":"\"/updates/\"","children":[{"start":44,"value":"/updates/","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":13,"gherkinStepLine":12,"keywordType":"Outcome","textWithKeyword":"And the header contains a link to \"About\" at \"/about/\"","stepMatchArguments":[{"group":{"start":30,"value":"\"About\"","children":[{"start":31,"value":"About","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"},{"group":{"start":41,"value":"\"/about/\"","children":[{"start":42,"value":"/about/","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":14,"gherkinStepLine":13,"keywordType":"Outcome","textWithKeyword":"And the header contains an external link to \"Documentation\"","stepMatchArguments":[{"group":{"start":40,"value":"\"Documentation\"","children":[{"start":41,"value":"Documentation","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":15,"gherkinStepLine":14,"keywordType":"Outcome","textWithKeyword":"And the header contains an external link to \"GitHub\"","stepMatchArguments":[{"group":{"start":40,"value":"\"GitHub\"","children":[{"start":41,"value":"GitHub","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]}]},
  {"pwTestLine":18,"pickleLine":16,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":7,"keywordType":"Context","textWithKeyword":"Given the app is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":19,"gherkinStepLine":17,"keywordType":"Context","textWithKeyword":"Given the about page is rendered with breadcrumbs","stepMatchArguments":[]},{"pwStepLine":20,"gherkinStepLine":18,"keywordType":"Outcome","textWithKeyword":"Then the breadcrumb shows \"Home\" linking to \"/\"","stepMatchArguments":[{"group":{"start":21,"value":"\"Home\"","children":[{"start":22,"value":"Home","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"},{"group":{"start":39,"value":"\"/\"","children":[{"start":40,"value":"/","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":21,"gherkinStepLine":19,"keywordType":"Outcome","textWithKeyword":"And the current page should not appear in the breadcrumb","stepMatchArguments":[]},{"pwStepLine":22,"gherkinStepLine":20,"keywordType":"Outcome","textWithKeyword":"And all breadcrumb segments should be clickable links","stepMatchArguments":[]},{"pwStepLine":23,"gherkinStepLine":21,"keywordType":"Outcome","textWithKeyword":"And breadcrumb text should wrap naturally without horizontal truncation","stepMatchArguments":[]}]},
  {"pwTestLine":26,"pickleLine":23,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":7,"keywordType":"Context","textWithKeyword":"Given the app is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":27,"gherkinStepLine":24,"keywordType":"Context","textWithKeyword":"Given an update detail page is rendered with adjacent updates","stepMatchArguments":[]},{"pwStepLine":28,"gherkinStepLine":25,"keywordType":"Outcome","textWithKeyword":"Then a \"Previous\" link is displayed with the previous update title","stepMatchArguments":[{"group":{"start":2,"value":"\"Previous\"","children":[{"start":3,"value":"Previous","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]},{"pwStepLine":29,"gherkinStepLine":26,"keywordType":"Outcome","textWithKeyword":"And a \"Next\" link is displayed with the next update title","stepMatchArguments":[{"group":{"start":2,"value":"\"Next\"","children":[{"start":3,"value":"Next","children":[{"children":[]}]},{"children":[{"children":[]}]}]},"parameterTypeName":"string"}]}]},
]; // bdd-data-end