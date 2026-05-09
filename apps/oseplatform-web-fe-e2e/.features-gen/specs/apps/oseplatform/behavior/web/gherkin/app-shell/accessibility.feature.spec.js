// Generated from: ../../specs/apps/oseplatform/behavior/web/gherkin/app-shell/accessibility.feature
import { test } from "playwright-bdd";

test.describe('Accessibility compliance', () => {

  test.beforeEach('Background', async ({ Given }, testInfo) => { if (testInfo.error) return;
    await Given('the app is running'); 
  });
  
  test('Home page passes axe-core accessibility scan', async ({ When, Then, page }) => { 
    await When('a visitor opens the home page', null, { page }); 
    await Then('the page should have no accessibility violations', null, { page }); 
  });

  test('Headings follow a proper hierarchy', async ({ When, Then, page }) => { 
    await When('a visitor opens the home page', null, { page }); 
    await Then('headings should follow a proper hierarchy starting with a single h1', null, { page }); 
  });

  test('All interactive elements are keyboard accessible', async ({ When, Then, And, page }) => { 
    await When('a visitor opens the home page', null, { page }); 
    await And('the visitor presses Tab repeatedly', null, { page }); 
    await Then('focus should move through all interactive elements in logical order', null, { page }); 
    await And('no interactive element should be skipped or unreachable by keyboard', null, { page }); 
  });

  test('Text color contrast meets WCAG AA standard', async ({ When, Then, And, page }) => { 
    await When('a visitor opens any page on the site', null, { page }); 
    await Then('all body text should meet a minimum contrast ratio of 4.5:1 against its background', null, { page }); 
    await And('large text and headings should meet a minimum contrast ratio of 3:1 against their background', null, { page }); 
  });

  test('Focus indicators are visible on interactive elements', async ({ When, Then, And, page }) => { 
    await When('a visitor navigates to an interactive element using the keyboard', null, { page }); 
    await Then('a visible focus indicator should be displayed on that element', null, { page }); 
    await And('the focus indicator should have sufficient contrast against the surrounding background', null, { page }); 
  });

});

// == technical section ==

test.use({
  $test: [({}, use) => use(test), { scope: 'test', box: true }],
  $uri: [({}, use) => use('../../specs/apps/oseplatform/behavior/web/gherkin/app-shell/accessibility.feature'), { scope: 'test', box: true }],
  $bddFileData: [({}, use) => use(bddFileData), { scope: "test", box: true }],
});

const bddFileData = [ // bdd-data-start
  {"pwTestLine":10,"pickleLine":10,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":8,"keywordType":"Context","textWithKeyword":"Given the app is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":11,"gherkinStepLine":11,"keywordType":"Action","textWithKeyword":"When a visitor opens the home page","stepMatchArguments":[]},{"pwStepLine":12,"gherkinStepLine":12,"keywordType":"Outcome","textWithKeyword":"Then the page should have no accessibility violations","stepMatchArguments":[]}]},
  {"pwTestLine":15,"pickleLine":14,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":8,"keywordType":"Context","textWithKeyword":"Given the app is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":16,"gherkinStepLine":15,"keywordType":"Action","textWithKeyword":"When a visitor opens the home page","stepMatchArguments":[]},{"pwStepLine":17,"gherkinStepLine":16,"keywordType":"Outcome","textWithKeyword":"Then headings should follow a proper hierarchy starting with a single h1","stepMatchArguments":[]}]},
  {"pwTestLine":20,"pickleLine":18,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":8,"keywordType":"Context","textWithKeyword":"Given the app is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":21,"gherkinStepLine":19,"keywordType":"Action","textWithKeyword":"When a visitor opens the home page","stepMatchArguments":[]},{"pwStepLine":22,"gherkinStepLine":20,"keywordType":"Action","textWithKeyword":"And the visitor presses Tab repeatedly","stepMatchArguments":[]},{"pwStepLine":23,"gherkinStepLine":21,"keywordType":"Outcome","textWithKeyword":"Then focus should move through all interactive elements in logical order","stepMatchArguments":[]},{"pwStepLine":24,"gherkinStepLine":22,"keywordType":"Outcome","textWithKeyword":"And no interactive element should be skipped or unreachable by keyboard","stepMatchArguments":[]}]},
  {"pwTestLine":27,"pickleLine":24,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":8,"keywordType":"Context","textWithKeyword":"Given the app is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":28,"gherkinStepLine":25,"keywordType":"Action","textWithKeyword":"When a visitor opens any page on the site","stepMatchArguments":[]},{"pwStepLine":29,"gherkinStepLine":26,"keywordType":"Outcome","textWithKeyword":"Then all body text should meet a minimum contrast ratio of 4.5:1 against its background","stepMatchArguments":[{"group":{"start":54,"value":"4.5","children":[]},"parameterTypeName":"float"},{"group":{"start":58,"value":"1","children":[]},"parameterTypeName":"int"}]},{"pwStepLine":30,"gherkinStepLine":27,"keywordType":"Outcome","textWithKeyword":"And large text and headings should meet a minimum contrast ratio of 3:1 against their background","stepMatchArguments":[{"group":{"start":64,"value":"3","children":[]},"parameterTypeName":"int"},{"group":{"start":66,"value":"1","children":[]},"parameterTypeName":"int"}]}]},
  {"pwTestLine":33,"pickleLine":29,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":8,"keywordType":"Context","textWithKeyword":"Given the app is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":34,"gherkinStepLine":30,"keywordType":"Action","textWithKeyword":"When a visitor navigates to an interactive element using the keyboard","stepMatchArguments":[]},{"pwStepLine":35,"gherkinStepLine":31,"keywordType":"Outcome","textWithKeyword":"Then a visible focus indicator should be displayed on that element","stepMatchArguments":[]},{"pwStepLine":36,"gherkinStepLine":32,"keywordType":"Outcome","textWithKeyword":"And the focus indicator should have sufficient contrast against the surrounding background","stepMatchArguments":[]}]},
]; // bdd-data-end