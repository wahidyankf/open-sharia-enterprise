// Generated from: ../../specs/apps/oseplatform/behavior/web/gherkin/app-shell/theme.feature
import { test } from "playwright-bdd";

test.describe('Theme', () => {

  test.beforeEach('Background', async ({ Given }, testInfo) => { if (testInfo.error) return;
    await Given('the app is running'); 
  });
  
  test('Default theme is light mode', async ({ Given, Then, page }) => { 
    await Given('the site loads without a stored theme preference', null, { page }); 
    await Then('the theme is set to light mode', null, { page }); 
  });

  test('Theme toggle switches between modes', async ({ Given, When, Then, page }) => { 
    await Given('the site is in light mode', null, { page }); 
    await When('the user clicks the theme toggle and selects dark mode', null, { page }); 
    await Then('the site switches to dark mode', null, { page }); 
  });

});

// == technical section ==

test.use({
  $test: [({}, use) => use(test), { scope: 'test', box: true }],
  $uri: [({}, use) => use('../../specs/apps/oseplatform/behavior/web/gherkin/app-shell/theme.feature'), { scope: 'test', box: true }],
  $bddFileData: [({}, use) => use(bddFileData), { scope: "test", box: true }],
});

const bddFileData = [ // bdd-data-start
  {"pwTestLine":10,"pickleLine":9,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":7,"keywordType":"Context","textWithKeyword":"Given the app is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":11,"gherkinStepLine":10,"keywordType":"Context","textWithKeyword":"Given the site loads without a stored theme preference","stepMatchArguments":[]},{"pwStepLine":12,"gherkinStepLine":11,"keywordType":"Outcome","textWithKeyword":"Then the theme is set to light mode","stepMatchArguments":[]}]},
  {"pwTestLine":15,"pickleLine":13,"tags":[],"steps":[{"pwStepLine":7,"gherkinStepLine":7,"keywordType":"Context","textWithKeyword":"Given the app is running","isBg":true,"stepMatchArguments":[]},{"pwStepLine":16,"gherkinStepLine":14,"keywordType":"Context","textWithKeyword":"Given the site is in light mode","stepMatchArguments":[]},{"pwStepLine":17,"gherkinStepLine":15,"keywordType":"Action","textWithKeyword":"When the user clicks the theme toggle and selects dark mode","stepMatchArguments":[]},{"pwStepLine":18,"gherkinStepLine":16,"keywordType":"Outcome","textWithKeyword":"Then the site switches to dark mode","stepMatchArguments":[]}]},
]; // bdd-data-end