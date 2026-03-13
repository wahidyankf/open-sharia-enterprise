package com.demobektkt.unit.steps

import io.cucumber.java.Before

class UnitCucumberHooks {
  @Before
  fun beforeScenario() {
    UnitTestWorld.reset()
  }
}
