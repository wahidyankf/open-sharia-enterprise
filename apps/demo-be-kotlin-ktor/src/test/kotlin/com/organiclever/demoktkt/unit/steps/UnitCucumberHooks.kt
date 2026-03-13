package com.organiclever.demoktkt.unit.steps

import io.cucumber.java.Before

class UnitCucumberHooks {
  @Before
  fun beforeScenario() {
    UnitTestServer.start()
    UnitTestWorld.reset()
  }
}
