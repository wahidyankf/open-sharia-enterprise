package com.organiclever.demoktkt.integration.steps

import io.cucumber.java.Before

class CucumberHooks {
  @Before
  fun beforeScenario() {
    TestServer.start()
    TestWorld.reset()
  }
}
