package com.aademobejasb.unit.attachments;

import com.aademobejasb.unit.steps.BaseUnitCucumberContextConfig;
import com.aademobejasb.unit.steps.UnitTestApplication;
import io.cucumber.spring.CucumberContextConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

/**
 * Cucumber context configuration for the Attachments unit test suite.
 */
@CucumberContextConfiguration
@SpringBootTest(
        classes = UnitTestApplication.class,
        webEnvironment = SpringBootTest.WebEnvironment.NONE)
@ActiveProfiles("unit-test")
public class AttachmentsUnitContextConfig extends BaseUnitCucumberContextConfig {}
