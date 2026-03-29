package com.ademobejavx.unit.steps;

import com.ademobejavx.unit.UnitFactory;
import com.ademobejavx.support.DirectCallService;
import com.ademobejavx.support.ScenarioState;
import com.ademobejavx.support.ServiceResponse;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.When;

public class UnitUserAccountSteps {

    private final ScenarioState state;

    public UnitUserAccountSteps(ScenarioState state) {
        this.state = state;
    }

    private DirectCallService svc() {
        return UnitFactory.getService();
    }

    @When("^alice sends GET /api/v1/users/me$")
    public void aliceSendsGetMe() throws Exception {
        String token = state.getAccessToken();
        ServiceResponse response = svc().getMe(token);
        state.setLastResponse(response);
    }

    @When("^alice sends PATCH /api/v1/users/me with body \\{ \"displayName\": \"([^\"]*)\" \\}$")
    public void aliceSendsPatchMe(String displayName) throws Exception {
        String token = state.getAccessToken();
        ServiceResponse response = svc().updateMe(token, displayName);
        state.setLastResponse(response);
    }

    @When("^alice sends POST /api/v1/users/me/password with body \\{ \"oldPassword\": \"([^\"]*)\", \"newPassword\": \"([^\"]*)\" \\}$")
    public void aliceSendsChangePassword(String oldPassword, String newPassword) throws Exception {
        String token = state.getAccessToken();
        ServiceResponse response = svc().changePassword(token, oldPassword, newPassword);
        state.setLastResponse(response);
    }

    @When("^alice sends POST /api/v1/users/me/deactivate$")
    public void aliceSendsDeactivate() throws Exception {
        String token = state.getAccessToken();
        ServiceResponse response = svc().deactivateMe(token);
        state.setLastResponse(response);
    }

    @Given("^alice has deactivated her own account via POST /api/v1/users/me/deactivate$")
    public void aliceHasDeactivatedHerAccount() throws Exception {
        aliceSendsDeactivate();
    }

    @When("^the client sends GET /api/v1/users/me with alice's access token$")
    public void clientSendsGetMeWithAlicesToken() throws Exception {
        String token = state.getAccessToken();
        ServiceResponse response = svc().getMe(token);
        state.setLastResponse(response);
    }
}
