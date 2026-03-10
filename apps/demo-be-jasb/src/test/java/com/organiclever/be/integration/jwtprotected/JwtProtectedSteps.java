package com.organiclever.be.integration.jwtprotected;

import com.organiclever.be.auth.repository.UserRepository;
import com.organiclever.be.integration.steps.TokenStore;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.web.context.WebApplicationContext;

// Most steps for the security feature are provided by AuthSteps in integration.steps package.
// This class provides security-specific steps only.
@Scope("cucumber-glue")
public class JwtProtectedSteps {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private TokenStore tokenStore;

    @Autowired
    private WebApplicationContext webApplicationContext;
}
