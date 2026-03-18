package com.demobejavx.handler;

import com.demobejavx.contracts.HealthResponse;
import io.vertx.core.Handler;
import io.vertx.ext.web.RoutingContext;

public class HealthHandler implements Handler<RoutingContext> {

    @Override
    public void handle(RoutingContext ctx) {
        HealthResponse resp = new HealthResponse().status("UP");
        AuthHandler.sendJson(ctx, 200, resp);
    }
}
