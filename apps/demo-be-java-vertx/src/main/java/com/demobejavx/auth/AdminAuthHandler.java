package com.demobejavx.auth;

import com.demobejavx.domain.model.User;
import io.vertx.ext.web.RoutingContext;

public class AdminAuthHandler implements io.vertx.core.Handler<RoutingContext> {

    @Override
    public void handle(RoutingContext ctx) {
        String role = ctx.get("role");
        if (!User.ROLE_ADMIN.equals(role)) {
            ctx.fail(403);
            return;
        }
        ctx.next();
    }
}
