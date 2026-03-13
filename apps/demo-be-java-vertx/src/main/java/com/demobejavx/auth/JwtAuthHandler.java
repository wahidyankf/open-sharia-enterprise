package com.demobejavx.auth;

import com.auth0.jwt.exceptions.JWTVerificationException;
import com.demobejavx.domain.model.User;
import com.demobejavx.repository.TokenRevocationRepository;
import com.demobejavx.repository.UserRepository;
import io.vertx.core.Future;
import io.vertx.ext.web.RoutingContext;
import java.util.Optional;

public class JwtAuthHandler implements io.vertx.core.Handler<RoutingContext> {

    private final JwtService jwtService;
    private final TokenRevocationRepository revocationRepo;
    private final UserRepository userRepo;

    public JwtAuthHandler(JwtService jwtService, TokenRevocationRepository revocationRepo,
            UserRepository userRepo) {
        this.jwtService = jwtService;
        this.revocationRepo = revocationRepo;
        this.userRepo = userRepo;
    }

    @Override
    public void handle(RoutingContext ctx) {
        String authHeader = ctx.request().getHeader("Authorization");
        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            ctx.fail(401);
            return;
        }
        String token = authHeader.substring(7);

        JwtService.Claims claims;
        try {
            claims = jwtService.validate(token);
        } catch (JWTVerificationException e) {
            ctx.fail(401);
            return;
        }

        revocationRepo.isRevoked(claims.jti())
                .compose(revoked -> {
                    if (revoked) {
                        return Future.<Optional<User>>failedFuture("revoked");
                    }
                    return userRepo.findById(claims.subject());
                })
                .onSuccess(userOpt -> {
                    if (userOpt.isEmpty()) {
                        ctx.fail(401);
                        return;
                    }
                    User user = userOpt.get();
                    if (!User.STATUS_ACTIVE.equals(user.status())) {
                        ctx.fail(401);
                        return;
                    }
                    ctx.put("userId", claims.subject());
                    ctx.put("role", claims.role());
                    ctx.put("jti", claims.jti());
                    ctx.next();
                })
                .onFailure(err -> ctx.fail(401));
    }
}
