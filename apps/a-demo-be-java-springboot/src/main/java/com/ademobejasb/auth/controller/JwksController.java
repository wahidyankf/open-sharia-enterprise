package com.ademobejasb.auth.controller;

import com.ademobejasb.security.JwtUtil;
import java.util.List;
import java.util.Map;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class JwksController {

    private final JwtUtil jwtUtil;

    public JwksController(final JwtUtil jwtUtil) {
        this.jwtUtil = jwtUtil;
    }

    @GetMapping("/.well-known/jwks.json")
    public Map<String, Object> getJwks() {
        return Map.of("keys", List.of(jwtUtil.getJwksKey()));
    }
}
