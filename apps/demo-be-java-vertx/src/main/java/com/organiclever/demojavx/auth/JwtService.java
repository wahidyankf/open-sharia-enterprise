package com.organiclever.demojavx.auth;

import com.auth0.jwt.JWT;
import com.auth0.jwt.algorithms.Algorithm;
import com.auth0.jwt.exceptions.JWTVerificationException;
import com.auth0.jwt.interfaces.DecodedJWT;
import com.organiclever.demojavx.domain.model.User;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Base64;
import java.util.Date;
import java.util.UUID;

public class JwtService {

    private static final String ISSUER = "demo-be-java-vertx";
    private static final long ACCESS_TOKEN_MINUTES = 15;
    private static final long REFRESH_TOKEN_DAYS = 7;

    private final Algorithm algorithm;
    private final RSAPublicKey publicKey;
    private final String keyId;

    public JwtService(String secret) {
        KeyPairGenerator gen;
        try {
            gen = KeyPairGenerator.getInstance("RSA");
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException("RSA not available", e);
        }
        gen.initialize(2048);
        KeyPair pair = gen.generateKeyPair();
        this.publicKey = (RSAPublicKey) pair.getPublic();
        RSAPrivateKey privateKey = (RSAPrivateKey) pair.getPrivate();
        this.algorithm = Algorithm.RSA256(publicKey, privateKey);
        this.keyId = UUID.randomUUID().toString();
        // secret param reserved for future HMAC mode — currently uses RSA
        @SuppressWarnings("unused") String ignoredSecret = secret;
    }

    public TokenPair generateTokenPair(User user) {
        String accessJti = UUID.randomUUID().toString();
        String refreshJti = UUID.randomUUID().toString();
        Instant now = Instant.now();

        String accessToken = JWT.create()
                .withJWTId(accessJti)
                .withSubject(user.id())
                .withIssuer(ISSUER)
                .withClaim("role", user.role())
                .withClaim("username", user.username())
                .withClaim("type", "access")
                .withIssuedAt(Date.from(now))
                .withExpiresAt(Date.from(now.plus(ACCESS_TOKEN_MINUTES, ChronoUnit.MINUTES)))
                .sign(algorithm);

        String refreshToken = JWT.create()
                .withJWTId(refreshJti)
                .withSubject(user.id())
                .withIssuer(ISSUER)
                .withClaim("type", "refresh")
                .withIssuedAt(Date.from(now))
                .withExpiresAt(Date.from(now.plus(REFRESH_TOKEN_DAYS, ChronoUnit.DAYS)))
                .sign(algorithm);

        return new TokenPair(accessToken, refreshToken, accessJti, refreshJti);
    }

    public String generateExpiredRefreshToken(User user) {
        return JWT.create()
                .withJWTId(UUID.randomUUID().toString())
                .withSubject(user.id())
                .withIssuer(ISSUER)
                .withClaim("type", "refresh")
                .withIssuedAt(Date.from(Instant.now().minus(8, ChronoUnit.DAYS)))
                .withExpiresAt(Date.from(Instant.now().minus(1, ChronoUnit.DAYS)))
                .sign(algorithm);
    }

    public Claims validate(String token) throws JWTVerificationException {
        DecodedJWT decoded = JWT.require(algorithm)
                .withIssuer(ISSUER)
                .build()
                .verify(token);
        return new Claims(
                decoded.getId(),
                decoded.getSubject(),
                decoded.getClaim("role").asString(),
                decoded.getClaim("type").asString(),
                decoded.getExpiresAt().toInstant()
        );
    }

    public Claims decode(String token) {
        DecodedJWT decoded = JWT.decode(token);
        return new Claims(
                decoded.getId(),
                decoded.getSubject(),
                decoded.getClaim("role").asString(),
                decoded.getClaim("type").asString(),
                decoded.getExpiresAt() != null ? decoded.getExpiresAt().toInstant() : Instant.now()
        );
    }

    public String getJwks() {
        String n = Base64.getUrlEncoder().withoutPadding()
                .encodeToString(publicKey.getModulus().toByteArray());
        String e = Base64.getUrlEncoder().withoutPadding()
                .encodeToString(publicKey.getPublicExponent().toByteArray());
        return "{\"keys\":[{\"kty\":\"RSA\",\"use\":\"sig\",\"kid\":\""
                + keyId + "\",\"n\":\"" + n + "\",\"e\":\"" + e + "\"}]}";
    }

    public record TokenPair(
            String accessToken,
            String refreshToken,
            String accessJti,
            String refreshJti) {
    }

    public record Claims(
            String jti,
            String subject,
            String role,
            String type,
            Instant expiresAt) {
    }
}
