package com.organiclever.demojavx.repository.pg;

import com.organiclever.demojavx.domain.model.TokenRevocation;
import com.organiclever.demojavx.repository.TokenRevocationRepository;
import io.vertx.core.Future;
import io.vertx.sqlclient.Pool;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.Tuple;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class PgTokenRevocationRepository implements TokenRevocationRepository {

    private final Pool pool;

    public PgTokenRevocationRepository(Pool pool) {
        this.pool = pool;
    }

    @Override
    public Future<TokenRevocation> save(TokenRevocation revocation) {
        String id = UUID.randomUUID().toString();
        Instant now = revocation.revokedAt();
        return pool.preparedQuery(
                        "INSERT INTO revoked_tokens (id, jti, user_id, revoked_at)"
                                + " VALUES ($1::uuid, $2, $3::uuid, $4)"
                                + " ON CONFLICT (jti) DO NOTHING"
                                + " RETURNING id, jti, user_id, revoked_at")
                .execute(Tuple.of(
                        id,
                        revocation.jti(),
                        revocation.userId(),
                        OffsetDateTime.ofInstant(now, java.time.ZoneOffset.UTC)))
                .map(rows -> {
                    if (rows.size() > 0) {
                        return rowToRevocation(rows.iterator().next());
                    }
                    return revocation;
                });
    }

    @Override
    public Future<Boolean> isRevoked(String jti) {
        return pool.preparedQuery("SELECT 1 FROM revoked_tokens WHERE jti = $1")
                .execute(Tuple.of(jti))
                .map(rows -> rows.size() > 0);
    }

    public Future<List<TokenRevocation>> findByUserId(String userId) {
        return pool.preparedQuery(
                        "SELECT id, jti, user_id, revoked_at FROM revoked_tokens"
                                + " WHERE user_id = $1::uuid")
                .execute(Tuple.of(userId))
                .map(rows -> {
                    List<TokenRevocation> result = new ArrayList<>();
                    rows.forEach(row -> result.add(rowToRevocation(row)));
                    return result;
                });
    }

    @Override
    public Future<Void> deleteByUserId(String userId) {
        return pool.preparedQuery("DELETE FROM revoked_tokens WHERE user_id = $1::uuid")
                .execute(Tuple.of(userId))
                .mapEmpty();
    }

    public Pool getPool() {
        return pool;
    }

    private TokenRevocation rowToRevocation(Row row) {
        OffsetDateTime revokedAt = row.getOffsetDateTime("revoked_at");
        Instant instant = revokedAt != null ? revokedAt.toInstant() : Instant.now();
        return new TokenRevocation(
                row.getString("jti"),
                row.getUUID("user_id").toString(),
                instant);
    }
}
