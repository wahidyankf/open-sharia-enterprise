package com.organiclever.demojavx.repository.pg;

import com.organiclever.demojavx.domain.model.Attachment;
import com.organiclever.demojavx.repository.AttachmentRepository;
import io.vertx.core.Future;
import io.vertx.sqlclient.Pool;
import java.util.List;
import java.util.Optional;

public class PgAttachmentRepository implements AttachmentRepository {

    private final Pool pool;

    public PgAttachmentRepository(Pool pool) {
        this.pool = pool;
    }

    @Override
    public Future<Attachment> save(Attachment attachment) {
        return Future.failedFuture("PG not implemented yet");
    }

    @Override
    public Future<Optional<Attachment>> findById(String id) {
        return Future.failedFuture("PG not implemented yet");
    }

    @Override
    public Future<List<Attachment>> findByExpenseId(String expenseId) {
        return Future.failedFuture("PG not implemented yet");
    }

    @Override
    public Future<Boolean> deleteById(String id) {
        return Future.failedFuture("PG not implemented yet");
    }

    public Pool getPool() {
        return pool;
    }
}
