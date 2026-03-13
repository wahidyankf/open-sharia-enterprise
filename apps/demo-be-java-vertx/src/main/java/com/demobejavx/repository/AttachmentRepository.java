package com.demobejavx.repository;

import com.demobejavx.domain.model.Attachment;
import io.vertx.core.Future;
import java.util.List;
import java.util.Optional;

public interface AttachmentRepository {

    Future<Attachment> save(Attachment attachment);

    Future<Optional<Attachment>> findById(String id);

    Future<List<Attachment>> findByExpenseId(String expenseId);

    Future<Boolean> deleteById(String id);
}
