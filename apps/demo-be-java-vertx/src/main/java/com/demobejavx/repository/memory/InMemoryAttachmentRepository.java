package com.demobejavx.repository.memory;

import com.demobejavx.domain.model.Attachment;
import com.demobejavx.repository.AttachmentRepository;
import io.vertx.core.Future;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

public class InMemoryAttachmentRepository implements AttachmentRepository {

    private final ConcurrentHashMap<String, Attachment> store = new ConcurrentHashMap<>();
    private final AtomicLong idSequence = new AtomicLong(1);

    @Override
    public Future<Attachment> save(Attachment attachment) {
        String id = String.valueOf(idSequence.getAndIncrement());
        Attachment saved = attachment.withId(id);
        store.put(id, saved);
        return Future.succeededFuture(saved);
    }

    @Override
    public Future<Optional<Attachment>> findById(String id) {
        return Future.succeededFuture(Optional.ofNullable(store.get(id)));
    }

    @Override
    public Future<List<Attachment>> findByExpenseId(String expenseId) {
        List<Attachment> result = store.values().stream()
                .filter(a -> a.expenseId().equals(expenseId))
                .toList();
        return Future.succeededFuture(new ArrayList<>(result));
    }

    @Override
    public Future<Boolean> deleteById(String id) {
        boolean removed = store.remove(id) != null;
        return Future.succeededFuture(removed);
    }

    public void reset() {
        store.clear();
        idSequence.set(1);
    }
}
