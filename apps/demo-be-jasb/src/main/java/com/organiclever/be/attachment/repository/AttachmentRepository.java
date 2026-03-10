package com.organiclever.be.attachment.repository;

import com.organiclever.be.attachment.model.Attachment;
import com.organiclever.be.expense.model.Expense;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AttachmentRepository extends JpaRepository<Attachment, UUID> {
    List<Attachment> findAllByExpense(Expense expense);

    Optional<Attachment> findByIdAndExpense(UUID id, Expense expense);
}
