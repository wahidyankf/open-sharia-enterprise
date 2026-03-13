package com.demobejasb.expense.repository;

import com.demobejasb.auth.model.User;
import com.demobejasb.expense.model.Expense;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ExpenseRepository extends JpaRepository<Expense, UUID> {
    Page<Expense> findAllByUser(User user, Pageable pageable);

    Optional<Expense> findByIdAndUser(UUID id, User user);
}
