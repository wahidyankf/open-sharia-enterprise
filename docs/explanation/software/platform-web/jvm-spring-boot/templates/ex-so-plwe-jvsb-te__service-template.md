---
title: Service Layer Template
description: Template for Spring Boot application services
category: explanation
subcategory: platform-web-templates
tags:
  - spring-boot
  - template
  - service
last_updated: 2026-01-25
---

# Service Layer Template

```java
package com.oseplatform.[domain].application.service;

import com.oseplatform.[domain].domain.model.*;
import com.oseplatform.[domain].domain.repository.*;
import com.oseplatform.[domain].api.dto.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Service
@Transactional
public class [Resource]Service {

    private final Logger logger = LoggerFactory.getLogger([Resource]Service.class);

    private final [Resource]Repository repository;
    private final ApplicationEventPublisher eventPublisher;

    public [Resource]Service(
        [Resource]Repository repository,
        ApplicationEventPublisher eventPublisher
    ) {
        this.repository = repository;
        this.eventPublisher = eventPublisher;
    }

    public [Resource]Response create(Create[Resource]Request request) {
        logger.debug("Creating [resource]: {}", request);

        // Functional core: pure domain logic
        [Resource] [resourceVar] = [Resource].create(
            request.field1(),
            request.field2()
        );

        // Validate business rules
        ValidationResult validation = [resourceVar].validate();
        if (validation.hasErrors()) {
            throw new [Resource]ValidationException(validation.errors());
        }

        // Imperative shell: side effects
        [Resource] saved = repository.save([resourceVar]);
        eventPublisher.publishEvent(new [Resource]CreatedEvent(saved.getId()));

        logger.info("[Resource] created: {}", saved.getId());

        return [Resource]Mapper.toResponse(saved);
    }

    @Transactional(readOnly = true)
    public Optional<[Resource]Response> findById(String id) {
        logger.debug("Finding [resource] by id: {}", id);

        return repository.findById(id)
            .map([Resource]Mapper::toResponse);
    }

    @Transactional(readOnly = true)
    public Page<[Resource]Response> findAll(String filter, Pageable pageable) {
        logger.debug("Finding all [resource]s with filter: {}", filter);

        Page<[Resource]> page = filter != null
            ? repository.findByFilter(filter, pageable)
            : repository.findAll(pageable);

        return page.map([Resource]Mapper::toResponse);
    }

    public [Resource]Response update(String id, Update[Resource]Request request) {
        logger.debug("Updating [resource]: {}", id);

        [Resource] [resourceVar] = repository.findById(id)
            .orElseThrow(() -> new [Resource]NotFoundException(id));

        [resourceVar].update(request.field1(), request.field2());

        [Resource] saved = repository.save([resourceVar]);
        eventPublisher.publishEvent(new [Resource]UpdatedEvent(saved.getId()));

        logger.info("[Resource] updated: {}", id);

        return [Resource]Mapper.toResponse(saved);
    }

    public void delete(String id) {
        logger.debug("Deleting [resource]: {}", id);

        [Resource] [resourceVar] = repository.findById(id)
            .orElseThrow(() -> new [Resource]NotFoundException(id));

        repository.delete([resourceVar]);
        eventPublisher.publishEvent(new [Resource]DeletedEvent(id));

        logger.info("[Resource] deleted: {}", id);
    }
}
```

**Replace**:

- `[domain]` - Your bounded context
- `[resource]` - Resource name lowercase
- `[Resource]` - Resource name PascalCase
- `[resourceVar]` - Resource variable name (camelCase)

---

**Last Updated**: 2026-01-25
