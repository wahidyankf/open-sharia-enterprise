---
title: REST Controller Template
description: Template for Spring Boot REST controllers
category: explanation
subcategory: platform-web-templates
tags:
  - spring-boot
  - template
  - rest-controller
last_updated: 2026-01-25
---

# REST Controller Template

```java
package com.oseplatform.[domain].api.rest;

import com.oseplatform.[domain].application.service.*;
import com.oseplatform.[domain].api.dto.*;
import jakarta.validation.Valid;
import org.springframework.data.domain.*;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import java.net.URI;

@RestController
@RequestMapping("/api/v1/[resource]")
@Validated
public class [Resource]Controller {

    private final [Resource]Service service;

    public [Resource]Controller([Resource]Service service) {
        this.service = service;
    }

    @PostMapping
    public ResponseEntity<[Resource]Response> create(
        @Valid @RequestBody Create[Resource]Request request
    ) {
        [Resource]Response response = service.create(request);

        URI location = ServletUriComponentsBuilder
            .fromCurrentRequest()
            .path("/{id}")
            .buildAndExpand(response.id())
            .toUri();

        return ResponseEntity.created(location).body(response);
    }

    @GetMapping("/{id}")
    public ResponseEntity<[Resource]Response> getById(@PathVariable String id) {
        return service.findById(id)
            .map(ResponseEntity::ok)
            .orElse(ResponseEntity.notFound().build());
    }

    @GetMapping
    public ResponseEntity<Page<[Resource]Response>> list(
        @RequestParam(required = false) String filter,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "20") int size,
        @RequestParam(defaultValue = "createdAt,desc") String[] sort
    ) {
        Pageable pageable = PageRequest.of(page, size, Sort.by(parseSort(sort)));
        Page<[Resource]Response> results = service.findAll(filter, pageable);
        return ResponseEntity.ok(results);
    }

    @PutMapping("/{id}")
    public ResponseEntity<[Resource]Response> update(
        @PathVariable String id,
        @Valid @RequestBody Update[Resource]Request request
    ) {
        [Resource]Response response = service.update(id, request);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@PathVariable String id) {
        service.delete(id);
        return ResponseEntity.noContent().build();
    }

    private Sort.Order[] parseSort(String[] sortParams) {
        return java.util.Arrays.stream(sortParams)
            .map(param -> {
                String[] parts = param.split(",");
                String property = parts[0];
                Sort.Direction direction = parts.length > 1 && parts[1].equalsIgnoreCase("desc")
                    ? Sort.Direction.DESC
                    : Sort.Direction.ASC;
                return new Sort.Order(direction, property);
            })
            .toArray(Sort.Order[]::new);
    }
}
```

**Replace**:

- `[domain]` - Your bounded context (e.g., `zakat`, `murabaha`)
- `[resource]` - Resource name lowercase (e.g., `calculations`, `contracts`)
- `[Resource]` - Resource name PascalCase (e.g., `Calculation`, `Contract`)

---

**Last Updated**: 2026-01-25
