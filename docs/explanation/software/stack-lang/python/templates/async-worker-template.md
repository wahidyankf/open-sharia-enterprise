---
title: Async Worker Template
description: Copy-paste ready async worker template with background task processing, retry logic, and error handling using asyncio for Python applications in Sharia-compliant financial services
category: explanation
subcategory: stack-lang
tags:
  - python
  - template
  - async-worker
  - background-tasks
  - asyncio
related:
  - ../ex-so-stla-py__concurrency-and-parallelism.md
principles:
  - explicit-over-implicit
last_updated: 2025-01-23
---

# Async Worker Template

## Overview

Async workers process background tasks with retry logic and error handling. Use for long-running operations, email sending, and batch processing.

**Use this template when**:

- Long-running operations
- Email/notification sending
- Batch processing
- Scheduled tasks

**Examples**: Send Zakat receipts, process donation batches, generate reports

## Complete Code Template

```python
"""Async worker template."""

import asyncio
from decimal import Decimal
from typing import Optional
from dataclasses import dataclass
import logging

logger = logging.getLogger(__name__)


@dataclass
class WorkerTask:
    """Background task definition."""

    task_id: str
    task_type: str
    payload: dict


class AsyncWorker:
    """Async background worker with retry logic."""

    def __init__(self, max_retries: int = 3):
        self.max_retries = max_retries
        self._running = False

    async def start(self):
        """Start worker."""
        self._running = True
        logger.info("Worker started")

        while self._running:
            try:
                task = await self._fetch_task()
                if task:
                    await self._process_task(task)
                else:
                    await asyncio.sleep(1)  # No tasks, wait

            except Exception as e:
                logger.error(f"Worker error: {e}")
                await asyncio.sleep(5)  # Error backoff

    async def stop(self):
        """Stop worker gracefully."""
        self._running = False
        logger.info("Worker stopped")

    async def _fetch_task(self) -> Optional[WorkerTask]:
        """Fetch next task from queue."""
        # Implement task queue fetch
        return None

    async def _process_task(self, task: WorkerTask):
        """Process task with retry logic."""
        for attempt in range(self.max_retries):
            try:
                logger.info(f"Processing task {task.task_id}, attempt {attempt + 1}")

                if task.task_type == "send_zakat_receipt":
                    await self._send_zakat_receipt(task.payload)
                elif task.task_type == "process_batch":
                    await self._process_batch(task.payload)
                else:
                    logger.warning(f"Unknown task type: {task.task_type}")

                logger.info(f"Task {task.task_id} completed")
                return

            except Exception as e:
                logger.error(f"Task {task.task_id} failed (attempt {attempt + 1}): {e}")

                if attempt < self.max_retries - 1:
                    backoff = 2**attempt  # Exponential backoff
                    await asyncio.sleep(backoff)
                else:
                    logger.error(f"Task {task.task_id} exhausted retries")
                    await self._handle_failed_task(task, e)

    async def _send_zakat_receipt(self, payload: dict):
        """Send Zakat receipt email."""
        payer_id = payload["payer_id"]
        zakat_amount = Decimal(payload["zakat_amount"])

        logger.info(f"Sending Zakat receipt for ${zakat_amount} to {payer_id}")

        # Simulate email sending
        await asyncio.sleep(0.1)

        # Send email (implement with actual email service)
        # await email_service.send(...)

        logger.info(f"Receipt sent to {payer_id}")

    async def _process_batch(self, payload: dict):
        """Process batch of calculations."""
        batch_id = payload["batch_id"]
        items = payload["items"]

        logger.info(f"Processing batch {batch_id} with {len(items)} items")

        for item in items:
            # Process each item
            await self._process_item(item)

        logger.info(f"Batch {batch_id} completed")

    async def _process_item(self, item: dict):
        """Process single item."""
        # Implement item processing
        await asyncio.sleep(0.01)  # Simulate work

    async def _handle_failed_task(self, task: WorkerTask, error: Exception):
        """Handle permanently failed task."""
        logger.error(f"Permanently failed task {task.task_id}: {error}")
        # Send to dead letter queue, alert monitoring, etc.


# Usage
async def main():
    worker = AsyncWorker(max_retries=3)

    # Start worker
    worker_task = asyncio.create_task(worker.start())

    # Run for some time
    await asyncio.sleep(60)

    # Stop worker gracefully
    await worker.stop()
    await worker_task


if __name__ == "__main__":
    asyncio.run(main())
```

## Usage Example

```python
# Create and start worker
worker = AsyncWorker(max_retries=3)

# Start in background
asyncio.create_task(worker.start())

# Worker processes tasks automatically
```

## Best Practices

### Do: Use Exponential Backoff

```python
# GOOD: Exponential backoff for retries
backoff = 2**attempt  # 1s, 2s, 4s, 8s, ...
await asyncio.sleep(backoff)
```

### Do: Log Extensively

```python
# GOOD: Comprehensive logging
logger.info(f"Processing task {task_id}")
logger.error(f"Task failed: {error}")
```

## References

- [Concurrency and Parallelism](../ex-so-stla-py__concurrency-and-parallelism.md)

---

**Last Updated**: 2025-01-23
**Python Version**: 3.11+ (baseline), 3.12+ (stable maintenance), 3.13.x (latest stable)
**Maintainers**: OSE Platform Documentation Team
