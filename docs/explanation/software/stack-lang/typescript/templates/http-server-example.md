---
title: TypeScript HTTP Server Example
description: Complete HTTP server implementation using Express or Fastify with TypeScript, controllers, middleware, error handling, and testing patterns for REST APIs
category: template
tags:
  - typescript
  - http
  - express
  - fastify
  - rest-api
  - controllers
  - middleware
  - error-handling
  - ts-5.0
  - ts-5.1
  - ts-5.2
  - ts-5.3
  - ts-5.4
  - ts-5.5
  - ts-5.6
  - ts-5.9
related:
  - service-layer-template.md
  - project-structure.md
  - ex-so-stla-ts__best-practices.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
created: 2026-01-24
updated: 2026-01-24
---

# TypeScript HTTP Server Example

Complete HTTP server implementation using Express or Fastify with proper error handling, middleware, and testing.

## Table of Contents

1. [Overview](#overview)
2. [Express Implementation](#express-implementation)
3. [Fastify Implementation](#fastify-implementation)
4. [Controllers](#controllers)
5. [Middleware](#middleware)
6. [Error Handling](#error-handling)
7. [Testing](#testing)

## Overview

This example demonstrates building a production-ready HTTP server with:

- **Express** or **Fastify** as web framework
- TypeScript type safety
- Dependency injection
- Error handling middleware
- Request validation
- Testing with supertest

## Express Implementation

### Complete Server

```typescript
// src/infrastructure/http/express/server.ts
import express, { Express, Request, Response, NextFunction } from "express";
import cors from "cors";
import helmet from "helmet";
import { DonationController } from "./controllers/DonationController";
import { errorHandler } from "./middleware/errorHandler";
import { Logger } from "@/infrastructure/logging/Logger";

export class ExpressServer {
  private app: Express;
  private readonly port: number;

  constructor(
    private readonly donationController: DonationController,
    private readonly logger: Logger,
    port: number = 3000,
  ) {
    this.app = express();
    this.port = port;
    this.setupMiddleware();
    this.setupRoutes();
    this.setupErrorHandling();
  }

  private setupMiddleware(): void {
    this.app.use(helmet()); // Security headers
    this.app.use(cors()); // CORS support
    this.app.use(express.json()); // JSON body parser
    this.app.use(express.urlencoded({ extended: true }));
  }

  private setupRoutes(): void {
    // Health check
    this.app.get("/health", (_req: Request, res: Response) => {
      res.json({ status: "ok", timestamp: new Date().toISOString() });
    });

    // Donation routes
    this.app.post("/api/donations", this.donationController.create.bind(this.donationController));
    this.app.get("/api/donations/:id", this.donationController.getById.bind(this.donationController));
    this.app.post("/api/donations/:id/process", this.donationController.process.bind(this.donationController));
  }

  private setupErrorHandling(): void {
    this.app.use(errorHandler(this.logger));
  }

  start(): void {
    this.app.listen(this.port, () => {
      this.logger.info(`Server listening on port ${this.port}`);
    });
  }

  stop(): void {
    this.logger.info("Shutting down server");
  }

  getApp(): Express {
    return this.app;
  }
}
```

### Controller

```typescript
// src/infrastructure/http/express/controllers/DonationController.ts
import { Request, Response, NextFunction } from "express";
import { ProcessDonationService } from "@/application/services/ProcessDonationService";
import { CreateDonationService } from "@/application/services/CreateDonationService";
import { DonationID } from "@/domain/value-objects/DonationID";
import { UserID } from "@/domain/value-objects/UserID";
import { Money } from "@/domain/value-objects/Money";

export class DonationController {
  constructor(
    private readonly processDonationService: ProcessDonationService,
    private readonly createDonationService: CreateDonationService,
  ) {}

  async create(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { donorId, recipientId, amountCents, currency, createdBy } = req.body;

      const result = await this.createDonationService.execute({
        donorId: DonorID.create(donorId).unwrap(),
        recipientId: RecipientID.create(recipientId).unwrap(),
        amount: Money.fromCents(amountCents, currency).unwrap(),
        createdBy: UserID.create(createdBy).unwrap(),
      });

      if (result.isErr()) {
        return next(result.error);
      }

      res.status(201).json(result.value);
    } catch (error) {
      next(error);
    }
  }

  async getById(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      const donationId = DonationID.create(id);
      if (donationId.isErr()) {
        return next(donationId.error);
      }

      const result = await this.getDonationService.execute({
        donationId: donationId.value,
      });

      if (result.isErr()) {
        return next(result.error);
      }

      res.status(200).json(result.value);
    } catch (error) {
      next(error);
    }
  }

  async process(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const { processedBy } = req.body;

      const result = await this.processDonationService.execute({
        donationId: DonationID.create(id).unwrap(),
        processedBy: UserID.create(processedBy).unwrap(),
      });

      if (result.isErr()) {
        return next(result.error);
      }

      res.status(200).json(result.value);
    } catch (error) {
      next(error);
    }
  }
}
```

### Middleware

```typescript
// src/infrastructure/http/express/middleware/errorHandler.ts
import { Request, Response, NextFunction } from "express";
import { Logger } from "@/infrastructure/logging/Logger";
import { DonationNotFoundError } from "@/application/errors/DonationNotFoundError";

export function errorHandler(logger: Logger) {
  return (err: Error, req: Request, res: Response, next: NextFunction): void => {
    logger.error("HTTP error", { error: err, path: req.path, method: req.method });

    if (err instanceof DonationNotFoundError) {
      res.status(404).json({
        error: "Not Found",
        message: err.message,
      });
      return;
    }

    if (err.name === "ValidationError") {
      res.status(422).json({
        error: "Validation Error",
        message: err.message,
      });
      return;
    }

    res.status(500).json({
      error: "Internal Server Error",
      message: "An unexpected error occurred",
    });
  };
}

// src/infrastructure/http/express/middleware/authentication.ts
import { Request, Response, NextFunction } from "express";

export function authentication(req: Request, res: Response, next: NextFunction): void {
  const authHeader = req.headers.authorization;

  if (!authHeader) {
    res.status(401).json({ error: "Unauthorized", message: "Missing authorization header" });
    return;
  }

  // Verify token logic here
  next();
}
```

## Fastify Implementation

### Complete Server

```typescript
// src/infrastructure/http/fastify/server.ts
import Fastify, { FastifyInstance } from "fastify";
import cors from "@fastify/cors";
import helmet from "@fastify/helmet";
import { DonationController } from "./controllers/DonationController";
import { Logger } from "@/infrastructure/logging/Logger";

export class FastifyServer {
  private server: FastifyInstance;
  private readonly port: number;

  constructor(
    private readonly donationController: DonationController,
    private readonly logger: Logger,
    port: number = 3000,
  ) {
    this.server = Fastify({
      logger: false, // Use custom logger
    });
    this.port = port;
    this.setupPlugins();
    this.setupRoutes();
  }

  private async setupPlugins(): Promise<void> {
    await this.server.register(helmet);
    await this.server.register(cors, {
      origin: true,
    });
  }

  private setupRoutes(): void {
    // Health check
    this.server.get("/health", async () => {
      return { status: "ok", timestamp: new Date().toISOString() };
    });

    // Donation routes
    this.server.post("/api/donations", this.donationController.create.bind(this.donationController));
    this.server.get("/api/donations/:id", this.donationController.getById.bind(this.donationController));
    this.server.post("/api/donations/:id/process", this.donationController.process.bind(this.donationController));
  }

  async start(): Promise<void> {
    try {
      await this.server.listen({ port: this.port, host: "0.0.0.0" });
      this.logger.info(`Server listening on port ${this.port}`);
    } catch (error) {
      this.logger.error("Failed to start server", { error });
      throw error;
    }
  }

  async stop(): Promise<void> {
    await this.server.close();
    this.logger.info("Server stopped");
  }

  getServer(): FastifyInstance {
    return this.server;
  }
}
```

### Controller

```typescript
// src/infrastructure/http/fastify/controllers/DonationController.ts
import { FastifyRequest, FastifyReply } from "fastify";
import { ProcessDonationService } from "@/application/services/ProcessDonationService";
import { CreateDonationService } from "@/application/services/CreateDonationService";

interface CreateDonationBody {
  donorId: string;
  recipientId: string;
  amountCents: number;
  currency: string;
  createdBy: string;
}

interface ProcessDonationParams {
  id: string;
}

interface ProcessDonationBody {
  processedBy: string;
}

export class DonationController {
  constructor(
    private readonly processDonationService: ProcessDonationService,
    private readonly createDonationService: CreateDonationService,
  ) {}

  async create(req: FastifyRequest<{ Body: CreateDonationBody }>, reply: FastifyReply): Promise<void> {
    const { donorId, recipientId, amountCents, currency, createdBy } = req.body;

    const result = await this.createDonationService.execute({
      donorId: DonorID.create(donorId).unwrap(),
      recipientId: RecipientID.create(recipientId).unwrap(),
      amount: Money.fromCents(amountCents, currency).unwrap(),
      createdBy: UserID.create(createdBy).unwrap(),
    });

    if (result.isErr()) {
      throw result.error;
    }

    reply.status(201).send(result.value);
  }

  async getById(req: FastifyRequest<{ Params: { id: string } }>, reply: FastifyReply): Promise<void> {
    const { id } = req.params;

    const donationId = DonationID.create(id);
    if (donationId.isErr()) {
      throw donationId.error;
    }

    const result = await this.getDonationService.execute({
      donationId: donationId.value,
    });

    if (result.isErr()) {
      throw result.error;
    }

    reply.status(200).send(result.value);
  }

  async process(
    req: FastifyRequest<{ Params: ProcessDonationParams; Body: ProcessDonationBody }>,
    reply: FastifyReply,
  ): Promise<void> {
    const { id } = req.params;
    const { processedBy } = req.body;

    const result = await this.processDonationService.execute({
      donationId: DonationID.create(id).unwrap(),
      processedBy: UserID.create(processedBy).unwrap(),
    });

    if (result.isErr()) {
      throw result.error;
    }

    reply.status(200).send(result.value);
  }
}
```

## Controllers

### Best Practices

Controllers should:

- Be thin (orchestrate, don't implement)
- Delegate to application services
- Handle HTTP concerns only (request/response)
- Use proper status codes
- Validate input parameters

```typescript
// ✅ Good: Thin controller
export class DonationController {
  async create(req: Request, res: Response, next: NextFunction): Promise<void> {
    const result = await this.createDonationService.execute(req.body);
    if (result.isErr()) return next(result.error);
    res.status(201).json(result.value);
  }
}

// ❌ Bad: Controller with business logic
export class DonationController {
  async create(req: Request, res: Response): Promise<void> {
    const donation = new Donation();
    donation.amount = req.body.amount;
    if (donation.amount < 100) {
      // Business logic in controller!
      throw new Error("Amount too small");
    }
    await this.repository.save(donation);
    res.status(201).json(donation);
  }
}
```

## Middleware

### Authentication Middleware

```typescript
// src/infrastructure/http/middleware/authentication.ts
import { Request, Response, NextFunction } from "express";
import jwt from "jsonwebtoken";

export function authentication(req: Request, res: Response, next: NextFunction): void {
  try {
    const token = req.headers.authorization?.split(" ")[1];

    if (!token) {
      res.status(401).json({ error: "Unauthorized" });
      return;
    }

    const decoded = jwt.verify(token, process.env.JWT_SECRET!);
    req.user = decoded;
    next();
  } catch (error) {
    res.status(401).json({ error: "Invalid token" });
  }
}
```

### Request Validation Middleware

```typescript
// src/infrastructure/http/middleware/validation.ts
import { Request, Response, NextFunction } from "express";
import { z, ZodSchema } from "zod";

export function validate(schema: ZodSchema) {
  return (req: Request, res: Response, next: NextFunction): void => {
    try {
      schema.parse(req.body);
      next();
    } catch (error) {
      if (error instanceof z.ZodError) {
        res.status(422).json({
          error: "Validation Error",
          details: error.errors,
        });
        return;
      }
      next(error);
    }
  };
}

// Usage
const createDonationSchema = z.object({
  donorId: z.string().uuid(),
  recipientId: z.string().uuid(),
  amountCents: z.number().positive(),
  currency: z.string().length(3),
  createdBy: z.string().uuid(),
});

app.post("/api/donations", validate(createDonationSchema), donationController.create);
```

## Error Handling

### Error Types

```typescript
// src/infrastructure/http/errors/HttpError.ts
export class HttpError extends Error {
  constructor(
    public readonly statusCode: number,
    message: string,
    public readonly details?: unknown,
  ) {
    super(message);
    this.name = this.constructor.name;
  }
}

export class NotFoundError extends HttpError {
  constructor(resource: string, id: string) {
    super(404, `${resource} not found: ${id}`);
  }
}

export class ValidationError extends HttpError {
  constructor(message: string, details?: unknown) {
    super(422, message, details);
  }
}

export class UnauthorizedError extends HttpError {
  constructor(message: string = "Unauthorized") {
    super(401, message);
  }
}
```

### Error Handler

```typescript
// src/infrastructure/http/middleware/errorHandler.ts
import { Request, Response, NextFunction } from "express";
import { HttpError } from "@/infrastructure/http/errors/HttpError";
import { Logger } from "@/infrastructure/logging/Logger";

export function errorHandler(logger: Logger) {
  return (err: Error, req: Request, res: Response, next: NextFunction): void => {
    if (err instanceof HttpError) {
      res.status(err.statusCode).json({
        error: err.name,
        message: err.message,
        details: err.details,
      });
      return;
    }

    logger.error("Unhandled error", { error: err, path: req.path });

    res.status(500).json({
      error: "Internal Server Error",
      message: "An unexpected error occurred",
    });
  };
}
```

## Testing

### Controller Tests

```typescript
// tests/infrastructure/http/controllers/DonationController.test.ts
import { describe, it, expect, vi, beforeEach } from "vitest";
import request from "supertest";
import express, { Express } from "express";
import { DonationController } from "@/infrastructure/http/controllers/DonationController";
import { ProcessDonationService } from "@/application/services/ProcessDonationService";
import { Ok, Err } from "@/shared/result";

describe("DonationController", () => {
  let app: Express;
  let mockProcessDonationService: jest.Mocked<ProcessDonationService>;

  beforeEach(() => {
    mockProcessDonationService = {
      execute: vi.fn(),
    } as any;

    const controller = new DonationController(mockProcessDonationService);

    app = express();
    app.use(express.json());
    app.post("/api/donations/:id/process", controller.process.bind(controller));
  });

  it("should process donation successfully", async () => {
    mockProcessDonationService.execute.mockResolvedValue(
      Ok({
        donationId: DonationID.create("donation-1").unwrap(),
        status: DonationStatus.create("PROCESSED").unwrap(),
        processedAt: new Date(),
      }),
    );

    const response = await request(app)
      .post("/api/donations/donation-1/process")
      .send({ processedBy: "user-1" })
      .expect(200);

    expect(response.body).toMatchObject({
      donationId: expect.any(Object),
      status: expect.any(Object),
    });
  });

  it("should return 404 when donation not found", async () => {
    mockProcessDonationService.execute.mockResolvedValue(
      Err(new DonationNotFoundError(DonationID.create("donation-1").unwrap())),
    );

    await request(app).post("/api/donations/donation-1/process").send({ processedBy: "user-1" }).expect(404);
  });
});
```

### Integration Tests

```typescript
// tests/integration/http/DonationAPI.integration.test.ts
import { describe, it, expect, beforeAll, afterAll } from "vitest";
import request from "supertest";
import { ExpressServer } from "@/infrastructure/http/express/server";
import { DataSource } from "typeorm";

describe("Donation API Integration", () => {
  let server: ExpressServer;
  let dataSource: DataSource;

  beforeAll(async () => {
    dataSource = new DataSource({
      type: "postgres",
      host: "localhost",
      port: 5432,
      username: "test",
      password: "test",
      database: "test_db",
      synchronize: true,
      entities: [DonationEntity],
    });

    await dataSource.initialize();

    // Create server with real dependencies
    const donationRepository = new TypeORMDonationRepository(dataSource.manager);
    const processDonationService = new ProcessDonationService(donationRepository, eventPublisher);
    const donationController = new DonationController(processDonationService, createDonationService);

    server = new ExpressServer(donationController, logger);
  });

  afterAll(async () => {
    await dataSource.destroy();
  });

  it("should create and process donation end-to-end", async () => {
    // Create donation
    const createResponse = await request(server.getApp())
      .post("/api/donations")
      .send({
        donorId: "donor-1",
        recipientId: "recipient-1",
        amountCents: 10000,
        currency: "USD",
        createdBy: "user-1",
      })
      .expect(201);

    const donationId = createResponse.body.donationId;

    // Process donation
    const processResponse = await request(server.getApp())
      .post(`/api/donations/${donationId}/process`)
      .send({ processedBy: "user-2" })
      .expect(200);

    expect(processResponse.body.status).toBe("PROCESSED");
  });
});
```

## Related Documentation

- [Service Layer Template](./service-layer-template.md) - Application services
- [Project Structure](./project-structure.md) - Hexagonal architecture
- [TypeScript Best Practices](../ex-so-stla-ts__best-practices.md) - Coding standards

---

**Last Updated**: 2026-01-24
**TypeScript Version**: 5.0+
**Express Version**: 4.x
**Fastify Version**: 4.x
