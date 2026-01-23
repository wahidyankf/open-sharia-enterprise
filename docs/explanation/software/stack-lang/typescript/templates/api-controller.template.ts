/**
 * API Controller Template
 * Handles HTTP requests with validation.
 */

import { Router, Request, Response } from "express";
import { z } from "zod";

const createEntitySchema = z.object({
  propertyName: z.string(),
  amount: z.number().positive(),
});

export class EntityController {
  private router: Router;

  constructor(private readonly useCase: UseCaseName) {
    this.router = Router();
    this.setupRoutes();
  }

  private setupRoutes(): void {
    this.router.post("/", this.create.bind(this));
    this.router.get("/:id", this.getById.bind(this));
  }

  private async create(req: Request, res: Response): Promise<void> {
    try {
      const data = createEntitySchema.parse(req.body);
      const result = await this.useCase.execute(data);

      if (!result.ok) {
        res.status(400).json({ error: result.error.message });
        return;
      }

      res.status(201).json(result.value);
    } catch (error) {
      res.status(500).json({ error: "Internal server error" });
    }
  }

  private async getById(req: Request, res: Response): Promise<void> {
    // Implementation
  }

  getRouter(): Router {
    return this.router;
  }
}
