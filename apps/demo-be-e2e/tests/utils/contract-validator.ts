import Ajv from "ajv";
import addFormats from "ajv-formats";
import { readFileSync } from "fs";
import { resolve } from "path";

interface OpenAPISpec {
  paths: Record<
    string,
    Record<
      string,
      {
        responses?: Record<
          string,
          {
            content?: Record<
              string,
              {
                schema?: Record<string, unknown>;
              }
            >;
          }
        >;
      }
    >
  >;
}

const specPath = resolve(__dirname, "../../../../specs/apps/demo/contracts/generated/openapi-bundled.json");

let spec: OpenAPISpec | null = null;
let ajv: Ajv | null = null;

function getSpec(): OpenAPISpec {
  if (!spec) {
    spec = JSON.parse(readFileSync(specPath, "utf-8")) as OpenAPISpec;
  }
  return spec;
}

function getAjv(): Ajv {
  if (!ajv) {
    ajv = new Ajv({ allErrors: true, strict: false });
    addFormats(ajv);
  }
  return ajv;
}

/**
 * Validate a response body against the OpenAPI contract schema.
 * Returns null if valid, or an error message string if invalid.
 */
export function validateResponseAgainstContract(
  path: string,
  method: string,
  statusCode: number,
  body: unknown,
): string | null {
  const openapi = getSpec();
  const lowerMethod = method.toLowerCase();
  const statusStr = String(statusCode);

  const pathItem = openapi.paths[path];
  if (!pathItem) return null; // Path not in spec — skip validation

  const operation = pathItem[lowerMethod];
  if (!operation) return null; // Method not in spec — skip validation

  const response = operation.responses?.[statusStr];
  if (!response) return null; // Status code not in spec — skip validation

  const jsonContent = response.content?.["application/json"];
  if (!jsonContent?.schema) return null; // No JSON schema defined — skip validation

  const validator = getAjv();
  const valid = validator.validate(jsonContent.schema, body);
  if (valid) return null;

  return `Contract violation for ${method.toUpperCase()} ${path} ${statusCode}: ${validator.errorsText()}`;
}
