module DemoBeFsgi.Contracts.ContractWrappers

// Request wrappers with [<CLIMutable>] for JSON deserialization.
// Field names match the camelCase JSON keys from the OpenAPI spec.
// The generated types in generated-contracts/ use PascalCase and lack [<CLIMutable>],
// so these thin wrappers ensure System.Text.Json can deserialize request bodies.

[<CLIMutable>]
type RegisterRequest =
    { username: string
      email: string
      password: string }

[<CLIMutable>]
type LoginRequest = { username: string; password: string }

[<CLIMutable>]
type RefreshRequest = { refreshToken: string }

[<CLIMutable>]
type UpdateProfileRequest = { displayName: string }

[<CLIMutable>]
type ChangePasswordRequest =
    { oldPassword: string
      newPassword: string }

[<CLIMutable>]
type CreateExpenseRequest =
    { amount: string
      currency: string
      category: string
      description: string
      date: string
      ``type``: string
      quantity: System.Nullable<float>
      unit: string }

[<CLIMutable>]
type UpdateExpenseRequest =
    { amount: string
      currency: string
      category: string
      description: string
      date: string
      ``type``: string
      quantity: System.Nullable<float>
      unit: string }

[<CLIMutable>]
type DisableRequest = { reason: string }

[<CLIMutable>]
type PromoteAdminRequest = { username: string }
