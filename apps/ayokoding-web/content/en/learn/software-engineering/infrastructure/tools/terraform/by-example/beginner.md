---
title: "Beginner"
date: 2025-12-29T23:43:13+07:00
draft: false
weight: 10000001
description: "Examples 1-28: Terraform fundamentals covering HCL syntax, providers, resources, variables, data sources, and state management basics (0-40% coverage)"
categories: ["learn"]
tags: ["terraform", "tutorial", "by-example", "beginner", "hcl", "providers", "resources", "iac"]
---

Learn Terraform fundamentals through 28 annotated code examples. Each example is self-contained, runnable, and heavily commented to show what each line does, expected outputs, and key takeaways.

## Group 1: Hello World & Installation

### Example 1: Hello World - Minimal Configuration

Terraform configurations are HCL files describing infrastructure resources. Every configuration needs a provider block and at least one resource. This minimal example demonstrates Terraform execution flow without creating real cloud resources.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["main.tf<br/>Configuration"] --> B["terraform init<br/>Download Providers"]
    B --> C["terraform plan<br/>Preview Changes"]
    C --> D["terraform apply<br/>Create Resources"]
    D --> E["terraform.tfstate<br/>State File"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#CC78BC,color:#fff
    style E fill:#CA9161,color:#fff
```

**Code**:

```hcl
# main.tf
terraform {
  required_version = ">= 1.0"              # => Minimum Terraform version required
  # => Blocks older versions from running this configuration
}

# Local provider for testing (no cloud credentials needed)
provider "local" {
  # => Provider block initializes the local provider for file operations
  # => Local provider enables testing without cloud accounts
}

# Create a local file resource
resource "local_file" "hello" {
  filename = "${path.module}/hello.txt"    # => path.module is current directory
  # => path.module resolves to directory containing this .tf file
  content  = "Hello, Terraform!"           # => File content to write
  # => terraform plan shows: +local_file.hello (will be created)
  # => terraform apply creates hello.txt in current directory
  # => State tracks filename, content, and file permissions
}

# Output the file path
output "file_path" {
  value = local_file.hello.filename        # => Reference resource attribute
  # => Output: file_path = "./hello.txt"
  # => Outputs appear after terraform apply completes
}
```

**Run**:

```bash
# $ terraform init     # => Downloads local provider plugin
# $ terraform plan     # => Shows: Plan: 1 to add, 0 to change, 0 to destroy
# $ terraform apply    # => Prompts for confirmation, creates hello.txt
# $ cat hello.txt      # => Hello, Terraform!
# $ terraform destroy  # => Removes hello.txt
```

**Key Takeaway**: Terraform workflow is init → plan → apply → destroy. The `terraform plan` command is critical for previewing changes before applying them. Always review the plan output before running apply in production.

**Why It Matters**: Terraform's plan/apply workflow prevents the production disasters common with manual console changes (ClickOps). GitHub uses this same workflow to manage 10,000+ AWS resources—the plan command shows "this will delete your production database" before it happens, catching human errors that would cause multi-hour outages. This safety mechanism, combined with version-controlled HCL, makes infrastructure changes as safe and reviewable as application code, preventing the "works on my machine" problems that plague manual provisioning.

---

### Example 2: Terraform CLI Basics

Understanding Terraform CLI commands enables effective infrastructure management. Each command serves a specific purpose in the Terraform workflow.

**Code**:

```hcl
# version_check.tf
terraform {
  required_version = ">= 1.0"

  required_providers {
    local = {
      source  = "hashicorp/local"          # => Provider source address
      version = "~> 2.0"                   # => Allow patch updates (2.0.x)
    }
  }
}

provider "local" {}

resource "local_file" "demo" {
  filename = "demo.txt"
  content  = "Terraform version: ${terraform.version}"  # => Built-in variable
}
```

**CLI Commands**:

```bash
# Display Terraform version
# $ terraform version
# => Terraform v1.9.0 on darwin_amd64

# Format HCL files to canonical style
# $ terraform fmt
# => Rewrites .tf files with consistent formatting

# Validate configuration syntax
# $ terraform validate
# => Success! The configuration is valid.

# Show current state
# $ terraform show
# => Displays resources in state file

# List resources in state
# $ terraform state list
# => local_file.demo

# Output values
# $ terraform output
# => Shows all output values from configuration

# Generate dependency graph
# $ terraform graph | dot -Tpng > graph.png
# => Creates visual dependency graph (requires graphviz)
```

**Key Takeaway**: Use `terraform fmt` before commits to maintain consistent formatting. Use `terraform validate` to catch syntax errors early. Use `terraform show` and `state list` to inspect infrastructure state without modifying resources.

**Why It Matters**: Terraform's CLI commands enable safe infrastructure inspection without AWS console access—critical when managing thousands of resources across multiple accounts. The `terraform validate` command catches syntax errors before cloud API calls, preventing failed deployments that waste time and money. The `terraform fmt` command enforces consistent style across teams, making code reviews as straightforward as application code reviews, supporting the Infrastructure as Code principle that infrastructure should have the same quality standards as software.

---

## Group 2: HCL Syntax Basics

### Example 3: HCL Block Types

HCL (HashiCorp Configuration Language) has four main block types: terraform, provider, resource, and data. Understanding block structure is fundamental to writing Terraform configurations.

**Code**:

```hcl
# hcl_blocks.tf

# Terraform block - configuration settings
terraform {
  required_version = ">= 1.0"              # => Constraint on Terraform binary version

  required_providers {                     # => Provider dependencies
    local = {
      source  = "hashicorp/local"
      version = "~> 2.0"
    }
  }

  backend "local" {                        # => State storage backend
    path = "terraform.tfstate"             # => Local file backend (default)
  }
}

# Provider block - provider configuration
provider "local" {
  # => No arguments needed for local provider
  # => Other providers require credentials, regions, etc.
}

# Resource block - infrastructure component
resource "local_file" "example" {          # => resource type = local_file
  filename = "example.txt"                 # => resource name = example
  content  = "Block syntax example"        # => Argument: content

  file_permission = "0644"                 # => Argument: permissions
  # => Full identifier: local_file.example
}

# Data source block - query existing resources
data "local_file" "existing" {
  filename = "example.txt"                 # => Read existing file
  # => Data sources don't create resources, only read
  # => Full identifier: data.local_file.existing
}

# Output block - expose values
output "file_content" {
  value       = data.local_file.existing.content  # => Reference data source
  description = "Content of the example file"     # => Optional description
  # => Shown after terraform apply
}
```

**Key Takeaway**: HCL blocks follow pattern `block_type "label1" "label2" { arguments }`. Resources have two labels (type and name), data sources have two labels, providers have one label. Use dot notation to reference blocks: `resource_type.name.attribute`.

**Why It Matters**: HCL's declarative block syntax makes infrastructure configuration readable by non-programmers, unlike imperative shell scripts that require programming expertise. This enables collaboration between infrastructure teams, security teams, and auditors—all can review Terraform code and understand "what will be created" without decoding complex logic. The consistent block structure across all providers (AWS, Azure, GCP, Kubernetes) means learning Terraform once enables managing any infrastructure, avoiding vendor-specific scripting languages that lock teams into single clouds.

---

### Example 4: HCL Data Types and Expressions

HCL supports multiple data types: strings, numbers, booleans, lists, maps, and objects. Expressions enable dynamic values through interpolation, functions, and operators.

**Code**:

```hcl
# data_types.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {}

# Local values - reusable expressions
locals {
  # Primitive types
  string_value  = "hello"                  # => String type
  number_value  = 42                       # => Number type (int or float)
  bool_value    = true                     # => Boolean type

  # Collection types
  list_value    = ["item1", "item2", "item3"]           # => List (ordered)
  map_value     = { key1 = "value1", key2 = "value2" }  # => Map (key-value)
  set_value     = toset(["a", "b", "c"])                # => Set (unique, unordered)

  # Object type (structured)
  object_value  = {
    name    = "example"
    port    = 8080
    enabled = true
  }

  # String interpolation
  interpolated  = "Value is ${local.number_value}"      # => "Value is 42"

  # String template with directives
  template      = <<-EOT
    Name: ${local.object_value.name}
    Port: ${local.object_value.port}
  EOT
  # => Multi-line string with interpolation

  # Conditional expression
  conditional   = local.bool_value ? "yes" : "no"       # => Ternary operator

  # Arithmetic operations
  arithmetic    = local.number_value * 2 + 10           # => 42 * 2 + 10 = 94

  # String operations
  uppercase     = upper(local.string_value)             # => "HELLO"
  concatenated  = "${local.string_value}-world"         # => "hello-world"

  # List operations
  first_item    = local.list_value[0]                   # => "item1" (index access)
  list_length   = length(local.list_value)              # => 3

  # Map operations
  map_lookup    = local.map_value["key1"]               # => "value1" (key access)
}

resource "local_file" "types_demo" {
  filename = "types.txt"
  content  = <<-EOT
    String: ${local.string_value}
    Number: ${local.number_value}
    Boolean: ${local.bool_value}
    List: ${join(", ", local.list_value)}
    Conditional: ${local.conditional}
    Arithmetic: ${local.arithmetic}
  EOT
}

output "demonstrations" {
  value = {
    interpolated  = local.interpolated
    uppercase     = local.uppercase
    list_length   = local.list_length
    first_item    = local.first_item
  }
  # => Shows all computed values
}
```

**Key Takeaway**: Use `locals` block for computed values shared across configuration. Use `${}` for string interpolation. HCL supports rich expressions including conditionals, arithmetic, and built-in functions. Reference built-in functions at terraform.io/language/functions.

**Why It Matters**: HCL's data types and expressions eliminate the need for external scripting languages (Python, Bash) to generate configuration. Uber manages thousands of servers using HCL's conditional logic to deploy different instance types by environment—production gets large instances, development gets small ones, all from a single codebase. This keeps infrastructure logic in one place where Terraform's plan command can validate it, instead of scattered across shell scripts that might fail mid-deployment without safety checks.

---

### Example 5: Comments and Documentation

Proper documentation through comments and descriptions makes Terraform configurations maintainable. HCL supports single-line and multi-line comments.

**Code**:

```hcl
# comments.tf

# Single-line comment using hash
// Single-line comment using double-slash (both valid)

/*
  Multi-line comment
  using C-style syntax
  spans multiple lines
*/

terraform {
  required_version = ">= 1.0"              # Inline comment
}

provider "local" {}

# Resource with documentation
resource "local_file" "documented" {
  filename = "documented.txt"
  content  = "Well-documented configuration"

  # File permissions in octal notation
  file_permission = "0644"                 # rw-r--r-- (owner: rw, group: r, other: r)
}

# Variable with description
variable "environment" {
  description = <<-EOT
    Environment name for resource tagging.
    Valid values: development, staging, production
  EOT
  type        = string
  default     = "development"

  # Validation rule with explanation
  validation {
    condition     = contains(["development", "staging", "production"], var.environment)
    error_message = "Environment must be development, staging, or production."
  }
}

# Output with description
output "file_info" {
  description = "Information about the created file"
  value = {
    filename    = local_file.documented.filename
    permissions = local_file.documented.file_permission
  }
}
```

**Key Takeaway**: Use `#` or `//` for single-line comments, `/* */` for multi-line. Add `description` to variables and outputs for documentation. Use inline comments to explain non-obvious configuration choices. Documentation is essential for team collaboration and future maintenance.

**Why It Matters**: Well-documented Terraform code enables safe infrastructure changes during oncall incidents when the original author isn't available. Variable descriptions appear in Terraform Cloud UI and documentation generators, helping new team members understand required inputs without reading source code. This documentation-in-code approach means infrastructure knowledge lives in version control instead of outdated wiki pages, reducing the "tribal knowledge" problem that causes production outages when experienced engineers leave the team.

---

### Example 6: HCL Functions

Terraform provides 100+ built-in functions for string manipulation, collection operations, numeric calculations, encoding, filesystem operations, and more. Functions cannot be user-defined.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Input String:<br/>  Hello, Terraform!  "] --> B["trimspace#40;#41;<br/>Remove whitespace"]
    B --> C["Trimmed:<br/>Hello, Terraform!"]
    C --> D1["lower#40;#41;<br/>To lowercase"]
    C --> D2["upper#40;#41;<br/>To uppercase"]
    C --> D3["replace#40;#41;<br/>Substitute text"]
    D1 --> E1["hello, terraform!"]
    D2 --> E2["HELLO, TERRAFORM!"]
    D3 --> E3["Hello, HCL!"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D1 fill:#CC78BC,color:#fff
    style D2 fill:#CC78BC,color:#fff
    style D3 fill:#CC78BC,color:#fff
    style E1 fill:#CA9161,color:#fff
    style E2 fill:#CA9161,color:#fff
    style E3 fill:#CA9161,color:#fff
```

**Code**:

```hcl
# functions.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {}

locals {
  # String functions
  original        = "  Hello, Terraform!  "
  # => original stores raw string with leading/trailing whitespace
  trimmed         = trimspace(local.original)           # => "Hello, Terraform!" (remove whitespace)
  lowercased      = lower(local.trimmed)                # => "hello, terraform!"
  # => lower converts all characters to lowercase
  uppercased      = upper(local.trimmed)                # => "HELLO, TERRAFORM!"
  replaced        = replace(local.trimmed, "Terraform", "HCL")  # => "Hello, HCL!"
  # => replace finds first match and substitutes
  split_result    = split(", ", local.trimmed)          # => ["Hello", "Terraform!"]
  joined          = join("-", split(", ", local.trimmed))  # => "Hello-Terraform!"
  # => join concatenates list elements with separator

  # Numeric functions
  numbers         = [10, 5, 20, 15]
  max_num         = max(local.numbers...)               # => 20 (variadic expansion)
  # => ... operator expands list into separate arguments
  min_num         = min(local.numbers...)               # => 5
  sum_num         = sum(local.numbers)                  # => 50
  # => sum adds all elements (10 + 5 + 20 + 15 = 50)

  # Collection functions
  list            = ["a", "b", "c", "d"]
  list_length     = length(local.list)                  # => 4
  contains_b      = contains(local.list, "b")           # => true
  # => contains checks if element exists in list
  first_elem      = element(local.list, 0)              # => "a"
  # => element accesses list by index (0-based)
  last_elem       = element(local.list, length(local.list) - 1)  # => "d"
  reversed        = reverse(local.list)                 # => ["d", "c", "b", "a"]
  sorted          = sort(local.list)                    # => ["a", "b", "c", "d"]
  distinct        = distinct(["a", "b", "a", "c"])      # => ["a", "b", "c"]
  # => distinct removes duplicate values

  # Map functions
  map_data        = { env = "prod", region = "us-east-1", team = "platform" }
  map_keys        = keys(local.map_data)                # => ["env", "region", "team"]
  # => keys extracts all map keys as list
  map_values      = values(local.map_data)              # => ["prod", "us-east-1", "platform"]
  merged_map      = merge(local.map_data, { tier = "web" })  # => Adds tier key
  # => merge combines maps (right map overwrites left on key collision)

  # Type conversion
  string_to_num   = tonumber("42")                      # => 42 (number type)
  # => tonumber converts string to number (fails if non-numeric)
  num_to_string   = tostring(42)                        # => "42" (string type)
  to_set          = toset(["a", "b", "a"])              # => Set with unique values
  # => toset creates set (unique, unordered collection)

  # Encoding functions
  json_string     = jsonencode({ key = "value" })       # => '{"key":"value"}'
  yaml_string     = yamlencode({ key = "value" })       # => 'key: value\n'
  base64_encoded  = base64encode("hello")               # => "aGVsbG8="
  # => base64encode encodes string to Base64 format
  base64_decoded  = base64decode("aGVsbG8=")            # => "hello"
  # => base64decode decodes Base64 string to original

  # Date/Time functions
  timestamp       = timestamp()                         # => "2024-01-15T10:30:00Z" (UTC)
  formatted_date  = formatdate("YYYY-MM-DD", timestamp())  # => "2024-01-15"
}

resource "local_file" "functions_demo" {
  filename = "functions.txt"
  content  = <<-EOT
    Original: "${local.original}"
    Trimmed: "${local.trimmed}"
    Uppercase: ${local.uppercased}
    Replaced: ${local.replaced}
    Joined: ${local.joined}

    Max: ${local.max_num}
    Sum: ${local.sum_num}

    List Length: ${local.list_length}
    Reversed: ${jsonencode(local.reversed)}

    Timestamp: ${local.timestamp}
  EOT
}

output "function_results" {
  value = {
    trimmed     = local.trimmed
    uppercased  = local.uppercased
    max_num     = local.max_num
    list_length = local.list_length
  }
}
```

**Key Takeaway**: Terraform functions enable complex transformations without external scripting. Use `...` operator for variadic expansion (spreading list elements as separate arguments). Functions are pure—same input always produces same output. Reference complete function catalog at terraform.io/language/functions.

**Why It Matters**: Built-in functions replace brittle shell scripts and external data sources that complicate infrastructure deployment. Slack uses functions like `cidrsubnet()` to automatically calculate subnet CIDRs across 20+ AWS regions, eliminating manual IP planning that causes subnet conflicts. Pure functions mean Terraform plans are reproducible—same configuration always produces same plan, unlike shell scripts with hidden dependencies on system state, ensuring infrastructure changes are predictable across team members' machines and CI/CD systems.

---

## Group 3: Providers & Resources

### Example 7: Provider Configuration

Providers are plugins that manage resources for specific platforms (AWS, Azure, GCP, Kubernetes, etc.). Each provider requires configuration with credentials, regions, and version constraints.

**Code**:

```hcl
# providers.tf
terraform {
  required_version = ">= 1.0"

  required_providers {
    # AWS provider
    aws = {
      source  = "hashicorp/aws"            # => Provider source (namespace/name)
      version = "~> 5.0"                   # => Pessimistic constraint (~> allows rightmost increment)
    }

    # Azure provider
    azurerm = {
      source  = "hashicorp/azurerm"
      version = ">= 3.0, < 4.0"            # => Range constraint
    }

    # Local provider (for examples)
    local = {
      source  = "hashicorp/local"
      version = "~> 2.0"
    }
  }
}

# AWS provider configuration
provider "aws" {
  region = "us-east-1"                     # => AWS region
  # => Credentials read from environment variables:
  # => AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY
  # => Or from ~/.aws/credentials file

  default_tags {
    tags = {
      Environment = "development"
      ManagedBy   = "Terraform"
    }
  }
  # => Default tags applied to all AWS resources
}

# Multiple provider configurations (aliases)
provider "aws" {
  alias  = "west"                          # => Provider alias for multi-region
  region = "us-west-2"
}

# Azure provider configuration
provider "azurerm" {
  features {}                              # => Required features block
  # => Credentials from Azure CLI (az login) or environment variables
}

# Local provider (no configuration needed)
provider "local" {}

# Resource using default AWS provider
resource "local_file" "provider_demo" {
  filename = "providers.txt"
  content  = <<-EOT
    Providers configured:
    - AWS (default): us-east-1
    - AWS (west): us-west-2
    - Azure: features enabled
    - Local: file operations
  EOT
}

# Resource using aliased provider
# resource "aws_s3_bucket" "west_bucket" {
#   provider = aws.west                    # => Use aliased provider explicitly
#   bucket   = "example-west-bucket"
# }
```

**Key Takeaway**: Declare providers in `required_providers` block with source and version. Configure providers with credentials and region. Use provider aliases for multi-region deployments. Version constraints prevent breaking changes: `~> 5.0` means `>= 5.0, < 6.0`.

**Why It Matters**: Terraform's provider ecosystem with 3,000+ providers enables multi-cloud infrastructure without vendor lock-in. Twitch manages AWS, Datadog, PagerDuty, and GitHub resources from a single codebase using different providers—impossible with cloud-specific tools like CloudFormation or ARM templates. Version constraints prevent the "upgrade broke production" incidents common with unversioned scripts, ensuring provider updates are deliberate and tested, not accidental surprises during routine deployments.

---

### Example 8: Resource Basics

Resources are the most important element in Terraform—they represent infrastructure components to create, update, or delete. Each resource has a type, name, and arguments.

**Code**:

```hcl
# resources.tf
terraform {
  required_version = ">= 1.0"
  required_providers {
    local = {
      source  = "hashicorp/local"
      version = "~> 2.0"
    }
  }
}

provider "local" {}

# Simple resource
resource "local_file" "simple" {
  filename = "simple.txt"                  # => Required argument
  content  = "Simple resource example"
}

# Resource with multiple arguments
resource "local_file" "detailed" {
  filename        = "detailed.txt"
  content         = "Detailed resource"
  file_permission = "0644"                 # => Optional argument
}

# Resource with lifecycle customization
resource "local_file" "lifecycle_demo" {
  filename = "lifecycle.txt"
  content  = "Lifecycle example"

  lifecycle {
    create_before_destroy = true           # => Create new before destroying old
    prevent_destroy       = false          # => Prevent accidental deletion (true in prod)
    ignore_changes        = [content]      # => Ignore changes to content attribute
  }
  # => Lifecycle meta-arguments control resource behavior
}

# Resource with count (multiple instances)
resource "local_file" "counted" {
  count    = 3                             # => Create 3 instances
  filename = "file_${count.index}.txt"    # => count.index: 0, 1, 2
  content  = "File number ${count.index}"
  # => Creates: file_0.txt, file_1.txt, file_2.txt
}

# Resource with depends_on (explicit dependency)
resource "local_file" "first" {
  filename = "first.txt"
  content  = "Created first"
}

resource "local_file" "second" {
  filename   = "second.txt"
  content    = "Created after first"
  depends_on = [local_file.first]          # => Explicit dependency
  # => Terraform creates first.txt before second.txt
}

# Output resource attributes
output "resource_info" {
  value = {
    simple_id       = local_file.simple.id
    simple_filename = local_file.simple.filename
    counted_files   = [for f in local_file.counted : f.filename]
  }
  # => Shows resource attributes after apply
}
```

**Key Takeaway**: Resources follow pattern `resource "type" "name" { arguments }`. Use `count` for multiple similar resources. Use `depends_on` for explicit dependencies when implicit dependencies (via references) aren't sufficient. Lifecycle meta-arguments control creation, destruction, and update behavior.

**Why It Matters**: Resource blocks are the foundation of infrastructure as code—each block declares desired state, and Terraform reconciles actual infrastructure to match. This declarative approach means you specify "what" (10 web servers) instead of "how" (loop that creates servers), making configurations self-documenting and idempotent. Unlike imperative scripts that break when run twice, Terraform resources can be applied repeatedly without creating duplicates, enabling safe automation and recovery from partial failures.

---

### Example 9: Resource Dependencies

Terraform automatically determines resource creation order based on references. Explicit dependencies use `depends_on`. Understanding dependency graph prevents race conditions.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["local_file.base"] --> B["local_file.depends_implicit"]
    A --> C["local_file.depends_explicit"]
    B --> D["local_file.final"]
    C --> D

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#CC78BC,color:#fff
```

**Code**:

```hcl
# dependencies.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {
  version = "~> 2.0"
}

# Base resource (no dependencies)
resource "local_file" "base" {
  filename = "base.txt"
  content  = "Base file created first"
  # => No dependencies, can be created immediately
}

# Implicit dependency (via reference)
resource "local_file" "depends_implicit" {
  filename = "implicit.txt"
  content  = "Base file: ${local_file.base.filename}"  # => References base
  # => Terraform detects dependency automatically
  # => Creates base.txt before implicit.txt
}

# Explicit dependency (via depends_on)
resource "local_file" "depends_explicit" {
  filename   = "explicit.txt"
  content    = "Depends on base (explicit)"
  depends_on = [local_file.base]           # => Explicit dependency declaration
  # => Use depends_on when no direct reference exists
}

# Multiple dependencies
resource "local_file" "final" {
  filename = "final.txt"
  content  = <<-EOT
    Implicit: ${local_file.depends_implicit.filename}
    Explicit: ${local_file.depends_explicit.filename}
  EOT
  # => Implicit dependencies on both depends_implicit and depends_explicit
  # => Terraform creates in correct order: base -> implicit/explicit -> final
}

# View dependency graph
# $ terraform graph | dot -Tpng > dependencies.png
```

**Key Takeaway**: Terraform builds dependency graph from resource references. Use implicit dependencies (references) when possible—they're clearer than `depends_on`. Use `depends_on` only for side-effect dependencies that can't be expressed through references. Run `terraform graph` to visualize dependencies.

**Why It Matters**: Automatic dependency resolution prevents the "database provisioned before VPC" race conditions that plague manual provisioning and unordered scripts. Terraform's dependency graph ensures infrastructure components are created in correct order—networks before servers, load balancers before DNS records—without fragile sleep statements or manual sequencing. This eliminates the "deployment sometimes fails with timeout errors" flakiness common in CI/CD pipelines using imperative infrastructure scripts, making deployments reliably reproducible.

---

### Example 10: Resource Lifecycle

Lifecycle meta-arguments control resource creation, update, and destruction behavior. Essential for zero-downtime deployments and protecting critical resources.

**Code**:

```hcl
# lifecycle.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {
  version = "~> 2.0"
}

# create_before_destroy: Create replacement before destroying old
resource "local_file" "zero_downtime" {
  filename = "zero_downtime.txt"
  content  = "Version 2"                   # => Change this to trigger replacement
  # => Modifying content forces resource recreation

  lifecycle {
    create_before_destroy = true           # => Create new, then destroy old
    # => Ensures resource always exists during replacement
  }
  # => Useful for resources that must always exist (e.g., DNS records)
  # => Prevents downtime during updates requiring replacement
  # => Order: Create new file → Verify creation → Delete old file
}

# prevent_destroy: Prevent accidental deletion
resource "local_file" "critical" {
  filename = "critical.txt"
  content  = "Critical production data"
  # => Content represents critical state (database, secrets, etc.)

  lifecycle {
    prevent_destroy = true                 # => terraform destroy will fail
    # => Error: "Cannot destroy resource with prevent_destroy = true"
  }
  # => Use for databases, state stores, and other critical resources
  # => Must set to false before destroying
  # => Protects against accidental deletion in production
}

# ignore_changes: Ignore changes to specific attributes
resource "local_file" "ignore_demo" {
  filename = "ignore.txt"
  content  = "Original content"            # => Can be changed externally
  # => External processes can modify this file

  lifecycle {
    ignore_changes = [content]             # => Ignore content changes
    # ignore_changes = all                 # => Ignore all attribute changes
    # => Terraform won't revert external modifications
  }
  # => Terraform won't detect content changes
  # => Useful when external systems modify resources
  # => State drift tolerated for specified attributes
}

# replace_triggered_by: Force replacement when related resource changes
resource "local_file" "trigger_source" {
  filename = "trigger.txt"
  content  = "Trigger version 1"           # => Change this
}

resource "local_file" "triggered" {
  filename = "triggered.txt"
  content  = "Depends on trigger"

  lifecycle {
    replace_triggered_by = [
      local_file.trigger_source.id         # => Replace when trigger_source changes
    ]
  }
  # => Forces replacement even if own attributes unchanged
}

# Combined lifecycle rules
resource "local_file" "combined" {
  filename = "combined.txt"
  content  = "Production configuration"

  lifecycle {
    create_before_destroy = true           # => Zero downtime
    prevent_destroy       = false          # => Allow destruction (set true in prod)
    ignore_changes        = [file_permission]  # => Ignore permission changes
  }
}
```

**Key Takeaway**: Use `create_before_destroy` for zero-downtime updates. Use `prevent_destroy` to protect critical resources. Use `ignore_changes` when external systems modify resources. Lifecycle rules are powerful—use with caution in production.

**Why It Matters**: Lifecycle rules prevent catastrophic production mistakes like deleting a database because an AMI ID changed. `prevent_destroy` has saved countless teams from the "terraform destroy accidentally deleted our production data" disaster, forcing deliberate removal instead of accidental deletion. `create_before_destroy` enables zero-downtime deployments—new load balancer spins up before old one terminates—essential for services maintaining 99.99% uptime SLAs where even brief downtime costs thousands in revenue and customer trust.

---

## Group 4: Variables

### Example 11: Input Variables

Input variables make Terraform configurations reusable and environment-agnostic. Variables support types, defaults, descriptions, and validation rules.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Variable Input<br/>#40;CLI/File/Env#41;"] --> B["Type Check"]
    B --> C{"Type Valid?"}
    C -->|No| D["Error:<br/>Type mismatch"]
    C -->|Yes| E["Validation Rules"]
    E --> F{"Validation<br/>Passes?"}
    F -->|No| G["Error:<br/>Custom message"]
    F -->|Yes| H["Variable Available<br/>in Configuration"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#CC78BC,color:#fff
    style E fill:#DE8F05,color:#fff
    style F fill:#029E73,color:#fff
    style G fill:#CC78BC,color:#fff
    style H fill:#CA9161,color:#fff
```

**Code**:

```hcl
# variables.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {}

# Simple string variable with default
variable "environment" {
  description = "Environment name"
  # => Description appears in terraform console and docs
  type        = string
  # => type = string enforces string type (rejected: numbers, bools, lists)
  default     = "development"              # => Used if not provided
  # => Optional: Allows terraform apply without -var flag
}

# Variable without default (required)
variable "project_name" {
  description = "Project name for resource naming"
  type        = string
  # => No default = required variable
  # => Must be provided via CLI, file, or environment variable
  # => terraform apply fails if not provided
}

# Number variable
variable "instance_count" {
  description = "Number of instances to create"
  type        = number
  # => type = number enforces numeric type (rejected: strings, bools)
  default     = 1
  # => Default prevents need for explicit value in most cases
}

# Boolean variable
variable "enable_monitoring" {
  description = "Enable monitoring features"
  type        = bool
  # => type = bool enforces true/false (rejected: strings, numbers)
  default     = false
  # => Monitoring disabled by default (opt-in pattern)
}

# List variable
variable "allowed_cidrs" {
  description = "List of allowed CIDR blocks"
  type        = list(string)
  # => type = list(string) enforces list of strings
  # => Rejected: non-string list elements, single string values
  default     = ["10.0.0.0/8"]
  # => Single private network CIDR in default configuration
}

# Map variable
variable "tags" {
  description = "Resource tags"
  type        = map(string)
  # => type = map(string) enforces string-keyed, string-valued map
  default = {
    ManagedBy = "Terraform"
    # => All infrastructure tagged with IaC tool for auditing
  }
}

# Object variable (structured)
variable "server_config" {
  description = "Server configuration"
  type = object({
    instance_type = string
    # => Server size (t3.micro, t3.small, etc.)
    disk_size     = number
    # => Disk size in GB
    enable_backup = bool
    # => Backup enabled/disabled flag
  })
  # => object type enforces exact schema - all fields required
  default = {
    instance_type = "t3.micro"
    # => Free tier eligible instance type
    disk_size     = 20
    # => Minimum disk for typical workloads
    enable_backup = false
    # => Backups disabled by default (cost optimization)
  }
}

# Variable with validation
variable "region" {
  description = "AWS region"
  type        = string
  default     = "us-east-1"

  validation {
    condition     = can(regex("^us-", var.region))  # => Must start with "us-"
    # => can() prevents error if regex fails, returns false instead
    error_message = "Region must be a US region."
    # => Custom error shown when validation fails
  }
  # => Validation runs before plan/apply
  # => Prevents invalid region values (e.g., "eu-west-1")
}

# Sensitive variable (masked in output)
variable "api_key" {
  description = "API key for external service"
  type        = string
  sensitive   = true                       # => Value hidden in plan/apply output
  # => Shows (sensitive value) instead of actual string
  default     = ""
  # => Empty default (should be overridden in production)
}

# Use variables in resources
resource "local_file" "config" {
  filename = "${var.environment}-config.txt"
  # => var.environment references variable value
  # => Produces: "development-config.txt" with default
  content  = <<-EOT
    Project: ${var.project_name}
    Environment: ${var.environment}
    Instances: ${var.instance_count}
    Monitoring: ${var.enable_monitoring}
    Region: ${var.region}
    Tags: ${jsonencode(var.tags)}
    # => jsonencode converts map to JSON string
    Server: ${var.server_config.instance_type}
    # => Access object fields with dot notation
  EOT
  # => Heredoc syntax for multi-line strings
}
```

**Usage**:

```bash
# Provide via command line
# $ terraform apply -var="project_name=myapp" -var="environment=production"

# Provide via environment variable
# $ export TF_VAR_project_name="myapp"
# $ terraform apply

# Provide via variable file
# $ echo 'project_name = "myapp"' > terraform.tfvars
# $ terraform apply

# Provide via auto-loaded file
# terraform.tfvars (automatically loaded)
# *.auto.tfvars (automatically loaded)
```

**Key Takeaway**: Declare variables in `variables.tf` with type, description, and optional default. Use `var.name` to reference variables. Variable files (`*.tfvars`) separate configuration from code. Mark sensitive variables with `sensitive = true` to prevent exposure in logs.

**Why It Matters**: Input variables enable reusable infrastructure modules deployed across environments—same code provisions development (1 server), staging (3 servers), and production (50 servers) by changing tfvars files. This eliminates environment-specific codebases that diverge and cause "works in staging, fails in prod" surprises. Type validation catches configuration errors during plan—"instance_count must be number" fails fast instead of hitting AWS API limits, preventing deployment failures that waste engineer time and cloud costs.

---

### Example 12: Output Values

Output values expose information about infrastructure after apply. Outputs can be queried via CLI, used by other Terraform configurations, or displayed to users.

**Code**:

```hcl
# outputs.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {}

variable "prefix" {
  type    = string
  default = "demo"
}

resource "local_file" "output_demo" {
  filename = "${var.prefix}-output.txt"
  content  = "Output demonstration"
}

# Simple output
output "file_id" {
  description = "Unique identifier for the file"
  value       = local_file.output_demo.id  # => Resource attribute
  # => terraform output file_id
}

# Output with multiple attributes
output "file_info" {
  description = "Complete file information"
  value = {
    id          = local_file.output_demo.id
    filename    = local_file.output_demo.filename
    content     = local_file.output_demo.content
    permissions = local_file.output_demo.file_permission
  }
  # => terraform output file_info
  # => Returns map with all attributes
}

# Sensitive output (hidden by default)
output "sensitive_data" {
  description = "Sensitive information"
  value       = "secret-api-key-12345"
  sensitive   = true                       # => Marked as sensitive
  # => terraform output sensitive_data (shows <sensitive>)
  # => terraform output -raw sensitive_data (shows actual value)
}

# Output with computed value
output "file_path" {
  description = "Absolute file path"
  value       = abspath(local_file.output_demo.filename)
  # => Processes value through function
}

# Conditional output
output "status_message" {
  description = "Status based on configuration"
  value       = var.prefix == "prod" ? "Production mode" : "Development mode"
}

# List output
locals {
  file_list = [
    local_file.output_demo.filename,
    "${var.prefix}-backup.txt"
  ]
}

output "all_files" {
  description = "List of all managed files"
  value       = local.file_list
}

# Output for module consumers
output "module_export" {
  description = "Values for parent module or root configuration"
  value = {
    resource_id = local_file.output_demo.id
    filename    = local_file.output_demo.filename
  }
  # => When this config is used as a module, parent can reference:
  # => module.child_module.module_export.resource_id
}
```

**Usage**:

```bash
# Display all outputs
# $ terraform output
# => Shows all outputs after apply

# Display specific output
# $ terraform output file_id
# => Shows single output value

# Display as JSON
# $ terraform output -json
# => {"file_id": {"value": "...", "type": "string"}}

# Display raw value (no quotes)
# $ terraform output -raw file_path
# => /full/path/to/file.txt

# Use outputs in scripts
# $ FILE_ID=$(terraform output -raw file_id)
```

**Key Takeaway**: Outputs expose computed values after apply. Use `sensitive = true` for secrets. Outputs from child modules are accessed via `module.name.output_name`. Use `-json` flag for machine-readable output in automation scripts.

**Why It Matters**: Outputs enable infrastructure composition where network team manages VPCs and application team consumes VPC IDs without tight coupling. This separation of concerns is how Netflix manages infrastructure at scale—dozens of teams provision independent components that reference each other's outputs through remote state. Machine-readable JSON outputs integrate Terraform with CI/CD pipelines, automatically updating application configuration with new load balancer DNS names without manual copy-paste that causes "forgot to update config" outages.

---

### Example 13: Local Values

Local values (locals) are named expressions computed once and reused throughout configuration. Locals reduce duplication and improve readability.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Variables<br/>project, environment"] --> B["Computed Local:<br/>prefix"]
    A --> C["Computed Local:<br/>is_production"]
    C --> D["Computed Local:<br/>backup_enabled"]
    B --> E["Computed Local:<br/>common_tags"]
    C --> F["Computed Local:<br/>config"]
    B --> G["Computed Local:<br/>resource_names"]
    F --> G
    G --> H["Resources<br/>#40;for_each#41;"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#DE8F05,color:#fff
    style D fill:#029E73,color:#fff
    style E fill:#029E73,color:#fff
    style F fill:#029E73,color:#fff
    style G fill:#CC78BC,color:#fff
    style H fill:#CA9161,color:#fff
```

**Code**:

```hcl
# locals.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {}

variable "environment" {
  type    = string
  default = "development"
}

variable "project" {
  type    = string
  default = "myapp"
}

# Locals block
locals {
  # Simple computed value
  prefix = "${var.project}-${var.environment}"  # => "myapp-development"

  # Conditional logic
  is_production = var.environment == "production"
  backup_enabled = local.is_production ? true : false

  # Common tags used across resources
  common_tags = {
    Project     = var.project
    Environment = var.environment
    ManagedBy   = "Terraform"
    Timestamp   = timestamp()
  }

  # Complex computation
  config = {
    instance_count = local.is_production ? 3 : 1
    instance_type  = local.is_production ? "large" : "small"
    monitoring     = local.is_production ? "detailed" : "basic"
  }

  # List transformation
  availability_zones = ["us-east-1a", "us-east-1b", "us-east-1c"]
  az_suffix          = [for az in local.availability_zones : substr(az, -1, 1)]  # => ["a", "b", "c"]

  # Map transformation
  region_map = {
    us-east-1 = "virginia"
    us-west-2 = "oregon"
  }
  region_name = lookup(local.region_map, "us-east-1", "unknown")  # => "virginia"

  # Function composition
  resource_names = [for i in range(local.config.instance_count) : "${local.prefix}-instance-${i}"]
  # => ["myapp-development-instance-0"] for development
  # => ["myapp-production-instance-0", ..., "-2"] for production
}

# Use locals in resources
resource "local_file" "config_file" {
  for_each = toset(local.resource_names)       # => Create one file per name

  filename = "${each.value}.txt"
  content  = <<-EOT
    Resource: ${each.value}
    Environment: ${var.environment}
    Production: ${local.is_production}
    Backup: ${local.backup_enabled}
    Config: ${jsonencode(local.config)}
  EOT
}

resource "local_file" "tags_file" {
  filename = "${local.prefix}-tags.txt"
  content  = jsonencode(local.common_tags)
}

output "computed_values" {
  value = {
    prefix          = local.prefix
    is_production   = local.is_production
    resource_names  = local.resource_names
    config          = local.config
  }
}
```

**Key Takeaway**: Use `locals` for computed values used multiple times—reduces duplication and improves maintainability. Locals are computed once per apply. Reference with `local.name` (singular, not `locals.name`). Locals can reference other locals and variables.

**Why It Matters**: Locals eliminate configuration duplication that causes "updated naming convention in 3 places, forgot the 4th" bugs. Instead of repeating "${var.project}-${var.environment}" across 50 resource names, compute it once as `local.prefix` and reference everywhere—one-line changes instead of error-prone find-replace. This DRY principle is critical for large infrastructures where Uber manages thousands of resources with consistent tagging and naming, preventing the chaos of inconsistent resource names that make cost allocation and security audits impossible.

---

## Group 5: Data Sources

### Example 14: Data Source Basics

Data sources query existing infrastructure or external data without creating resources. Essential for referencing resources created outside Terraform or reading cloud provider information.

**Code**:

```hcl
# data_sources.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {}

# Create a resource first
resource "local_file" "source" {
  filename = "source.txt"
  content  = "Original content"
  # => This resource must exist before data source can read it
}

# Data source reads existing resource
data "local_file" "existing" {
  filename = "source.txt"                  # => Read existing file
  # => Data source queries file at apply time
  depends_on = [local_file.source]        # => Ensure file exists first
  # => depends_on creates explicit ordering (source created before read)
  # => Data sources read-only, never create/modify/destroy
  # => Query happens during terraform plan and apply
}

# Use data source attributes
resource "local_file" "copy" {
  filename = "copy.txt"
  content  = data.local_file.existing.content  # => Reference data source
  # => Content copied from source.txt (dynamic reference)
  # => If source.txt changes, copy.txt updates on next apply
}

output "data_source_info" {
  value = {
    id          = data.local_file.existing.id
    # => Unique identifier for the file
    filename    = data.local_file.existing.filename
    # => File path (source.txt)
    content     = data.local_file.existing.content
    # => File content as string
    content_b64 = data.local_file.existing.content_base64
    # => Base64-encoded content
  }
  # => Shows all attributes from data source
  # => Data sources expose provider-specific attributes
}

# Multiple data sources
data "local_file" "file1" {
  filename = "source.txt"
  # => First data source (reads source.txt)
}

data "local_file" "file2" {
  filename = "copy.txt"
  depends_on = [local_file.copy]
  # => Second data source (reads copy.txt after it's created)
}

output "combined_content" {
  value = "${data.local_file.file1.content} + ${data.local_file.file2.content}"
  # => Combines content from multiple data sources
  # => String interpolation with data source attributes
}
```

**Real-world example (AWS)**:

```hcl
# Query existing VPC
data "aws_vpc" "existing" {
  filter {
    name   = "tag:Name"
    values = ["production-vpc"]
  }
}

# Use VPC ID in new resource
resource "aws_subnet" "new" {
  vpc_id            = data.aws_vpc.existing.id  # => Reference existing VPC
  cidr_block        = "10.0.1.0/24"
  availability_zone = "us-east-1a"
}
```

**Key Takeaway**: Data sources query existing resources with `data "type" "name" { }` blocks. Reference attributes with `data.type.name.attribute`. Use data sources to integrate Terraform with existing infrastructure or external systems. Data sources are read-only—they never create or modify resources.

**Why It Matters**: Data sources enable gradual Terraform adoption without rip-and-replace migrations. Organizations can Terraform new infrastructure while querying existing manually-created VPCs, security groups, and databases through data sources. This "brownfield infrastructure" pattern is how Airbnb migrated to Terraform over 18 months—incremental adoption that references existing resources instead of requiring risky weekend migrations. Data sources also enable split responsibilities where platform teams manage networks and application teams query VPC IDs, preventing coordination bottlenecks.

---

### Example 15: External Data Source

The external data source executes external programs and captures their JSON output. Enables integration with custom scripts, APIs, or systems without native Terraform providers.

**Code**:

```hcl
# external_data.tf
terraform {
  required_version = ">= 1.0"
  required_providers {
    external = {
      source  = "hashicorp/external"
      version = "~> 2.0"
    }
    local = {
      source  = "hashicorp/local"
      version = "~> 2.0"
    }
  }
}

provider "external" {}
provider "local" {}

# Create external script
resource "local_file" "script" {
  filename        = "${path.module}/query.sh"
  file_permission = "0755"
  content         = <<-EOT
    #!/bin/bash
    # External data script must output JSON to stdout

    # Read input from stdin (optional)
    eval "$(jq -r '@sh "INPUT_VAR=\(.input_var)"')"

    # Perform computation or API call
    TIMESTAMP=$(date +%s)
    HOSTNAME=$(hostname)

    # Output JSON
    jq -n \
      --arg timestamp "$TIMESTAMP" \
      --arg hostname "$HOSTNAME" \
      --arg input "$INPUT_VAR" \
      '{"timestamp": $timestamp, "hostname": $hostname, "input": $input}'
  EOT
}

# External data source
data "external" "example" {
  program = ["bash", "${path.module}/query.sh"]  # => Command to execute

  query = {
    input_var = "test-value"                      # => Passed to script as JSON on stdin
  }
  # => Script must output JSON: {"key": "value"}

  depends_on = [local_file.script]
}

# Use external data
resource "local_file" "result" {
  filename = "external_result.txt"
  content  = <<-EOT
    Timestamp: ${data.external.example.result.timestamp}
    Hostname: ${data.external.example.result.hostname}
    Input: ${data.external.example.result.input}
  EOT
  # => data.external.name.result contains all JSON output keys
}

output "external_output" {
  value = data.external.example.result           # => Complete JSON output as map
}
```

**Python example**:

```hcl
# Python script (query.py)
resource "local_file" "python_script" {
  filename        = "${path.module}/query.py"
  file_permission = "0755"
  content         = <<-EOT
    #!/usr/bin/env python3
    import json
    import sys

    # Read input from stdin
    input_data = json.load(sys.stdin)

    # Process data
    result = {
        "api_key": input_data.get("key_name", "default"),
        "computed": "value-from-api-call"
    }

    # Output JSON to stdout
    json.dump(result, sys.stdout)
  EOT
}

data "external" "python_data" {
  program = ["python3", "${path.module}/query.py"]
  query   = { key_name = "production" }
  depends_on = [local_file.python_script]
}
```

**Key Takeaway**: External data source enables integration with any system that outputs JSON. Script must read JSON from stdin and write JSON to stdout. Use sparingly—prefer native Terraform providers when available. External programs run during `terraform plan`, making plans slower.

**Why It Matters**: External data sources bridge Terraform with proprietary systems lacking native providers—querying internal CMDB APIs, fetching credentials from custom vaults, or integrating legacy infrastructure databases. This extensibility prevented infrastructure teams from being blocked by missing providers while waiting for community or vendor support. However, external scripts bypass Terraform's dependency tracking and plan safety, so use only when native providers are unavailable, and migrate to proper providers when they become available to maintain infrastructure reliability.

---

## Group 6: State Management Basics

### Example 16: Understanding State

Terraform state tracks real infrastructure and maps resources to configuration. State is critical for Terraform operation—it determines what resources exist and what changes are needed.

**Code**:

```hcl
# state_demo.tf
terraform {
  required_version = ">= 1.0"

  # Backend configuration (where state is stored)
  backend "local" {
    path = "terraform.tfstate"             # => Local state file (default)
  }
}

provider "local" {}

resource "local_file" "state_example" {
  filename = "state_example.txt"
  content  = "State management demo"
}

# State commands (run via CLI)
# $ terraform state list
# => local_file.state_example (lists all resources in state)

# $ terraform state show local_file.state_example
# => Shows complete resource state and all attributes

# $ terraform state pull
# => Outputs state as JSON to stdout

# $ terraform state push terraform.tfstate.backup
# => Uploads local state file to configured backend (dangerous!)
```

**State file structure**:

```json
{
  "version": 4,
  "terraform_version": "1.9.0",
  "serial": 1,
  "lineage": "unique-uuid",
  "outputs": {},
  "resources": [
    {
      "mode": "managed",
      "type": "local_file",
      "name": "state_example",
      "provider": "provider[\"registry.terraform.io/hashicorp/local\"]",
      "instances": [
        {
          "schema_version": 0,
          "attributes": {
            "filename": "state_example.txt",
            "content": "State management demo",
            "id": "hash-of-content"
          }
        }
      ]
    }
  ]
}
```

**Key Takeaway**: State file (`terraform.tfstate`) maps configuration to real infrastructure. Never edit state files manually—use `terraform state` commands. State contains sensitive data—always store securely. Local state works for solo work; use remote state for teams (covered in intermediate).

**Why It Matters**: State files are Terraform's memory—without state, Terraform can't determine what infrastructure exists or what changes are needed. This mapping is why Terraform is idempotent (safe to run repeatedly) unlike imperative scripts that create duplicates. State contains resource IDs, IP addresses, passwords, and other secrets—lost or leaked state causes either infrastructure orphaning (can't destroy old resources) or security breaches (credentials exposed). Proper state management is critical: GitHub locks production state with S3 versioning and encryption, preventing the catastrophic "deleted state file, can't manage 10,000 resources" scenario.

---

### Example 17: State Commands

Terraform CLI provides commands to inspect and modify state. Essential for debugging, refactoring, and recovering from state issues.

**Code**:

```hcl
# state_operations.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {}

resource "local_file" "file1" {
  filename = "file1.txt"
  content  = "File 1 content"
}

resource "local_file" "file2" {
  filename = "file2.txt"
  content  = "File 2 content"
}

resource "local_file" "file3" {
  filename = "file3.txt"
  content  = "File 3 content"
}
```

**State operations**:

```bash
# List all resources in state
# $ terraform state list
# => local_file.file1
# => local_file.file2
# => local_file.file3

# Show specific resource details
# $ terraform state show local_file.file1
# => Displays all attributes for file1

# Move resource within state (rename)
# $ terraform state mv local_file.file1 local_file.renamed
# => Updates state to reflect new resource name
# => Update configuration to match: resource "local_file" "renamed" { }

# Remove resource from state (without destroying)
# $ terraform state rm local_file.file2
# => Removes from state but doesn't delete actual file
# => Terraform no longer manages this resource

# Import existing resource into state
# $ terraform import local_file.file2 file2.txt
# => Adds existing file2.txt to state management
# => Must have matching resource block in configuration

# Replace resource (force recreation)
# $ terraform apply -replace=local_file.file3
# => Forces destroy and recreate even if no changes
# => Useful for resources with external changes

# Refresh state from actual infrastructure
# $ terraform refresh
# => Updates state to match real infrastructure
# => Deprecated: use terraform apply -refresh-only

# Unlock state (if locked by crashed operation)
# $ terraform force-unlock <lock-id>
# => Removes state lock forcefully
# => Use with caution—only if lock is stale
```

**Refactoring example**:

```hcl
# Before refactoring
resource "local_file" "old_name" {
  filename = "file.txt"
  content  = "content"
}

# After refactoring (must update state)
resource "local_file" "new_name" {
  filename = "file.txt"
  content  = "content"
}

# Update state to match
# $ terraform state mv local_file.old_name local_file.new_name
```

**Key Takeaway**: Use `terraform state list` to view resources, `state show` for details, `state mv` for renaming, `state rm` for removing from management. Always backup state before manual operations (`cp terraform.tfstate terraform.tfstate.backup`). State commands enable refactoring without destroying infrastructure.

**Why It Matters**: State commands enable safe infrastructure refactoring that preserves resources during code reorganization. Teams can rename resources (`state mv`), split monolithic configurations into modules, or change resource identifiers without destroying and recreating production databases. This refactoring capability is critical for evolving infrastructure codebases—Airbnb reorganized thousands of resources across 50+ Terraform modules over months without downtime using state commands. Manual state operations are risky, but with proper backups, they prevent the "refactoring destroyed production" disasters common with tools lacking state management.

---

### Example 18: State Locking

State locking prevents concurrent modifications when multiple team members run Terraform simultaneously. Local backend doesn't support locking; remote backends do.

**Code**:

```hcl
# state_locking.tf
terraform {
  required_version = ">= 1.0"

  # Local backend (no locking support)
  backend "local" {
    path = "terraform.tfstate"
    # => No locking—unsafe for team usage
  }

  # S3 backend with DynamoDB locking (example for reference)
  # backend "s3" {
  #   bucket         = "terraform-state-bucket"
  #   key            = "project/terraform.tfstate"
  #   region         = "us-east-1"
  #   dynamodb_table = "terraform-state-lock"    # => Lock table
  #   encrypt        = true
  #   # => State file encrypted at rest
  #   # => DynamoDB prevents concurrent applies
  # }
}

provider "local" {}

resource "local_file" "locking_demo" {
  filename = "locking.txt"
  content  = "State locking demonstration"
}

# Lock behavior
# $ terraform apply
# => Acquires lock on state before apply
# => Other terraform commands wait for lock release
# => Lock automatically released after apply completes or fails

# Force unlock (emergency only)
# $ terraform force-unlock <lock-id>
# => Use only if lock is stale (crashed operation)
```

**Lock states**:

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["State: Unlocked"] --> B["User 1: terraform apply"]
    B --> C["State: Locked by User 1"]
    C --> D["User 2: terraform apply"]
    D --> E["User 2: Waits..."]
    C --> F["User 1: Apply Complete"]
    F --> G["State: Unlocked"]
    G --> H["User 2: Acquires Lock"]

    style A fill:#029E73,color:#fff
    style C fill:#DE8F05,color:#fff
    style E fill:#CC78BC,color:#fff
    style G fill:#029E73,color:#fff
```

**Key Takeaway**: State locking prevents race conditions in team environments. Local backend doesn't support locking—use remote backends (S3+DynamoDB, Terraform Cloud, Azure Blob) for teams. Locks are acquired automatically during state-modifying operations (apply, destroy).

**Why It Matters**: State locking prevents the catastrophic race condition where two engineers run `terraform apply` simultaneously and corrupt state, losing track of infrastructure. Without locking, concurrent applies cause "state file conflicts" requiring manual merge—impossible when state contains binary data. This happened at scale before remote backends: split-brain state corrupted production tracking, orphaning resources that accumulated cloud costs unnoticed. Remote backends with locking (S3+DynamoDB, Terraform Cloud) make concurrent operations safe, essential for teams where CI/CD and engineers share infrastructure, preventing the coordination overhead of manual scheduling.

---

**🎯 Beginner level (Examples 1-18) complete!** Continue to Examples 19-28 below for variables deep-dive and state management patterns.

## Group 7: Variables Deep Dive

### Example 19: Variable Validation

Variable validation ensures inputs meet requirements before infrastructure changes. Prevents invalid configurations from reaching cloud providers.

**Code**:

```hcl
# validation.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {}

# String pattern validation
variable "environment" {
  description = "Environment name"
  type        = string

  validation {
    condition     = can(regex("^(dev|staging|prod)$", var.environment))
    error_message = "Environment must be dev, staging, or prod."
  }
  # => Validates environment matches allowed values
}

# Numeric range validation
variable "instance_count" {
  description = "Number of instances (1-10)"
  type        = number
  default     = 1

  validation {
    condition     = var.instance_count >= 1 && var.instance_count <= 10
    error_message = "Instance count must be between 1 and 10."
  }
}

# String length validation
variable "project_name" {
  description = "Project name (3-20 characters)"
  type        = string

  validation {
    condition     = length(var.project_name) >= 3 && length(var.project_name) <= 20
    error_message = "Project name must be 3-20 characters long."
  }
}

# CIDR validation
variable "vpc_cidr" {
  description = "VPC CIDR block"
  type        = string
  default     = "10.0.0.0/16"

  validation {
    condition     = can(cidrhost(var.vpc_cidr, 0))  # => Validates CIDR format
    error_message = "VPC CIDR must be valid IPv4 CIDR notation."
  }
}

# Complex object validation
variable "server_config" {
  description = "Server configuration"
  type = object({
    cpu    = number
    memory = number
  })

  validation {
    condition     = var.server_config.cpu >= 1 && var.server_config.cpu <= 64
    error_message = "CPU count must be 1-64."
  }

  validation {
    condition     = var.server_config.memory >= 1 && var.server_config.memory <= 256
    error_message = "Memory must be 1-256 GB."
  }
  # => Multiple validation blocks allowed
}

# List content validation
variable "availability_zones" {
  description = "List of availability zones"
  type        = list(string)

  validation {
    condition     = length(var.availability_zones) >= 2
    error_message = "At least 2 availability zones required for HA."
  }

  validation {
    condition     = alltrue([for az in var.availability_zones : can(regex("^[a-z]{2}-[a-z]+-[0-9][a-z]$", az))])
    error_message = "Availability zones must match pattern: us-east-1a."
  }
}

resource "local_file" "validated_config" {
  filename = "validated.txt"
  content  = <<-EOT
    Environment: ${var.environment}
    Instances: ${var.instance_count}
    Project: ${var.project_name}
    VPC CIDR: ${var.vpc_cidr}
  EOT
}
```

**Validation error example**:

```bash
# $ terraform plan -var="environment=production"
# => Error: Invalid value for variable
# => Environment must be dev, staging, or prod.
```

**Key Takeaway**: Use validation blocks to enforce constraints on variable values. Validation runs during `terraform plan`, catching errors before infrastructure changes. Use `can()` function for format validation (regex, CIDR). Multiple validation blocks allowed per variable.

**Why It Matters**: Variable validation catches configuration errors in CI/CD before reaching cloud providers, preventing expensive deployment failures. Instead of hitting AWS quota limits because instance_count=1000 (typo for 10), validation rejects invalid values immediately with clear error messages. This shifts error detection left—"environment must be prod/staging/dev" fails in 1 second during plan instead of discovering incorrect tags on 500 deployed resources 30 minutes later. Validation is especially critical for modules consumed by multiple teams, enforcing organizational standards like "CIDR blocks must not overlap" that prevent production networking conflicts.

---

### Example 20: Variable Precedence

Terraform loads variables from multiple sources with defined precedence. Understanding precedence prevents unexpected values in production.

**Code**:

```hcl
# precedence.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {}

variable "app_name" {
  description = "Application name"
  type        = string
  default     = "default-app"              # => Precedence: 1 (lowest)
}

variable "environment" {
  description = "Environment"
  type        = string
  default     = "development"
}

resource "local_file" "precedence_demo" {
  filename = "precedence.txt"
  content  = <<-EOT
    App: ${var.app_name}
    Environment: ${var.environment}
  EOT
}
```

**Variable precedence (lowest to highest)**:

1. **Variable defaults in configuration**

   ```hcl
   variable "app_name" {
     default = "default-app"               # => Precedence: 1
   }
   ```

2. **Environment variables (`TF_VAR_name`)**

   ```bash
   # $ export TF_VAR_app_name="env-app"   # => Precedence: 2
   # $ terraform apply
   ```

3. **`terraform.tfvars` file**

   ```hcl
   # terraform.tfvars (automatically loaded)
   app_name = "tfvars-app"                 # => Precedence: 3
   ```

4. **`*.auto.tfvars` files (alphabetically)**

   ```hcl
   # dev.auto.tfvars (automatically loaded)
   app_name = "auto-tfvars-app"            # => Precedence: 4
   ```

5. **`-var-file` CLI option**

   ```bash
   # $ terraform apply -var-file="custom.tfvars"
   # custom.tfvars: app_name = "var-file-app"  # => Precedence: 5
   ```

6. **`-var` CLI option (highest)**

   ```bash
   # $ terraform apply -var="app_name=cli-app"  # => Precedence: 6 (highest)
   ```

**Complete example**:

```bash
# Configuration has default: "default-app"
# terraform.tfvars has: "tfvars-app"
# CLI provides: -var="app_name=cli-app"
# $ terraform apply -var="app_name=cli-app"
# => Result: "cli-app" (CLI has highest precedence)
```

**Key Takeaway**: CLI `-var` has highest precedence, followed by `-var-file`, `*.auto.tfvars`, `terraform.tfvars`, `TF_VAR_*` environment variables, and variable defaults. Use `terraform.tfvars` for environment defaults, CLI `-var` for overrides. Never commit sensitive values to tfvars files.

**Why It Matters**: Understanding precedence prevents production incidents from unexpected variable values. Teams commit terraform.tfvars with safe defaults but override sensitive values via CI/CD environment variables (highest precedence) at deploy time, keeping secrets out of version control. This layered configuration enables flexible workflows: developers use local tfvars for testing, CI/CD injects production credentials via -var, and emergency overrides use CLI flags without file modifications. Precedence mistakes cause real damage—Slack incident where dev.auto.tfvars accidentally deployed to production because precedence wasn't understood, requiring emergency rollback.

---

### Example 21: Sensitive Variables

Sensitive variables prevent accidental exposure in logs, plan output, and state files. Essential for secrets, API keys, and credentials.

**Code**:

```hcl
# sensitive.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {}

# Sensitive variable declaration
variable "api_key" {
  description = "API key for external service"
  type        = string
  sensitive   = true                       # => Marks variable as sensitive
  # => Value hidden in plan/apply output as <sensitive>
}

variable "database_password" {
  description = "Database password"
  type        = string
  sensitive   = true
}

# Sensitive locals
locals {
  connection_string = "postgres://user:${var.database_password}@host:5432/db"
  # => Local inherits sensitive marking from var.database_password
}

resource "local_file" "config" {
  filename = "config.txt"
  content  = <<-EOT
    API Key: ${var.api_key}
    Connection: ${local.connection_string}
  EOT
  # => File content contains secrets (handle carefully)
}

# Sensitive output
output "api_key_output" {
  description = "API key (sensitive)"
  value       = var.api_key
  sensitive   = true                       # => Output marked sensitive
  # => terraform output shows: <sensitive>
  # => terraform output -raw api_key_output shows actual value
}

output "connection_string" {
  description = "Database connection string"
  value       = local.connection_string
  sensitive   = true
}

# Non-sensitive output (derived from sensitive)
output "api_key_length" {
  description = "Length of API key (non-sensitive)"
  value       = length(var.api_key)
  # => Shows: 32 (length is not sensitive, actual value is)
}
```

**Usage**:

```bash
# Provide sensitive variables
# $ terraform apply -var="api_key=secret123" -var="database_password=pass123"

# Or via environment variables (recommended)
# $ export TF_VAR_api_key="secret123"
# $ export TF_VAR_database_password="pass123"
# $ terraform apply

# Plan output (sensitive values hidden)
# => var.api_key: <sensitive>
# => var.database_password: <sensitive>

# Output sensitive value
# $ terraform output api_key_output
# => <sensitive>

# Force show sensitive value
# $ terraform output -raw api_key_output
# => secret123 (actual value shown)
```

**State file behavior**:

```json
{
  "resources": [
    {
      "instances": [
        {
          "attributes": {
            "content": "API Key: secret123\n..."
          }
        }
      ]
    }
  ]
}
```

**⚠️ Sensitive values appear in plaintext in state file!** Always encrypt state (S3 encryption, Terraform Cloud encryption).

**Key Takeaway**: Mark sensitive variables with `sensitive = true` to prevent exposure in logs. Sensitive variables propagate to locals and outputs that reference them. Sensitive marking is for display only—values still appear in state file. Always encrypt state storage for production.

**Why It Matters**: Sensitive variable marking prevents accidental credential leaks in CI/CD logs, plan output pasted in Slack, or Terraform Cloud UI screenshots shared with stakeholders. However, sensitive values still appear plaintext in state files—this is why Terraform Cloud encrypts state and GitHub uses S3 encryption at rest. The sensitive flag is a safety net, not security—it prevents casual exposure but can't protect against state file exfiltration. Real security requires encrypted state storage, access controls, and secret managers (Vault, AWS Secrets Manager) instead of hardcoded values.

---

### Example 22: Variable Files Organization

Organizing variable files by environment and purpose improves maintainability. Standard patterns separate shared values from environment-specific overrides.

**Code**:

**Directory structure**:

```
project/
├── main.tf
├── variables.tf           # => Variable declarations (no values)
├── outputs.tf
├── terraform.tfvars       # => Shared defaults
├── environments/
│   ├── dev.tfvars        # => Development overrides
│   ├── staging.tfvars    # => Staging overrides
│   └── prod.tfvars       # => Production overrides
└── secrets/
    └── dev.secrets.tfvars  # => Sensitive values (gitignored)
```

**`variables.tf`** (declarations only):

```hcl
variable "project_name" {
  description = "Project name"
  type        = string
}

variable "environment" {
  description = "Environment name"
  type        = string
}

variable "instance_count" {
  description = "Number of instances"
  type        = number
}

variable "enable_monitoring" {
  description = "Enable monitoring"
  type        = bool
}

variable "api_key" {
  description = "API key"
  type        = string
  sensitive   = true
}
```

**`terraform.tfvars`** (shared defaults):

```hcl
# Shared across all environments
project_name       = "myapp"
enable_monitoring  = true
```

**`environments/dev.tfvars`** (development):

```hcl
environment     = "development"
instance_count  = 1
```

**`environments/prod.tfvars`** (production):

```hcl
environment     = "production"
instance_count  = 3
```

**`secrets/dev.secrets.tfvars`** (sensitive, gitignored):

```hcl
api_key = "dev-api-key-12345"
```

**`.gitignore`**:

```
# Ignore sensitive variable files
secrets/
*.secrets.tfvars

# Ignore state files
*.tfstate
*.tfstate.*
.terraform/
```

**Usage**:

```bash
# Development environment
# $ terraform apply -var-file="environments/dev.tfvars" -var-file="secrets/dev.secrets.tfvars"

# Production environment
# $ terraform apply -var-file="environments/prod.tfvars" -var-file="secrets/prod.secrets.tfvars"

# CI/CD usage
# $ terraform apply \
#   -var-file="environments/${ENV}.tfvars" \
#   -var="api_key=${API_KEY_FROM_CI}"
```

**Main configuration** (`main.tf`):

```hcl
terraform {
  required_version = ">= 1.0"
}

provider "local" {}

resource "local_file" "config" {
  filename = "${var.environment}-config.txt"
  content  = <<-EOT
    Project: ${var.project_name}
    Environment: ${var.environment}
    Instances: ${var.instance_count}
    Monitoring: ${var.enable_monitoring}
  EOT
}
```

**Key Takeaway**: Separate variable declarations (`variables.tf`), shared defaults (`terraform.tfvars`), and environment overrides (`env.tfvars`). Never commit sensitive values—use gitignored `*.secrets.tfvars` or CI/CD secrets. Use `-var-file` to combine multiple variable files.

**Why It Matters**: Organized variable files enable clean environment promotion in CI/CD pipelines—same code deploys to dev/staging/prod by swapping tfvars files. This pattern is standard at Netflix and Uber: infrastructure code lives in one repository, environment-specific values in separate files, preventing the "three divergent codebases" problem where dev uses different modules than production. Gitignored secrets files prevent the security breach of committed API keys discovered by GitHub scanners, while CI/CD-injected variables enable secret rotation without code changes, supporting compliance requirements for credential management.

---

## Group 8: Data Sources Advanced

### Example 23: Data Source Dependencies

Data sources can depend on resources and other data sources. Understanding dependency order prevents errors when querying infrastructure that doesn't exist yet.

**Code**:

```hcl
# data_dependencies.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {}

# Step 1: Create base resource
resource "local_file" "base" {
  filename = "base.txt"
  content  = "Base file for data source"
}

# Step 2: Data source depends on resource
data "local_file" "read_base" {
  filename   = local_file.base.filename    # => Implicit dependency via reference
  # => Terraform creates base.txt before reading it
  # => No explicit depends_on needed when using references
}

# Step 3: Create resource using data source
resource "local_file" "derived" {
  filename = "derived.txt"
  content  = "Content from base: ${data.local_file.read_base.content}"
  # => Implicit dependency chain: base -> read_base -> derived
}

# Explicit dependency (when no reference exists)
resource "local_file" "source_explicit" {
  filename = "explicit.txt"
  content  = "Explicit source"
}

data "local_file" "read_explicit" {
  filename   = "explicit.txt"
  depends_on = [local_file.source_explicit]  # => Explicit dependency
  # => Use when data source doesn't reference resource attributes
}

# Multiple dependencies
resource "local_file" "file1" {
  filename = "file1.txt"
  content  = "File 1"
}

resource "local_file" "file2" {
  filename = "file2.txt"
  content  = "File 2"
}

data "local_file" "read_file1" {
  filename   = local_file.file1.filename
}

data "local_file" "read_file2" {
  filename   = local_file.file2.filename
}

# Resource depends on multiple data sources
resource "local_file" "combined" {
  filename = "combined.txt"
  content  = "${data.local_file.read_file1.content} + ${data.local_file.read_file2.content}"
  # => Implicit dependencies: file1 -> read_file1 -\
  # =>                       file2 -> read_file2 --> combined
}

output "dependency_chain" {
  value = {
    base_content    = data.local_file.read_base.content
    derived_content = local_file.derived.content
    combined        = local_file.combined.content
  }
}
```

**Dependency graph**:

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["local_file.base"] --> B["data.local_file.read_base"]
    B --> C["local_file.derived"]

    D["local_file.file1"] --> E["data.local_file.read_file1"]
    F["local_file.file2"] --> G["data.local_file.read_file2"]
    E --> H["local_file.combined"]
    G --> H

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#0173B2,color:#fff
    style E fill:#DE8F05,color:#fff
    style F fill:#0173B2,color:#fff
    style G fill:#DE8F05,color:#fff
    style H fill:#CC78BC,color:#fff
```

**Key Takeaway**: Data sources automatically depend on referenced resources. Use explicit `depends_on` only when no reference exists (e.g., querying by name/tag instead of ID). Terraform executes in dependency order: resources first, then data sources that read them.

**Why It Matters**: Understanding data source dependencies prevents "data source queried before resource exists" errors that cause CI/CD pipeline failures. Implicit dependencies through references ensure Terraform creates VPC before data source queries its subnets, eliminating race conditions. This automatic ordering is critical for complex infrastructures where GitHub manages hundreds of interdependent resources—explicit depends_on everywhere would be unmaintainable. Proper dependency handling enables eventually-consistent APIs (AWS) where resources need propagation time before being queryable, preventing intermittent failures that plagued early Infrastructure as Code tools.

---

### Example 24: Terraform Data Source

The `terraform_remote_state` data source reads outputs from other Terraform configurations. Enables infrastructure composition without tight coupling.

**Code**:

**Configuration 1 (network)** - `network/main.tf`:

```hcl
terraform {
  required_version = ">= 1.0"

  backend "local" {
    path = "network.tfstate"               # => Separate state file
  }
}

provider "local" {}

resource "local_file" "network_config" {
  filename = "network.txt"
  content  = "VPC: 10.0.0.0/16"
}

# Export values for other configurations
output "vpc_cidr" {
  value = "10.0.0.0/16"
}

output "subnet_cidrs" {
  value = ["10.0.1.0/24", "10.0.2.0/24"]
}

output "vpc_id" {
  value = "vpc-12345"
}
```

**Configuration 2 (application)** - `app/main.tf`:

```hcl
terraform {
  required_version = ">= 1.0"

  backend "local" {
    path = "app.tfstate"                   # => Different state file
  }
}

provider "local" {}

# Read outputs from network configuration
data "terraform_remote_state" "network" {
  backend = "local"

  config = {
    path = "../network/network.tfstate"   # => Path to network state
  }
  # => Reads outputs from network configuration
}

# Use network outputs
resource "local_file" "app_config" {
  filename = "app.txt"
  content  = <<-EOT
    VPC ID: ${data.terraform_remote_state.network.outputs.vpc_id}
    VPC CIDR: ${data.terraform_remote_state.network.outputs.vpc_cidr}
    Subnets: ${jsonencode(data.terraform_remote_state.network.outputs.subnet_cidrs)}
  EOT
  # => References network outputs via data source
}

output "app_network_info" {
  value = {
    vpc_id       = data.terraform_remote_state.network.outputs.vpc_id
    vpc_cidr     = data.terraform_remote_state.network.outputs.vpc_cidr
    subnet_count = length(data.terraform_remote_state.network.outputs.subnet_cidrs)
  }
}
```

**S3 backend example** (production pattern):

```hcl
# Network configuration with S3 backend
terraform {
  backend "s3" {
    bucket = "terraform-state-bucket"
    key    = "network/terraform.tfstate"
    region = "us-east-1"
  }
}

# Application reads from S3
data "terraform_remote_state" "network" {
  backend = "s3"

  config = {
    bucket = "terraform-state-bucket"
    key    = "network/terraform.tfstate"
    region = "us-east-1"
  }
}
```

**Usage workflow**:

```bash
# Apply network configuration first
# $ cd network
# $ terraform init
# $ terraform apply

# Then apply application configuration
# $ cd ../app
# $ terraform init
# $ terraform apply
# => Reads network outputs from remote state
```

**Key Takeaway**: Use `terraform_remote_state` data source to share outputs between configurations. This enables infrastructure composition—network team manages VPC, application team reads VPC outputs. Only outputs are accessible (not full state). Always define outputs explicitly for values other configurations need.

**Why It Matters**: Remote state data sources enable organizational scaling where platform teams provision shared infrastructure (networks, clusters) and product teams consume outputs to deploy applications, eliminating coordination bottlenecks. This separation of concerns is how Netflix supports hundreds of microservices teams—platform provides VPC IDs through remote state, teams deploy independently without requesting network changes. Only exposing outputs (not full state) provides abstraction boundaries and security: application teams can't access database passwords stored in network team's state, following principle of least privilege for infrastructure access.

---

## Group 9: State Management Patterns

### Example 25: State Backup and Recovery

Terraform automatically creates backup files before state modifications. Understanding backup strategy prevents catastrophic data loss.

**Code**:

```hcl
# state_backup.tf
terraform {
  required_version = ">= 1.0"

  backend "local" {
    path = "terraform.tfstate"
  }
}

provider "local" {}

resource "local_file" "backup_demo" {
  filename = "backup_demo.txt"
  content  = "State backup demonstration"
}
```

**Backup behavior**:

```bash
# Initial apply creates state
# $ terraform apply
# => Creates: terraform.tfstate

# Modify configuration and apply
# content = "Updated content"
# $ terraform apply
# => Creates backup: terraform.tfstate.backup (previous state)
# => Updates: terraform.tfstate (new state)

# Backup file structure
# $ ls -la
# => terraform.tfstate         (current state)
# => terraform.tfstate.backup  (previous state)

# Manual backup before risky operations
# $ cp terraform.tfstate terraform.tfstate.$(date +%Y%m%d-%H%M%S)
# => terraform.tfstate.20240115-143022 (timestamped backup)
```

**State recovery scenarios**:

**Scenario 1: Restore from automatic backup**

```bash
# Current state corrupted or wrong
# $ mv terraform.tfstate terraform.tfstate.broken
# $ mv terraform.tfstate.backup terraform.tfstate
# $ terraform plan
# => Uses restored state
```

**Scenario 2: Restore from manual backup**

```bash
# $ cp terraform.tfstate.20240115-143022 terraform.tfstate
# $ terraform plan
# => Restored to specific point in time
```

**Scenario 3: State lost but infrastructure exists**

```bash
# State file deleted but resources exist
# $ terraform import local_file.backup_demo backup_demo.txt
# => Re-imports existing resource into state
```

**Remote state backup** (S3 example):

```hcl
terraform {
  backend "s3" {
    bucket         = "terraform-state-bucket"
    key            = "project/terraform.tfstate"
    region         = "us-east-1"

    # S3 versioning provides automatic backup
    # Enable versioning on S3 bucket
  }
}

# S3 bucket versioning configuration (separate Terraform config)
resource "aws_s3_bucket_versioning" "state_bucket" {
  bucket = "terraform-state-bucket"

  versioning_configuration {
    status = "Enabled"                     # => S3 keeps all state versions
  }
}
```

**Backup best practices**:

```hcl
# Script for production state backup
#!/bin/bash
# backup_state.sh

STATE_FILE="terraform.tfstate"
BACKUP_DIR="state_backups"
TIMESTAMP=$(date +%Y%m%d-%H%M%S)

mkdir -p "$BACKUP_DIR"
cp "$STATE_FILE" "$BACKUP_DIR/terraform.tfstate.$TIMESTAMP"

# Keep only last 10 backups
cd "$BACKUP_DIR"
ls -t terraform.tfstate.* | tail -n +11 | xargs rm -f
```

**Key Takeaway**: Terraform creates `.backup` files automatically before state changes. Always backup state before manual operations. Use S3 versioning or Git for remote state backups. Test state recovery procedures regularly. Never commit state files to public repositories—they contain sensitive data.

**Why It Matters**: State file loss causes infrastructure orphaning—resources still running and accumulating costs but Terraform can't manage or destroy them. This happened to numerous teams before proper backup strategies: S3 bucket deleted, state lost, thousands of dollars in zombie resources until manual cleanup. Automatic backups and S3 versioning prevent these incidents, enabling point-in-time recovery when state corruption occurs. Testing recovery procedures is critical—Dropbox discovered their state backup process was broken during a real outage, learning the hard way that untested backups aren't backups.

---

### Example 26: State Refresh

State refresh updates Terraform state to match real infrastructure without modifying resources. Essential when infrastructure changes outside Terraform.

**Code**:

```hcl
# state_refresh.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {}

resource "local_file" "refresh_demo" {
  filename = "refresh_demo.txt"
  content  = "Original content"
}

output "file_content" {
  value = local_file.refresh_demo.content
}
```

**Refresh scenarios**:

**Scenario 1: Manual file modification**

```bash
# Apply initial configuration
# $ terraform apply
# => Creates refresh_demo.txt with "Original content"

# Manually modify file outside Terraform
# $ echo "Modified content" > refresh_demo.txt

# Refresh state to detect drift
# $ terraform apply -refresh-only
# => Detects: content changed from "Original content" to "Modified content"
# => Updates state without modifying infrastructure
# => Shows: # local_file.refresh_demo will be updated in-place

# Review detected changes
# $ terraform show
# => State now reflects "Modified content"

# Next apply will restore to configuration
# $ terraform apply
# => Changes: content from "Modified content" to "Original content"
```

**Scenario 2: Resource deleted outside Terraform**

```bash
# Delete file manually
# $ rm refresh_demo.txt

# Refresh detects missing resource
# $ terraform apply -refresh-only
# => Detects: local_file.refresh_demo has been deleted
# => Removes from state

# Plan shows resource will be created
# $ terraform plan
# => Plan: 1 to add, 0 to change, 0 to destroy
```

**Refresh modes**:

```bash
# Refresh-only (safe, no infrastructure changes)
# $ terraform apply -refresh-only
# => Updates state only, no resource modifications
# => Prompts for confirmation

# Auto-approve refresh
# $ terraform apply -refresh-only -auto-approve
# => Refreshes without confirmation (CI/CD usage)

# Refresh during plan (default behavior)
# $ terraform plan
# => Automatically refreshes state before planning
# => Detected changes shown in plan output

# Disable refresh during plan
# $ terraform plan -refresh=false
# => Uses existing state without refreshing
# => Faster but may miss drift
```

**Drift detection workflow**:

```hcl
# Check for configuration drift
# $ terraform plan -refresh-only

# If drift detected, choose action:
# 1. Apply to restore to configuration
# $ terraform apply

# 2. Update configuration to match reality
# Edit .tf files to match current state

# 3. Accept drift and update state
# $ terraform apply -refresh-only -auto-approve
```

**Key Takeaway**: Use `terraform apply -refresh-only` to update state without modifying infrastructure. Refresh detects drift (changes made outside Terraform). Refresh happens automatically during `plan` and `apply`. Use `-refresh=false` to skip refresh for faster plans when drift detection isn't needed.

**Why It Matters**: Drift detection identifies unauthorized manual changes that violate infrastructure-as-code principles and create security vulnerabilities. When junior engineer fixes production outage via AWS console instead of Terraform, refresh-only detects the change, prompting either restoration to defined configuration or explicit code update. This prevents configuration drift where actual infrastructure diverges from code, causing the "infrastructure doesn't match documentation" problem that makes incident response dangerous. Organizations like GitHub run scheduled drift detection to audit infrastructure compliance, catching unauthorized changes before they compound into unmanageable chaos.

---

### Example 27: Targeting Specific Resources

Resource targeting applies changes to specific resources instead of entire configuration. Useful for debugging, gradual rollouts, and working around dependency issues.

**Code**:

```hcl
# targeting.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {}

resource "local_file" "file1" {
  filename = "file1.txt"
  content  = "File 1 content"
}

resource "local_file" "file2" {
  filename = "file2.txt"
  content  = "File 2 content"
}

resource "local_file" "file3" {
  filename = "file3.txt"
  content  = "File 3 content"
}

resource "local_file" "file4" {
  filename = "file4.txt"
  content  = "File 4 depends on file3"
  depends_on = [local_file.file3]
}
```

**Targeting operations**:

**Target single resource**:

```bash
# Plan changes for specific resource only
# $ terraform plan -target=local_file.file1
# => Plans changes only for file1, ignores file2, file3, file4

# Apply to specific resource
# $ terraform apply -target=local_file.file1
# => Creates/updates only file1
# => Other resources unchanged
```

**Target multiple resources**:

```bash
# Plan multiple targets
# $ terraform plan -target=local_file.file1 -target=local_file.file2
# => Plans changes for file1 and file2

# Apply multiple targets
# $ terraform apply -target=local_file.file1 -target=local_file.file2
```

**Target with dependencies**:

```bash
# Target resource with dependencies
# $ terraform apply -target=local_file.file4
# => Terraform includes dependencies automatically
# => Applies: file3 (dependency) and file4 (target)
```

**Target modules** (covered in intermediate):

```bash
# Target entire module
# $ terraform apply -target=module.network

# Target resource in module
# $ terraform apply -target=module.network.aws_vpc.main
```

**Destroy specific resources**:

```bash
# Destroy only file2
# $ terraform destroy -target=local_file.file2
# => Removes only file2, leaves file1, file3, file4
```

**Use cases for targeting**:

1. **Debugging specific resources**:

   ```bash
   # $ terraform apply -target=aws_instance.problematic
   ```

2. **Gradual rollout**:

   ```bash
   # $ terraform apply -target=aws_instance.web[0]
   # $ terraform apply -target=aws_instance.web[1]
   ```

3. **Work around temporary issues**:

   ```bash
   # $ terraform apply -target=module.database
   # Wait for database ready, then:
   # $ terraform apply  # Apply rest of configuration
   ```

**⚠️ Warning**: Targeting creates partial state updates and can lead to inconsistencies. Use sparingly, primarily for debugging. Always follow targeted apply with full apply to ensure consistency.

```bash
# After targeted operations, always run full apply
# $ terraform apply -target=resource.name  # Targeted operation
# $ terraform apply                        # Full apply to fix consistency
```

**Key Takeaway**: Use `-target` to limit operations to specific resources and their dependencies. Essential for debugging but dangerous for regular use—it can create state inconsistencies. Always run full `terraform apply` after targeted operations to restore consistency.

**Why It Matters**: Targeting enables surgical infrastructure fixes during production incidents without touching unrelated resources, reducing blast radius. When database performance degrades, target database configuration changes without risking network or compute modifications. However, overuse creates partial state that causes "plan shows unexpected changes" confusion. This is why Netflix restricts targeting to incident response and requires follow-up full applies—targeted operations are emergency tools, not standard practice. Targeting also enables gradual rollouts: deploy 1 instance, verify, target next instance, preventing "deployed 100 broken instances simultaneously" failures.

---

### Example 28: Import Existing Resources

Terraform import brings existing infrastructure under Terraform management. Essential for migrating to Terraform or recovering from state loss.

**Code**:

```hcl
# import.tf
terraform {
  required_version = ">= 1.0"
}

provider "local" {}

# Step 1: Create resource block (without apply)
resource "local_file" "imported" {
  filename = "existing.txt"
  content  = "This file already exists"
  # => Resource block must exist before import
  # => Configuration should match existing resource
}

resource "local_file" "imported2" {
  filename = "existing2.txt"
  content  = "Another existing file"
}
```

**Import workflow**:

**Step 1: Create existing resource manually** (simulating pre-existing infrastructure):

```bash
# $ echo "This file already exists" > existing.txt
# $ echo "Another existing file" > existing2.txt
```

**Step 2: Write resource block** (configuration must match existing resource):

```hcl
resource "local_file" "imported" {
  filename = "existing.txt"
  content  = "This file already exists"
}
```

**Step 3: Import resource**:

```bash
# Import syntax: terraform import RESOURCE_ADDRESS RESOURCE_ID
# $ terraform import local_file.imported existing.txt
# => Reads existing.txt and adds to state
# => Output: local_file.imported: Import successful!
```

**Step 4: Verify import**:

```bash
# Check state
# $ terraform state list
# => local_file.imported

# View imported resource
# $ terraform state show local_file.imported

# Verify configuration matches
# $ terraform plan
# => Should show: No changes. Infrastructure is up-to-date.
# => If changes shown, configuration doesn't match existing resource
```

**Import multiple resources**:

```bash
# Import each resource individually
# $ terraform import local_file.imported existing.txt
# $ terraform import local_file.imported2 existing2.txt
```

**Complex import example (AWS)**:

```hcl
# Resource configuration
resource "aws_instance" "imported_server" {
  ami           = "ami-0c55b159cbfafe1f0"
  instance_type = "t2.micro"
  # ... other required attributes
}

# Import command
# $ terraform import aws_instance.imported_server i-1234567890abcdef0
# => Imports existing EC2 instance by instance ID
```

**Import challenges and solutions**:

**Challenge 1: Unknown attribute values**

```bash
# Import first, then extract attributes
# $ terraform import aws_instance.web i-12345
# $ terraform state show aws_instance.web
# => Copy attribute values to configuration
```

**Challenge 2: Complex resources**

```hcl
# Use terraform state show output to guide configuration
# $ terraform state show resource.name > resource.json
# => Review all attributes and add to configuration
```

**Challenge 3: Import blocks (Terraform 1.5+)**

```hcl
# Import block (declarative import)
import {
  to = local_file.imported
  id = "existing.txt"
}

resource "local_file" "imported" {
  filename = "existing.txt"
  content  = "This file already exists"
}

# Run plan to see import
# $ terraform plan
# => Shows: local_file.imported will be imported

# Apply to execute import
# $ terraform apply
# => Imports resource into state
```

**Import workflow diagram**:

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Existing Resource<br/>in Cloud"] --> B["Write Resource<br/>Configuration"]
    B --> C["terraform import<br/>ADDR ID"]
    C --> D["Verify:<br/>terraform plan"]
    D --> E{Changes?}
    E -->|Yes| F["Adjust Config<br/>to Match"]
    E -->|No| G["Import Complete"]
    F --> D

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#CC78BC,color:#fff
    style E fill:#CA9161,color:#fff
    style F fill:#DE8F05,color:#fff
    style G fill:#029E73,color:#fff
```

**Key Takeaway**: Import brings existing resources under Terraform management. Must write resource configuration before importing. Import adds resource to state but doesn't generate configuration automatically. Use `terraform state show` to view imported attributes and adjust configuration. Import is one-way—it doesn't modify existing resources.

**Why It Matters**: Import enables gradual Terraform adoption without destroying and recreating existing infrastructure—critical for production systems that can't tolerate downtime. Organizations like Airbnb imported 5,000+ manually-created resources over months, bringing infrastructure under version control and enabling safe automation. Import also recovers from state loss: if state file is deleted but resources still exist, import reconstructs state and restores Terraform management. The import workflow (write config, import, verify plan) ensures configuration matches reality before any modifications, preventing the "import caused unexpected changes" surprises that plagued early import implementations.

---

**🎯 Beginner level complete!** You've covered HCL fundamentals, providers, resources, variables, data sources, and state management. These foundations enable basic infrastructure provisioning and management. Proceed to [Intermediate](/en/learn/software-engineering/infrastructure/tools/terraform/by-example/intermediate) for modules, remote state, workspaces, and production patterns.
