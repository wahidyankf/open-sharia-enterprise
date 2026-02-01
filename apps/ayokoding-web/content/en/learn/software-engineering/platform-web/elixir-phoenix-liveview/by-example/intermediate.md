---
title: "Intermediate"
date: 2026-02-01T00:00:00+07:00
draft: false
weight: 10000002
description: "Examples 31-60: Phoenix LiveView intermediate patterns including forms with validation, state management strategies, PubSub real-time features, and file upload handling"
tags: ["phoenix", "liveview", "tutorial", "by-example", "intermediate", "forms", "validation", "pubsub"]
---

**Intermediate examples** cover forms and validation, state management patterns, real-time communication with PubSub, and file upload handling. These examples assume understanding of LiveView basics (mount, assigns, events, templates) and demonstrate practical patterns for production applications.

## Forms and Validation (Examples 31-40)

Forms in LiveView combine Ecto changesets with real-time validation, providing immediate feedback without page reloads.

### Example 31: Form Basics with Changesets

Forms in LiveView use Ecto changesets for validation and transformation. The changeset tracks form state and validation errors in real-time.

**Code**:

```elixir
defmodule MyAppWeb.UserFormLive do
  use MyAppWeb, :live_view
  alias MyApp.Accounts.User
  import Ecto.Changeset

  # Initialize form with empty changeset
  def mount(_params, _session, socket) do
    changeset = change_user(%User{}) # => Empty changeset for User struct
    # => changeset.valid? is true (no validations run yet)
    socket = assign(socket, :form, to_form(changeset)) # => Convert changeset to form struct
    # => form ready for rendering with .to_form/1
    {:ok, socket} # => Socket ready with form assign
  end

  # Handle form validation on input changes
  def handle_event("validate", %{"user" => user_params}, socket) do
    # Run validations without persisting
    changeset =
      %User{}
      |> change_user(user_params) # => Applies user_params to empty User
      |> Map.put(:action, :validate) # => Marks changeset as validation (shows errors)
    # => changeset may have errors if validation fails

    socket = assign(socket, :form, to_form(changeset)) # => Update form with validation results
    {:noreply, socket} # => Re-render with error messages
  end

  def render(assigns) do
    ~H"""
    <.form for={@form} phx-change="validate" phx-submit="save">
      <.input field={@form[:name]} label="Name" />
      <.input field={@form[:email]} label="Email" type="email" />
      <.button>Save</.button>
    </.form>
    """
  end

  defp change_user(user, attrs \\ %{}) do
    # Define validation rules
    user
    |> cast(attrs, [:name, :email]) # => Allow name and email fields
    |> validate_required([:name, :email]) # => Both fields required
    |> validate_format(:email, ~r/@/) # => Email must contain @
  end
end
```

**Key Takeaway**: Use `phx-change="validate"` to trigger validation on every input change, providing real-time feedback with Ecto changesets.

### Example 32: Form Validation - Live Error Display

LiveView automatically displays validation errors as users type, using the changeset's error tracking.

**Code**:

```elixir
defmodule MyAppWeb.ProductFormLive do
  use MyAppWeb, :live_view
  import Ecto.Changeset

  def mount(_params, _session, socket) do
    changeset = product_changeset(%{}) # => Empty changeset
    {:ok, assign(socket, form: to_form(changeset))} # => Form ready
  end

  def handle_event("validate", %{"product" => params}, socket) do
    changeset =
      params
      |> product_changeset()
      |> Map.put(:action, :validate) # => Show errors immediately
    # => If price < 0, changeset.errors includes {:price, {"must be greater than 0", []}}

    {:noreply, assign(socket, form: to_form(changeset))} # => Errors displayed in form
  end

  def render(assigns) do
    ~H"""
    <.form for={@form} phx-change="validate">
      <.input field={@form[:name]} label="Product Name" />
      <%!-- Errors shown below input automatically --%>

      <.input field={@form[:price]} label="Price" type="number" step="0.01" />
      <%!-- If price < 0, error "must be greater than 0" appears --%>

      <.input field={@form[:quantity]} label="Quantity" type="number" />
      <%!-- If quantity not integer, error "must be an integer" appears --%>
    </.form>
    """
  end

  defp product_changeset(attrs) do
    data = %{name: nil, price: nil, quantity: nil} # => Empty data
    types = %{name: :string, price: :decimal, quantity: :integer} # => Field types

    {data, types}
    |> cast(attrs, [:name, :price, :quantity]) # => Cast with type validation
    |> validate_required([:name, :price, :quantity]) # => All required
    |> validate_number(:price, greater_than: 0) # => Price must be positive
    |> validate_number(:quantity, greater_than_or_equal_to: 0) # => Quantity >= 0
  end
end
```

**Key Takeaway**: Phoenix form components automatically display validation errors from the changeset when `:action` is set to `:validate`.

### Example 33: Multi-field Forms

Handle complex forms with multiple related fields and cross-field validation.

**Code**:

```elixir
defmodule MyAppWeb.AddressFormLive do
  use MyAppWeb, :live_view
  import Ecto.Changeset

  def mount(_params, _session, socket) do
    changeset = address_changeset(%{}) # => Empty address
    {:ok, assign(socket, form: to_form(changeset))} # => Ready to render
  end

  def handle_event("validate", %{"address" => params}, socket) do
    changeset =
      params
      |> address_changeset()
      |> Map.put(:action, :validate) # => Validate immediately

    {:noreply, assign(socket, form: to_form(changeset))} # => Display errors
  end

  def handle_event("save", %{"address" => params}, socket) do
    case address_changeset(params) do
      %{valid?: true} = changeset ->
        # Apply changeset to get validated data
        address = apply_changes(changeset) # => Extract validated address map
        # => address: %{street: "...", city: "...", postal_code: "..."}
        IO.inspect(address, label: "Saved Address") # => Output: Saved Address: %{...}
        {:noreply, socket} # => Success

      %{valid?: false} = changeset ->
        # Invalid data, show errors
        {:noreply, assign(socket, form: to_form(changeset))} # => Display validation errors
    end
  end

  def render(assigns) do
    ~H"""
    <.form for={@form} phx-change="validate" phx-submit="save">
      <.input field={@form[:street]} label="Street Address" />
      <.input field={@form[:city]} label="City" />
      <.input field={@form[:state]} label="State" />
      <.input field={@form[:postal_code]} label="Postal Code" />
      <.input field={@form[:country]} label="Country" />
      <.button>Save Address</.button>
    </.form>
    """
  end

  defp address_changeset(attrs) do
    data = %{street: nil, city: nil, state: nil, postal_code: nil, country: nil}
    types = %{street: :string, city: :string, state: :string, postal_code: :string, country: :string}

    {data, types}
    |> cast(attrs, [:street, :city, :state, :postal_code, :country])
    |> validate_required([:street, :city, :postal_code, :country]) # => State optional
    |> validate_length(:postal_code, min: 5, max: 10) # => Postal code length
    |> validate_format(:postal_code, ~r/^[0-9A-Z\s-]+$/i) # => Alphanumeric + space/dash
  end
end
```

**Key Takeaway**: Use `apply_changes/1` to extract validated data from valid changesets for processing or persistence.

### Example 34: Nested Forms - Embedded Schemas

Handle nested data structures like addresses embedded in user forms using `inputs_for`.

**Code**:

```elixir
defmodule MyAppWeb.UserWithAddressLive do
  use MyAppWeb, :live_view
  import Ecto.Changeset

  def mount(_params, _session, socket) do
    changeset = user_changeset(%{}) # => User with empty address
    {:ok, assign(socket, form: to_form(changeset))} # => Ready
  end

  def handle_event("validate", %{"user" => params}, socket) do
    changeset =
      params
      |> user_changeset()
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, form: to_form(changeset))}
  end

  def render(assigns) do
    ~H"""
    <.form for={@form} phx-change="validate">
      <.input field={@form[:name]} label="Name" />
      <.input field={@form[:email]} label="Email" type="email" />

      <%!-- Nested address fields using inputs_for --%>
      <.inputs_for :let={address_form} field={@form[:address]}>
        <h3>Address</h3>
        <.input field={address_form[:street]} label="Street" />
        <.input field={address_form[:city]} label="City" />
        <.input field={address_form[:postal_code]} label="Postal Code" />
      </.inputs_for>
    </.form>
    """
  end

  defp user_changeset(attrs) do
    data = %{name: nil, email: nil, address: %{street: nil, city: nil, postal_code: nil}}
    types = %{name: :string, email: :string, address: :map}

    {data, types}
    |> cast(attrs, [:name, :email])
    |> validate_required([:name, :email])
    |> cast_embed(:address, with: &address_changeset/2) # => Validate nested address
    # => address validation errors appear under address fields
  end

  defp address_changeset(address, attrs) do
    types = %{street: :string, city: :string, postal_code: :string}

    {address, types}
    |> cast(attrs, [:street, :city, :postal_code])
    |> validate_required([:street, :city, :postal_code]) # => All address fields required
  end
end
```

**Key Takeaway**: Use `cast_embed/3` for nested data and `inputs_for` in templates to render nested form fields with automatic validation.

### Example 35: Form Recovery - phx-auto-recover

Preserve form state during LiveView reconnections using `phx-auto-recover`.

**Code**:

```elixir
defmodule MyAppWeb.LongFormLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    changeset = article_changeset(%{}) # => Empty article
    {:ok, assign(socket, form: to_form(changeset))} # => Ready
  end

  def handle_event("validate", %{"article" => params}, socket) do
    changeset =
      params
      |> article_changeset()
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, form: to_form(changeset))} # => Update form
  end

  def render(assigns) do
    ~H"""
    <%!-- phx-auto-recover="ignore" preserves form inputs during reconnection --%>
    <.form for={@form} phx-change="validate" phx-auto-recover="ignore">
      <.input field={@form[:title]} label="Article Title" />

      <%!-- Long textarea benefits most from auto-recovery --%>
      <.input field={@form[:content]} label="Content" type="textarea" rows="20" />
      <%!-- If LiveView disconnects/reconnects, content preserved in browser --%>

      <.input field={@form[:tags]} label="Tags (comma-separated)" />
      <.button>Save Draft</.button>
    </.form>
    """
  end

  defp article_changeset(attrs) do
    data = %{title: nil, content: nil, tags: nil}
    types = %{title: :string, content: :string, tags: :string}

    {data, types}
    |> cast(attrs, [:title, :content, :tags])
    |> validate_required([:title, :content])
    |> validate_length(:title, min: 5, max: 200)
    |> validate_length(:content, min: 50)
  end
end
```

**Key Takeaway**: Add `phx-auto-recover="ignore"` to forms to preserve user input during LiveView disconnections, critical for long-form content.

### Example 36: Submit Without Page Reload

Handle form submission with server-side processing and client-side feedback without full page reloads.

**Code**:

```elixir
defmodule MyAppWeb.ContactFormLive do
  use MyAppWeb, :live_view
  import Ecto.Changeset

  def mount(_params, _session, socket) do
    changeset = contact_changeset(%{}) # => Empty contact form
    socket =
      socket
      |> assign(:form, to_form(changeset))
      |> assign(:submitted, false) # => Track submission state

    {:ok, socket} # => Ready
  end

  def handle_event("validate", %{"contact" => params}, socket) do
    changeset =
      params
      |> contact_changeset()
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, form: to_form(changeset))} # => Live validation
  end

  def handle_event("submit", %{"contact" => params}, socket) do
    changeset = contact_changeset(params) # => Final validation

    case changeset do
      %{valid?: true} ->
        # Extract and process data
        contact = apply_changes(changeset) # => %{name: "...", email: "...", message: "..."}
        IO.inspect(contact, label: "Contact Submission") # => Log submission

        # Simulate sending email
        Process.sleep(500) # => Simulate network delay

        socket =
          socket
          |> assign(:submitted, true) # => Mark as submitted
          |> assign(:form, to_form(contact_changeset(%{}))) # => Reset form
        # => Form cleared, submitted flag true

        {:noreply, socket} # => Re-render with success message

      %{valid?: false} ->
        # Show validation errors
        changeset = Map.put(changeset, :action, :validate)
        {:noreply, assign(socket, form: to_form(changeset))} # => Display errors
    end
  end

  def render(assigns) do
    ~H"""
    <div>
      <%= if @submitted do %>
        <div class="alert alert-success">
          Thank you! Your message has been sent.
        </div>
      <% end %>

      <.form for={@form} phx-change="validate" phx-submit="submit">
        <.input field={@form[:name]} label="Name" />
        <.input field={@form[:email]} label="Email" type="email" />
        <.input field={@form[:message]} label="Message" type="textarea" rows="5" />
        <.button>Send Message</.button>
      </.form>
    </div>
    """
  end

  defp contact_changeset(attrs) do
    data = %{name: nil, email: nil, message: nil}
    types = %{name: :string, email: :string, message: :string}

    {data, types}
    |> cast(attrs, [:name, :email, :message])
    |> validate_required([:name, :email, :message])
    |> validate_format(:email, ~r/@/)
    |> validate_length(:message, min: 10)
  end
end
```

**Key Takeaway**: Handle `phx-submit` events to process forms server-side without page reloads, showing success messages by updating assigns.

### Example 37: Form Input Types - Text, Checkbox, Select

LiveView supports all standard HTML5 input types with automatic value binding.

**Code**:

```elixir
defmodule MyAppWeb.PreferencesFormLive do
  use MyAppWeb, :live_view
  import Ecto.Changeset

  def mount(_params, _session, socket) do
    changeset = preferences_changeset(%{}) # => Empty preferences
    {:ok, assign(socket, form: to_form(changeset))} # => Ready
  end

  def handle_event("validate", %{"preferences" => params}, socket) do
    changeset =
      params
      |> preferences_changeset()
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, form: to_form(changeset))}
  end

  def render(assigns) do
    ~H"""
    <.form for={@form} phx-change="validate">
      <%!-- Text input --%>
      <.input field={@form[:username]} label="Username" />

      <%!-- Checkbox input - boolean value --%>
      <.input field={@form[:newsletter]} label="Subscribe to newsletter" type="checkbox" />
      <%!-- Checked = true, unchecked = false --%>

      <%!-- Select input - dropdown --%>
      <.input
        field={@form[:theme]}
        label="Theme"
        type="select"
        options={[{"Light", "light"}, {"Dark", "dark"}, {"Auto", "auto"}]}
      />
      <%!-- Options: [{"Display", "value"}, ...] --%>

      <%!-- Number input --%>
      <.input field={@form[:items_per_page]} label="Items per page" type="number" />

      <%!-- Email input with HTML5 validation --%>
      <.input field={@form[:email]} label="Email" type="email" />
    </.form>
    """
  end

  defp preferences_changeset(attrs) do
    data = %{username: nil, newsletter: false, theme: "auto", items_per_page: 10, email: nil}
    types = %{username: :string, newsletter: :boolean, theme: :string, items_per_page: :integer, email: :string}

    {data, types}
    |> cast(attrs, [:username, :newsletter, :theme, :items_per_page, :email])
    |> validate_required([:username, :email])
    |> validate_inclusion(:theme, ["light", "dark", "auto"]) # => Theme must be valid
    |> validate_number(:items_per_page, greater_than: 0, less_than_or_equal_to: 100)
  end
end
```

**Key Takeaway**: Phoenix form components handle all HTML5 input types automatically, casting values to appropriate Elixir types via changesets.

### Example 38: Custom Form Components

Create reusable form components for complex input patterns.

**Code**:

```elixir
defmodule MyAppWeb.CustomFormLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    changeset = event_changeset(%{}) # => Empty event
    {:ok, assign(socket, form: to_form(changeset))} # => Ready
  end

  def handle_event("validate", %{"event" => params}, socket) do
    changeset =
      params
      |> event_changeset()
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, form: to_form(changeset))}
  end

  def render(assigns) do
    ~H"""
    <.form for={@form} phx-change="validate">
      <.input field={@form[:title]} label="Event Title" />

      <%!-- Custom date-time picker component --%>
      <.date_time_input field={@form[:starts_at]} label="Start Date & Time" />

      <%!-- Custom duration selector --%>
      <.duration_input field={@form[:duration_minutes]} label="Duration" />
    </.form>
    """
  end

  # Custom date-time input component
  def date_time_input(assigns) do
    ~H"""
    <div class="form-group">
      <label><%= @label %></label>
      <input
        type="datetime-local"
        id={@field.id}
        name={@field.name}
        value={@field.value}
        class="form-control"
      />
      <%!-- Display errors if present --%>
      <.error :for={msg <- @field.errors}><%= msg %></.error>
    </div>
    """
  end

  # Custom duration selector (hours + minutes)
  def duration_input(assigns) do
    ~H"""
    <div class="form-group">
      <label><%= @label %></label>
      <select name={@field.name} id={@field.id} class="form-control">
        <option value="30">30 minutes</option>
        <option value="60" selected={@field.value == "60"}>1 hour</option>
        <option value="90">1.5 hours</option>
        <option value="120">2 hours</option>
      </select>
      <.error :for={msg <- @field.errors}><%= msg %></.error>
    </div>
    """
  end

  defp event_changeset(attrs) do
    data = %{title: nil, starts_at: nil, duration_minutes: 60}
    types = %{title: :string, starts_at: :naive_datetime, duration_minutes: :integer}

    {data, types}
    |> cast(attrs, [:title, :starts_at, :duration_minutes])
    |> validate_required([:title, :starts_at])
    |> validate_number(:duration_minutes, greater_than: 0)
  end
end
```

**Key Takeaway**: Create custom function components for complex inputs by accessing `@field.id`, `@field.name`, `@field.value`, and `@field.errors`.

### Example 39: File Upload Basics

Enable file uploads with `allow_upload` configuration and upload validation.

**Code**:

```elixir
defmodule MyAppWeb.AvatarUploadLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:uploaded_files, []) # => Track uploaded files
      |> allow_upload(:avatar, # => Configure avatar upload
        accept: ~w(.jpg .jpeg .png), # => Allowed extensions
        max_entries: 1, # => Single file only
        max_file_size: 5_000_000 # => 5MB limit (bytes)
      )
    # => Upload configuration stored in socket

    {:ok, socket} # => Ready for uploads
  end

  def handle_event("validate", _params, socket) do
    # Validation happens automatically based on allow_upload config
    {:noreply, socket} # => Errors shown if file invalid
  end

  def handle_event("save", _params, socket) do
    # Consume uploaded files
    uploaded_files =
      consume_uploaded_entries(socket, :avatar, fn %{path: path}, entry ->
        # path: temporary file path on server
        # entry: upload metadata (client_name, content_type, etc.)
        dest = Path.join("priv/static/uploads", entry.client_name) # => Destination path
        File.cp!(path, dest) # => Copy to permanent location
        {:ok, "/uploads/#{entry.client_name}"} # => Return public URL
      end)
    # => uploaded_files: ["/uploads/avatar.jpg"] or []

    socket = assign(socket, :uploaded_files, uploaded_files) # => Store uploaded paths
    {:noreply, socket} # => Display uploaded files
  end

  def render(assigns) do
    ~H"""
    <div>
      <form phx-change="validate" phx-submit="save">
        <%!-- File input with upload configuration --%>
        <.live_file_input upload={@uploads.avatar} />
        <%!-- Automatically validates against allow_upload rules --%>

        <%!-- Show validation errors --%>
        <%= for entry <- @uploads.avatar.entries do %>
          <div>
            <%= entry.client_name %> - <%= entry.progress %>%
            <%!-- Display upload errors --%>
            <%= for err <- upload_errors(@uploads.avatar, entry) do %>
              <p class="error"><%= error_to_string(err) %></p>
            <% end %>
          </div>
        <% end %>

        <button type="submit">Upload</button>
      </form>

      <%!-- Display uploaded files --%>
      <%= for file <- @uploaded_files do %>
        <img src={file} alt="Uploaded avatar" width="200" />
      <% end %>
    </div>
    """
  end

  defp error_to_string(:too_large), do: "File too large (max 5MB)"
  defp error_to_string(:not_accepted), do: "Invalid file type (jpg, jpeg, png only)"
end
```

**Key Takeaway**: Use `allow_upload/3` to configure uploads with validation rules, then `consume_uploaded_entries/3` to process uploaded files.

### Example 40: Form Progress Tracking

Track multi-step form progress with client-side state and server validation.

**Code**:

```elixir
defmodule MyAppWeb.WizardFormLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    changeset = registration_changeset(%{}) # => Empty registration

    socket =
      socket
      |> assign(:form, to_form(changeset))
      |> assign(:current_step, 1) # => Start at step 1
      |> assign(:max_step, 3) # => 3 steps total

    {:ok, socket} # => Ready
  end

  def handle_event("validate", %{"registration" => params}, socket) do
    changeset =
      params
      |> registration_changeset()
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, form: to_form(changeset))}
  end

  def handle_event("next_step", _params, socket) do
    # Move to next step if not at max
    new_step = min(socket.assigns.current_step + 1, socket.assigns.max_step)
    {:noreply, assign(socket, :current_step, new_step)} # => Increment step
  end

  def handle_event("prev_step", _params, socket) do
    # Move to previous step if not at first
    new_step = max(socket.assigns.current_step - 1, 1)
    {:noreply, assign(socket, :current_step, new_step)} # => Decrement step
  end

  def render(assigns) do
    ~H"""
    <div>
      <%!-- Progress indicator --%>
      <div class="progress-bar">
        Step <%= @current_step %> of <%= @max_step %>
        <%= trunc((@current_step / @max_step) * 100) %>% complete
      </div>

      <.form for={@form} phx-change="validate">
        <%!-- Step 1: Personal Info --%>
        <%= if @current_step == 1 do %>
          <.input field={@form[:name]} label="Full Name" />
          <.input field={@form[:email]} label="Email" type="email" />
        <% end %>

        <%!-- Step 2: Address --%>
        <%= if @current_step == 2 do %>
          <.input field={@form[:street]} label="Street Address" />
          <.input field={@form[:city]} label="City" />
        <% end %>

        <%!-- Step 3: Confirmation --%>
        <%= if @current_step == 3 do %>
          <p>Review your information:</p>
          <p>Name: <%= @form[:name].value %></p>
          <p>Email: <%= @form[:email].value %></p>
          <p>Address: <%= @form[:street].value %>, <%= @form[:city].value %></p>
        <% end %>

        <%!-- Navigation buttons --%>
        <button type="button" phx-click="prev_step" disabled={@current_step == 1}>
          Previous
        </button>
        <button type="button" phx-click="next_step" disabled={@current_step == @max_step}>
          Next
        </button>
        <%= if @current_step == @max_step do %>
          <button type="submit">Submit</button>
        <% end %>
      </.form>
    </div>
    """
  end

  defp registration_changeset(attrs) do
    data = %{name: nil, email: nil, street: nil, city: nil}
    types = %{name: :string, email: :string, street: :string, city: :string}

    {data, types}
    |> cast(attrs, [:name, :email, :street, :city])
    |> validate_required([:name, :email])
  end
end
```

**Key Takeaway**: Track multi-step form progress with a step counter assign, conditionally rendering form sections based on current step.

## State Management (Examples 41-50)

State management patterns optimize LiveView performance and handle complex data flows.

### Example 41: Temporary Assigns

Use temporary assigns for large lists that don't need to persist in memory between updates.

**Code**:

```elixir
defmodule MyAppWeb.LogViewerLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:logs, fetch_logs()) # => Load initial logs
      |> assign(:page, 1) # => Current page number

    {:ok, socket, temporary_assigns: [logs: []]} # => logs cleared after render
    # => After render, socket.assigns.logs becomes []
    # => Reduces memory for large log lists
  end

  def handle_event("load_more", _params, socket) do
    page = socket.assigns.page + 1 # => Increment page
    new_logs = fetch_logs(page) # => Fetch next page

    socket =
      socket
      |> assign(:logs, new_logs) # => Assign new logs (old logs already cleared)
      |> assign(:page, page) # => Update page number

    {:noreply, socket} # => Render new logs, then clear from memory
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Application Logs</h2>
      <ul>
        <%= for log <- @logs do %>
          <li><%= log.timestamp %> - <%= log.message %></li>
        <% end %>
      </ul>
      <button phx-click="load_more">Load More</button>
    </div>
    """
  end

  defp fetch_logs(page \\ 1) do
    # Simulate fetching logs from database
    Enum.map(1..50, fn i ->
      %{timestamp: DateTime.utc_now(), message: "Log entry #{(page - 1) * 50 + i}"}
    end)
  end
end
```

**Key Takeaway**: Use `temporary_assigns` in mount's return tuple to automatically clear large data after rendering, reducing LiveView process memory.

### Example 42: assign_new for Lazy Evaluation

Use `assign_new/3` to lazily compute expensive assigns only when they don't exist.

**Code**:

```elixir
defmodule MyAppWeb.DashboardLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:user_id, 123) # => Set user_id
      |> assign_new(:stats, fn -> compute_expensive_stats(123) end) # => Lazy load stats
    # => compute_expensive_stats only runs if :stats not already assigned

    {:ok, socket} # => Ready
  end

  def handle_event("refresh_stats", _params, socket) do
    # Force recalculation by removing and re-adding
    stats = compute_expensive_stats(socket.assigns.user_id) # => Recompute
    {:noreply, assign(socket, :stats, stats)} # => Update stats
  end

  def handle_params(_params, _uri, socket) do
    # assign_new won't recompute stats on navigation
    socket = assign_new(socket, :stats, fn -> compute_expensive_stats(socket.assigns.user_id) end)
    # => stats remain from previous load
    {:noreply, socket} # => Keep existing stats
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Dashboard</h2>
      <p>Total Sales: <%= @stats.total_sales %></p>
      <p>Active Users: <%= @stats.active_users %></p>
      <button phx-click="refresh_stats">Refresh</button>
    </div>
    """
  end

  defp compute_expensive_stats(user_id) do
    IO.puts("Computing expensive stats for user #{user_id}...")
    Process.sleep(1000) # => Simulate expensive computation
    %{total_sales: 10_000, active_users: 250} # => Stats data
  end
end
```

**Key Takeaway**: Use `assign_new/3` to lazily compute expensive assigns only when missing, preventing redundant calculations on navigation.

### Example 43: Update Patterns - update/3

Use `update/3` to modify existing assigns based on their current value.

**Code**:

```elixir
defmodule MyAppWeb.CounterListLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:counters, %{a: 0, b: 0, c: 0}) # => Three independent counters
      |> assign(:total_clicks, 0) # => Global click counter

    {:ok, socket} # => Ready
  end

  def handle_event("increment", %{"counter" => key}, socket) do
    counter_key = String.to_atom(key) # => Convert "a" to :a

    socket =
      socket
      |> update(:counters, fn counters ->
        # Update nested map value
        Map.update!(counters, counter_key, &(&1 + 1)) # => Increment specific counter
        # => If counter_key is :a, counters becomes %{a: 1, b: 0, c: 0}
      end)
      |> update(:total_clicks, &(&1 + 1)) # => Increment total
      # => total_clicks goes from 0 to 1

    {:noreply, socket} # => Re-render with updated counters
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Counters</h2>
      <%= for {key, value} <- @counters do %>
        <div>
          Counter <%= key %>: <%= value %>
          <button phx-click="increment" phx-value-counter={key}>+</button>
        </div>
      <% end %>
      <p>Total Clicks: <%= @total_clicks %></p>
    </div>
    """
  end
end
```

**Key Takeaway**: Use `update/3` to modify assigns based on their current value, ideal for counters, toggles, and nested data updates.

### Example 44: Stream Collections

Use streams for efficiently rendering and updating large lists with automatic DOM diffing.

**Code**:

```elixir
defmodule MyAppWeb.TaskListLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    tasks = [
      %{id: 1, title: "Task 1", completed: false},
      %{id: 2, title: "Task 2", completed: false}
    ]

    socket =
      socket
      |> stream(:tasks, tasks) # => Initialize stream with tasks
      # => Stream tracks items by :id field

    {:ok, socket} # => Ready
  end

  def handle_event("add_task", %{"title" => title}, socket) do
    new_task = %{id: System.unique_integer([:positive]), title: title, completed: false}
    # => Create new task with unique ID

    socket = stream_insert(socket, :tasks, new_task, at: 0) # => Prepend to stream
    # => Only new task sent to client, existing tasks unchanged
    {:noreply, socket} # => Efficient update
  end

  def handle_event("delete_task", %{"id" => id_str}, socket) do
    id = String.to_integer(id_str) # => Convert to integer
    socket = stream_delete_by_dom_id(socket, :tasks, "tasks-#{id}") # => Remove from stream
    # => Only deletion sent to client
    {:noreply, socket} # => Task removed from DOM
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Task List</h2>
      <form phx-submit="add_task">
        <input type="text" name="title" placeholder="New task" />
        <button>Add</button>
      </form>

      <%!-- Stream rendering with phx-update="stream" --%>
      <ul id="tasks" phx-update="stream">
        <%= for {dom_id, task} <- @streams.tasks do %>
          <li id={dom_id}>
            <%= task.title %>
            <button phx-click="delete_task" phx-value-id={task.id}>Delete</button>
          </li>
        <% end %>
      </ul>
    </div>
    """
  end
end
```

**Key Takeaway**: Use `stream/3` for large lists to enable efficient DOM updates - only changed items are sent to client, not entire list.

### Example 45: Reset Stream on Disconnect

Prevent stream memory leaks by resetting streams when clients disconnect.

**Code**:

```elixir
defmodule MyAppWeb.ActivityFeedLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    if connected?(socket) do
      # Connected over WebSocket
      Phoenix.PubSub.subscribe(MyApp.PubSub, "activities") # => Subscribe to updates
      activities = load_recent_activities() # => Load from database

      socket =
        socket
        |> stream(:activities, activities, reset: true) # => Reset on reconnect
        # => Clears any stale stream data from disconnection

      {:ok, socket}
    else
      # Initial HTTP render (not connected yet)
      {:ok, assign(socket, :activities_loaded, false)} # => Defer loading
    end
  end

  def handle_info({:new_activity, activity}, socket) do
    # Received from PubSub
    socket = stream_insert(socket, :activities, activity, at: 0) # => Prepend new activity
    {:noreply, socket} # => Update feed
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Activity Feed</h2>
      <ul id="activities" phx-update="stream">
        <%= for {dom_id, activity} <- @streams.activities do %>
          <li id={dom_id}><%= activity.description %></li>
        <% end %>
      </ul>
    </div>
    """
  end

  defp load_recent_activities do
    # Simulate database query
    [
      %{id: 1, description: "User logged in"},
      %{id: 2, description: "New comment posted"}
    ]
  end
end
```

**Key Takeaway**: Use `reset: true` with streams in mount when `connected?/1` to prevent memory leaks from accumulated stream data during disconnections.

### Example 46: Pagination with Streams

Implement efficient pagination using streams for large datasets.

**Code**:

```elixir
defmodule MyAppWeb.ProductListLive do
  use MyAppWeb, :live_view

  @page_size 20

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:page, 1) # => Current page
      |> load_products() # => Load first page

    {:ok, socket} # => Ready
  end

  def handle_event("load_next_page", _params, socket) do
    socket =
      socket
      |> update(:page, &(&1 + 1)) # => Increment page
      |> load_products() # => Load next page

    {:noreply, socket} # => Append products to stream
  end

  defp load_products(socket) do
    page = socket.assigns.page
    offset = (page - 1) * @page_size # => Calculate offset
    products = fetch_products(offset, @page_size) # => Query database
    # => products: [%{id: 1, name: "Product 1"}, ...]

    if page == 1 do
      # First page: initialize stream
      stream(socket, :products, products) # => Create new stream
    else
      # Subsequent pages: append to stream
      Enum.reduce(products, socket, fn product, acc ->
        stream_insert(acc, :products, product, at: -1) # => Append to end
      end)
    end
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Products</h2>
      <ul id="products" phx-update="stream">
        <%= for {dom_id, product} <- @streams.products do %>
          <li id={dom_id}><%= product.name %></li>
        <% end %>
      </ul>
      <button phx-click="load_next_page">Load More</button>
    </div>
    """
  end

  defp fetch_products(offset, limit) do
    # Simulate database query
    Enum.map((offset + 1)..(offset + limit), fn i ->
      %{id: i, name: "Product #{i}"}
    end)
  end
end
```

**Key Takeaway**: Combine streams with pagination to efficiently load and render large datasets, appending new pages without re-sending existing items.

### Example 47: Infinite Scroll

Implement infinite scroll by detecting when user scrolls near bottom and loading more content.

**Code**:

```elixir
defmodule MyAppWeb.InfiniteScrollLive do
  use MyAppWeb, :live_view

  @page_size 20

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:page, 1)
      |> assign(:has_more, true) # => Track if more items available
      |> load_page()

    {:ok, socket}
  end

  def handle_event("load-more", _params, socket) do
    if socket.assigns.has_more do
      socket =
        socket
        |> update(:page, &(&1 + 1))
        |> load_page()

      {:noreply, socket}
    else
      {:noreply, socket} # => No more items
    end
  end

  defp load_page(socket) do
    page = socket.assigns.page
    items = fetch_items(page, @page_size)
    has_more = length(items) == @page_size # => Check if more available

    socket =
      socket
      |> assign(:has_more, has_more)
      |> then(fn socket ->
        if page == 1 do
          stream(socket, :items, items)
        else
          Enum.reduce(items, socket, fn item, acc ->
            stream_insert(acc, :items, item, at: -1)
          end)
        end
      end)

    socket
  end

  def render(assigns) do
    ~H"""
    <div id="infinite-scroll-container" phx-hook="InfiniteScroll">
      <ul id="items" phx-update="stream">
        <%= for {dom_id, item} <- @streams.items do %>
          <li id={dom_id}><%= item.content %></li>
        <% end %>
      </ul>

      <%= if @has_more do %>
        <div id="loading-trigger">Loading...</div>
      <% else %>
        <div>No more items</div>
      <% end %>
    </div>
    """
  end

  defp fetch_items(page, limit) do
    offset = (page - 1) * limit
    # Simulate limited dataset
    if offset < 100 do
      Enum.map((offset + 1)..min(offset + limit, 100), fn i ->
        %{id: i, content: "Item #{i}"}
      end)
    else
      [] # => No more items
    end
  end
end
```

**Client Hook** (assets/js/app.js):

```javascript
// InfiniteScroll hook detects when user scrolls near bottom
let Hooks = {};
Hooks.InfiniteScroll = {
  mounted() {
    this.observer = new IntersectionObserver(
      (entries) => {
        // => Triggered when loading-trigger becomes visible
        if (entries[0].isIntersecting) {
          this.pushEvent("load-more", {}); // => Request more items from server
        }
      },
      { threshold: 1.0 },
    );

    const trigger = document.getElementById("loading-trigger");
    if (trigger) {
      this.observer.observe(trigger); // => Watch loading-trigger element
    }
  },
  destroyed() {
    if (this.observer) {
      this.observer.disconnect(); // => Cleanup
    }
  },
};
```

**Key Takeaway**: Combine streams with IntersectionObserver client hook to automatically load more content when user scrolls near bottom.

### Example 48: Live Navigation - patch vs navigate

Understand the difference between `patch` (same LiveView) and `navigate` (different LiveView).

**Code**:

```elixir
defmodule MyAppWeb.BlogLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    {:ok, assign(socket, :posts, load_posts())} # => Load all posts
  end

  def handle_params(params, _uri, socket) do
    # Called on navigation and initial load
    post_id = params["id"] # => Extract post ID from URL
    selected_post = find_post(socket.assigns.posts, post_id) # => Find post by ID

    socket = assign(socket, :selected_post, selected_post) # => Set selected post
    {:noreply, socket} # => Update view
  end

  def handle_event("select_post", %{"id" => id}, socket) do
    # patch keeps LiveView process alive, just updates params
    {:noreply, push_patch(socket, to: "/blog?id=#{id}")} # => Update URL, call handle_params
    # => Same LiveView process, no remount
  end

  def handle_event("go_to_settings", _params, socket) do
    # navigate terminates current LiveView, starts new one
    {:noreply, push_navigate(socket, to: "/settings")} # => Different LiveView
    # => Current process terminates, new LiveView mounts
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Blog Posts</h2>
      <ul>
        <%= for post <- @posts do %>
          <li>
            <button phx-click="select_post" phx-value-id={post.id}>
              <%= post.title %>
            </button>
          </li>
        <% end %>
      </ul>

      <%= if @selected_post do %>
        <div>
          <h3><%= @selected_post.title %></h3>
          <p><%= @selected_post.content %></p>
        </div>
      <% end %>

      <button phx-click="go_to_settings">Settings</button>
    </div>
    """
  end

  defp load_posts do
    [
      %{id: 1, title: "Post 1", content: "Content 1"},
      %{id: 2, title: "Post 2", content: "Content 2"}
    ]
  end

  defp find_post(posts, nil), do: nil
  defp find_post(posts, id_str) do
    id = String.to_integer(id_str)
    Enum.find(posts, &(&1.id == id))
  end
end
```

**Key Takeaway**: Use `push_patch/2` for navigation within same LiveView (keeps process alive), `push_navigate/2` for different LiveViews (terminates current).

### Example 49: Query Parameters

Handle URL query parameters for bookmarkable state and sharing.

**Code**:

```elixir
defmodule MyAppWeb.SearchLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    {:ok, assign(socket, :results, [])} # => Empty results initially
  end

  def handle_params(params, _uri, socket) do
    # Extract query parameters from URL
    query = params["q"] || "" # => Search query
    category = params["category"] || "all" # => Filter category
    page = params["page"] || "1" # => Pagination

    results = perform_search(query, category, page) # => Search with params

    socket =
      socket
      |> assign(:query, query)
      |> assign(:category, category)
      |> assign(:page, String.to_integer(page))
      |> assign(:results, results)

    {:noreply, socket} # => Render with query state
  end

  def handle_event("search", %{"q" => query, "category" => category}, socket) do
    # Update URL with new query parameters
    {:noreply, push_patch(socket, to: "/search?q=#{query}&category=#{category}&page=1")}
    # => URL updated, handle_params called with new params
  end

  def handle_event("next_page", _params, socket) do
    next_page = socket.assigns.page + 1
    query = socket.assigns.query
    category = socket.assigns.category

    {:noreply, push_patch(socket, to: "/search?q=#{query}&category=#{category}&page=#{next_page}")}
  end

  def render(assigns) do
    ~H"""
    <div>
      <form phx-submit="search">
        <input type="text" name="q" value={@query} placeholder="Search..." />
        <select name="category">
          <option value="all" selected={@category == "all"}>All</option>
          <option value="products" selected={@category == "products"}>Products</option>
          <option value="articles" selected={@category == "articles"}>Articles</option>
        </select>
        <button>Search</button>
      </form>

      <div>
        <h3>Results (Page <%= @page %>)</h3>
        <ul>
          <%= for result <- @results do %>
            <li><%= result %></li>
          <% end %>
        </ul>
        <button phx-click="next_page">Next Page</button>
      </div>
    </div>
    """
  end

  defp perform_search(query, category, page) do
    # Simulate search
    ["Result for #{query} in #{category} (page #{page})"]
  end
end
```

**Key Takeaway**: Use `handle_params/3` to extract URL query parameters and `push_patch/2` to update them, enabling bookmarkable and shareable state.

### Example 50: Flash Messages

Display temporary success/error messages using flash assigns.

**Code**:

```elixir
defmodule MyAppWeb.TaskFormLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    {:ok, assign(socket, :task_title, "")} # => Empty form
  end

  def handle_event("save_task", %{"title" => title}, socket) do
    case save_task(title) do
      {:ok, _task} ->
        socket =
          socket
          |> put_flash(:info, "Task created successfully!") # => Success flash
          |> assign(:task_title, "") # => Clear form

        {:noreply, socket} # => Flash shown at top

      {:error, reason} ->
        socket = put_flash(socket, :error, "Failed to create task: #{reason}") # => Error flash
        {:noreply, socket} # => Error shown at top
    end
  end

  def handle_event("clear_flash", _params, socket) do
    socket = clear_flash(socket) # => Remove all flash messages
    {:noreply, socket} # => Flash cleared
  end

  def render(assigns) do
    ~H"""
    <div>
      <%!-- Flash messages displayed at top --%>
      <div :if={@flash["info"]} class="alert alert-info">
        <%= @flash["info"] %>
        <button phx-click="clear_flash">×</button>
      </div>

      <div :if={@flash["error"]} class="alert alert-error">
        <%= @flash["error"] %>
        <button phx-click="clear_flash">×</button>
      </div>

      <form phx-submit="save_task">
        <input type="text" name="title" value={@task_title} placeholder="Task title" />
        <button>Save</button>
      </form>
    </div>
    """
  end

  defp save_task(""), do: {:error, "Title cannot be empty"}
  defp save_task(title) do
    # Simulate save
    {:ok, %{id: 1, title: title}}
  end
end
```

**Key Takeaway**: Use `put_flash/3` to set temporary messages and access via `@flash` in templates for user feedback without persistent state.

## PubSub and Real-time (Examples 51-55)

Phoenix.PubSub enables real-time multi-user synchronization through publish/subscribe messaging.

### Example 51: Phoenix.PubSub Basics

Use Phoenix.PubSub to broadcast messages between LiveView processes.

**Code**:

```elixir
defmodule MyAppWeb.ChatRoomLive do
  use MyAppWeb, :live_view

  @topic "chat:lobby"

  def mount(_params, _session, socket) do
    if connected?(socket) do
      # Subscribe to chat topic when connected via WebSocket
      Phoenix.PubSub.subscribe(MyApp.PubSub, @topic) # => Listen for broadcasts
      # => Any broadcast to "chat:lobby" received in handle_info
    end

    socket =
      socket
      |> assign(:messages, []) # => Empty message list
      |> assign(:username, "User#{:rand.uniform(1000)}") # => Random username

    {:ok, socket} # => Ready
  end

  def handle_event("send_message", %{"text" => text}, socket) do
    message = %{
      username: socket.assigns.username,
      text: text,
      timestamp: DateTime.utc_now()
    }

    # Broadcast to all subscribers
    Phoenix.PubSub.broadcast(MyApp.PubSub, @topic, {:new_message, message})
    # => Sent to all LiveView processes subscribed to "chat:lobby"
    # => Including this process (will receive in handle_info)

    {:noreply, socket} # => Don't update yet (wait for broadcast)
  end

  def handle_info({:new_message, message}, socket) do
    # Received broadcast from any process (including self)
    socket = update(socket, :messages, fn messages ->
      [message | messages] # => Prepend new message
    end)

    {:noreply, socket} # => Re-render with new message
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Chat Room</h2>
      <div id="messages">
        <%= for msg <- Enum.reverse(@messages) do %>
          <p><strong><%= msg.username %>:</strong> <%= msg.text %></p>
        <% end %>
      </div>

      <form phx-submit="send_message">
        <input type="text" name="text" placeholder="Type a message..." />
        <button>Send</button>
      </form>
    </div>
    """
  end
end
```

**Key Takeaway**: Use `Phoenix.PubSub.subscribe/2` to listen and `broadcast/3` to publish messages, enabling real-time multi-user features.

### Example 52: Subscribe to Multiple Topics

Subscribe to multiple PubSub topics to receive updates from different sources.

**Code**:

```elixir
defmodule MyAppWeb.DashboardLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    if connected?(socket) do
      # Subscribe to multiple topics
      Phoenix.PubSub.subscribe(MyApp.PubSub, "users:activity") # => User events
      Phoenix.PubSub.subscribe(MyApp.PubSub, "orders:new") # => New orders
      Phoenix.PubSub.subscribe(MyApp.PubSub, "system:alerts") # => System alerts
      # => All three topics will send messages to this process
    end

    socket =
      socket
      |> assign(:recent_activities, [])
      |> assign(:new_orders_count, 0)
      |> assign(:alerts, [])

    {:ok, socket} # => Ready
  end

  def handle_info({:user_activity, activity}, socket) do
    # From "users:activity" topic
    socket = update(socket, :recent_activities, fn activities ->
      [activity | Enum.take(activities, 9)] # => Keep last 10 activities
    end)
    {:noreply, socket}
  end

  def handle_info({:new_order, _order}, socket) do
    # From "orders:new" topic
    socket = update(socket, :new_orders_count, &(&1 + 1)) # => Increment counter
    {:noreply, socket}
  end

  def handle_info({:system_alert, alert}, socket) do
    # From "system:alerts" topic
    socket = update(socket, :alerts, fn alerts ->
      [alert | alerts] # => Add alert to list
    end)
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Dashboard</h2>

      <div class="panel">
        <h3>Recent Activity</h3>
        <ul>
          <%= for activity <- @recent_activities do %>
            <li><%= activity %></li>
          <% end %>
        </ul>
      </div>

      <div class="panel">
        <h3>New Orders</h3>
        <p><%= @new_orders_count %> new orders</p>
      </div>

      <div class="panel">
        <h3>System Alerts</h3>
        <%= for alert <- @alerts do %>
          <div class="alert"><%= alert %></div>
        <% end %>
      </div>
    </div>
    """
  end
end
```

**Key Takeaway**: Pattern match on different message types in `handle_info/2` to handle updates from multiple PubSub topics in a single LiveView.

### Example 53: Broadcast Updates

Broadcast state changes to all connected users for real-time synchronization.

**Code**:

```elixir
defmodule MyAppWeb.DocumentEditorLive do
  use MyAppWeb, :live_view

  def mount(%{"doc_id" => doc_id}, _session, socket) do
    topic = "document:#{doc_id}" # => Topic per document

    if connected?(socket) do
      Phoenix.PubSub.subscribe(MyApp.PubSub, topic) # => Subscribe to this document
    end

    document = load_document(doc_id) # => Load from database

    socket =
      socket
      |> assign(:doc_id, doc_id)
      |> assign(:topic, topic)
      |> assign(:content, document.content)
      |> assign(:active_users, 1) # => This user

    {:ok, socket}
  end

  def handle_event("update_content", %{"content" => new_content}, socket) do
    # User edited content
    doc_id = socket.assigns.doc_id
    save_document(doc_id, new_content) # => Persist to database

    # Broadcast to all other users editing this document
    Phoenix.PubSub.broadcast(
      MyApp.PubSub,
      socket.assigns.topic,
      {:content_updated, new_content}
    )
    # => All subscribers (except self if using broadcast_from) receive update

    socket = assign(socket, :content, new_content) # => Update local state
    {:noreply, socket}
  end

  def handle_info({:content_updated, new_content}, socket) do
    # Another user updated content
    socket = assign(socket, :content, new_content) # => Sync content
    {:noreply, socket} # => Display updated content
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Document Editor (Doc <%= @doc_id %>)</h2>
      <p><%= @active_users %> active users</p>

      <form phx-change="update_content">
        <textarea name="content" rows="20" cols="80"><%= @content %></textarea>
      </form>

      <p class="hint">Changes sync in real-time to all users</p>
    </div>
    """
  end

  defp load_document(_id), do: %{content: "Initial content"}
  defp save_document(_id, content), do: IO.puts("Saved: #{content}")
end
```

**Key Takeaway**: Broadcast updates to document-specific topics to synchronize state across all users viewing the same resource in real-time.

### Example 54: handle_info for PubSub Messages

Use `handle_info/2` to receive and process PubSub messages in LiveView.

**Code**:

```elixir
defmodule MyAppWeb.NotificationLive do
  use MyAppWeb, :live_view

  def mount(%{"user_id" => user_id}, _session, socket) do
    if connected?(socket) do
      # Subscribe to user-specific notifications
      Phoenix.PubSub.subscribe(MyApp.PubSub, "notifications:#{user_id}")
    end

    socket =
      socket
      |> assign(:notifications, [])
      |> assign(:unread_count, 0)

    {:ok, socket}
  end

  def handle_info({:notification, notification}, socket) do
    # Received notification from PubSub
    # notification: %{id: 1, title: "...", body: "...", read: false}

    socket =
      socket
      |> update(:notifications, fn notifications ->
        [notification | notifications] # => Prepend notification
      end)
      |> update(:unread_count, &(&1 + 1)) # => Increment unread

    {:noreply, socket} # => Display notification
  end

  def handle_info({:notification_read, notification_id}, socket) do
    # Another client marked notification as read
    socket =
      socket
      |> update(:notifications, fn notifications ->
        Enum.map(notifications, fn notif ->
          if notif.id == notification_id do
            %{notif | read: true} # => Mark as read
          else
            notif
          end
        end)
      end)
      |> update(:unread_count, &max(&1 - 1, 0)) # => Decrement unread

    {:noreply, socket} # => Update UI
  end

  def handle_event("mark_read", %{"id" => id_str}, socket) do
    notification_id = String.to_integer(id_str)

    # Broadcast to sync across user's devices
    Phoenix.PubSub.broadcast(
      MyApp.PubSub,
      "notifications:#{socket.assigns.user_id}",
      {:notification_read, notification_id}
    )

    {:noreply, socket} # => Will receive broadcast in handle_info
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Notifications (<%= @unread_count %> unread)</h2>
      <ul>
        <%= for notif <- @notifications do %>
          <li class={if notif.read, do: "read", else: "unread"}>
            <strong><%= notif.title %></strong>: <%= notif.body %>
            <%= unless notif.read do %>
              <button phx-click="mark_read" phx-value-id={notif.id}>Mark Read</button>
            <% end %>
          </li>
        <% end %>
      </ul>
    </div>
    """
  end
end
```

**Key Takeaway**: Pattern match on message tuples in `handle_info/2` to handle different PubSub message types with distinct processing logic.

### Example 55: Multi-user Synchronization

Synchronize state across multiple users in real-time using presence tracking and broadcasts.

**Code**:

```elixir
defmodule MyAppWeb.WhiteboardLive do
  use MyAppWeb, :live_view
  alias Phoenix.PubSub

  @topic "whiteboard:shared"

  def mount(_params, _session, socket) do
    if connected?(socket) do
      PubSub.subscribe(MyApp.PubSub, @topic) # => Subscribe to whiteboard updates

      # Announce presence
      user_id = "user_#{:rand.uniform(1000)}"
      PubSub.broadcast(MyApp.PubSub, @topic, {:user_joined, user_id})
    end

    socket =
      socket
      |> assign(:shapes, []) # => Drawn shapes
      |> assign(:active_users, []) # => List of active users

    {:ok, socket}
  end

  def handle_event("draw_shape", %{"x" => x, "y" => y, "type" => type}, socket) do
    shape = %{id: System.unique_integer([:positive]), x: x, y: y, type: type}

    # Broadcast to all users
    PubSub.broadcast(MyApp.PubSub, @topic, {:shape_drawn, shape})

    {:noreply, socket} # => Will receive broadcast
  end

  def handle_info({:shape_drawn, shape}, socket) do
    # Another user drew a shape
    socket = update(socket, :shapes, fn shapes ->
      [shape | shapes] # => Add to whiteboard
    end)
    {:noreply, socket} # => Render shape
  end

  def handle_info({:user_joined, user_id}, socket) do
    socket = update(socket, :active_users, fn users ->
      [user_id | users] # => Add user
    end)
    {:noreply, socket} # => Update user count
  end

  def handle_info({:user_left, user_id}, socket) do
    socket = update(socket, :active_users, fn users ->
      List.delete(users, user_id) # => Remove user
    end)
    {:noreply, socket} # => Update user count
  end

  def terminate(_reason, socket) do
    # User disconnected
    user_id = socket.assigns[:user_id]
    if user_id do
      PubSub.broadcast(MyApp.PubSub, @topic, {:user_left, user_id})
    end
    :ok
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Collaborative Whiteboard</h2>
      <p><%= length(@active_users) %> active users</p>

      <div id="canvas" phx-click="draw_shape" phx-value-type="circle" style="border: 1px solid black; width: 600px; height: 400px; position: relative;">
        <%= for shape <- @shapes do %>
          <div style={"position: absolute; left: #{shape.x}px; top: #{shape.y}px; width: 20px; height: 20px; border-radius: 50%; background: blue;"}></div>
        <% end %>
      </div>
    </div>
    """
  end
end
```

**Key Takeaway**: Combine PubSub broadcasts with presence tracking (`user_joined`, `user_left`) to build real-time collaborative applications with multi-user synchronization.

## File Uploads (Examples 56-60)

File uploads in LiveView provide progress tracking, validation, and efficient handling of uploaded content.

### Example 56: Upload Configuration

Configure uploads with validation rules using `allow_upload/3`.

**Code**:

```elixir
defmodule MyAppWeb.FileUploadLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:uploaded_files, []) # => Track uploads
      |> allow_upload(:documents, # => Upload identifier
        accept: ~w(.pdf .doc .docx), # => Allowed file types
        max_entries: 5, # => Maximum 5 files at once
        max_file_size: 10_000_000, # => 10MB per file
        auto_upload: false # => Manual upload control
      )
    # => Upload config stored in socket.assigns.uploads.documents

    {:ok, socket}
  end

  def handle_event("validate", _params, socket) do
    # Automatic validation based on allow_upload configuration
    # Errors appear in @uploads.documents.errors
    {:noreply, socket} # => Display validation errors
  end

  def handle_event("upload", _params, socket) do
    # Process validated uploads
    uploaded_files =
      consume_uploaded_entries(socket, :documents, fn %{path: path}, entry ->
        # path: temporary server path
        # entry: %{client_name: "file.pdf", content_type: "application/pdf"}

        dest = Path.join("priv/static/uploads", entry.client_name)
        File.cp!(path, dest) # => Copy to permanent location
        {:ok, "/uploads/#{entry.client_name}"} # => Return public URL
      end)

    socket = assign(socket, :uploaded_files, uploaded_files) # => Store URLs
    {:noreply, socket} # => Display uploaded files
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>File Upload</h2>

      <form phx-change="validate" phx-submit="upload">
        <.live_file_input upload={@uploads.documents} />

        <%!-- Show global upload errors --%>
        <%= for err <- @uploads.documents.errors do %>
          <p class="error"><%= error_to_string(err) %></p>
        <% end %>

        <%!-- Show per-entry progress and errors --%>
        <%= for entry <- @uploads.documents.entries do %>
          <div>
            <p><%= entry.client_name %> (<%= entry.progress %>%)</p>
            <%= for err <- upload_errors(@uploads.documents, entry) do %>
              <p class="error"><%= error_to_string(err) %></p>
            <% end %>
          </div>
        <% end %>

        <button type="submit">Upload</button>
      </form>

      <h3>Uploaded Files</h3>
      <ul>
        <%= for file <- @uploaded_files do %>
          <li><a href={file}><%= file %></a></li>
        <% end %>
      </ul>
    </div>
    """
  end

  defp error_to_string(:too_large), do: "File too large (max 10MB)"
  defp error_to_string(:too_many_files), do: "Too many files (max 5)"
  defp error_to_string(:not_accepted), do: "Invalid file type (pdf, doc, docx only)"
end
```

**Key Takeaway**: Configure uploads with `allow_upload/3` specifying accepted types, size limits, and max entries for automatic validation.

### Example 57: Progress Tracking

Track upload progress in real-time and display to users.

**Code**:

```elixir
defmodule MyAppWeb.ProgressUploadLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:uploaded_files, [])
      |> allow_upload(:photos,
        accept: ~w(.jpg .jpeg .png .gif),
        max_entries: 10,
        max_file_size: 5_000_000,
        chunk_size: 64_000 # => Upload in 64KB chunks for progress tracking
      )

    {:ok, socket}
  end

  def handle_event("validate", _params, socket) do
    {:noreply, socket} # => Validation automatic
  end

  def handle_event("upload", _params, socket) do
    uploaded_files =
      consume_uploaded_entries(socket, :photos, fn %{path: path}, entry ->
        dest = Path.join("priv/static/uploads/photos", entry.client_name)
        File.cp!(path, dest)
        {:ok, %{url: "/uploads/photos/#{entry.client_name}", name: entry.client_name}}
      end)

    socket = update(socket, :uploaded_files, fn files -> files ++ uploaded_files end)
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Photo Upload with Progress</h2>

      <form phx-change="validate" phx-submit="upload">
        <.live_file_input upload={@uploads.photos} />

        <%!-- Progress bars for each file --%>
        <%= for entry <- @uploads.photos.entries do %>
          <div class="upload-entry">
            <p><%= entry.client_name %></p>
            <div class="progress-bar">
              <div class="progress-fill" style={"width: #{entry.progress}%"}>
                <%= entry.progress %>%
              </div>
            </div>

            <%!-- Show file metadata --%>
            <p class="meta">
              Size: <%= format_bytes(entry.client_size) %> |
              Type: <%= entry.client_type %>
            </p>

            <%!-- Entry-specific errors --%>
            <%= for err <- upload_errors(@uploads.photos, entry) do %>
              <p class="error"><%= error_to_string(err) %></p>
            <% end %>
          </div>
        <% end %>

        <button type="submit" disabled={length(@uploads.photos.entries) == 0}>
          Upload <%= length(@uploads.photos.entries) %> Photos
        </button>
      </form>

      <h3>Uploaded Photos</h3>
      <div class="photo-grid">
        <%= for photo <- @uploaded_files do %>
          <div>
            <img src={photo.url} alt={photo.name} width="200" />
            <p><%= photo.name %></p>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  defp format_bytes(bytes) when bytes < 1024, do: "#{bytes} B"
  defp format_bytes(bytes) when bytes < 1_048_576, do: "#{div(bytes, 1024)} KB"
  defp format_bytes(bytes), do: "#{div(bytes, 1_048_576)} MB"

  defp error_to_string(:too_large), do: "File too large (max 5MB)"
  defp error_to_string(:not_accepted), do: "Invalid file type"
end
```

**Key Takeaway**: Access `entry.progress`, `entry.client_size`, and `entry.client_type` to display real-time upload progress and metadata.

### Example 58: File Validation

Implement custom file validation beyond built-in rules.

**Code**:

```elixir
defmodule MyAppWeb.ValidatedUploadLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:uploaded_files, [])
      |> allow_upload(:images,
        accept: ~w(.jpg .jpeg .png),
        max_entries: 3,
        max_file_size: 2_000_000
      )

    {:ok, socket}
  end

  def handle_event("validate", _params, socket) do
    # Additional custom validation beyond allow_upload config
    socket =
      Enum.reduce(socket.assigns.uploads.images.entries, socket, fn entry, acc_socket ->
        # Check image dimensions (requires reading file)
        with {:ok, dimensions} <- get_image_dimensions(entry) do
          if dimensions.width > 4000 or dimensions.height > 4000 do
            # Cancel upload with custom error
            cancel_upload(acc_socket, :images, entry.ref)
          else
            acc_socket
          end
        else
          _ -> acc_socket
        end
      end)

    {:noreply, socket}
  end

  def handle_event("upload", _params, socket) do
    uploaded_files =
      consume_uploaded_entries(socket, :images, fn %{path: path}, entry ->
        # Validate one more time before saving
        case validate_image_content(path) do
          :ok ->
            dest = Path.join("priv/static/uploads/images", entry.client_name)
            File.cp!(path, dest)
            {:ok, "/uploads/images/#{entry.client_name}"}

          {:error, reason} ->
            {:postpone, reason} # => Postpone this entry, keep others
        end
      end)

    socket = assign(socket, :uploaded_files, uploaded_files)
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Image Upload with Validation</h2>

      <form phx-change="validate" phx-submit="upload">
        <.live_file_input upload={@uploads.images} />

        <ul class="requirements">
          <li>JPG or PNG format</li>
          <li>Maximum 2MB per file</li>
          <li>Maximum 3 files</li>
          <li>Maximum 4000x4000 pixels</li>
        </ul>

        <%= for entry <- @uploads.images.entries do %>
          <div>
            <%= entry.client_name %>
            <%= for err <- upload_errors(@uploads.images, entry) do %>
              <p class="error"><%= error_to_string(err) %></p>
            <% end %>
          </div>
        <% end %>

        <button type="submit">Upload Images</button>
      </form>

      <h3>Uploaded Images</h3>
      <%= for image <- @uploaded_files do %>
        <img src={image} alt="Uploaded" width="300" />
      <% end %>
    </div>
    """
  end

  defp get_image_dimensions(_entry) do
    # Simulate dimension check (would use actual image library)
    {:ok, %{width: 2000, height: 1500}}
  end

  defp validate_image_content(_path) do
    # Validate file content (check magic bytes, scan for malicious content)
    :ok
  end

  defp error_to_string(:too_large), do: "File too large"
  defp error_to_string(:not_accepted), do: "Invalid file type"
  defp error_to_string(:external_client_failure), do: "Upload failed"
end
```

**Key Takeaway**: Use `cancel_upload/3` to reject uploads with custom validation errors, and `{:postpone, reason}` in consume to skip problematic files.

### Example 59: Consume Uploaded Entries

Process uploaded files with `consume_uploaded_entries/3` callback.

**Code**:

```elixir
defmodule MyAppWeb.BatchUploadLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:results, [])
      |> allow_upload(:csv_files,
        accept: ~w(.csv),
        max_entries: 10,
        max_file_size: 50_000_000
      )

    {:ok, socket}
  end

  def handle_event("validate", _params, socket) do
    {:noreply, socket}
  end

  def handle_event("upload", _params, socket) do
    # Process each uploaded file
    results =
      consume_uploaded_entries(socket, :csv_files, fn %{path: path}, entry ->
        # path: temporary file path (deleted after consume completes)
        # entry: metadata (client_name, content_type, client_size, etc.)

        case process_csv(path, entry.client_name) do
          {:ok, row_count} ->
            # Success: return processed data
            {:ok, %{
              name: entry.client_name,
              status: :success,
              rows: row_count,
              size: entry.client_size
            }}

          {:error, reason} ->
            # Error: return error info
            {:ok, %{
              name: entry.client_name,
              status: :error,
              error: reason
            }}
        end
      end)
    # => results: list of return values from callback

    socket = assign(socket, :results, results) # => Display results
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>CSV Batch Upload</h2>

      <form phx-change="validate" phx-submit="upload">
        <.live_file_input upload={@uploads.csv_files} />

        <%= for entry <- @uploads.csv_files.entries do %>
          <div>
            <%= entry.client_name %> - <%= format_bytes(entry.client_size) %>
          </div>
        <% end %>

        <button type="submit">Process CSV Files</button>
      </form>

      <h3>Processing Results</h3>
      <table>
        <thead>
          <tr>
            <th>File</th>
            <th>Status</th>
            <th>Rows</th>
            <th>Size</th>
          </tr>
        </thead>
        <tbody>
          <%= for result <- @results do %>
            <tr>
              <td><%= result.name %></td>
              <td class={result.status}><%= result.status %></td>
              <td><%= result[:rows] || "-" %></td>
              <td><%= format_bytes(result[:size] || 0) %></td>
            </tr>
          <% end %>
        </tbody>
      </table>
    </div>
    """
  end

  defp process_csv(path, name) do
    # Simulate CSV processing
    IO.puts("Processing #{name}...")
    row_count = :rand.uniform(1000)
    {:ok, row_count}
  end

  defp format_bytes(bytes) when bytes < 1024, do: "#{bytes} B"
  defp format_bytes(bytes) when bytes < 1_048_576, do: "#{div(bytes, 1024)} KB"
  defp format_bytes(bytes), do: "#{div(bytes, 1_048_576)} MB"
end
```

**Key Takeaway**: `consume_uploaded_entries/3` receives temporary file path and entry metadata, returning your processed data which accumulates in a list.

### Example 60: Multiple Upload Configurations

Configure multiple independent upload inputs in a single LiveView.

**Code**:

```elixir
defmodule MyAppWeb.ProfileUploadLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:avatar_url, nil)
      |> assign(:document_urls, [])
      |> allow_upload(:avatar, # => First upload config
        accept: ~w(.jpg .jpeg .png),
        max_entries: 1,
        max_file_size: 1_000_000 # => 1MB for avatar
      )
      |> allow_upload(:documents, # => Second upload config
        accept: ~w(.pdf .doc .docx),
        max_entries: 5,
        max_file_size: 10_000_000 # => 10MB for documents
      )
    # => Two independent upload configurations

    {:ok, socket}
  end

  def handle_event("validate", _params, socket) do
    {:noreply, socket} # => Both uploads validated independently
  end

  def handle_event("save_avatar", _params, socket) do
    # Process only avatar upload
    [avatar_url] =
      consume_uploaded_entries(socket, :avatar, fn %{path: path}, entry ->
        dest = Path.join("priv/static/uploads/avatars", entry.client_name)
        File.cp!(path, dest)
        {:ok, "/uploads/avatars/#{entry.client_name}"}
      end)

    socket = assign(socket, :avatar_url, avatar_url) # => Update avatar
    {:noreply, socket}
  end

  def handle_event("save_documents", _params, socket) do
    # Process only documents upload
    document_urls =
      consume_uploaded_entries(socket, :documents, fn %{path: path}, entry ->
        dest = Path.join("priv/static/uploads/documents", entry.client_name)
        File.cp!(path, dest)
        {:ok, "/uploads/documents/#{entry.client_name}"}
      end)

    socket = update(socket, :document_urls, fn urls -> urls ++ document_urls end)
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Profile Setup</h2>

      <%!-- Avatar upload section --%>
      <div class="section">
        <h3>Profile Avatar</h3>
        <form phx-change="validate" phx-submit="save_avatar">
          <.live_file_input upload={@uploads.avatar} />
          <%= for entry <- @uploads.avatar.entries do %>
            <div><%= entry.client_name %> - <%= entry.progress %>%</div>
          <% end %>
          <button type="submit">Upload Avatar</button>
        </form>

        <%= if @avatar_url do %>
          <img src={@avatar_url} alt="Avatar" width="150" />
        <% end %>
      </div>

      <%!-- Documents upload section --%>
      <div class="section">
        <h3>Supporting Documents</h3>
        <form phx-change="validate" phx-submit="save_documents">
          <.live_file_input upload={@uploads.documents} />
          <%= for entry <- @uploads.documents.entries do %>
            <div><%= entry.client_name %> - <%= entry.progress %>%</div>
          <% end %>
          <button type="submit">Upload Documents</button>
        </form>

        <h4>Uploaded Documents</h4>
        <ul>
          <%= for doc <- @document_urls do %>
            <li><a href={doc}><%= Path.basename(doc) %></a></li>
          <% end %>
        </ul>
      </div>
    </div>
    """
  end
end
```

**Key Takeaway**: Call `allow_upload/3` multiple times with different names to configure independent upload inputs with separate validation rules and processing.

## Next Steps

Continue to advanced examples covering LiveComponents, JavaScript interop, testing, and production patterns:

- [Advanced Examples (61-85)](/en/learn/software-engineering/platform-web/elixir-phoenix-liveview/by-example/advanced)

Or review fundamentals:

- [Beginner Examples (1-30)](/en/learn/software-engineering/platform-web/elixir-phoenix-liveview/by-example/beginner)
