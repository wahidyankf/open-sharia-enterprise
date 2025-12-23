---
title: "Building CLI Applications"
date: 2025-12-21T18:10:00+07:00
draft: false
description: "Create command-line tools in Elixir with escript, argument parsing, output formatting, and distribution as standalone executables."
weight: 1000018
tags: ["elixir", "cli", "escript", "command-line", "tools", "how-to"]
---

**Need to build command-line tools?** Elixir's escript enables standalone CLI applications with argument parsing, configuration management, and professional output formatting.

## Prerequisites

- Basic Elixir syntax
- Understanding of Mix projects
- Completed [Beginner Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner)

## Problem

Building professional CLI tools requires argument parsing, configuration management, user-friendly help text, error handling, and distribution as standalone executables. You need structured patterns for CLI architecture, output formatting, and testing.

**Challenges:**

- Parsing complex command-line arguments with flags and options
- Providing helpful error messages and usage documentation
- Handling configuration files and environment variables
- Building standalone executables for distribution
- Testing CLI behavior without manual execution

## Solution

Use **escript** to build self-contained executable files, **OptionParser** for argument parsing, and structured patterns for professional CLI applications.

## How It Works

### 1. Basic CLI with escript

Configure escript in mix.exs:

```elixir
# mix.exs
defmodule MyCLI.MixProject do
  use Mix.Project

  def project do
    [
      app: :my_cli,
      version: "0.1.0",
      elixir: "~> 1.14",
      escript: [main_module: MyCLI],
      deps: deps()
    ]
  end

  defp deps do
    []
  end
end
```

Main module:

```elixir
# lib/my_cli.ex
defmodule MyCLI do
  @moduledoc """
  Command-line interface for My Application.
  """

  def main(args) do
    args
    |> parse_args()
    |> process()
    |> output()
  end

  defp parse_args(args) do
    {opts, words, invalid} = OptionParser.parse(args,
      switches: [
        help: :boolean,
        version: :boolean,
        verbose: :boolean,
        output: :string
      ],
      aliases: [
        h: :help,
        v: :version,
        o: :output
      ]
    )

    case {opts, words, invalid} do
      {[help: true], _, _} -> :help
      {[version: true], _, _} -> :version
      {opts, words, []} -> {:run, opts, words}
      {_, _, invalid} -> {:error, :invalid_args, invalid}
    end
  end

  defp process(:help), do: :help
  defp process(:version), do: :version
  defp process({:run, opts, words}) do
    {:ok, %{opts: opts, words: words}}
  end
  defp process({:error, :invalid_args, invalid}) do
    {:error, "Invalid arguments: #{inspect(invalid)}"}
  end

  defp output(:help) do
    IO.puts """
    Usage: my_cli [options] [arguments]

    Options:
      -h, --help           Show this help message
      -v, --version        Show version information
      --verbose            Enable verbose output
      -o, --output FILE    Write output to FILE

    Examples:
      my_cli --help
      my_cli --verbose input.txt
      my_cli -o output.txt process data.csv
    """
  end

  defp output(:version) do
    IO.puts "my_cli version 0.1.0"
  end

  defp output({:ok, %{opts: opts, words: words}}) do
    if opts[:verbose] do
      IO.puts "Processing with options: #{inspect(opts)}"
      IO.puts "Arguments: #{inspect(words)}"
    end

    IO.puts "Running..."
  end

  defp output({:error, message}) do
    IO.puts :stderr, "Error: #{message}"
    System.halt(1)
  end
end
```

Build and run:

```bash
mix escript.build
./my_cli --help
./my_cli --version
./my_cli --verbose input.txt
```

### 2. File Processing CLI

```elixir
defmodule FileProcessor.CLI do
  def main(args) do
    args
    |> parse_args()
    |> validate()
    |> process_files()
    |> display_results()
  end

  defp parse_args(args) do
    {opts, files, _} = OptionParser.parse(args,
      switches: [
        recursive: :boolean,
        pattern: :string,
        format: :string
      ],
      aliases: [
        r: :recursive,
        p: :pattern,
        f: :format
      ]
    )

    %{
      files: files,
      recursive: opts[:recursive] || false,
      pattern: opts[:pattern] || "*",
      format: opts[:format] || "txt"
    }
  end

  defp validate(%{files: []} = config) do
    {:error, "No files specified"}
  end
  defp validate(config), do: {:ok, config}

  defp process_files({:error, _} = error), do: error
  defp process_files({:ok, config}) do
    results = config.files
    |> Enum.flat_map(fn path ->
      if config.recursive do
        find_files_recursive(path, config.pattern)
      else
        [path]
      end
    end)
    |> Enum.map(&process_file(&1, config))

    {:ok, results}
  end

  defp find_files_recursive(path, pattern) do
    path
    |> Path.join("**/*.#{pattern}")
    |> Path.wildcard()
  end

  defp process_file(path, config) do
    case File.read(path) do
      {:ok, content} ->
        %{
          path: path,
          lines: String.split(content, "\n") |> length(),
          size: byte_size(content),
          status: :ok
        }

      {:error, reason} ->
        %{path: path, status: :error, reason: reason}
    end
  end

  defp display_results({:error, message}) do
    IO.puts :stderr, "Error: #{message}"
    System.halt(1)
  end

  defp display_results({:ok, results}) do
    total_files = length(results)
    successful = Enum.count(results, &(&1.status == :ok))
    total_lines = results
      |> Enum.filter(&(&1.status == :ok))
      |> Enum.map(& &1.lines)
      |> Enum.sum()

    IO.puts """

    Results:
    --------
    Total files: #{total_files}
    Processed: #{successful}
    Total lines: #{total_lines}
    """

    Enum.each(results, fn result ->
      case result.status do
        :ok ->
          IO.puts "✓ #{result.path} (#{result.lines} lines, #{result.size} bytes)"

        :error ->
          IO.puts :stderr, "✗ #{result.path} (#{result.reason})"
      end
    end)
  end
end
```

### 3. Interactive CLI with Prompts

```elixir
defmodule InteractiveCLI do
  def main(_args) do
    IO.puts "Welcome to Interactive CLI"
    IO.puts "Type 'help' for commands, 'quit' to exit\n"

    loop()
  end

  defp loop do
    input = IO.gets("> ") |> String.trim()

    case process_command(input) do
      :quit ->
        IO.puts "Goodbye!"

      :continue ->
        loop()
    end
  end

  defp process_command("quit"), do: :quit
  defp process_command("exit"), do: :quit

  defp process_command("help") do
    IO.puts """
    Available commands:
      help    - Show this help
      status  - Show status
      config  - Show configuration
      quit    - Exit the application
    """
    :continue
  end

  defp process_command("status") do
    IO.puts "Status: Running"
    IO.puts "Uptime: #{:erlang.statistics(:wall_clock) |> elem(0)} ms"
    :continue
  end

  defp process_command("config") do
    config = Application.get_all_env(:my_cli)
    IO.inspect(config, label: "Configuration")
    :continue
  end

  defp process_command(unknown) do
    IO.puts "Unknown command: #{unknown}"
    IO.puts "Type 'help' for available commands"
    :continue
  end
end
```

### 4. Progress Indicators

```elixir
defmodule ProgressCLI do
  def main(args) do
    case parse_args(args) do
      {:ok, count} ->
        process_items(count)

      {:error, message} ->
        IO.puts :stderr, message
        System.halt(1)
    end
  end

  defp parse_args([count_str]) do
    case Integer.parse(count_str) do
      {count, ""} when count > 0 ->
        {:ok, count}

      _ ->
        {:error, "Invalid count. Usage: progress_cli <number>"}
    end
  end
  defp parse_args(_), do: {:error, "Usage: progress_cli <number>"}

  defp process_items(total) do
    IO.puts "Processing #{total} items...\n"

    1..total
    |> Enum.each(fn i ->
      Process.sleep(100)  # Simulate work
      show_progress(i, total)
    end)

    IO.puts "\n\nCompleted!"
  end

  defp show_progress(current, total) do
    percentage = div(current * 100, total)
    bar_length = 50
    filled = div(percentage * bar_length, 100)
    empty = bar_length - filled

    bar = String.duplicate("█", filled) <> String.duplicate("░", empty)

    # Clear line and print progress
    IO.write "\r[#{bar}] #{percentage}% (#{current}/#{total})"
  end
end
```

### 5. Configuration Management

```elixir
defmodule ConfigCLI do
  @config_file ".mycli.config"

  def main(args) do
    args
    |> parse_args()
    |> load_config()
    |> merge_config()
    |> execute()
  end

  defp parse_args(args) do
    {opts, commands, _} = OptionParser.parse(args,
      switches: [
        config: :string,
        env: :string
      ]
    )

    %{
      config_file: opts[:config] || @config_file,
      environment: opts[:env] || "development",
      commands: commands,
      cli_opts: opts
    }
  end

  defp load_config(%{config_file: path} = context) do
    config = case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, config} -> config
          {:error, _} -> %{}
        end

      {:error, _} ->
        %{}
    end

    Map.put(context, :file_config, config)
  end

  defp merge_config(context) do
    # Priority: CLI args > Environment vars > Config file > Defaults
    defaults = %{
      "timeout" => 30,
      "retries" => 3,
      "verbose" => false
    }

    env_config = %{
      "timeout" => get_env_int("MYCLI_TIMEOUT"),
      "retries" => get_env_int("MYCLI_RETRIES"),
      "verbose" => get_env_bool("MYCLI_VERBOSE")
    }
    |> Enum.reject(fn {_k, v} -> is_nil(v) end)
    |> Map.new()

    cli_config = context.cli_opts
    |> Enum.map(fn {k, v} -> {to_string(k), v} end)
    |> Map.new()

    final_config = defaults
    |> Map.merge(context.file_config)
    |> Map.merge(env_config)
    |> Map.merge(cli_config)

    Map.put(context, :config, final_config)
  end

  defp get_env_int(key) do
    case System.get_env(key) do
      nil -> nil
      value ->
        case Integer.parse(value) do
          {int, ""} -> int
          _ -> nil
        end
    end
  end

  defp get_env_bool(key) do
    case System.get_env(key) do
      "true" -> true
      "false" -> false
      _ -> nil
    end
  end

  defp execute(%{commands: [], config: config}) do
    IO.puts "Configuration:"
    IO.inspect(config, pretty: true)
  end

  defp execute(%{commands: commands, config: config}) do
    IO.puts "Executing: #{Enum.join(commands, " ")}"
    IO.puts "With config: #{inspect(config)}"
    # Execute commands...
  end
end
```

## Variations

### Colored Output with IO.ANSI

```elixir
defmodule ColorCLI do
  require IO.ANSI

  def main(_args) do
    success("Operation completed successfully")
    warning("This is a warning message")
    error("An error occurred")
    info("Informational message")
  end

  defp success(message) do
    IO.puts [IO.ANSI.green(), "✓ ", IO.ANSI.reset(), message]
  end

  defp warning(message) do
    IO.puts [IO.ANSI.yellow(), "⚠ ", IO.ANSI.reset(), message]
  end

  defp error(message) do
    IO.puts :stderr, [IO.ANSI.red(), "✗ ", IO.ANSI.reset(), message]
  end

  defp info(message) do
    IO.puts [IO.ANSI.blue(), "ℹ ", IO.ANSI.reset(), message]
  end
end
```

### Table Output

```elixir
defmodule TableCLI do
  def print_table(headers, rows) do
    column_widths = calculate_widths(headers, rows)

    print_separator(column_widths)
    print_row(headers, column_widths, true)
    print_separator(column_widths)

    Enum.each(rows, fn row ->
      print_row(row, column_widths, false)
    end)

    print_separator(column_widths)
  end

  defp calculate_widths(headers, rows) do
    all_rows = [headers | rows]

    Enum.reduce(all_rows, [], fn row, widths ->
      row
      |> Enum.with_index()
      |> Enum.map(fn {cell, idx} ->
        current_width = Enum.at(widths, idx, 0)
        max(current_width, String.length(to_string(cell)))
      end)
    end)
  end

  defp print_separator(widths) do
    separator = widths
    |> Enum.map(fn width -> String.duplicate("-", width + 2) end)
    |> Enum.join("+")

    IO.puts "+#{separator}+"
  end

  defp print_row(cells, widths, bold) do
    formatted = cells
    |> Enum.zip(widths)
    |> Enum.map(fn {cell, width} ->
      String.pad_trailing(to_string(cell), width)
    end)
    |> Enum.join(" | ")

    if bold do
      IO.puts [IO.ANSI.bright(), "| #{formatted} |", IO.ANSI.reset()]
    else
      IO.puts "| #{formatted} |"
    end
  end
end

# Usage
TableCLI.print_table(
  ["Name", "Age", "City"],
  [
    ["Alice", 30, "New York"],
    ["Bob", 25, "London"],
    ["Charlie", 35, "Tokyo"]
  ]
)
```

## Advanced Patterns

### 1. Subcommands

```elixir
defmodule GitLikeCLI do
  def main(args) do
    case args do
      ["init" | rest] -> Commands.Init.run(rest)
      ["clone" | rest] -> Commands.Clone.run(rest)
      ["status" | rest] -> Commands.Status.run(rest)
      ["commit" | rest] -> Commands.Commit.run(rest)
      ["--help"] -> show_help()
      _ -> IO.puts :stderr, "Unknown command. Use --help for usage."
    end
  end

  defp show_help do
    IO.puts """
    Git-like CLI Tool

    Usage: gitlike <command> [options]

    Commands:
      init      Initialize a new repository
      clone     Clone a repository
      status    Show working tree status
      commit    Record changes to the repository

    Options:
      --help    Show this help message
    """
  end
end

defmodule Commands.Init do
  def run(args) do
    {opts, _, _} = OptionParser.parse(args,
      switches: [bare: :boolean, path: :string]
    )

    path = opts[:path] || "."
    bare = opts[:bare] || false

    IO.puts "Initializing repository in #{path}"
    if bare, do: IO.puts "Creating bare repository"

    # Implementation...
  end
end
```

### 2. Testing CLI Applications

```elixir
# test/my_cli_test.exs
defmodule MyCLITest do
  use ExUnit.Case

  import ExUnit.CaptureIO

  test "shows help message" do
    output = capture_io(fn ->
      MyCLI.main(["--help"])
    end)

    assert output =~ "Usage:"
    assert output =~ "--help"
  end

  test "shows version" do
    output = capture_io(fn ->
      MyCLI.main(["--version"])
    end)

    assert output =~ "version 0.1.0"
  end

  test "processes valid arguments" do
    output = capture_io(fn ->
      MyCLI.main(["--verbose", "input.txt"])
    end)

    assert output =~ "Running..."
    assert output =~ "input.txt"
  end

  test "handles invalid arguments" do
    output = capture_io(:stderr, fn ->
      catch_exit(MyCLI.main(["--invalid"]))
    end)

    assert output =~ "Invalid arguments"
  end
end
```

### 3. Building for Distribution

```bash
# Build escript
mix escript.build

# Make executable
chmod +x my_cli

# Install to system path
sudo cp my_cli /usr/local/bin/

# Or use Mix task for releases
mix release
```

## Use Cases

**CLI Tools:**

- Code generators and scaffolding tools
- File processors and converters
- Database migration tools
- Deployment automation scripts
- Development utilities

**System Administration:**

- Log analyzers
- System monitoring tools
- Backup utilities
- Configuration management

**Data Processing:**

- CSV/JSON processors
- Report generators
- Data validation tools
- Batch converters

## Best Practices

1. **Provide helpful help text:**
   Include examples and common use cases

2. **Use exit codes correctly:**

   ```elixir
   System.halt(0)  # Success
   System.halt(1)  # General error
   System.halt(2)  # Invalid usage
   ```

3. **Validate input early:**
   Fail fast with clear error messages

4. **Support standard input/output:**

   ```elixir
   # Read from stdin
   IO.stream(:stdio, :line)
   |> Enum.map(&process/1)
   ```

5. **Handle signals gracefully:**
   ```elixir
   System.at_exit(fn _ ->
     IO.puts "Cleaning up..."
   end)
   ```

## Common Pitfalls

1. **Not handling invalid arguments:** Always validate input
2. **Poor error messages:** Be specific about what went wrong
3. **No progress indication:** Long operations need feedback
4. **Hardcoded paths:** Use configuration or arguments
5. **Not testing:** CLI code is code - test it

## Related Resources

- [Mix Guide](/en/learn/software-engineering/programming-language/elixir/how-to/mix)
- [File I/O Guide](/en/learn/software-engineering/programming-language/elixir/how-to/file-io)
- [Testing Guide](/en/learn/software-engineering/programming-language/elixir/how-to/testing)
- [OptionParser Documentation](https://hexdocs.pm/elixir/OptionParser.html)
- [IO.ANSI Documentation](https://hexdocs.pm/elixir/IO.ANSI.html)
