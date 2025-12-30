---
title: "Build Cli Applications"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 1000016
description: "Build professional command-line tools with argparse, Click, and Typer frameworks"
tags: ["python", "cli", "argparse", "click", "typer", "command-line"]
categories: ["learn"]
---

## Problem

Building command-line applications requires parsing arguments, handling user input, and providing clear error messages. Python's argparse offers comprehensive features but verbose syntax. Modern frameworks like Click and Typer provide cleaner APIs but require learning framework patterns. Balancing functionality with usability is essential.

This guide shows effective CLI application development in Python.

## argparse for Standard CLI

### Basic Argument Parsing

```python
import argparse

def create_parser():
    parser = argparse.ArgumentParser(
        description='Process data files',
        epilog='Example: python app.py input.txt -o output.txt'
    )

    # Positional argument
    parser.add_argument('input', help='Input file path')

    # Optional argument
    parser.add_argument(
        '--output', '-o',
        default='output.txt',
        help='Output file path (default: output.txt)'
    )

    # Boolean flag
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Enable verbose output'
    )

    return parser

parser = create_parser()
args = parser.parse_args()

print(f"Input: {args.input}")
print(f"Output: {args.output}")
if args.verbose:
    print("Verbose mode enabled")
```

**Why it matters**: argparse is part of the standard library, requiring no dependencies. Provides automatic help generation and type validation.

### Argument Types and Validation

```python
import argparse
from pathlib import Path

def positive_int(value):
    """Validate positive integer."""
    try:
        ivalue = int(value)
        if ivalue <= 0:
            raise argparse.ArgumentTypeError(f"{value} must be positive")
        return ivalue
    except ValueError:
        raise argparse.ArgumentTypeError(f"{value} must be an integer")

def existing_file(value):
    """Validate file exists."""
    path = Path(value)
    if not path.exists():
        raise argparse.ArgumentTypeError(f"{value} does not exist")
    if not path.is_file():
        raise argparse.ArgumentTypeError(f"{value} is not a file")
    return path

parser = argparse.ArgumentParser()

parser.add_argument(
    '--count',
    type=positive_int,
    default=10,
    help='Number of items (must be positive)'
)

parser.add_argument(
    '--config',
    type=existing_file,
    help='Configuration file path'
)

parser.add_argument(
    '--format',
    choices=['json', 'xml', 'csv'],
    default='json',
    help='Output format'
)

parser.add_argument(
    '--tags',
    nargs='+',
    help='One or more tags'
)

args = parser.parse_args()
```

### Subcommands

```python
import argparse

def create_parser():
    parser = argparse.ArgumentParser(description='Data management tool')
    subparsers = parser.add_subparsers(dest='command', required=True)

    # Add command
    add_parser = subparsers.add_parser('add', help='Add new record')
    add_parser.add_argument('name', help='Record name')
    add_parser.add_argument('--priority', type=int, default=0)

    # List command
    list_parser = subparsers.add_parser('list', help='List all records')
    list_parser.add_argument('--filter', help='Filter by pattern')

    # Delete command
    delete_parser = subparsers.add_parser('delete', help='Delete record')
    delete_parser.add_argument('id', type=int, help='Record ID')

    return parser

def main():
    parser = create_parser()
    args = parser.parse_args()

    if args.command == 'add':
        add_record(args.name, args.priority)
    elif args.command == 'list':
        list_records(args.filter)
    elif args.command == 'delete':
        delete_record(args.id)

def add_record(name, priority):
    print(f"Adding {name} with priority {priority}")

def list_records(filter_pattern):
    print(f"Listing records (filter: {filter_pattern})")

def delete_record(record_id):
    print(f"Deleting record {record_id}")

if __name__ == '__main__':
    main()
```

## Click Framework

**Note**: Click 8.3+ requires Python 3.10 or later. For Python 3.7-3.9, use Click 8.1.x.

### Basic Click Application

```python
import click

@click.command()
@click.argument('input_file', type=click.Path(exists=True))
@click.option('--output', '-o', default='output.txt', help='Output file')
@click.option('--verbose', '-v', is_flag=True, help='Verbose output')
def process(input_file, output, verbose):
    """Process INPUT_FILE and write results to output."""
    if verbose:
        click.echo(f"Processing {input_file}...")

    # Process file
    with open(input_file) as f:
        data = f.read()

    with open(output, 'w') as f:
        f.write(data.upper())

    click.echo(f"Results written to {output}")

if __name__ == '__main__':
    process()
```

**Why it matters**: Click provides declarative syntax with automatic help generation. Decorator-based API is cleaner than argparse for complex applications.

### Click Options and Types

```python
import click

@click.command()
@click.option('--count', type=int, default=1, help='Number of iterations')
@click.option('--name', prompt='Your name', help='Name to greet')
@click.option('--greeting', default='Hello', help='Greeting word')
@click.option('--loud', is_flag=True, help='Uppercase output')
@click.option(
    '--format',
    type=click.Choice(['json', 'xml', 'csv']),
    default='json',
    help='Output format'
)
@click.option(
    '--config',
    type=click.File('r'),
    help='Configuration file'
)
def greet(count, name, greeting, loud, format, config):
    """Greet NAME COUNT times."""
    message = f"{greeting}, {name}!"

    if loud:
        message = message.upper()

    for _ in range(count):
        click.echo(message)

    if config:
        click.echo(f"Config: {config.read()}")

@click.command()
@click.option('--username', prompt=True)
@click.option('--password', prompt=True, hide_input=True,
              confirmation_prompt=True)
def login(username, password):
    """Login with credentials."""
    click.echo(f"Logging in as {username}")

@click.command()
@click.confirmation_option(prompt='Are you sure you want to delete?')
def delete():
    """Delete all data."""
    click.echo('Deleting...')
```

### Click Groups and Subcommands

```python
import click

@click.group()
def cli():
    """Data management tool."""
    pass

@cli.command()
@click.argument('name')
@click.option('--priority', type=int, default=0)
def add(name, priority):
    """Add a new record."""
    click.echo(f"Adding {name} with priority {priority}")

@cli.command()
@click.option('--filter', help='Filter pattern')
def list(filter):
    """List all records."""
    click.echo(f"Listing records (filter: {filter})")

@cli.command()
@click.argument('record_id', type=int)
def delete(record_id):
    """Delete a record."""
    click.echo(f"Deleting record {record_id}")

if __name__ == '__main__':
    cli()

```

### Click Utilities

```python
import click
import time

@click.command()
def download():
    """Download files."""
    items = range(100)
    with click.progressbar(items, label='Downloading') as bar:
        for item in bar:
            time.sleep(0.01)  # Simulate work

@click.command()
def status():
    """Show status with colors."""
    click.secho('Success!', fg='green', bold=True)
    click.secho('Warning!', fg='yellow')
    click.secho('Error!', fg='red', bold=True)

    # Conditional styling
    click.echo(click.style('Info', fg='blue'))

@click.command()
@click.option('--yes', is_flag=True, help='Skip confirmation')
def dangerous(yes):
    """Perform dangerous operation."""
    if not yes:
        click.confirm('Are you sure?', abort=True)

    click.echo('Performing operation...')

@click.command()
def logs():
    """Show logs with pagination."""
    lines = [f"Log line {i}" for i in range(1000)]
    click.echo_via_pager('\n'.join(lines))
```

## Typer Framework

### Basic Typer Application

```python
import typer
from pathlib import Path

app = typer.Typer()

@app.command()
def process(
    input_file: Path = typer.Argument(..., help="Input file path"),
    output: Path = typer.Option(Path("output.txt"), help="Output file"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Verbose output")
):
    """Process INPUT_FILE and write results to output."""
    if verbose:
        typer.echo(f"Processing {input_file}...")

    # Process file
    data = input_file.read_text()
    output.write_text(data.upper())

    typer.echo(f"Results written to {output}")

if __name__ == "__main__":
    app()
```

**Why it matters**: Typer combines Click's ease-of-use with modern Python type hints. Type annotations provide automatic validation and IDE support.

### Typer with Type Hints

```python
import typer
from typing import Optional, List
from enum import Enum
from pathlib import Path

class Format(str, Enum):
    json = "json"
    xml = "xml"
    csv = "csv"

app = typer.Typer()

@app.command()
def convert(
    input_file: Path,
    output_format: Format,
    output_file: Optional[Path] = None,
    tags: Optional[List[str]] = typer.Option(None, "--tag", help="Add tags"),
    count: int = typer.Option(1, min=1, max=100, help="Iteration count"),
    verbose: bool = False
):
    """Convert INPUT_FILE to specified format."""
    if verbose:
        typer.echo(f"Converting {input_file} to {output_format.value}")

    if tags:
        typer.echo(f"Tags: {', '.join(tags)}")

    typer.echo(f"Processing {count} times...")

@app.command()
def login(
    username: str = typer.Option(..., prompt=True),
    password: str = typer.Option(..., prompt=True, hide_input=True)
):
    """Login with credentials."""
    typer.echo(f"Logging in as {username}")

if __name__ == "__main__":
    app()
```

### Typer Subcommands

```python
import typer

app = typer.Typer()

@app.command()
def add(
    name: str,
    priority: int = typer.Option(0, help="Priority level")
):
    """Add a new record."""
    typer.echo(f"Adding {name} with priority {priority}")

@app.command()
def list(
    filter_pattern: Optional[str] = typer.Option(None, "--filter", help="Filter")
):
    """List all records."""
    typer.echo(f"Listing records (filter: {filter_pattern})")

@app.command()
def delete(
    record_id: int,
    force: bool = typer.Option(False, "--force", "-f", help="Skip confirmation")
):
    """Delete a record."""
    if not force:
        confirmed = typer.confirm("Are you sure?")
        if not confirmed:
            raise typer.Abort()

    typer.echo(f"Deleting record {record_id}")

if __name__ == "__main__":
    app()
```

## stdin, stdout, stderr

### Reading from stdin

```python
import sys
import argparse

def read_input(filename=None):
    if filename and filename != '-':
        with open(filename) as f:
            return f.read()
    else:
        # Read from stdin
        return sys.stdin.read()

def process_lines():
    for line in sys.stdin:
        # Process each line
        print(line.upper(), end='')

parser = argparse.ArgumentParser()
parser.add_argument('input', nargs='?', default='-', help='Input file or - for stdin')
args = parser.parse_args()

data = read_input(args.input)
print(data)

```

### Writing to stdout and stderr

```python
import sys

print("Normal output")
sys.stdout.write("Output to stdout\n")

print("Error message", file=sys.stderr)
sys.stderr.write("Error to stderr\n")

def process_data(data):
    # Progress/status to stderr (won't interfere with piping)
    print(f"Processing {len(data)} items...", file=sys.stderr)

    # Actual data to stdout (can be piped)
    for item in data:
        print(item)  # Goes to stdout

def main():
    try:
        process_data(data)
        sys.exit(0)  # Success
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)  # Failure
```

### Click stdin/stdout

```python
import click

@click.command()
@click.argument('input', type=click.File('r'), default='-')
@click.argument('output', type=click.File('w'), default='-')
def transform(input, output):
    """Transform INPUT to OUTPUT (use - for stdin/stdout)."""
    data = input.read()
    result = data.upper()
    output.write(result)

```

## Entry Points with setuptools

### Creating Entry Points

```python
from setuptools import setup, find_packages

setup(
    name='mytool',
    version='1.0.0',
    packages=find_packages(),
    install_requires=[
        'click>=8.0',
        'requests>=2.25',
    ],
    entry_points={
        'console_scripts': [
            'mytool=mytool.cli:main',
            'mytool-admin=mytool.admin:admin_main',
        ],
    },
)

import click

@click.command()
def main():
    """Main CLI entry point."""
    click.echo("MyTool CLI")

if __name__ == '__main__':
    main()

```

### pyproject.toml Entry Points

```toml
[project]
name = "mytool"
version = "1.0.0"
dependencies = [
    "click>=8.0",
    "requests>=2.25",
]

[project.scripts]
mytool = "mytool.cli:main"
mytool-admin = "mytool.admin:admin_main"

```

## Testing CLI Applications

### Testing argparse

```python
import argparse
import pytest
from io import StringIO
import sys

def create_parser():
    parser = argparse.ArgumentParser()
    parser.add_argument('input')
    parser.add_argument('--output', default='output.txt')
    return parser

def test_parser_with_all_args():
    parser = create_parser()
    args = parser.parse_args(['input.txt', '--output', 'result.txt'])

    assert args.input == 'input.txt'
    assert args.output == 'result.txt'

def test_parser_with_defaults():
    parser = create_parser()
    args = parser.parse_args(['input.txt'])

    assert args.input == 'input.txt'
    assert args.output == 'output.txt'

def test_parser_missing_required():
    parser = create_parser()

    with pytest.raises(SystemExit):
        parser.parse_args([])
```

### Testing Click

```python
import click
from click.testing import CliRunner

@click.command()
@click.argument('name')
@click.option('--greeting', default='Hello')
def greet(name, greeting):
    """Greet NAME."""
    click.echo(f"{greeting}, {name}!")

def test_greet_default():
    runner = CliRunner()
    result = runner.invoke(greet, ['Alice'])

    assert result.exit_code == 0
    assert result.output == "Hello, Alice!\n"

def test_greet_custom():
    runner = CliRunner()
    result = runner.invoke(greet, ['Bob', '--greeting', 'Hi'])

    assert result.exit_code == 0
    assert result.output == "Hi, Bob!\n"

@click.command()
@click.argument('input', type=click.File('r'))
def process_file(input):
    """Process input file."""
    data = input.read()
    click.echo(data.upper())

def test_process_file():
    runner = CliRunner()

    with runner.isolated_filesystem():
        # Create test file
        with open('test.txt', 'w') as f:
            f.write('hello world')

        result = runner.invoke(process_file, ['test.txt'])

        assert result.exit_code == 0
        assert result.output == "HELLO WORLD\n"
```

### Testing Typer

```python
import typer
from typer.testing import CliRunner

app = typer.Typer()

@app.command()
def hello(name: str, greeting: str = "Hello"):
    """Greet NAME."""
    typer.echo(f"{greeting}, {name}!")

def test_hello_default():
    runner = CliRunner()
    result = runner.invoke(app, ["Alice"])

    assert result.exit_code == 0
    assert "Hello, Alice!" in result.stdout

def test_hello_custom():
    runner = CliRunner()
    result = runner.invoke(app, ["Bob", "--greeting", "Hi"])

    assert result.exit_code == 0
    assert "Hi, Bob!" in result.stdout
```

## Best Practices

### Error Handling

```python
import click
import sys

@click.command()
@click.argument('filename', type=click.Path(exists=True))
def process(filename):
    """Process file."""
    try:
        with open(filename) as f:
            data = f.read()

        # Process data
        result = transform(data)
        click.echo(result)

    except FileNotFoundError:
        click.echo(f"Error: File not found: {filename}", err=True)
        sys.exit(1)
    except PermissionError:
        click.echo(f"Error: Permission denied: {filename}", err=True)
        sys.exit(1)
    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)

def transform(data):
    return data.upper()
```

### Configuration Files

```python
import click
from pathlib import Path
import yaml

@click.command()
@click.option('--config', type=click.Path(), help='Config file path')
@click.option('--api-key', help='API key')
@click.option('--endpoint', help='API endpoint')
def api_client(config, api_key, endpoint):
    """API client with config file support."""
    settings = {}

    # Load from config file
    if config:
        config_path = Path(config)
        if config_path.exists():
            with config_path.open() as f:
                settings = yaml.safe_load(f)

    # Command line overrides config file
    if api_key:
        settings['api_key'] = api_key
    if endpoint:
        settings['endpoint'] = endpoint

    click.echo(f"API Key: {settings.get('api_key')}")
    click.echo(f"Endpoint: {settings.get('endpoint')}")
```

### Shell Completion

```python
import click

@click.command()
@click.argument('name')
def greet(name):
    """Greet NAME."""
    click.echo(f"Hello, {name}!")

if __name__ == '__main__':
    greet()


import typer

app = typer.Typer()

@app.command()
def main():
    """Application with completion."""
    typer.echo("Hello!")

if __name__ == "__main__":
    app()

```

## Summary

CLI applications in Python use argparse for standard library approach, Click for decorator-based syntax, or Typer for type-hint driven development. argparse requires no dependencies, Click provides cleaner API, Typer adds type safety through annotations.

argparse handles arguments through add_argument() with types, choices, and custom validators. Subparsers enable git-style commands. Verbose but comprehensive - suitable for simple scripts and maximum compatibility.

Click uses decorators for commands, @click.argument for positional args, @click.option for flags. Automatic help generation, type validation, and utilities like progress bars, styled output, confirmation prompts. Clean API for complex CLI applications.

Typer builds on Click with type hints replacing decorators' explicit types. Path, Optional, List, Enum from typing provide validation. Modern Python approach with IDE support and automatic documentation.

stdin/stdout/stderr handling separates data flow from user messages. Read stdin with sys.stdin.read(), write errors to sys.stderr, output data to sys.stdout. Enables piping and redirection - fundamental Unix philosophy.

Entry points through setuptools or pyproject.toml install commands globally. console_scripts map command names to Python functions. Users run installed commands without knowing Python paths.

Testing CLI apps uses CliRunner from Click/Typer or direct argparse parsing. isolated_filesystem() creates temporary directories. Capture output, verify exit codes, test error handling. Essential for reliable CLI tools.

Best practices include graceful error handling with clear messages, configuration file support, shell completion, progress indication for long operations. Exit codes communicate success/failure to shell scripts.

Choose argparse for simple scripts with no dependencies, Click for feature-rich CLI with clean syntax, Typer for modern type-safe development. All three produce professional command-line interfaces.

## Related Content

- [Python Best Practices](/en/learn/software-engineering/programming-languages/python/explanation/best-practices)
- [How to Handle Errors Effectively](/en/learn/software-engineering/programming-languages/python/how-to/handle-errors-effectively)
- [How to Write Effective Tests](/en/learn/software-engineering/programming-languages/python/how-to/write-effective-tests)
