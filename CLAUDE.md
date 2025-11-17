# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the **open-sharia-fintech** project - a fintech application built with Node.js. The project is in early stages with basic initialization completed.

## Environment Setup

The project uses **Volta** for Node.js and npm version management:

- **Node.js**: 24.11.1 (LTS)
- **npm**: 11.6.2

These versions are pinned in `package.json` under the `volta` field. When you run `npm` commands, Volta automatically ensures the correct versions are used.

## Project Structure

Currently minimal - the project has:

- `package.json` - Node.js project manifest with Volta pinning
- `README.md` - Empty, ready for documentation
- `.gitignore` - Configured for Node.js and fintech-specific files
- No source code yet

## Common Development Commands

As the project develops, typical commands will include:

- `npm install` - Install dependencies
- `npm run build` - Build the project (to be configured)
- `npm test` - Run tests (to be configured)
- `npm run lint` - Lint code (to be configured)
- `npm run dev` - Start development server (to be configured)

## Important Notes

- Do not stage or commit changes unless explicitly instructed
- The project license is MIT
- Claude Code settings files (`.claude/settings.local.json*`) are gitignored
