# Organic Lever

This is a [Next.js](https://nextjs.org) project for Organic Lever, a tool designed to boost software team productivity.

## Getting Started

First, run the development server:

`npm run dev`

Open [http://localhost:3000](http://localhost:3000) with your browser to see the result.

## Features

- Authentication system with login and logout functionality
- Dashboard for authenticated users
- Home page with hero section and team information
- About page

## Project Structure

- `src/app`: Contains the main application code
  - `api`: API routes for authentication
  - `contexts`: React contexts, including AuthContext
  - `login`: Login page component
  - `dashboard`: Dashboard page component
- `tests`: Contains Playwright tests for the application

## Authentication

The project uses a simple authentication system with email and password. User data is currently stored in `src/data/users.json`.

## Testing

To run the tests:

`npm run test`

## Learn More

To learn more about Next.js, take a look at the following resources:

- [Next.js Documentation](https://nextjs.org/docs) - learn about Next.js features and API.
- [Learn Next.js](https://nextjs.org/learn) - an interactive Next.js tutorial.

## Deployment

This project is currently under development. Deployment instructions will be added in the future.
