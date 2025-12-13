---
title: "With Docker Compose"
weight: 803
date: 2025-03-16T07:20:00+07:00
draft: false
---

## Introduction

Docker Compose simplifies the process of setting up and managing multi-container applications. This article will guide you through building a PostgreSQL Docker image with persistent data using Docker Compose. We'll cover the file structure, setup instructions, and execution of an initialization script during container creation.

## File Structure

Before we begin, let's set up the file structure required for building the PostgreSQL Docker image:

1. **docker-compose.yml**: The Docker Compose file defines and configures the services for your application. It contains the specifications for building and running the PostgreSQL container. Here's an example:

```docker
version: '3.9'
services:
  postgres:
    image: postgres:latest
    environment:
      POSTGRES_USER: myuser
      POSTGRES_PASSWORD: mypassword
      POSTGRES_DB: mydatabase
    volumes:
      - ~/postgresql-project:/var/lib/postgresql/data
      - ./init.sql:/docker-entrypoint-initdb.d/init.sql
    ports:
      - 5432:5432
    networks:
      - app-network

networks:
  app-network:
    driver: bridge
```

## Setup Instructions

To get started, follow these steps:

1. Create a new directory for your project and navigate to it in your terminal.
2. Create the `docker-compose.yml` file and paste the above content into it.
3. Create an `init.sql` file in the same directory as the `docker-compose.yml` file. Customize this file to include any SQL statements you want to execute during container initialization.

   ```sql
   -- Example: Create a sample table
   CREATE TABLE IF NOT EXISTS users (
     id SERIAL PRIMARY KEY,
     name VARCHAR(100) NOT NULL
   );
   ```

4. Save the `init.sql` file.

## Building the Docker Image and Starting the Container

With the file structure and setup complete, you can now build the Docker image and start the PostgreSQL container using Docker Compose.

1. Open your terminal and navigate to the directory where the `docker-compose.yml` file is located.
2. Build the Docker image and start the container by running the following command:

   ```bash
   docker-compose up -d
   ```

   The `-d` flag detaches the containers and runs them in the background.

   Docker Compose will build the PostgreSQL image based on the specifications in the `docker-compose.yml` file and start the container. The `init.sql` script will be executed during container initialization, ensuring the specified database schema or setup tasks are applied.

3. Confirm that the container is running by executing the following command:

   ```bash
   docker-compose ps
   ```

   You should see the `postgres` service listed with the corresponding container ID, status, and other details.

## Connecting to the PostgreSQL Container

To connect to the running PostgreSQL container and interact with the database, you can use a PostgreSQL client tool such as `psql`. Follow these steps:

1. Install `psql` on your local machine if you haven't already. You can install it using your package manager or downloading it from the official PostgreSQL website.
2. Use the following command to connect to the running container:

   ```bash
   psql -h localhost -p 5432 -U myuser -d mydatabase
   ```

   Replace `myuser` with the appropriate PostgreSQL username and `mydatabase` with the desired database name. You will be prompted to enter the password for the user.

   Once connected, you can execute SQL queries, create tables, insert data, and perform other operations as needed.

.

## Stopping and Removing the Containers

To stop and remove the PostgreSQL container, use the following command:

```bash
docker-compose down
```

This command stops and removes the containers, networks, and associated resources created by Docker Compose.

## Conclusion

Using Docker Compose simplifies the process of building and managing multi-container applications. By following the steps outlined in this article, you can easily set up a PostgreSQL Docker image with persistent data using Docker Compose. The automatic execution of the `init.sql` script during container initialization allows you to conveniently initialize your database with the desired schema or setup tasks. This approach provides a flexible and efficient way to work with PostgreSQL in a containerized environment.
