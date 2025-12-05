---
title: "With Dockerfile"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Docker allows for easy containerization of applications, and with Dockerfiles, you can define the steps to build an image. This article will guide you through building a PostgreSQL Docker image with persistent data using a Dockerfile. We'll cover the file structure, setup instructions, and execution of an initialization script during container creation.

## File Structure

Before we begin, let's set up the file structure required for building the PostgreSQL Docker image:

1. **Dockerfile**: The Dockerfile contains the instructions to build the Docker image. It specifies the base image, sets environment variables, exposes ports, copies files, and executes commands. Here's an example:

   ```
   # Use the official PostgreSQL base image
   FROM postgres:latest

   # Set environment variables
   ENV POSTGRES_USER=myuser
   ENV POSTGRES_PASSWORD=mypassword
   ENV POSTGRES_DB=mydatabase

   # Expose the PostgreSQL port
   EXPOSE 5432

   # Copy the initialization script
   COPY init.sql /docker-entrypoint-initdb.d/

   # Set the volume for persistent data
   VOLUME /var/lib/postgresql/data

   ```

2. **init.sql**: The `init.sql` file contains any SQL statements you want to execute during container initialization. Customize this file as needed.

   ```
   -- Example: Create a sample table
   CREATE TABLE IF NOT EXISTS users (
     id SERIAL PRIMARY KEY,
     name VARCHAR(100) NOT NULL
   );

   ```

## Setup Instructions

To get started, follow these steps:

1. Create a new directory for your project and navigate to it in your terminal.
2. Create the `Dockerfile` and `init.sql` files with the content described above.
3. Save the files in the same directory.

## Building the Docker Image and Starting the Container

With the file structure and setup complete, you can build the Docker image and start the PostgreSQL container using the Dockerfile.

1. Open your terminal and navigate to the directory where the `Dockerfile` is located.
2. Build the Docker image by running the following command:

   ```
   docker build -t my-postgres-image .

   ```

   This command builds the Docker image using the instructions in the `Dockerfile`. The `-t` flag tags the image with a name (e.g., `my-postgres-image`).

3. Start the PostgreSQL container using the following command:

   ```
   docker run -d -p 5432:5432 --name my-postgres-container my-postgres-image

   ```

   This command runs the Docker image as a container. The `-p` flag maps the container's port 5432 to the host's port 5432, allowing access to the PostgreSQL service. The `--name` flag assigns a name to the container.

   The initialization script in the `init.sql` file will be executed during container initialization, setting up the specified database schema or performing any necessary setup tasks.

4. Confirm that the container is running by executing the following command:

   ```
   docker ps

   ```

   You should see the `my-postgres-container` listed with the corresponding container ID, status, and other details.

## Connecting to the PostgreSQL Container

To connect to the running PostgreSQL container and interact with the database, you can use a PostgreSQL client tool such as `psql`. Follow these steps:

1. Install `psql` on your local machine if you haven't already. You can install it using your package manager or download it from the official PostgreSQL website.
2. Use the following command to connect to

the running container:

```
psql -h localhost -p 5432 -U myuser -d mydatabase

```

Replace `myuser` with the appropriate PostgreSQL username and `mydatabase` with the desired database name. You will be prompted to enter the password for the user.

Once connected, you can execute SQL queries, create tables, insert data, and perform other operations.

## Stopping and Removing the Container

To stop and remove the PostgreSQL container, use the following command:

```
docker stop my-postgres-container && docker rm my-postgres-container

```

This command stops and removes the container.

## Conclusion

Using a Dockerfile simplifies the process of building and managing Docker images. By following the steps outlined in this article, you can quickly build a PostgreSQL Docker image with persistent data. The initialization script executed during container initialization allows you to conveniently initialize your database with the desired schema or setup tasks. This approach provides a flexible and efficient way to work with PostgreSQL in a containerized environment.
