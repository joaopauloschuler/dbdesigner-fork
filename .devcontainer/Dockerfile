FROM mcr.microsoft.com/devcontainers/base:ubuntu-24.04
# Use the official devcontainer base image for Ubuntu 24.04

ENV DEBIAN_FRONTEND=noninteractive

# Install Free Pascal, Lazarus, LCL/GUI dependencies, and database client libraries
RUN apt-get update  && apt-get install -y --no-install-recommends     # --- Free Pascal & Lazarus ---
    fpc     fpc-source     lazarus     # --- Build tools ---
    python3-pip     build-essential     # --- GTK2 / LCL widget set dependencies ---
    libgtk2.0-dev     libcairo2-dev     libpango1.0-dev     libgdk-pixbuf-2.0-dev     libx11-dev     # --- Headless GUI testing ---
    xvfb     # --- Database client libraries (runtime) ---
    libsqlite3-dev     libmysqlclient-dev     libpq-dev  && apt-get clean  && rm -rf /var/lib/apt/lists/*

# Ensure workspace folder exists (devcontainers typically mount the repo)
WORKDIR /workspaces/dbdesigner-fork

# Use the default non-root user provided by the base image
USER vscode
