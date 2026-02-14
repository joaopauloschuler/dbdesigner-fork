FROM mcr.microsoft.com/devcontainers/base:ubuntu-24.04
# Use the official devcontainer base image for Ubuntu 24.04

ENV DEBIAN_FRONTEND=noninteractive

# Install Free Pascal, fpc-source, Lazarus and LCL/GUI dependencies
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    fpc \
    fpc-source \
    lazarus \
    python3-pip \
    build-essential \
    libgtk2.0-dev \
    libcairo2-dev \
    libpango1.0-dev \
    libgdk-pixbuf2.0-dev \
    libx11-dev \
    libsqlite3-dev \
 || true \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

# Ensure workspace folder exists (devcontainers typically mount the repo)
WORKDIR /workspaces/dbdesigner-fork

# Use the default non-root user provided by the base image
USER vscode
