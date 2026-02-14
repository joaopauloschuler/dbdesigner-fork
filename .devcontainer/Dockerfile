FROM mcr.microsoft.com/devcontainers/base:ubuntu-24.04

ENV DEBIAN_FRONTEND=noninteractive

# Install Free Pascal, Lazarus, LCL/GUI dependencies, and database client libraries
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
    libgdk-pixbuf-2.0-dev \
    libx11-dev \
    xvfb \
    libsqlite3-dev \
    sqlite3 \
    libmysqlclient-dev \
    libpq-dev \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /workspaces/dbdesigner-fork

USER vscode
