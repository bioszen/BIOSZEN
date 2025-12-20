#!/usr/bin/env bash
# Setup script for running R tests in CI or Codex environment
# Installs R 4.3.3 and required packages
set -e

# Install R base if not already installed
if ! command -v R >/dev/null 2>&1; then
    apt-get update
    DEBIAN_FRONTEND=noninteractive apt-get install -y r-base
fi

# Install packages needed for the test suite
Rscript -e "install.packages('testthat', repos='https://cloud.r-project.org')"
Rscript -e "install.packages('tibble', repos='https://cloud.r-project.org')"

