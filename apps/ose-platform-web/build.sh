#!/bin/bash
set -e

# Ensure we're in the correct directory
cd "$(dirname "$0")"

# Install Go if not already installed
if ! command -v go &> /dev/null; then
  echo "Installing Go..."
  curl -OL https://go.dev/dl/go1.25.5.linux-amd64.tar.gz
  tar -C $HOME -xzf go1.25.5.linux-amd64.tar.gz
  export PATH=$PATH:$HOME/go/bin
fi

# Check if Hugo is already installed
if ! command -v hugo &>/dev/null; then
    echo "Hugo not found, installing..."

    # Determine the appropriate Hugo download URL
    HUGO_VERSION="0.152.2"
    HUGO_URL="https://github.com/gohugoio/hugo/releases/download/v${HUGO_VERSION}/hugo_extended_${HUGO_VERSION}_Linux-64bit.tar.gz"

    # Download Hugo using curl
    curl -L -o hugo.tar.gz "$HUGO_URL"

    # Extract Hugo
    tar -xzf hugo.tar.gz

    # Move Hugo to a directory in PATH
    mkdir -p $HOME/bin
    mv hugo $HOME/bin/hugo
    export PATH=$PATH:$HOME/bin

    # Cleanup
    rm hugo.tar.gz
fi

# Verify Hugo installation
hugo version

# Clean public directory if it exists
if [ -d "public" ]; then
    rm -rf public
fi

# Build the site with all assets
hugo --gc --minify

# Verify build output
if [ -d "public" ]; then
    echo "Build successful. Contents of public directory:"
    ls -la public

    # Verify static assets
    echo "Checking static assets:"
    ls -la public/css 2>/dev/null || echo "No CSS directory"
    ls -la public/js 2>/dev/null || echo "No JS directory"
    ls -la public/fonts 2>/dev/null || echo "No fonts directory"
else
    echo "Error: Public directory not created"
    exit 1
fi
