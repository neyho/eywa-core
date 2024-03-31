#!/bin/bash

# Determine OS and Architecture
OS=$(uname -s)
ARCH=$(uname -m)


OS=$(uname -s)
ARCH=$(uname -m)

if [ "$OS" = "Darwin" ]; then
    OS="macos"
elif [ "$OS" = "Linux" ]; then
    OS="linux"
fi

# Example filename: yourapp-linux-x86_64
LOCATION="eywa_cli/${OS}/${ARCH}/0.1.9/eywa"


# Construct S3 URL
S3URL="https://s3.eu-central-1.amazonaws.com/eywa.public/$LOCATION"


# Define install location and download the file
INSTALL_DIR="$HOME/.eywa/bin/"


# Create the installation directory if it doesn't exist
mkdir -p "$INSTALL_DIR"


# Check if Eywa is already installed
if [ -f "$INSTALL_DIR/eywa" ]; then
    echo "Eywa is already installed at ${INSTALL_DIR}eywa."
    exit 0
fi

OS=$(uname -s)
if [ "$OS" = "Darwin" ]; then
    # For macOS, use .zshrc for the Zsh shell
    SHELL_PROFILE="$HOME/.zshrc"
else
    # For Linux and others, default to .bashrc
    SHELL_PROFILE="$HOME/.bashrc"
fi

BIN_DIR="~/.eywa/bin/"

# Check if the installation directory is already in PATH
if [[ ":$PATH:" != *":$BIN_DIR:"* ]]; then
    echo "Adding $BIN_DIR to your PATH."

    # Add the directory to PATH for the current session
    export PATH="$PATH:$BIN_DIR"

    # Check if the line is already in the shell profile to avoid duplication
    if ! grep -qxF "export PATH=\$PATH:$BIN_DIR" "$SHELL_PROFILE"; then
        echo "export PATH=\$PATH:$BIN_DIR" >> "$SHELL_PROFILE"
    fi
else
    echo "$BIN_DIR is already in your PATH."
fi

# Download target
curl -o "${INSTALL_DIR}/eywa" "$S3URL"

# Make the file executable
chmod +x "${INSTALL_DIR}/eywa"

#
echo "${LOCATION} installed successfully and added to PATH."

