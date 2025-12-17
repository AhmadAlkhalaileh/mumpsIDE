#!/bin/bash

# Mumps Studio - Docker Connection Setup Script
# This script connects the snap to Docker and adds the user to the docker group

set -e

echo "=================================="
echo "Mumps Studio - Docker Setup"
echo "=================================="
echo ""

# Check if snap is installed
if ! snap list mumps-studio &>/dev/null; then
    echo "❌ Error: mumps-studio snap is not installed."
    echo "Please install the snap first using:"
    echo "  sudo snap install mumps-studio_1_amd64.snap --dangerous"
    exit 1
fi

echo "✓ Mumps Studio snap is installed"
echo ""

# Connect docker-support interface
echo "Connecting docker-support interface..."
if sudo snap connect mumps-studio:docker-support; then
    echo "✓ Docker support connected successfully"
else
    echo "⚠ Warning: Could not connect docker-support interface"
    echo "This might be normal if it's already connected."
fi
echo ""

# Check if Docker is installed
if ! command -v docker &>/dev/null; then
    echo "⚠ Warning: Docker is not installed on this system."
    echo ""
    echo "To install Docker, run:"
    echo "  sudo apt update"
    echo "  sudo apt install docker.io -y"
    echo "  sudo systemctl enable --now docker"
    echo ""
    read -p "Do you want to install Docker now? (y/n) " -n 1 -r
    echo ""
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        sudo apt update
        sudo apt install docker.io -y
        sudo systemctl enable --now docker
        echo "✓ Docker installed successfully"
    else
        echo "Skipping Docker installation."
        exit 0
    fi
fi

echo "✓ Docker is installed"
echo ""

# Add user to docker group
CURRENT_USER=$(whoami)
if groups "$CURRENT_USER" | grep -q '\bdocker\b'; then
    echo "✓ User '$CURRENT_USER' is already in the docker group"
else
    echo "Adding user '$CURRENT_USER' to the docker group..."
    sudo usermod -aG docker "$CURRENT_USER"
    echo "✓ User added to docker group"
    echo ""
    echo "⚠ IMPORTANT: You need to log out and log back in (or reboot)"
    echo "   for the docker group membership to take effect."
fi

echo ""
echo "=================================="
echo "Setup Complete!"
echo "=================================="
echo ""
echo "Docker connectivity is now configured for Mumps Studio."
echo ""
echo "If you just joined the docker group, remember to:"
echo "  1. Log out and log back in (or reboot)"
echo "  2. Test Docker access with: docker ps"
echo ""
echo "You can now launch Mumps Studio from your applications menu"
echo "or by running: mumps-studio"
echo ""
