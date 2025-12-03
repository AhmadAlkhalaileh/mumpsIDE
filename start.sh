#!/bin/bash

# Ahmad IDE Startup Script
# This script sets up Docker permissions and starts the application

echo "Starting Ahmad IDE..."

# Set environment variable for Docker access via sg
export AHMAD_IDE_USE_SG=1

# Start the application
npm start
