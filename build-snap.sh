#!/usr/bin/env bash
# Build script for Mumps Studio snap package
set -euo pipefail

echo "=== Mumps Studio Snap Build Script ==="
echo ""

BUILD_MODE="${BUILD_MODE:-lxd}"

# Prefer building in an isolated environment that matches the snap base (core22).
# This avoids staging libraries from your host OS that may not run on core22.
if [ "$BUILD_MODE" != "destructive" ] && ! command -v lxc >/dev/null 2>&1; then
    echo "ERROR: LXD is not installed or not on PATH (missing 'lxc' command)."
    echo ""
    echo "Install it with:"
    echo "  sudo snap install lxd"
    echo "  sudo lxd init --minimal"
    echo ""
    echo "Then rerun this script."
    echo ""
    echo "If you *must* build on the host, you can run:"
    echo "  BUILD_MODE=destructive ./build-snap.sh"
    echo "But this is not recommended unless your host matches the snap base (Ubuntu 22.04)."
    echo ""
    exit 1
fi

# Check if dependencies are installed
if [ ! -d "node_modules" ] || [ ! -f "node_modules/electron/dist/electron" ]; then
    echo "ERROR: Dependencies not found!"
    echo "Please run: npm install --include=dev"
    echo ""
    exit 1
fi

echo "✓ Dependencies found"
echo ""

# Clean old build artifacts (requires sudo)
echo "Cleaning old build artifacts..."
if [ -d "parts" ] || [ -d "prime" ] || [ -d "stage" ]; then
    echo "Removing old build directories (requires sudo)..."
    sudo rm -rf parts prime stage .snapcraft 2>/dev/null || true
    echo "✓ Cleaned"
fi

# Clean any old snap files
if ls *.snap 1> /dev/null 2>&1; then
    echo "Removing old snap files..."
    rm -f *.snap 2>/dev/null || sudo rm -f *.snap
    echo "✓ Removed old snaps"
fi

# Clean snapcraft cache
echo "Cleaning snapcraft cache..."
snapcraft clean 2>/dev/null || true
echo "✓ Snapcraft cache cleaned"
echo ""

# Build the snap
echo "Building snap package (this may take several minutes)..."
echo ""
if [ "$BUILD_MODE" = "destructive" ]; then
    echo "WARNING: Using destructive-mode. Prefer LXD builds for core22 compatibility."
    snapcraft pack --destructive-mode
else
    snapcraft pack --use-lxd
fi

echo ""
echo "=== Build Complete ==="
echo ""

# Find the generated snap file
SNAP_FILE=$(ls -t *.snap 2>/dev/null | head -1)

if [ -n "$SNAP_FILE" ]; then
    echo "✓ Snap package created: $SNAP_FILE"
    echo ""
    echo "File size: $(du -h "$SNAP_FILE" | cut -f1)"
    echo ""
    echo "Next steps:"
    echo "1. Install the snap:"
    echo "   sudo snap install --dangerous $SNAP_FILE"
    echo ""
    echo "2. Connect Docker (REQUIRED for Docker features):"
    echo "   ./connect-docker.sh"
    echo "   OR manually:"
    echo "   sudo snap connect mumps-studio:docker-support"
    echo "   sudo usermod -aG docker \$USER"
    echo ""
    echo "3. Run the app: mumps-studio"
    echo "4. Remove test install: sudo snap remove mumps-studio"
    echo ""
    echo "To upload to Snap Store:"
    echo "  snapcraft upload $SNAP_FILE"
    echo ""
    echo "See SNAP-STORE-GUIDE.md for detailed upload instructions"
    echo ""
else
    echo "ERROR: Snap file not found!"
    exit 1
fi
