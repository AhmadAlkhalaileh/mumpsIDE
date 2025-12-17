#!/bin/bash
# Build script for MUMPS IDE snap package
set -e

echo "=== MUMPS IDE Snap Build Script ==="
echo ""

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
snapcraft --destructive-mode

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
    echo "1. Test the snap: sudo snap install --dangerous --devmode $SNAP_FILE"
    echo "2. Run the app: mumps-ide"
    echo "3. Remove test install: sudo snap remove mumps-ide"
    echo ""
    echo "To upload to Snap Store:"
    echo "  snapcraft upload $SNAP_FILE"
    echo ""
    echo "See docs/SNAP-UPLOAD-GUIDE.md for detailed upload instructions"
    echo ""
else
    echo "ERROR: Snap file not found!"
    exit 1
fi
