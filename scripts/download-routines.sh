#!/bin/bash
# Download Vista Routines from Docker Container
# Usage: ./download-routines.sh [container-id] [output-dir]

set -e

CONTAINER_ID="${1:-8c21cf79fb67}"
OUTPUT_DIR="${2:-$HOME/Desktop/vista-routines}"

echo "======================================"
echo "Vista Routines Downloader"
echo "======================================"
echo "Container ID: $CONTAINER_ID"
echo "Output Directory: $OUTPUT_DIR"
echo ""

# Check if container is running
if ! docker ps --format '{{.ID}}' | grep -q "^${CONTAINER_ID}"; then
    echo "ERROR: Container $CONTAINER_ID is not running!"
    exit 1
fi

echo "✓ Container is running"

# Auto-discover localr and routines paths
echo ""
echo "Discovering Vista paths..."
PATHS=$(docker exec "$CONTAINER_ID" sh -c 'find /var/worldvista -maxdepth 3 -type d \( -name "localr" -o -name "routines" \) 2>/dev/null | head -20')

if [ -z "$PATHS" ]; then
    echo "ERROR: Could not find localr or routines directories!"
    exit 1
fi

echo "Found paths:"
echo "$PATHS"
echo ""

# Extract localr and routines paths
LOCALR_PATH=$(echo "$PATHS" | grep "localr" | head -1)
ROUTINES_PATH=$(echo "$PATHS" | grep "routines" | head -1)

if [ -z "$LOCALR_PATH" ]; then
    echo "WARNING: No localr directory found"
    LOCALR_PATH=""
fi

if [ -z "$ROUTINES_PATH" ]; then
    echo "WARNING: No routines directory found"
    ROUTINES_PATH=""
fi

# Create output directories
mkdir -p "$OUTPUT_DIR/localr"
mkdir -p "$OUTPUT_DIR/routines"

# Download localr files
if [ -n "$LOCALR_PATH" ]; then
    echo "Downloading from: $LOCALR_PATH"

    # Get list of .m files
    FILES=$(docker exec "$CONTAINER_ID" sh -c "ls \"$LOCALR_PATH\"/*.m 2>/dev/null || true")

    if [ -z "$FILES" ]; then
        echo "WARNING: No .m files found in $LOCALR_PATH"
    else
        FILE_COUNT=$(echo "$FILES" | wc -l)
        echo "Found $FILE_COUNT files in localr"

        # Copy files from container to local
        echo "Copying files..."
        docker cp "$CONTAINER_ID:$LOCALR_PATH/." "$OUTPUT_DIR/localr/"

        # Count only .m files that were copied
        COPIED_COUNT=$(find "$OUTPUT_DIR/localr" -name "*.m" 2>/dev/null | wc -l)
        echo "✓ Downloaded $COPIED_COUNT .m files to $OUTPUT_DIR/localr/"
    fi
fi

# Download routines files
if [ -n "$ROUTINES_PATH" ]; then
    echo ""
    echo "Downloading from: $ROUTINES_PATH"

    # Get list of .m files
    FILES=$(docker exec "$CONTAINER_ID" sh -c "ls \"$ROUTINES_PATH\"/*.m 2>/dev/null || true")

    if [ -z "$FILES" ]; then
        echo "WARNING: No .m files found in $ROUTINES_PATH"
    else
        FILE_COUNT=$(echo "$FILES" | wc -l)
        echo "Found $FILE_COUNT files in routines"

        # Copy files from container to local
        echo "Copying files..."
        docker cp "$CONTAINER_ID:$ROUTINES_PATH/." "$OUTPUT_DIR/routines/"

        # Count only .m files that were copied
        COPIED_COUNT=$(find "$OUTPUT_DIR/routines" -name "*.m" 2>/dev/null | wc -l)
        echo "✓ Downloaded $COPIED_COUNT .m files to $OUTPUT_DIR/routines/"
    fi
fi

# Summary
echo ""
echo "======================================"
echo "Download Complete!"
echo "======================================"
echo "Location: $(realpath "$OUTPUT_DIR")"

TOTAL_LOCALR=$(find "$OUTPUT_DIR/localr" -name "*.m" 2>/dev/null | wc -l)
TOTAL_ROUTINES=$(find "$OUTPUT_DIR/routines" -name "*.m" 2>/dev/null | wc -l)

echo "• localr: $TOTAL_LOCALR files"
echo "• routines: $TOTAL_ROUTINES files"
echo ""

# Offer to initialize git repo
if [ ! -d "$OUTPUT_DIR/.git" ]; then
    echo "Initialize Git repository? (y/n)"
    read -r INIT_GIT

    if [ "$INIT_GIT" = "y" ] || [ "$INIT_GIT" = "Y" ]; then
        cd "$OUTPUT_DIR"
        git init

        # Create .gitignore
        cat > .gitignore << 'EOF'
# Editor files
*.swp
*.swo
*~
.DS_Store

# Logs
*.log
EOF

        # Create README
        cat > README.md << EOF
# Vista Routines

Downloaded from Docker container: \`$CONTAINER_ID\`

## Structure

- \`localr/\` - Modified routines (after Vista upgrade)
- \`routines/\` - Original routines (before Vista upgrade)

## Statistics

- localr: $TOTAL_LOCALR files
- routines: $TOTAL_ROUTINES files

Downloaded on: $(date)
EOF

        git add .
        git commit -m "Initial commit: Download Vista routines from container $CONTAINER_ID

Downloaded $TOTAL_LOCALR files from localr
Downloaded $TOTAL_ROUTINES files from routines

Paths:
- localr: $LOCALR_PATH
- routines: $ROUTINES_PATH
"

        echo "✓ Git repository initialized"
        echo ""
        echo "Next steps:"
        echo "1. cd $OUTPUT_DIR"
        echo "2. git remote add origin <your-gitlab-url>"
        echo "3. git push -u origin main"
    fi
fi

echo ""
echo "Done!"
