#!/bin/bash
# Download Inter and Fira Code fonts for Mumps Studio
# Phase 3B - Local font bundling (Google Fonts Source)

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
FONTS_DIR="$SCRIPT_DIR/../assets/fonts"
TEMP_DIR="/tmp/mumps-studio-fonts"

echo "📦 Downloading fonts for Mumps Studio..."
rm -rf "$TEMP_DIR"
mkdir -p "$FONTS_DIR"
mkdir -p "$TEMP_DIR"

# Inter Font (UI) - Google Fonts
echo "⬇️  Downloading Inter..."
curl -L "https://gwfh.mranftl.com/api/fonts/inter?download=zip&subsets=latin,latin-ext&variants=regular,500,600" -o "$TEMP_DIR/inter.zip" --connect-timeout 30 --max-time 300
unzip -o -q "$TEMP_DIR/inter.zip" -d "$TEMP_DIR/inter"

echo "📂 Content of inter download:"
ls -R "$TEMP_DIR/inter"

# The gwfh zip contains source files directly with version numbers (e.g. inter-v13-...)
# Use wildcards to match any version. Note: filename usually contains 'regular' or '400'
cp "$TEMP_DIR"/inter/inter-*-regular.ttf "$FONTS_DIR/Inter-Regular.ttf" || cp "$TEMP_DIR"/inter/inter-*-400.ttf "$FONTS_DIR/Inter-Regular.ttf"
cp "$TEMP_DIR"/inter/inter-*-500.ttf "$FONTS_DIR/Inter-Medium.ttf"
cp "$TEMP_DIR"/inter/inter-*-600.ttf "$FONTS_DIR/Inter-SemiBold.ttf"

# Fira Code
echo "⬇️  Downloading Fira Code..."
curl -L "https://gwfh.mranftl.com/api/fonts/fira-code?download=zip&subsets=latin,latin-ext&variants=regular,500" -o "$TEMP_DIR/firacode.zip" --connect-timeout 30 --max-time 300
unzip -o -q "$TEMP_DIR/firacode.zip" -d "$TEMP_DIR/firacode"

echo "📂 Content of firacode download:"
ls -R "$TEMP_DIR/firacode"

cp "$TEMP_DIR"/firacode/fira-code-*-regular.ttf "$FONTS_DIR/FiraCode-Regular.ttf" || cp "$TEMP_DIR"/firacode/fira-code-*-400.ttf "$FONTS_DIR/FiraCode-Regular.ttf"
cp "$TEMP_DIR"/firacode/fira-code-*-500.ttf "$FONTS_DIR/FiraCode-Medium.ttf"

# Convert to WOFF2 (requires woff2_compress tool)
echo "🔄 Converting to WOFF2..."
if command -v woff2_compress &> /dev/null; then
    for font in "$FONTS_DIR"/*.ttf; do
        [ -f "$font" ] || continue
        echo "  Converting $(basename "$font")..."
        woff2_compress "$font"
    done
    # Remove original TTF files
    rm -f "$FONTS_DIR"/*.ttf
    echo "✅ Fonts converted to WOFF2"
else
    echo "⚠️  woff2_compress not found - keeping TTF files"
    echo "   Using TTF files directly. Modern browsers support them."
fi

# Cleanup
rm -rf "$TEMP_DIR"

echo "✅ Fonts downloaded successfully!"
ls -lh "$FONTS_DIR"
