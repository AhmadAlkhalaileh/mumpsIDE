#!/bin/bash
# Script to create the MUMPS IDE icon using ImageMagick
# Install ImageMagick if needed: sudo apt install imagemagick

# Create 512x512 icon with gradient background and "M" logo
convert -size 512x512 xc:transparent \
  -draw "roundrectangle 0,0 512,512 80,80" \
  -fill 'url(#gradient)' \
  -define gradient:angle=135 \
  -define gradient:vector='#667eea,#764ba2' \
  -draw "roundrectangle 0,0 512,512 80,80" \
  -fill white \
  -font "DejaVu-Sans-Bold" \
  -pointsize 340 \
  -gravity center \
  -annotate +0-10 'M' \
  icon.png

echo "Icon created: icon.png"
echo "You can now use this for:"
echo "- Snap packaging (snap/gui/icon.png)"
echo "- App icon"
echo "- Website/README"
