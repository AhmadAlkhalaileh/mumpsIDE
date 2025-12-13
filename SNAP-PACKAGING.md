# Ubuntu Snap Packaging Guide

## Overview

This guide explains how to package and publish MUMPS IDE as a Ubuntu Snap package.

## Prerequisites

Install snapcraft:
```bash
sudo snap install snapcraft --classic
```

Install LXD (for building in a clean container):
```bash
sudo snap install lxd
sudo lxd init --auto
```

## Project Structure

Your project should have:
```
mumps-ide/
├── snap/
│   └── snapcraft.yaml
├── main.js
├── package.json
├── index.html
├── styles/
├── src/
├── assets/
└── ... (all other files)
```

## Build the Snap

### 1. Prepare Icon

Create a 512x512 icon for your app:
```bash
# Place your icon.png in the project root
# OR create a simple one:
convert -size 512x512 xc:transparent \
  -fill '#667eea' -draw 'roundrectangle 50,50 462,462 60,60' \
  -fill white -font "DejaVu-Sans-Bold" -pointsize 280 \
  -gravity center -annotate +0+0 'M' \
  icon.png
```

### 2. Update package.json

Ensure your `package.json` has proper metadata:
```json
{
  "name": "mumps-ide",
  "version": "1.0.0",
  "description": "MUMPS Language IDE - Professional development environment",
  "main": "main.js",
  "author": "Ahmad Alkhalaileh",
  "license": "MIT"
}
```

### 3. Build the Snap

```bash
# Clean previous builds
snapcraft clean

# Build the snap (this will take several minutes)
snapcraft

# OR build in a container (recommended for cleaner builds)
snapcraft --use-lxd
```

This creates `mumps-ide_1.0_amd64.snap`

### 4. Test Locally

```bash
# Install locally in devmode
sudo snap install mumps-ide_1.0_amd64.snap --devmode --dangerous

# Run the app
snap run mumps-ide

# OR just:
mumps-ide

# Check logs
snap logs mumps-ide

# Uninstall
sudo snap remove mumps-ide
```

## Publish to Snap Store

### 1. Create Developer Account

1. Go to https://snapcraft.io/account
2. Sign up with Ubuntu One account
3. Register as a developer

### 2. Register Snap Name

```bash
# Login to Snap Store
snapcraft login

# Register the name (one-time only)
snapcraft register mumps-ide
```

### 3. Upload and Release

```bash
# Upload to store
snapcraft upload mumps-ide_1.0_amd64.snap

# Release to stable channel
snapcraft release mumps-ide <revision> stable
```

### 4. Update Listing

1. Go to https://snapcraft.io/mumps-ide/listing
2. Add:
   - Screenshots
   - Description
   - Contact information
   - Website URL
   - Source code repository

## Channels

Snaps use channels for release management:

- **stable**: Production releases
- **candidate**: Pre-release testing
- **beta**: Beta testing
- **edge**: Development builds

Release to specific channel:
```bash
snapcraft release mumps-ide <revision> beta
snapcraft release mumps-ide <revision> stable
```

## Auto-build with GitHub Actions

Create `.github/workflows/snap.yml`:

```yaml
name: Build Snap

on:
  push:
    tags:
      - 'v*'

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Build snap
        uses: snapcore/action-build@v1
        id: build

      - name: Upload snap
        uses: snapcore/action-publish@v1
        env:
          SNAPCRAFT_STORE_CREDENTIALS: ${{ secrets.STORE_LOGIN }}
        with:
          snap: ${{ steps.build.outputs.snap }}
          release: stable
```

## Troubleshooting

### Permission Issues

If users report permission errors:

1. Check plugs in `snapcraft.yaml`
2. Grant additional permissions:
```bash
sudo snap connect mumps-ide:docker
sudo snap connect mumps-ide:ssh-keys
```

### Electron Not Starting

If Electron fails to start:
```bash
# Add to launcher script in snapcraft.yaml:
--disable-gpu
--no-sandbox
--disable-software-rasterizer
```

### Docker Access

For Docker support:
```bash
# User must be in docker group
sudo usermod -aG docker $USER

# Connect docker plug
sudo snap connect mumps-ide:docker
```

## Update Process

### Release New Version

1. Update version in `snap/snapcraft.yaml` and `package.json`
2. Commit changes
3. Build new snap:
```bash
snapcraft clean
snapcraft --use-lxd
```
4. Upload and release:
```bash
snapcraft upload mumps-ide_1.1_amd64.snap
snapcraft release mumps-ide <revision> stable
```

### Users Auto-update

Users automatically receive updates:
```bash
# Force update check
sudo snap refresh mumps-ide

# Check current version
snap info mumps-ide
```

## Distribution

### Users Install

```bash
# From Snap Store
sudo snap install mumps-ide

# Or search in Software Center
# Search for "MUMPS IDE" or "mumps-ide"
```

### System Requirements

- Ubuntu 18.04 or later
- Snapd installed (usually pre-installed)
- 500MB disk space
- 2GB RAM recommended

## Files Included in Snap

The snap includes:
- All JavaScript/CSS/HTML files
- Node modules
- Electron runtime
- Assets (icons, fonts, etc.)
- Documentation

Files NOT included:
- Development dependencies
- Source maps
- Test files
- `.git` directory

## Security

Snaps run in confinement with specific permissions:

**Strict confinement** (recommended for store):
- Limited access to system
- Requires interface connections
- More secure

**Classic confinement** (avoid if possible):
- Full system access
- Not allowed in store without special permission

## Marketing

Once published, promote your snap:

1. **Snap Store listing**
   - Professional screenshots
   - Detailed description
   - Regular updates

2. **Social media**
   - Announce on Twitter, LinkedIn
   - Share in MUMPS communities

3. **Documentation**
   - Link to snap store from README
   - Installation instructions

## Example Installation Commands

For your README:

```markdown
## Installation

### Ubuntu/Debian (Snap)

```bash
sudo snap install mumps-ide
```

### Run

```bash
mumps-ide
```

Or search for "MUMPS IDE" in your application menu.
```

## Support

Help users with snap-specific issues:

```bash
# View logs
snap logs mumps-ide -n=50

# Check connections
snap connections mumps-ide

# Restart service
snap restart mumps-ide
```

## Resources

- Snap documentation: https://snapcraft.io/docs
- Forum: https://forum.snapcraft.io
- Ubuntu Snap Store: https://snapcraft.io/store
- Electron snap guide: https://www.electron.build/configuration/snap

---

**Created by Ahmad Alkhalaileh**
**Version 1.0 - December 2025**
