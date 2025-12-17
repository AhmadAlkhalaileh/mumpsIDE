# MUMPS Language IDE

Professional IDE for MUMPS/M programming language with Docker and SSH support.

Version: **1.3**

---

## Features

✓ Monaco-based code editor with MUMPS syntax highlighting
✓ Run and debug MUMPS code with YottaDB integration
✓ Docker connectivity - connect to any container
✓ SSH remote server support
✓ Integrated terminal with xterm.js
✓ Git integration for version control
✓ Project management and file organization
✓ Search and replace across files
✓ Extension system for customization

---

## For Developers (Building the Snap)

### Prerequisites

```bash
sudo snap install snapcraft --classic
npm install --include=dev
```

### Build Snap

```bash
./BUILD-SNAP.sh
```

Creates: `mumps-ide_1.3_amd64.snap`

### Test Locally

```bash
./INSTALL-LOCAL.sh
```

### Upload to Snap Store

See: **SNAP-STORE-GUIDE.md**

---

## For End Users (Installing)

### From Snap Store (After Publication)

```bash
sudo snap install mumps-ide
```

### Setup

```bash
# Connect interfaces (if not auto-connected)
sudo snap connect mumps-ide:docker
sudo snap connect mumps-ide:ssh-keys

# Add yourself to docker group
sudo usermod -aG docker $USER
newgrp docker
```

### Launch

```bash
mumps-ide
```

See: **USER-INSTALL-GUIDE.md** for complete instructions.

---

## What's Included in the Snap

- ✓ Electron-based IDE
- ✓ Monaco Editor
- ✓ xterm.js terminal
- ✓ Docker CLI (docker.io package)
- ✓ SSH client with sshpass
- ✓ Git integration
- ✓ All dependencies bundled

---

## System Requirements

- Ubuntu 22.04+ (or any snap-supporting Linux)
- 2GB RAM minimum, 4GB recommended
- 500MB disk space
- Docker (optional, for container features)
- SSH keys (optional, for remote features)

---

## Project Structure

```
mumps-ide/
├── BUILD-SNAP.sh              # Build production snap
├── INSTALL-LOCAL.sh           # Install for testing
├── SNAP-STORE-GUIDE.md        # How to publish to Snap Store
├── USER-INSTALL-GUIDE.md      # End user installation guide
├── index.html                 # Main UI
├── main.js                    # Electron main process
├── renderer.js                # Renderer process
├── bridge.js                  # MUMPS runtime bridge
├── preload.js                 # Electron preload
├── package.json               # Dependencies
├── snap/
│   └── snapcraft.yaml         # Snap configuration
├── src/                       # Source modules
├── styles/                    # CSS files
└── node_modules/              # Dependencies
```

---

## Fixes in v1.3

**Terminal:**
- ✓ Fixed xterm/Monaco AMD loader conflict
- ✓ Terminal loads properly in snap

**Docker:**
- ✓ Bundled docker.io package in snap
- ✓ Uses `$SNAP/usr/bin/docker`
- ✓ Works without host Docker installation

**SSH:**
- ✓ Bundled sshpass for password auth
- ✓ SSH connections work properly
- ✓ Remote code execution works

---

## Development

Built with:
- **Electron** - Cross-platform desktop framework
- **Monaco Editor** - VS Code's editor
- **xterm.js** - Terminal emulator
- **ssh2** - SSH client library
- **YottaDB** - MUMPS database (via Docker/SSH)

---

## License

Proprietary

---

## Author

**Ahmad Alkhalaileh**

- GitHub: https://github.com/ahmadalkhalaileh
- Email: ahmad@example.com

---

## Support

- **Documentation:** See `*.md` files in this directory
- **Issues:** GitHub Issues
- **Snap Store:** https://snapcraft.io/mumps-ide (after publication)

---

**Enjoy developing with MUMPS IDE! 🚀**
