# Mumps Studio

Professional IDE for MUMPS/M programming language with Docker and SSH support.

Version: **1.1**

---

## 📚 Quick Links

### For Users
- **[After Installation Guide](docs/AFTER-INSTALL.md)** - Setup Docker & get started
- **[User Install Guide](docs/USER-INSTALL-GUIDE.md)** - Installing from Snap Store

### For Developers
- **[Complete Build, Test & Upload Guide](docs/BUILD-TEST-UPLOAD.md)** - Everything you need
- **[Snap Store Publishing Guide](docs/SNAP-STORE-GUIDE.md)** - Upload & release process
- **[Docker Setup Details](docs/DOCKER-SETUP.md)** - Docker configuration

---

## ✨ Features

✓ Monaco-based code editor with MUMPS syntax highlighting
✓ Run and debug MUMPS code with YottaDB integration
✓ Docker connectivity - connect to any container
✓ SSH remote server support with **automatic YottaDB detection**
✓ Integrated terminal with xterm.js
✓ Git integration for version control
✓ Project management and file organization
✓ Search and replace across files
✓ Extension system for customization

---

## 🚀 Quick Start for Users

### Install from Snap Store
```bash
sudo snap install mumps-studio
```

### Enable Docker Features (Optional)
```bash
sudo snap connect mumps-studio:docker-support
sudo usermod -aG docker $USER
# Log out and log back in
```

### Launch
```bash
mumps-studio
```

**See [AFTER-INSTALL.md](docs/AFTER-INSTALL.md) for complete setup instructions.**

---

## 🛠️ Quick Start for Developers

### Build the Snap

```bash
# Install dependencies
npm install --include=dev

# Rebuild node-pty for Electron (required for interactive Terminal)
npm run rebuild:pty

# Build snap
./build-snap.sh
```

### Test Locally

```bash
sudo snap install --dangerous mumps-studio_1_amd64.snap
./connect-docker.sh
mumps-studio
```

### Upload to Snap Store

```bash
snapcraft login
snapcraft upload mumps-studio_1_amd64.snap --release=edge
snapcraft release mumps-studio 1 stable
```

**See [BUILD-TEST-UPLOAD.md](docs/BUILD-TEST-UPLOAD.md) for detailed instructions.**

---

## For End Users (Installing)

### From Snap Store (After Publication)

```bash
sudo snap install mumps-studio
```

### Setup

```bash
# Connect interfaces (if not auto-connected)
sudo snap connect mumps-studio:docker
sudo snap connect mumps-studio:ssh-keys
sudo snap connect mumps-studio:ssh-public-keys
sudo snap connect mumps-studio:removable-media

# Add yourself to docker group
sudo usermod -aG docker $USER
newgrp docker
```

### Launch

```bash
mumps-studio
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
mumps-studio/
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

- GitHub: https://github.com/AhmadAlkhalaileh/mumpsIDE
- Email: ahmad@example.com

---

## Support

- **Documentation:** See `*.md` files in this directory
- **Issues:** GitHub Issues
- **Snap Store:** https://snapcraft.io/mumps-studio (after publication)

---

**Enjoy developing with Mumps Studio! 🚀**
