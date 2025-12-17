# Mumps Studio - Quick Setup Guide

Thank you for installing Mumps Studio! Follow these quick steps to get started.

---

## ⚡ Quick Start

### Step 1: Enable Docker Features (Optional)

If you want to use Docker features, run these commands:

```bash
# Connect Docker support
sudo snap connect mumps-studio:docker-support

# Add your user to docker group
sudo usermod -aG docker $USER
```

**Important:** Log out and log back in (or reboot) for changes to take effect.

#### Verify Docker Setup
```bash
docker ps
```
If this works without `sudo`, you're all set! ✅

---

### Step 2: Launch Mumps Studio

```bash
mumps-studio
```

Or find it in your applications menu: **Mumps Studio**

---

## 🎯 What Works Immediately

✅ **SSH Connections** - Connect to remote servers with automatic YottaDB detection
✅ **Code Editor** - Full MUMPS syntax highlighting and editing
✅ **Terminal** - Integrated terminal
✅ **File Management** - Browse and edit files

---

## 🐳 What Needs Docker Setup

The following features require Docker setup (Step 1 above):

- 🔗 Connect to Docker containers
- 🚀 Run MUMPS code in containers
- 📦 Container management

**Note:** SSH and local features work immediately without Docker setup!

---

## 🔑 SSH with Auto-Detection

Mumps Studio automatically detects YottaDB installations on SSH servers!

1. Go to **Connections** panel
2. Select **SSH** tab
3. Enter your SSH credentials
4. Click **Connect**

The IDE will automatically find and configure YottaDB - no manual path configuration needed! 🎉

---

## 📚 Features Overview

### Code Editor
- Monaco-based editor with MUMPS syntax highlighting
- IntelliSense and code completion
- Multi-file editing with tabs

### Connections
- **Docker:** Connect to any Docker container with MUMPS/YottaDB
- **SSH:** Connect to remote servers with auto-detection
- Run code directly in connected environments

### Debugging
- Line-by-line debugging
- Breakpoints support
- Variable inspection
- Call stack viewing

### Terminal
- Integrated xterm.js terminal
- Runs in connected environment (Docker/SSH)
- Full color support

### Git Integration
- Basic git operations
- Commit and push changes
- View repository status

---

## ❓ Troubleshooting

### Docker not working?

1. **Check if Docker is installed:**
   ```bash
   docker --version
   ```
   If not installed:
   ```bash
   sudo apt update
   sudo apt install docker.io
   sudo systemctl enable --now docker
   ```

2. **Connect snap interface:**
   ```bash
   sudo snap connect mumps-studio:docker-support
   ```

3. **Add user to docker group:**
   ```bash
   sudo usermod -aG docker $USER
   ```

4. **Log out and log back in**

5. **Verify:**
   ```bash
   docker ps
   ```

### SSH connection issues?

1. **Check SSH permissions:**
   ```bash
   chmod 600 ~/.ssh/id_rsa
   chmod 644 ~/.ssh/id_rsa.pub
   ```

2. **Connect ssh-keys interface:**
   ```bash
   sudo snap connect mumps-studio:ssh-keys
   ```

3. **Restart the IDE**

### YottaDB not detected on SSH?

The IDE automatically searches for YottaDB in common locations. If it can't find it:

1. Check where MUMPS is installed on your SSH server:
   ```bash
   ssh your-server "which mumps"
   ```

2. Or search for it:
   ```bash
   ssh your-server "find /usr /opt -name mumps -type f"
   ```

3. Manually configure the path in **Connection Settings** if needed

---

## 🆘 Getting Help

- **GitHub Issues:** https://github.com/AhmadAlkhalaileh/mumpsIDE/issues
- **Documentation:** See included docs folder
- **Logs:** `snap logs mumps-studio`

---

## 🎉 You're Ready!

Mumps Studio is now ready to use. Start coding! 🚀

```mumps
; Your first MUMPS program
SET NAME="World"
WRITE "Hello, ",NAME,!
```

Happy coding! 💻
