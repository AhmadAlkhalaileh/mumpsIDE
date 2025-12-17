# Complete Snap Build, Test & Upload Guide

This guide covers the complete process from building to uploading your snap to the Snap Store.

---

## Prerequisites

### 1. System Requirements
```bash
# Ubuntu 22.04 recommended (matches snap base: core22)
lsb_release -a
```

### 2. Install Required Tools
```bash
# Install snapcraft
sudo snap install snapcraft --classic

# Install LXD for isolated builds (REQUIRED)
sudo snap install lxd
sudo lxd init --minimal

# Add your user to lxd group
sudo usermod -aG lxd $USER

# Log out and log back in for group changes to take effect
# Verify with:
groups | grep lxd
```

### 3. Install Dependencies
```bash
cd /home/ahmad/Desktop/Ahmad_IDE_2

# Install all Node.js dependencies including Electron
npm install --include=dev

# Verify Electron is installed
ls -lh node_modules/electron/dist/electron
```

---

## Step 1: Build the Snap

### Clean Previous Builds
```bash
# Remove old build artifacts
sudo rm -rf parts prime stage .snapcraft
rm -f *.snap

# Clean snapcraft cache
snapcraft clean
```

### Build the Snap
```bash
# Use the build script (recommended)
./build-snap.sh

# OR build manually with LXD
snapcraft pack --use-lxd
```

**Build time:** 5-15 minutes depending on your system.

**Expected output:**
```
✓ Snap package created: mumps-studio_1_amd64.snap
File size: ~200MB

Next steps:
1. Install the snap:
   sudo snap install --dangerous mumps-studio_1_amd64.snap

2. Connect Docker (REQUIRED for Docker features):
   ./connect-docker.sh
   OR manually:
   sudo snap connect mumps-studio:docker-support
   sudo usermod -aG docker $USER

3. Run the app: mumps-studio
```

---

## Step 2: Test the Snap

### A. Install Test Snap
```bash
# Install in strict mode
sudo snap install --dangerous mumps-studio_1_amd64.snap

# Check installation
snap list | grep mumps-studio
```

### B. Connect Required Interfaces

#### Connect Docker Support
```bash
# Option 1: Use the automated script
./connect-docker.sh

# Option 2: Manual connection
sudo snap connect mumps-studio:docker-support
sudo usermod -aG docker $USER

# Log out and log back in for group changes
# Then verify Docker access:
docker ps  # Should work without sudo
```

#### Verify All Connections
```bash
snap connections mumps-studio
```

**Expected connections:**
```
Interface           Plug                        Slot
docker-support      mumps-studio:docker-support :docker-support
home                mumps-studio:home           :home
network             mumps-studio:network        :network
network-bind        mumps-studio:network-bind   :network-bind
ssh-keys            mumps-studio:ssh-keys       :ssh-keys
x11                 mumps-studio:x11            :x11
```

### C. Test All Features

#### 1. Launch the Application
```bash
mumps-studio
```
✅ **Check:** App window opens without errors

#### 2. Test Docker Connection
1. Click **Connections** panel
2. Select **Docker** tab
3. Click **Refresh Containers**
4. **Expected:** List of running Docker containers appears
5. Select a container and click **Connect**
6. **Expected:** Status shows "Connected to [container-name]"

✅ **Check:** Docker connection successful

#### 3. Test SSH Connection (with Auto-Detection)
1. Click **Connections** panel
2. Select **SSH** tab
3. Enter SSH credentials:
   - Host: your-ssh-server
   - Port: 22
   - Username: your-username
   - Password: your-password
4. Click **Connect SSH**
5. **Check terminal output:**
   ```
   [SSH] Detecting YottaDB path...
   [SSH] Found via find: /path/to/yottadb
   [SSH] Auto-detected YottaDB at: /path/to/yottadb
   ```
6. **Expected:** SSH connected successfully with auto-detected YottaDB path

✅ **Check:** SSH connection successful with YottaDB detected

#### 4. Test MUMPS Code Execution
```mumps
; Test code
SET X=5
SET Y=10
SET Z=X+Y
WRITE "Result: ",Z,!
```
1. Paste code into editor
2. Click **Run** or press F5
3. **Expected:** Output shows "Result: 15"
4. **Expected:** No error like "mumps: No such file or directory"

✅ **Check:** Code runs successfully

#### 5. Test Terminal
1. Open integrated terminal
2. Run commands:
   ```bash
   ls
   pwd
   docker ps  # Should work if Docker connected
   ```

✅ **Check:** Terminal works

#### 6. Test File Operations
1. Create new file: File → New
2. Save file
3. Open existing file

✅ **Check:** File operations work

### D. Check Logs for Errors
```bash
# View snap logs
snap logs mumps-studio

# Check for permission errors
journalctl -xe | grep mumps-studio
```

### E. Test Strict Confinement
```bash
# Verify snap is in strict mode
snap list mumps-studio
# Should show: confinement: strict
```

---

## Step 3: Upload to Snap Store

### A. Register Your Snap Name
```bash
# Log in to Snap Store account
snapcraft login

# Register the snap name (first time only)
snapcraft register mumps-studio
```

**Note:** If the name is taken, choose an alternative like:
- `mumps-studio-ide`
- `mumps-dev-studio`
- `yottadb-studio`

### B. Upload the Snap
```bash
# Upload to edge channel (for testing)
snapcraft upload mumps-studio_1_amd64.snap --release=edge

# Expected output:
# Revision 1 created for mumps-studio
# Released to edge channel
```

### C. Track Upload Progress
```bash
# Check upload status
snapcraft status mumps-studio
```

**Expected output:**
```
Track    Arch    Channel    Version    Revision
latest   amd64   stable     -          -
                 candidate  -          -
                 beta       -          -
                 edge       1          1
```

### D. Test from Snap Store (Edge Channel)
```bash
# Remove local snap
sudo snap remove mumps-studio

# Install from edge channel
sudo snap install mumps-studio --edge

# Run and test
mumps-studio
```

### E. Promote to Stable Channel
Once testing is complete:

```bash
# Promote from edge to stable
snapcraft release mumps-studio 1 stable

# Or release to specific channels:
snapcraft release mumps-studio 1 candidate  # For beta testers
snapcraft release mumps-studio 1 beta       # For wider testing
snapcraft release mumps-studio 1 stable     # For public release
```

### F. Verify Public Listing
```bash
# Check on Snap Store
https://snapcraft.io/mumps-studio

# Public can now install with:
sudo snap install mumps-studio
```

---

## Step 4: Post-Installation User Instructions

Users who install your snap need to connect Docker. You have **two options**:

### Option A: Include Setup Script in Snap (Recommended)

Update `snapcraft.yaml` to include the setup script:

```yaml
parts:
  mumps-studio:
    override-build: |
      # ... existing build steps ...

      # Include setup script
      cp "${SNAPCRAFT_PART_SRC}/connect-docker.sh" "${SNAPCRAFT_PART_INSTALL}/bin/"
      chmod +x "${SNAPCRAFT_PART_INSTALL}/bin/connect-docker.sh"
```

Users can then run:
```bash
sudo snap install mumps-studio
/snap/mumps-studio/current/bin/connect-docker.sh
mumps-studio
```

### Option B: Show Instructions After First Launch

Create a notification in the app that shows on first launch:

**In your app's first-run screen, show:**
```
╔═══════════════════════════════════════════════════════════╗
║        Welcome to Mumps Studio!                           ║
║                                                           ║
║  Docker features require additional setup:                ║
║                                                           ║
║  Run this command in terminal:                            ║
║    sudo snap connect mumps-studio:docker-support         ║
║    sudo usermod -aG docker $USER                          ║
║                                                           ║
║  Then log out and log back in.                           ║
║                                                           ║
║  SSH and other features work immediately!                 ║
╚═══════════════════════════════════════════════════════════╝
```

---

## Troubleshooting

### Build Issues

**Issue:** "node_modules not found or incomplete"
```bash
# Solution:
npm install --include=dev
```

**Issue:** "LXD is not installed"
```bash
# Solution:
sudo snap install lxd
sudo lxd init --minimal
sudo usermod -aG lxd $USER
# Log out and log back in
```

**Issue:** Build fails with permission errors
```bash
# Solution:
sudo rm -rf parts prime stage .snapcraft
snapcraft clean
./build-snap.sh
```

### Testing Issues

**Issue:** Docker not working
```bash
# Solution:
sudo snap connect mumps-studio:docker-support
sudo usermod -aG docker $USER
# Log out and log back in
docker ps  # Test
```

**Issue:** SSH can't find YottaDB
- Check the terminal output for detection logs
- YottaDB is auto-detected now, but if it fails:
  1. Manually check on SSH server: `which mumps`
  2. Or: `find /usr /opt -name mumps -type f`
  3. Manually configure path in Connection settings

**Issue:** "Permission denied" errors
```bash
# Check connections
snap connections mumps-studio

# Reconnect missing interfaces
sudo snap connect mumps-studio:home
sudo snap connect mumps-studio:ssh-keys
```

### Upload Issues

**Issue:** "You must log in first"
```bash
snapcraft login
```

**Issue:** "Name already registered"
```bash
# Choose a different name
snapcraft register mumps-studio-ide
# Update name in snapcraft.yaml
# Rebuild and upload
```

---

## Summary Checklist

- [ ] Prerequisites installed (snapcraft, lxd, npm dependencies)
- [ ] Snap built successfully
- [ ] Snap installed locally for testing
- [ ] Docker connection tested and working
- [ ] SSH connection tested with auto-detection
- [ ] MUMPS code execution tested
- [ ] All features tested and working
- [ ] Logged in to Snap Store
- [ ] Snap name registered
- [ ] Snap uploaded to edge channel
- [ ] Tested installation from edge channel
- [ ] Promoted to stable channel
- [ ] Public listing verified
- [ ] User documentation updated with Docker setup steps

---

## Next Steps After Upload

1. **Monitor Reviews:** Check Snap Store for user reviews
2. **Update Version:** When releasing updates, increment version in `snapcraft.yaml`
3. **Promote Through Channels:** edge → beta → candidate → stable
4. **Documentation:** Keep Snap Store description updated
5. **Support:** Respond to issues on GitHub

---

## Quick Reference

```bash
# Build
./build-snap.sh

# Install & Test
sudo snap install --dangerous mumps-studio_1_amd64.snap
./connect-docker.sh
mumps-studio

# Upload
snapcraft login
snapcraft upload mumps-studio_1_amd64.snap --release=edge

# Promote
snapcraft release mumps-studio 1 stable
```
