# Docker Setup for Mumps Studio

## Quick Setup (Recommended)

If you already have Docker installed on your system:

```bash
# Add yourself to the docker group
sudo usermod -aG docker $USER

# Apply the group change
newgrp docker

# Verify it works
docker ps

# Restart Mumps Studio
mumps-studio
```

## Detailed Options

### Option 1: System Docker (Recommended)

If Docker is installed via apt/dnf:

```bash
# Check if docker is installed
docker --version

# Add user to docker group
sudo usermod -aG docker $USER

# Log out and log back in (or use newgrp)
newgrp docker

# Test
docker ps
```

### Option 2: Docker Snap

If you want to use Docker as a snap:

```bash
# Install docker snap
sudo snap install docker

# Connect the interface
sudo snap connect mumps-studio:docker
```

### Option 3: No Docker Group Method

Run Mumps Studio with docker permissions temporarily:

```bash
# This works but requires sudo each time
sudo -E mumps-studio
```

## Troubleshooting

### "permission denied" when connecting to Docker

**Error:** `sudo snap connect mumps-studio:docker` fails

**Solution:** This is normal if docker snap is not installed. Use Option 1 (system docker with group) instead.

### Docker works in terminal but not in Mumps Studio

**Solution:** Restart Mumps Studio after adding yourself to the docker group:

```bash
# Kill any running instance
pkill -f mumps-studio

# Start fresh
mumps-studio
```

### "Cannot connect to Docker daemon"

**Check if Docker is running:**

```bash
sudo systemctl status docker
```

**Start Docker:**

```bash
sudo systemctl start docker
sudo systemctl enable docker  # Start on boot
```

## Verify Docker Access in Mumps Studio

1. Open Mumps Studio
2. Click "Connections" in the status bar (bottom)
3. Click "Refresh Docker"
4. You should see your running containers

If you see an error message, follow the instructions shown in the IDE.
