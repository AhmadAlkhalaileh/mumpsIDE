# Docker Configuration Guide

## Overview

Ahmad IDE supports two Docker modes:
1. **Universal Mode** - Works with any Docker container (file editing only)
2. **Configured Mode** - Full YottaDB/MUMPS support (editing + run/debug)

---

## Universal Mode (Default)

**Use this when:**
- You just want to edit files in any Docker container
- You don't need to run/debug MUMPS code
- Your container doesn't have YottaDB installed

**Configuration:**
1. Open **Connections** panel (satellite icon)
2. Click **Refresh Docker**
3. Select your container from the list
4. Click **Use Universal** or close the panel

**What happens:**
- Files are saved to `/workspace` by default
- No YottaDB paths are configured
- Run/Debug buttons won't work (you'll get a "YottaDB not configured" error)

---

## Configured Mode (YottaDB)

**Use this when:**
- You need to run/debug MUMPS code
- Your container has YottaDB installed
- You want full IDE functionality

### Step 1: Find Your YottaDB Paths

Connect to your container and find these paths:

```bash
# Find YottaDB installation
docker exec <container-id> bash -c "find /opt -name mumps 2>/dev/null | head -1"

# Common locations:
# - /opt/yottadb/current
# - /opt/fis-gtm/YDB136
# - /usr/local/lib/yottadb/r###

# Find your environment directories
docker exec <container-id> bash -c "ls -la /var/worldvista/prod/"
```

### Step 2: Configure in IDE

1. Open **Connections** panel
2. Fill in **Docker Configuration** section:

**Common Configuration #1: WorldVista Standard**
```
Environment key: h
YottaDB path: /opt/fis-gtm/YDB136
Global directory path: /var/worldvista/prod/h/globals/mumps.gld
Routines path: /var/worldvista/prod/h/localr
```

**Common Configuration #2: Custom YottaDB**
```
Environment key: (leave empty)
YottaDB path: /opt/yottadb/current
Global directory path: /tmp/mumps.gld
Routines path: /workspace
```

3. Click **"Save Docker Config"**
4. Select your container
5. Close the panel

### Step 3: Verify

Write some MUMPS code:
```mumps
WRITE "Hello from YottaDB!",!
SET X=42
WRITE "The answer is: ",X,!
```

Click **Run** (or press Ctrl+Enter). You should see output in the terminal.

---

## Configuration Reference

### Field Explanations

| Field | Description | Required | Example |
|-------|-------------|----------|---------|
| **Environment key** | Namespace/environment name | No | `h`, `cc`, `prod` |
| **YottaDB path** | Directory containing `mumps` executable | Yes* | `/opt/fis-gtm/YDB136` |
| **Global directory path** | Location of `.gld` file | No | `/var/worldvista/prod/h/globals/mumps.gld` |
| **Routines path** | Where `.m` files are stored | No | `/var/worldvista/prod/h/localr` |

\* Required for Run/Debug functionality

### Storage Location

Docker configuration is saved in browser localStorage:
- Key: `ahmadIDE:dockerConfig`
- Saved once, applies to all containers you select
- Persists across IDE restarts

### Container Selection

The last selected container ID is saved in:
- Key: `ahmadIDE:lastContainerId`
- Used when you click "Use Universal"

---

## Common Setups

### Setup 1: WorldVista Production

```javascript
{
  "envKey": "h",
  "ydbPath": "/opt/fis-gtm/YDB136",
  "gldPath": "/var/worldvista/prod/h/globals/mumps.gld",
  "routinesPath": "/var/worldvista/prod/h/localr"
}
```

**File locations:**
- Routines: `/var/worldvista/prod/h/localr/ROUTINE.m`
- Globals: Stored in YottaDB database

### Setup 2: Development/Universal

```javascript
{
  "ydbPath": "/opt/yottadb/current",
  "gldPath": "/tmp/mumps.gld",
  "routinesPath": "/workspace"
}
```

**File locations:**
- Routines: `/workspace/ROUTINE.m` or `/workspace/folder/ROUTINE.m`
- Globals: Stored in `/tmp/mumps.dat`

### Setup 3: File Editing Only (Universal Mode)

```javascript
{}
```
(No configuration - leave all fields empty)

**File locations:**
- Routines: `/workspace/ROUTINE.m`
- Run/Debug: Not available

---

## Troubleshooting

### Error: "No Docker container selected"

**Cause:** You clicked "Use Universal" before selecting a container.

**Fix:**
1. Open Connections panel
2. Click on a container from the list
3. Then click "Use Universal"

### Error: "YottaDB not configured"

**Cause:** You're in Universal Mode but trying to run code.

**Fix:** Configure YottaDB paths (see Configured Mode above)

### Error: "bash: /opt/fis-gtm/YDB136/mumps: No such file or directory"

**Cause:** Wrong YottaDB path for your container.

**Fix:**
1. Find correct path: `docker exec <container-id> which mumps`
2. Update YottaDB path in Docker Configuration
3. Click "Save Docker Config"
4. Reselect your container

### Error: "%YDB-E-ZLINKFILE, Error while zlinking TMP..."

**Cause:** Routines path not included in `gtmroutines` environment variable.

**Fix:**
1. Ensure "Routines path" is configured (e.g., `/workspace`)
2. Save configuration
3. Reconnect to container

### Files don't appear in project tree

**Cause:** Project tree cache not refreshed.

**Fix:**
1. Right-click on project root
2. Select "Refresh"

Or reconnect to container:
1. Open Connections panel
2. Click on your container again

---

## Advanced: Creating Folders

### In Universal Mode

1. Right-click in project tree → **New** → **Folder**
2. Enter path: `/workspace/myapp`
3. Folder is created in Docker container
4. Refresh project tree to see it

### In Configured Mode

1. Right-click in project tree → **New** → **Folder**
2. Enter path: `myapp` (relative to routines path)
3. Full path: `/var/worldvista/prod/h/localr/myapp`
4. Refresh to see it

### Saving Files to Folders

When creating a new routine, include folder in name:
- `myapp/HELLO` → saves to `/workspace/myapp/HELLO.m`
- `localr/TEST` → saves to configured localr folder

---

## Implementation Details

### How Universal Mode Works

1. No YottaDB paths configured (`ydbPath: null`)
2. Routines path defaults to `/workspace`
3. Files are saved using `mkdir -p` + `base64 -d`
4. Works with any Docker container that has `bash` and `base64`

### How Configured Mode Works

1. YottaDB paths configured
2. Environment variables set:
   ```bash
   export gtm_dist=/opt/fis-gtm/YDB136
   export gtmgbldir=/var/.../mumps.gld
   export gtmroutines='/path/to/routines ...'
   ```
3. Code executed with: `/opt/fis-gtm/YDB136/mumps -direct`

### Configuration Merging

When you select a container, config is merged:
```javascript
{
  containerId: "8c21cf79fb67",  // From container selection
  ydbPath: "/opt/fis-gtm/YDB136", // From saved config
  gldPath: "/var/.../mumps.gld",  // From saved config
  routinesPath: "/var/.../localr" // From saved config
}
```

---

## Migration Guide

### From Old IDE (Hard-coded Paths)

**Before:**
- IDE only worked with specific container
- Paths hard-coded to YottaDB/WorldVista

**After:**
1. Open Connections panel
2. Configure your paths once
3. Click "Save Docker Config"
4. Works with any container you select

### Switching Between Containers

**Container A (Production - WorldVista):**
```
YottaDB path: /opt/fis-gtm/YDB136
Routines: /var/worldvista/prod/h/localr
```

**Container B (Development - Simple):**
```
YottaDB path: /opt/yottadb/current
Routines: /workspace
```

**How to switch:**
1. Save different configs for each container
2. OR: Change config in Connections panel
3. Click "Save Docker Config"
4. Select container

---

## See Also

- [SSH Configuration](./SSH-CONFIGURATION.md)
- [Architecture](./ARCHITECTURE.md)
- [API Reference](./API.md)
