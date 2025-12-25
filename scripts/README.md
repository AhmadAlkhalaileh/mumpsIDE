# Vista Routines Download Scripts

Download MUMPS routines from your Docker container to your local machine for Git version control.

## Quick Start (Bash Script - Recommended)

```bash
# Download routines to Desktop (default: ~/Desktop/vista-routines)
./scripts/download-routines.sh

# Or specify container ID and output directory
./scripts/download-routines.sh 8c21cf79fb67 ~/Desktop/my-vista-routines
```

## Advanced (Node.js Script)

```bash
# Download routines to Desktop using Node.js (default: ~/Desktop/vista-routines)
node scripts/download-routines.js

# Or specify container ID and output directory
node scripts/download-routines.js 8c21cf79fb67 ~/Desktop/my-vista-routines
```

## What It Does

1. **Auto-discovers** Vista paths in your container
   - Finds `localr/` (modified routines after patches)
   - Finds `routines/` (original routines before patches)

2. **Downloads** all `.m` files to your laptop
   - Creates `./vista-routines/localr/`
   - Creates `./vista-routines/routines/`

3. **Initializes Git repository** (optional)
   - Creates `.gitignore`
   - Creates `README.md` with statistics
   - Makes initial commit

## Usage Examples

### Download from specific container

```bash
./scripts/download-routines.sh abc123def456 ~/Desktop/my-routines
```

### Download without Git init

Just press 'n' when prompted:
```
Initialize Git repository? (y/n)
n
```

### Re-download to update existing directory

```bash
# This will overwrite existing files
./scripts/download-routines.sh 8c21cf79fb67 ~/Desktop/vista-routines
```

## Output Structure

```
vista-routines/
├── .git/                 # Git repository (if initialized)
├── .gitignore           # Ignores editor files
├── README.md            # Auto-generated with stats
├── localr/              # Modified routines
│   ├── XWBAPIC.m
│   ├── XWBLIB.m
│   └── ...
└── routines/            # Original routines
    ├── XWBAPIC.m
    ├── XWBLIB.m
    └── ...
```

## Next Steps After Download

### 1. Push to GitLab

```bash
cd ~/Desktop/vista-routines

# Add your GitLab remote
git remote add origin https://your-gitlab-server.com/your-username/vista-routines.git

# Push to GitLab
git push -u origin main
```

### 2. Compare localr vs routines

See what changed after patches:

```bash
cd ~/Desktop/vista-routines

# See all differences
diff -ur routines/ localr/

# See differences for specific routine
diff routines/XWBAPIC.m localr/XWBAPIC.m
```

### 3. Track specific patch changes

After downloading, you can use the Patch Tracking panel in Ahmad IDE to:
- Upload KIDS files
- Scan for changes
- Correlate patches with modified routines
- Commit changes to Git with proper attribution

## Troubleshooting

### Container not found

```bash
# List running containers
docker ps

# Use the correct container ID
./scripts/download-routines.sh <container-id>
```

### No .m files found

The script auto-discovers paths. If it can't find files:

```bash
# Manually check what's in your container
docker exec 8c21cf79fb67 find /var/worldvista -name "*.m" -type f

# Then manually specify paths if needed (edit the script)
```

### Permission denied

```bash
# Make scripts executable
chmod +x scripts/download-routines.sh
chmod +x scripts/download-routines.js
```

## Integration with Patch Tracking

These scripts work together with the Patch Tracking panel:

1. **Download routines** → Creates local Git repo
2. **Upload KIDS file** in Patch Tracking panel → Parses patch metadata
3. **Scan environment** → Detects changes in Docker container
4. **Correlate** → Maps changes to patch routines
5. **Commit** → Creates Git commit with patch info
6. **Push** → Upload to GitLab with full history

This gives you complete traceability: **WHO changed WHAT and WHY (patch ID)**.
