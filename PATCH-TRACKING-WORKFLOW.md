# Patch Tracking Workflow - Complete Guide

## 🚨 Recent Fix (2025-12-25)

**Bug Fixed**: `scanSpecificRoutines()` was returning 0 files even though files existed in Docker.

**Root Cause**: Docker exec commands with `&&` operators were not working correctly because:
- `docker exec container cd /path && md5sum file.m`
- Only executes `cd` in container, `md5sum` runs on HOST!

**Solution**: Wrap commands in `sh -c`:
- `docker exec container sh -c 'cd /path && md5sum file.m'`
- Now the entire command chain runs inside the container

**Files Changed**: `src/services/patchTracking/dockerScanner.js` lines 194, 221

---

## ✅ Complete Workflow (Step by Step)

### Step 1: Verify Docker Container Has Routines

First, check if your Docker container actually has routine files:

```bash
# Check what's in your container
docker exec 8c21cf79fb67 find /var/worldvista -name "*.m" -type f | head -20

# This should show paths like:
# /var/worldvista/prod/SOME_ENV/localr/ROUTINE.m
# /var/worldvista/prod/SOME_ENV/routines/ROUTINE.m
```

**If you see NO results**: Your container doesn't have routine files! You need to:
- Install/configure Vista in the container
- OR use a different container that has Vista routines

**If you see results but different paths**: Note the actual paths!

### Step 2: Download Routines from Docker to Desktop (OPTIONAL)

If you want a local Git repository on your Desktop:

```bash
cd /home/ahmad/Desktop/Ahmad_IDE_2/scripts

# This will auto-discover the paths and download
./download-routines.sh 8c21cf79fb67 ~/Desktop/vista-routines
```

This creates:
```
~/Desktop/vista-routines/
├── .git/                 # Git repository
├── localr/              # Modified routines
│   ├── ROUTINE1.m
│   └── ...
└── routines/            # Original routines
    ├── ROUTINE1.m
    └── ...
```

### Step 3: Push to GitLab (Optional)

```bash
cd ~/Desktop/vista-routines

# Add your GitLab remote
git remote add origin https://gitlab.ehs.com.jo/Ahmad.AlKhalaileh/docker.git

# For SSL issues, use SSH instead:
git remote remove origin
git remote add origin git@gitlab.ehs.com.jo:Ahmad.AlKhalaileh/docker.git

# Push
git push -u origin master
```

### Step 4: Use Patch Tracking in Ahmad IDE

Now you can use the Patch Tracking panel:

1. **Set Git Repository Path**:
   - Input: `~/Desktop/vista-routines`
   - Click "Set Path"
   - Should see: ✓ `Repository set`

2. **Upload KIDS Patch**:
   - Click "1. Upload Patch"
   - Select your `.KID` file
   - Should see patch info with routine names

3. **Scan Docker Environment**:
   - Click "2. Scan Docker"
   - It uses your current connection (8c21cf79fb67)
   - **Scans ONLY the routines from the patch** (fast!)
   - Should see: `Modified: X routines`

4. **Correlate**:
   - Click "3. Correlate"
   - Matches patch routines to detected changes
   - Should see: `X% match`

5. **Approve & Commit**:
   - Click "4. Approve & Commit"
   - Reviews the changes
   - **Downloads changed files from Docker to Git repo**
   - Commits with patch metadata
   - Pushes to GitLab

## 🎯 How It Works

### Smart Targeted Scanning:
1. Upload patch → extracts routine names (e.g., 4 routines)
2. Scan Docker → finds **only those 4 specific routines** (not all 4000+!)
3. **Download changed files from Docker to Git repo**
4. Commit → files exist, commit succeeds!
5. Push to GitLab with full metadata

### Performance:
- ❌ **OLD**: Scan 4172 files = 4172 commands = 5+ minutes
- ✅ **NEW**: Scan 4 files = 2 commands = 2 seconds

## 🔍 Troubleshooting

### "No .m files found in localr"

**Problem**: Docker paths are wrong or empty

**Solution**:
```bash
# Find actual paths
docker exec 8c21cf79fb67 find /var/worldvista -name "localr" -type d
docker exec 8c21cf79fb67 find /var/worldvista -name "routines" -type d

# The IDE will auto-discover these paths, but you can verify manually
```

### "Modified Routines (0)"

**Problem**: Patch routines don't exist in container

**Solution**:
- Check the routine names in your patch
- Verify those routines exist in the container:
  ```bash
  docker exec 8c21cf79fb67 ls /path/to/localr/ROUTINE_NAME.m
  ```

### "Not a Git repository"

**Problem**: You haven't set the Git repository path

**Solution**:
1. Download routines to Desktop first (Step 2)
2. In Ahmad IDE Patch Tracking panel, set the repo path to `~/Desktop/vista-routines`

## 📋 Quick Checklist

- [ ] Docker container is running
- [ ] Container has Vista routines (verify with `find` command)
- [ ] (Optional) Downloaded routines to `~/Desktop/vista-routines`
- [ ] (Optional) Git repo exists (`~/Desktop/vista-routines/.git`)
- [ ] Set repo path in Ahmad IDE Patch Tracking panel
- [ ] Upload KIDS file
- [ ] Scan finds the routines
- [ ] Commit downloads files and pushes to GitLab

## 🚀 Expected Output

When everything works correctly:

```
[Patch Tracker] Scanning 4 routines from patch UJO*3.0*28: LRAPD1, LRSPRPTA, LRAPCUM, LRAPBR1
[Docker Scanner] Scanning 4 specific routines from patch
[Docker Scanner] Executing: docker exec 8c21cf79fb67 sh -c 'cd /var/worldvista/prod/hakeem/localr && md5sum LRAPD1.m LRSPRPTA.m LRAPCUM.m LRAPBR1.m 2>/dev/null || true'
[Docker Scanner] Found 4 in localr, 4 in routines
[Change Detector] Modified: 4 routines
[GitLab] Downloading 4 changed routines from Docker...
[GitLab] Copying: LRAPD1
[GitLab] Copying: LRSPRPTA
[GitLab] Copying: LRAPCUM
[GitLab] Copying: LRAPBR1
[GitLab] Download complete
[GitLab] Running: git add localr/*.m
[GitLab] Running: git commit
[GitLab] Running: git push
[Patch Tracker] Commit executed successfully
```

## 💡 Key Features

✅ **Auto-discovers** Vista paths in Docker
✅ **Scans only specific routines** from patch (100x faster)
✅ **Auto-downloads** changed files from Docker to Git repo
✅ **Commits with full metadata** (author, patch ID, issue links)
✅ **Pushes to GitLab** with proper attribution

You now have complete traceability: **WHO changed WHAT and WHY (patch ID)**!

## 🐛 Technical Notes

### Docker Exec Command Handling

**IMPORTANT**: When using `docker exec` with complex commands containing operators like `&&`, `||`, or pipes, you MUST wrap the entire command in `sh -c '...'`:

```bash
# ❌ WRONG - Only cd runs in container, md5sum runs on host!
docker exec container cd /path && md5sum file.m

# ✅ CORRECT - Entire command runs in container
docker exec container sh -c 'cd /path && md5sum file.m'
```

This applies to all complex shell operations:
- Command chaining: `cmd1 && cmd2`
- Pipes: `cmd1 | cmd2`
- Redirects: `cmd > file`
- Conditionals: `cmd || echo "failed"`

### Path Discovery

The system automatically discovers Vista paths using:
```bash
docker exec container sh -c 'find /var/worldvista -maxdepth 3 -type d \( -name "localr" -o -name "routines" \) 2>/dev/null | head -20'
```

This finds all possible `localr` and `routines` directories and groups them by base path.
