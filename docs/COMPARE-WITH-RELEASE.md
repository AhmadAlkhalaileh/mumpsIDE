# Compare with Release Feature

**Build #PS-253.28294.345 Compatible**

## Overview

The "Compare with Release" feature allows you to compare local MUMPS routine files with their counterparts on a remote release server via SSH. It provides a modern, style diff viewer with whitespace detection, problems panel, and intuitive navigation.

## Features

- ✓ SSH connection to release server
- ✓ Automatic routine discovery via `find` command
- ✓ Select up to 2 remote versions for comparison
- ✓ Modern split-view diff editor with Dracula theme
- ✓ Whitespace-aware comparison (spaces vs tabs, trailing spaces)
- ✓ Problems panel with clickable entries
- ✓ Minimap showing change density
- ✓ Keyboard shortcuts (Ctrl/Cmd+Alt+R)
- ✓ Secure password storage in OS keychain

## Configuration

### 1. Enable the Extension

1. Open **Extensions** panel
2. Find **"Compare with Release"**
3. Toggle **Enable** if not already enabled

### 2. Configure Release Connection

1. Go to **File → Connections**
2. Scroll to **"Release Connection"** section
3. Fill in SSH credentials:
   - **Host/IP**: Release server address (e.g., `192.168.1.100`)
   - **Port**: SSH port (default: `22`)
   - **Username**: SSH username
   - **Password**: SSH password (stored securely in system keychain)
4. Click **Test Connection** to verify
5. Click **Save**

### Security Note

Passwords are stored securely using your operating system's keychain:
- **macOS**: Keychain Access
- **Windows**: Credential Manager
- **Linux**: libsecret

## Usage

### Basic Comparison

1. Open a MUMPS routine file (`.m` extension)
2. Right-click in the editor
3. Select **"Compare with Release"**
4. The extension will:
   - SSH to the release server
   - Run `find / -name ROUTINENAME.m 2>/dev/null`
   - Present a dialog with found paths

5. Select up to **2 remote versions** to compare
6. Click **Select**

### Diff View

The compare panel opens with three sections:

#### Left: Local File
- Shows your local routine content
- Green highlights = additions
- Orange highlights = modifications

#### Right: Release File(s)
- Shows remote routine content
- Red highlights = deletions
- Orange highlights = modifications
- Use tabs if you selected 2 remote versions

#### Problems Panel (Right)
- Lists all changes with line numbers
- Icons indicate change type:
  - `+` Added line
  - `-` Removed line
  - `±` Modified line
- `WS` badge = whitespace-only change
- Click any entry to jump to that line

#### Minimap (Far Right)
- Visual overview of change locations
- Color-coded by change type
- Click to jump to region

### Toolbar Actions

- **← →** Navigate between changes (or press `Shift+F7` / `F7`)
- **Show Whitespace** Toggle whitespace visualization
- **Refresh** Re-fetch remote file
- **Copy Path** Copy remote file path to clipboard
- **Settings** Open connection settings

### Keyboard Shortcuts

- **Ctrl/Cmd+Alt+R**: Compare with Release
- **F7**: Next change
- **Shift+F7**: Previous change

## Troubleshooting

### "Release Connection not configured"

**Solution**: Configure the release connection in File → Connections

### "Routine not found on release server"

**Possible causes**:
- Routine doesn't exist on the release server
- Insufficient permissions to search directories
- Network/SSH connection issue

**Solution**:
- Verify the routine name matches exactly
- Check SSH connection is working (Test Connection)
- Ensure your SSH user has read permissions

### "SSH connection failed"

**Possible causes**:
- Wrong host/port
- Incorrect credentials
- Firewall blocking SSH
- Host unreachable

**Solution**:
- Verify host and port
- Check username and password
- Test connection using `ssh username@host` from terminal
- Contact your network administrator

### "Command timeout"

**Possible causes**:
- Server is slow
- Too many files to search
- Network latency

**Solution**:
- Wait for the operation to complete
- The timeout is set to 30 seconds by default
- Consider optimizing the find command to search specific directories

## Advanced

### Cache Behavior

Find results are cached for **5 minutes** to improve performance. To force a refresh:
1. Click the **Refresh** button in the compare toolbar
2. Or wait 5 minutes and trigger the comparison again

### Customization

Edit these files to customize behavior:
- `src/extensions/bundled/compare-with-release.js` - Main logic
- `styles/compare-with-release.css` - UI styling

### Disabling the Feature

1. Go to **Extensions** panel
2. Find **"Compare with Release"**
3. Toggle **Disable**

This will:
- Remove the context menu entry
- Hide the compare panel
- Stop all background tasks

## Technical Details

### SSH Command

When comparing `ORWPCE1.m`, the extension runs:

```bash
find / -name "ORWPCE1.m" 2>/dev/null | head -n 50
```

This searches the entire filesystem and returns up to 50 matches.

### File Retrieval

Once you select remote path(s), the extension fetches content via:

```bash
cat "/path/to/ORWPCE1.m"
```

### Diff Algorithm

The extension uses a line-by-line comparison algorithm that:
1. Splits both files into lines
2. Compares each line pair
3. Detects:
   - Added lines (exists in local, not in remote)
   - Removed lines (exists in remote, not in local)
   - Modified lines (different content)
   - Whitespace-only changes (same when trimmed)

### Performance

- **Non-blocking**: All SSH operations run asynchronously
- **Cancellable**: You can cancel long-running operations
- **Cached**: Find results cached for 5 minutes
- **Timeout**: Commands timeout after 30 seconds

## FAQ

**Q: Can I compare with multiple release servers?**
A: Currently only one release connection is supported. You can change the connection settings at any time.

**Q: Can I compare non-MUMPS files?**
A: The feature is designed for `.m` files. The context menu only appears for MUMPS routine files.

**Q: Does it support SSH keys instead of passwords?**
A: Not in the current version. Password authentication only.

**Q: Can I export the diff as a patch file?**
A: This is planned for a future version.

## Support

For issues or feature requests:
1. Check the IDE logs (Help → Show Logs)
2. Look for `[Compare with Release]` entries
3. Report issues to your IDE administrator

## Version History

**v1.0.0** - Initial release
- SSH connection to release server
- Find and compare routine files
- Modern diff UI with  Dracula theme
- Problems panel and minimap
- Secure password storage
