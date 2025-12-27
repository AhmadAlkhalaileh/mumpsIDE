# Smart Rename (Safe Rename Tag/Label)

**Cross-routine tag/label renaming with safety detection and risk scoring**

## Overview

Smart Rename is an advanced refactoring feature for MUMPS that enables safe renaming of tags/labels across multiple routines. Unlike the standard F2 rename which only works within a single file, Smart Rename analyzes your entire workspace to find and update all references safely.

## Features

### ✅ Safe Cross-Routine Rename
- Renames tag definitions and all their references
- Works across multiple routines in your workspace
- Only modifies safe, explicitly detected patterns
- Prevents accidental changes to dynamic code

### 🎯 Pattern Detection
Smart Rename safely detects and renames:
- **DO TAG^ROUTINE** - Subroutine calls
- **GOTO TAG^ROUTINE** - Branch statements
- **$$TAG^ROUTINE(...)** - Extrinsic function calls
- **Local references** - DO TAG, G TAG, $$TAG within same routine

### ⚠️ Unsafe Pattern Protection
Will NOT modify (only shows as "uncertain"):
- **DO @variable** - Indirection
- **XECUTE** - Dynamic code execution
- **Computed entryrefs** - DO @X, $$@expression
- **String references** - TAG mentioned in string literals

### 📊 Risk Scoring (0-100)
Automatically computes a risk score based on:
- **Low (0-20)**: Single file or few safe changes
- **Medium (21-60)**: Multiple files or mixed patterns
- **High (61-100)**: Uncertain patterns or name collisions

Risk factors include:
- Number of files affected
- Number of cross-routine references
- Presence of unsafe/uncertain patterns
- Name collision with existing tags

### 🔍 Preview Before Apply
- See exactly what will change in each file
- Side-by-side before/after diff view
- Per-file change counts
- Risk assessment and warnings
- Checkboxes to control what gets applied:
  - ✓ Apply local changes (same routine)
  - ✓ Apply safe cross-routine changes
  - ☐ Include unsafe/uncertain changes (not recommended)

### 🔄 Transactional Apply
- All-or-nothing updates across multiple files
- Automatic rollback on any failure
- Preserves file encoding and line endings
- Updates open Monaco editor models

## Usage

### Triggering Smart Rename

1. **Context Menu**:
   - Place cursor on a tag/label definition (first word of a line)
   - Right-click → "Smart Rename Tag"

2. **Keyboard Shortcut**:
   - **Shift+Alt+F2** on a tag line

3. **Command Palette**:
   - Press **Ctrl+Shift+P** → Search "Smart Rename Tag"

### Rename Flow

1. **Position cursor** on tag/label you want to rename (must be at start of line)
2. **Trigger** Smart Rename command
3. **Enter new name** when prompted
4. **Review preview**:
   - Check risk score
   - Review changes in each file
   - Toggle options (local/cross-routine/unsafe)
5. **Apply** or **Cancel**

### Example

```mumps
PROCESS  ; Original tag name
    SET X=123
    DO VALIDATE
    QUIT

VALIDATE ; Helper tag
    ; ... code ...
    QUIT
```

Renaming `PROCESS` to `PROCESSDATA`:
- Updates the tag definition line
- Updates all `DO PROCESS` calls in same routine
- Finds `DO PROCESS^MYROUTINE` in other routines
- Shows preview with risk assessment
- Applies atomically across all files

## Architecture

### Module Structure

```
src/editor/mumps/
├── mumps-symbol-indexer.js        # Workspace symbol index
├── mumps-smart-rename.js          # Rename engine with risk scoring
├── mumps-smart-rename-dialog.js   # Multi-file preview UI
├── mumps-rename-transaction.js    # Atomic file updates
└── mumps-smart-rename-provider.js # Monaco integration
```

### How It Works

1. **Indexing** (`mumps-symbol-indexer.js`)
   - Builds from existing `CallIndexer` service
   - Indexes all tag definitions and references
   - Categorizes by confidence level (high/medium/low)

2. **Rename Computation** (`mumps-smart-rename.js`)
   - Detects tag under cursor
   - Checks for name collisions
   - Finds all safe references (local + cross-routine)
   - Computes risk score
   - Generates edit operations for Monaco

3. **Preview** (`mumps-smart-rename-dialog.js`)
   - Shows multi-file diff view
   - Displays risk score and factors
   - Provides toggles for local/cross-routine/unsafe changes
   - Can recompute on toggle change

4. **Apply** (`mumps-rename-transaction.js`)
   - Reads and backs up all affected files
   - Applies edits transactionally
   - Rolls back on any failure
   - Updates Monaco models for open files

### Integration Points

- **CallIndexer Service**: Provides routine/tag index
- **Monaco Editor**: Adds editor action (Shift+Alt+F2)
- **IPC Bridge**: Uses `window.ahmadIDE` for file operations
- **UI Primitives**: Uses Dialog, Checkbox, Button components
- **Theme System**: Styled via `dialogs.css` with CSS variables

## Safe Detection Rules

### ✅ Always Safe
```mumps
DO TAG^ROUTINE          ; Explicit call
DO TAG^ROUTINE(X,Y)     ; With args
GOTO TAG^ROUTINE        ; Branch
SET X=$$TAG^ROUTINE()   ; Extrinsic function
```

### ✅ Safe Within Same Routine
```mumps
DO TAG                  ; Local call
GOTO TAG                ; Local branch
SET X=$$TAG()           ; Local extrinsic
```

### ❌ Never Safe (Uncertain)
```mumps
DO @VAR                 ; Indirection
DO @("TAG^"_ROU)        ; Computed
XECUTE "DO TAG^ROU"     ; Dynamic code
SET MSG="Call TAG^ROU"  ; String mention
```

## Risk Scoring Algorithm

```javascript
score = 0

// Files affected
if (filesAffected === 1) score += 5
else if (filesAffected <= 3) score += 15
else if (filesAffected <= 10) score += 30
else score += 50

// Cross-routine references
score += min(20, floor(safeRefs / 5) * 5)

// Unsafe patterns
if (includeUnsafe) score += 40
else if (unsafeRefs > 0) score += 10

// Level classification
if (score <= 20) level = 'low'
else if (score <= 60) level = 'medium'
else level = 'high'
```

## Configuration

Currently uses global defaults. Future enhancements:
- Workspace-specific settings
- Custom risk thresholds
- Pattern whitelist/blacklist
- Auto-rename on low risk

## Testing

### Unit Tests (TODO)
```bash
npm test -- smart-rename
```

Test coverage:
- Safe pattern detection
- Unsafe pattern filtering
- Risk scoring accuracy
- Collision detection
- Transaction rollback

### Manual Testing Checklist
- [ ] Single-file rename (local only)
- [ ] Cross-routine rename (2-3 files)
- [ ] Large refactor (10+ files)
- [ ] Name collision detection
- [ ] Unsafe pattern filtering
- [ ] Transaction rollback on error
- [ ] Preview toggle behavior
- [ ] Monaco model updates

## Troubleshooting

### "Symbol indexer not available"
**Cause**: CallIndexer service not initialized
**Fix**: Wait for workspace to finish loading, or reload IDE

### "No references found"
**Cause**: Tag not used anywhere, or only in unsafe patterns
**Fix**: Use regular F2 rename for unused tags

### "Tag already exists"
**Cause**: Name collision with existing tag in same routine
**Fix**: Choose a different name, or manually remove conflicting tag first

### Changes not applied
**Cause**: File write error or permission issue
**Fix**: Check file permissions, ensure files are not read-only

### Performance with large workspaces
**Cause**: Indexing thousands of routines
**Fix**: Index is built lazily; will improve on subsequent renames

## Future Enhancements

- **Symbol index caching** - Persist index across sessions
- **Incremental updates** - Update index on file change
- **Preview improvements** - Monaco diff editor integration
- **Rename history** - Undo/redo cross-file renames
- **Find all references** - Standalone feature
- **Rename global variable** - Extend to ^GLOBAL support
- **Test coverage** - Comprehensive unit tests
- **Performance** - Web Worker for index building

## Technical Notes

### Why Not Use Monaco's Built-in Rename?
Monaco's rename provider (`provideRenameEdits`) is single-file only. For cross-file renames, we need:
- Workspace-wide symbol index
- Custom file I/O via IPC
- Risk assessment for MUMPS-specific patterns
- Transactional updates

### MUMPS-Specific Challenges
- **Case-insensitive** tag names
- **Indirection** makes static analysis incomplete
- **XECUTE** can generate code at runtime
- **No formal module system** - routines are global

### Design Decisions
1. **Conservative by default**: Only safe patterns auto-renamed
2. **Risk transparency**: Always show score and factors
3. **Preview mandatory**: No "rename without preview" option
4. **Atomic transactions**: All-or-nothing to prevent partial updates
5. **Extensible**: Modular design for future language features

## API Reference

### `createMumpsSmartRenameProvider({ deps })`
Main entry point, returns provider instance.

**Methods:**
- `initialize()` - Load modules, create instances
- `buildIndex()` - Build symbol index from CallIndexer
- `triggerSmartRenameTag()` - Execute rename flow
- `registerMonacoAction(editor)` - Add Shift+Alt+F2 command
- `getSymbolIndexer()` - Access underlying indexer

### `MumpsSymbolIndexer`
Workspace symbol index.

**Methods:**
- `buildFromCallIndexer(callIndexer)` - Populate from existing index
- `getTagDefinitions(tagName)` - Find all defs
- `getTagReferences(tagName, options)` - Find all refs
- `hasTagInRoutine(tagName, routineName)` - Collision check
- `clear()` - Reset index
- `getStats()` - Index statistics

### `createMumpsSmartRename({ deps })`
Rename computation engine.

**Methods:**
- `computeSmartRename({ model, oldTagName, newTagName, ...options })` - Returns rename plan with edits, risk, metadata

### `createMumpsSmartRenameDialog({ deps })`
Preview dialog UI.

**Methods:**
- `open(opts, token)` - Show dialog, returns Promise<UserChoice>

### `createMumpsRenameTransaction({ deps })`
Transactional file updater.

**Methods:**
- `applyTransaction(fileEdits, options)` - Apply edits atomically
- `previewTransaction(fileEdits)` - Dry-run preview

## Contributing

When contributing to Smart Rename:
1. Maintain modular structure (no god files)
2. Add tests for new pattern detection logic
3. Update risk scoring if needed
4. Document changes in this file
5. Test on real-world MUMPS codebases

## License

Part of Ahmad IDE. See root LICENSE.

---

**Version**: 1.0
**Author**: Ahmad Alkhalaileh
**Last Updated**: December 2025
