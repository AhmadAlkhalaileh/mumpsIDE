# Ahmad IDE Documentation Index

Welcome to the complete documentation for Ahmad IDE.

---

## Documentation Files

### 📘 [README.md](README.md)
**Main Documentation** - Complete user guide covering all features

**Contents:**
- Overview and features
- Installation and setup
- User interface guide (with screenshot references)
- Core features walkthrough
- Technical documentation
- Troubleshooting

**Audience:** All users (beginners to advanced)

---

### 📗 [API.md](API.md)
**API Reference** - Complete API documentation

**Contents:**
- IPC API (Renderer → Main)
- Bridge API (Internal)
- Debug Protocol specification
- Event APIs
- Data structures
- Error handling patterns

**Audience:** Developers extending or integrating with Ahmad IDE

---

### 📕 [ARCHITECTURE.md](ARCHITECTURE.md)
**Architecture Documentation** - Technical deep dive

**Contents:**
- System overview
- Process model (Electron main/renderer)
- Component architecture
- Data flow diagrams
- MUMPS integration details
- Debugging architecture
- Security model
- Performance considerations
- Design decisions and rationale

**Audience:** Developers and contributors

---

### 📸 [SCREENSHOT_GUIDE.md](SCREENSHOT_GUIDE.md)
**Screenshot Capture Guide** - Instructions for documentation screenshots

**Contents:**
- 30 screenshot checklist
- Step-by-step capture instructions
- Tips for best screenshots
- Sample MUMPS code for demos
- Automated capture options

**Audience:** Documentation maintainers

---

## Quick Start

**New Users:**
1. Start with [README.md](README.md) - Overview section
2. Follow [Installation & Setup](README.md#installation--setup)
3. Read [User Interface Guide](README.md#user-interface-guide)
4. Try [Core Features](README.md#core-features)

**Developers:**
1. Read [ARCHITECTURE.md](ARCHITECTURE.md) - System Overview
2. Review [API.md](API.md) - IPC API
3. Check [ARCHITECTURE.md](ARCHITECTURE.md) - Security Model
4. See [README.md](README.md#development-guide)

**Contributors:**
1. Read [ARCHITECTURE.md](ARCHITECTURE.md) - Complete
2. Review [API.md](API.md) - Complete
3. Follow [README.md](README.md#development-guide)
4. Use [SCREENSHOT_GUIDE.md](SCREENSHOT_GUIDE.md) for documentation updates

---

## Documentation Statistics

| File | Lines | Words | Topics |
|------|-------|-------|--------|
| README.md | ~1,500 | ~15,000 | 10 main sections, 30 screenshots |
| API.md | ~1,000 | ~8,000 | 50+ API methods, protocols |
| ARCHITECTURE.md | ~800 | ~7,000 | 9 architecture topics |
| SCREENSHOT_GUIDE.md | ~500 | ~3,500 | 30 screenshot instructions |
| **Total** | **~3,800** | **~33,500** | **Comprehensive coverage** |

---

## Screenshots Directory

```
docs/screenshots/
├── 01-main-window.png           - Full IDE window
├── 02-menu-bar.png               - Menu bar
├── 03-toolbar.png                - Toolbar buttons
├── 04-project-panel.png          - Project explorer
├── 05-editor-main.png            - Monaco editor
├── 06-terminal-panel.png         - Terminal tabs
├── 07-debug-panel.png            - Debug panel
├── 08-debug-toolbar.png          - Debug controls
├── 09-git-panel.png              - Git tool window
├── 10-connections-panel.png      - Connections dialog
├── 11-services-panel.png         - Services panel
├── 12-settings-panel.png         - Settings dialog
├── 13-shortcuts-panel.png        - Keyboard shortcuts
├── 14-find-dialog.png            - Find/Replace
├── 15-search-everywhere.png      - Search everywhere
├── 16-run-execution.png          - Code execution
├── 17-linting-results.png        - Linting errors
├── 18-debug-session.png          - Active debugging
├── 19-debug-variables.png        - Variables tab
├── 20-debug-callstack.png        - Call stack
├── 21-debug-console.png          - Debug console
├── 22-git-workflow.png           - Git status
├── 23-git-commit.png             - Commit dialog
├── 24-git-branches.png           - Branch selector
├── 25-git-diff.png               - File diff
├── 26-docker-connection.png      - Docker setup
├── 27-ssh-connection.png         - SSH setup
├── 28-routine-list.png           - Routine tree
├── 29-routine-search.png         - Search results
└── 30-terminal.png               - Terminal output
```

**Note:** Screenshot files need to be captured using [SCREENSHOT_GUIDE.md](SCREENSHOT_GUIDE.md)

---

## Topics Covered

### User Features
- ✅ Installation and setup
- ✅ User interface overview
- ✅ Code editing (Monaco)
- ✅ Running MUMPS code
- ✅ Code linting
- ✅ Debugging (AHMDBG)
- ✅ Git integration
- ✅ Docker connections
- ✅ SSH connections
- ✅ Routine management
- ✅ Terminal integration
- ✅ Keyboard shortcuts
- ✅ Troubleshooting

### Developer Topics
- ✅ Electron architecture
- ✅ IPC communication
- ✅ Security model
- ✅ MUMPS parser/lexer
- ✅ Debug protocol
- ✅ Bridge module
- ✅ Performance optimizations
- ✅ Design decisions

### API Coverage
- ✅ 50+ IPC methods documented
- ✅ Debug protocol specification
- ✅ Data structure definitions
- ✅ Error handling patterns
- ✅ Code examples for all APIs

---

## Documentation Principles

### Completeness
Every feature, API method, and UI element is documented with:
- Purpose and description
- Parameters and return values
- Code examples
- Screenshots (references)

### Accuracy
- Code examples tested and verified
- API signatures match actual implementation
- Screenshots reflect current UI (when captured)

### Accessibility
- Clear table of contents
- Cross-references between documents
- Beginner-friendly explanations
- Advanced technical details available

### Maintainability
- Modular structure (separate files by topic)
- Markdown format (easy to edit)
- Version controlled (Git)
- Screenshot guide for updates

---

## Contributing to Documentation

### Reporting Issues
If you find errors or missing information:
1. Check if information exists in another document
2. Review the relevant section carefully
3. File an issue with specific details
4. Suggest improvements

### Updating Documentation
When updating docs:
1. Follow existing style and format
2. Update all relevant cross-references
3. Add new screenshots if UI changed
4. Update this INDEX.md if adding new files
5. Test all code examples
6. Check all links work

### Adding Screenshots
1. Follow [SCREENSHOT_GUIDE.md](SCREENSHOT_GUIDE.md)
2. Save as PNG in `docs/screenshots/`
3. Use descriptive filenames
4. Update references in documentation
5. Optimize file size

---

## External Resources

### Ahmad IDE
- **Repository:** (Add GitHub/GitLab URL)
- **Issues:** (Add issue tracker URL)
- **Releases:** (Add releases URL)

### Dependencies
- **Electron:** https://www.electronjs.org/docs
- **Monaco Editor:** https://microsoft.github.io/monaco-editor/
- **xterm.js:** https://xtermjs.org/
- **ssh2:** https://github.com/mscdex/ssh2

### MUMPS/M Language
- **YottaDB:** https://yottadb.com/
- **GT.M:** https://gitlab.com/YottaDB/DB/YDB
- **MUMPS Standard:** http://71.174.62.16/Demo/AnnoStd

### Related Projects
- **VSCode MUMPS:** https://marketplace.visualstudio.com/items?itemName=jewuma.mumps
- **mumps-debug:** https://github.com/RashedBaharemand/mumps-debug

---

## License

Ahmad IDE is licensed under the **ISC License**.

See `package.json` for details.

---

## Version History

| Version | Date | Documentation Changes |
|---------|------|----------------------|
| 1.0.0 | 2024-12-12 | Initial complete documentation |

---

## Contact & Support

**For Questions:**
- Review documentation thoroughly first
- Check [Troubleshooting](README.md#troubleshooting) section
- Search existing issues

**For Bugs:**
- Include steps to reproduce
- Attach screenshots if UI-related
- Include console logs
- Specify environment (OS, versions)

**For Feature Requests:**
- Describe use case clearly
- Explain expected behavior
- Suggest implementation if possible

---

**Last Updated:** December 12, 2024
**Documentation Version:** 1.0.0
