#!/usr/bin/env node
/**
 * Download Vista Routines from Docker Container
 * Can be run standalone or integrated into Ahmad IDE
 */

const { exec } = require('child_process');
const fs = require('fs');
const path = require('path');
const util = require('util');

const execAsync = util.promisify(exec);

class RoutineDownloader {
    constructor(containerId, outputDir) {
        this.containerId = containerId || '8c21cf79fb67';
        this.outputDir = outputDir || path.join(require('os').homedir(), 'Desktop', 'vista-routines');
    }

    async execCommand(command) {
        try {
            const dockerCmd = `docker exec ${this.containerId} ${command}`;
            const { stdout, stderr } = await execAsync(dockerCmd, {
                maxBuffer: 10 * 1024 * 1024
            });
            return { stdout, stderr, code: 0 };
        } catch (error) {
            return {
                stdout: error.stdout || '',
                stderr: error.stderr || error.message,
                code: error.code || 1
            };
        }
    }

    async discoverPaths() {
        console.log('🔍 Discovering Vista paths...');

        const searchCmd = `sh -c 'find /var/worldvista -maxdepth 3 -type d \\( -name "localr" -o -name "routines" \\) 2>/dev/null | head -20'`;
        const result = await this.execCommand(searchCmd);

        if (result.code !== 0 || !result.stdout) {
            throw new Error('Could not find Vista directories');
        }

        const foundPaths = result.stdout.split('\n').filter(p => p.trim());
        console.log('Found paths:', foundPaths);

        const localrPath = foundPaths.find(p => p.includes('localr'));
        const routinesPath = foundPaths.find(p => p.includes('routines'));

        return { localrPath, routinesPath };
    }

    async listFiles(dirPath) {
        const listCmd = `ls "${dirPath}"/*.m 2>/dev/null || true`;
        const result = await this.execCommand(listCmd);

        if (!result.stdout || result.stdout.trim() === '') {
            return [];
        }

        return result.stdout.split('\n').filter(f => f.trim() && f.endsWith('.m'));
    }

    async downloadDirectory(containerPath, localPath) {
        console.log(`📥 Downloading from ${containerPath}...`);

        // Create local directory
        fs.mkdirSync(localPath, { recursive: true });

        // Use docker cp to copy entire directory
        const cpCmd = `docker cp ${this.containerId}:${containerPath}/. ${localPath}/`;
        await execAsync(cpCmd);

        // Count .m files
        const files = fs.readdirSync(localPath).filter(f => f.endsWith('.m'));
        console.log(`✓ Downloaded ${files.length} .m files`);

        return files.length;
    }

    async initGitRepo(stats) {
        const gitDir = path.join(this.outputDir, '.git');
        if (fs.existsSync(gitDir)) {
            console.log('⚠ Git repository already exists, skipping...');
            return;
        }

        console.log('📦 Initializing Git repository...');

        const gitCommands = [
            'git init',
            'git config user.name "Ahmad IDE"',
            'git config user.email "ahmad-ide@local"'
        ];

        for (const cmd of gitCommands) {
            await execAsync(cmd, { cwd: this.outputDir });
        }

        // Create .gitignore
        const gitignore = `# Editor files
*.swp
*.swo
*~
.DS_Store

# Logs
*.log
`;
        fs.writeFileSync(path.join(this.outputDir, '.gitignore'), gitignore);

        // Create README
        const readme = `# Vista Routines

Downloaded from Docker container: \`${this.containerId}\`

## Structure

- \`localr/\` - Modified routines (after Vista upgrade)
- \`routines/\` - Original routines (before Vista upgrade)

## Statistics

- localr: ${stats.localr} files
- routines: ${stats.routines} files

Downloaded on: ${new Date().toISOString()}

## Usage

This repository tracks Vista MUMPS routines for version control and patch management.

### Viewing Changes

\`\`\`bash
# See differences between localr and routines
diff -ur routines/ localr/
\`\`\`

### Updating from Container

\`\`\`bash
# Re-download routines
node ../scripts/download-routines.js ${this.containerId} .
\`\`\`
`;
        fs.writeFileSync(path.join(this.outputDir, 'README.md'), readme);

        // Initial commit
        await execAsync('git add .', { cwd: this.outputDir });
        await execAsync(`git commit -m "Initial commit: Download Vista routines from container ${this.containerId}

Downloaded ${stats.localr} files from localr
Downloaded ${stats.routines} files from routines

Paths:
- localr: ${stats.localrPath}
- routines: ${stats.routinesPath}
"`, { cwd: this.outputDir });

        console.log('✓ Git repository initialized');
        console.log('\nNext steps:');
        console.log(`1. cd ${this.outputDir}`);
        console.log('2. git remote add origin <your-gitlab-url>');
        console.log('3. git push -u origin main');
    }

    async download() {
        console.log('======================================');
        console.log('Vista Routines Downloader');
        console.log('======================================');
        console.log('Container ID:', this.containerId);
        console.log('Output Directory:', this.outputDir);
        console.log('');

        try {
            // Check container is running
            const psCmd = `docker ps --format '{{.ID}}' | grep ^${this.containerId}`;
            const { stdout } = await execAsync(psCmd);

            if (!stdout || !stdout.trim()) {
                throw new Error(`Container ${this.containerId} is not running!`);
            }

            console.log('✓ Container is running');
            console.log('');

            // Discover paths
            const { localrPath, routinesPath } = await this.discoverPaths();

            if (!localrPath && !routinesPath) {
                throw new Error('Could not find any Vista directories!');
            }

            console.log('');

            const stats = {
                localr: 0,
                routines: 0,
                localrPath,
                routinesPath
            };

            // Download localr
            if (localrPath) {
                const localPath = path.join(this.outputDir, 'localr');
                stats.localr = await this.downloadDirectory(localrPath, localPath);
            } else {
                console.log('⚠ No localr directory found');
            }

            console.log('');

            // Download routines
            if (routinesPath) {
                const localPath = path.join(this.outputDir, 'routines');
                stats.routines = await this.downloadDirectory(routinesPath, localPath);
            } else {
                console.log('⚠ No routines directory found');
            }

            console.log('');
            console.log('======================================');
            console.log('Download Complete!');
            console.log('======================================');
            console.log('Location:', path.resolve(this.outputDir));
            console.log('• localr:', stats.localr, 'files');
            console.log('• routines:', stats.routines, 'files');
            console.log('');

            // Initialize git repo
            await this.initGitRepo(stats);

            console.log('');
            console.log('✅ Done!');

        } catch (error) {
            console.error('');
            console.error('❌ Error:', error.message);
            process.exit(1);
        }
    }
}

// CLI usage
if (require.main === module) {
    const args = process.argv.slice(2);
    const containerId = args[0] || process.env.DOCKER_CONTAINER_ID || '8c21cf79fb67';
    const outputDir = args[1] || path.join(require('os').homedir(), 'Desktop', 'vista-routines');

    const downloader = new RoutineDownloader(containerId, outputDir);
    downloader.download().catch(error => {
        console.error('Fatal error:', error);
        process.exit(1);
    });
}

module.exports = RoutineDownloader;
