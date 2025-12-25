/**
 * KIDS Patch Parser
 * Parses Vista KIDS distribution files to extract patch metadata
 */

class KIDSParser {
    /**
     * Parse KIDS file content
     * @param {string} content - Raw KIDS file content
     * @returns {Object} Parsed patch metadata
     */
    parse(content) {
        if (!content || typeof content !== 'string') {
            throw new Error('Invalid KIDS file content');
        }

        const metadata = {
            patchId: null,
            package: null,
            packageCode: null,
            version: null,
            patchNumber: null,
            title: null,
            createdDate: null,
            author: null,
            description: [],
            gitlabIssues: [],
            routines: [],
            raw: content
        };

        try {
            // Extract patch ID (e.g., "UJO*3.0*28")
            const patchIdMatch = content.match(/\*\*KIDS\*\*:([A-Z]+\*[\d.]+\*\d+)\^/);
            if (patchIdMatch) {
                metadata.patchId = patchIdMatch[1];
                const parts = metadata.patchId.split('*');
                metadata.packageCode = parts[0];
                metadata.version = parts[1];
                metadata.patchNumber = parseInt(parts[2]);
            }

            // Extract package name
            const pkgMatch = content.match(/"BLD",\d+,0\)\n([^\^]+)\^/);
            if (pkgMatch) {
                metadata.package = pkgMatch[1];
            }

            // Extract title (first line after KIDS header)
            const titleMatch = content.match(/KIDS Distribution saved on[^\n]+\n([^\n]+)/);
            if (titleMatch) {
                metadata.title = titleMatch[1].trim();
            }

            // Extract creation date
            const dateMatch = content.match(/KIDS Distribution saved on ([A-Za-z]{3} \d+, \d{4}@\d+:\d+:\d+)/);
            if (dateMatch) {
                metadata.createdDate = this.parseKIDSDate(dateMatch[1]);
            }

            // Extract author from description
            const authorMatch = content.match(/Created (?:On|By) [^\n]+ By ([^\n]+)/i);
            if (authorMatch) {
                metadata.author = authorMatch[1].trim();
            }

            // Extract GitLab issue URLs
            const issueRegex = /https?:\/\/gitlab[^\s]+\/issues?\/(\d+)/gi;
            let issueMatch;
            while ((issueMatch = issueRegex.exec(content)) !== null) {
                const issueUrl = issueMatch[0];
                const issueNumber = issueMatch[1];
                if (!metadata.gitlabIssues.includes(issueUrl)) {
                    metadata.gitlabIssues.push({
                        url: issueUrl,
                        number: issueNumber
                    });
                }
            }

            // Extract routines from "NM" sections
            const routineRegex = /"BLD",\d+,"KRN",9\.8,"NM",\d+,0\)\n([A-Z0-9]+)\^\^/g;
            let routineMatch;
            while ((routineMatch = routineRegex.exec(content)) !== null) {
                const routineName = routineMatch[1];
                if (!metadata.routines.includes(routineName)) {
                    metadata.routines.push(routineName);
                }
            }

            // Extract description lines (between "BLD" description section)
            const descMatch = content.match(/"BLD",\d+,1,0\)\n\^\^\d+\^\d+[^\n]+\n([\s\S]*?)(?="BLD"|\n"QUES"|\n"PKG")/);
            if (descMatch) {
                const descLines = descMatch[1].split('\n');
                descLines.forEach(line => {
                    const lineMatch = line.match(/"BLD",\d+,1,\d+,0\)\n(.+)/);
                    if (lineMatch) {
                        const text = lineMatch[1].trim();
                        if (text && text !== ' ') {
                            metadata.description.push(text);
                        }
                    }
                });
            }

            console.log('[KIDS Parser] Parsed patch:', metadata.patchId);
            return metadata;

        } catch (error) {
            console.error('[KIDS Parser] Parse error:', error);
            throw new Error(`Failed to parse KIDS file: ${error.message}`);
        }
    }

    /**
     * Parse KIDS date format (e.g., "Jun 17, 2025@15:05:03")
     * @param {string} dateStr - KIDS date string
     * @returns {string} ISO date string
     */
    parseKIDSDate(dateStr) {
        try {
            const match = dateStr.match(/([A-Za-z]{3}) (\d+), (\d{4})@(\d+):(\d+):(\d+)/);
            if (!match) return null;

            const [, month, day, year, hour, minute, second] = match;
            const monthMap = {
                'Jan': '01', 'Feb': '02', 'Mar': '03', 'Apr': '04',
                'May': '05', 'Jun': '06', 'Jul': '07', 'Aug': '08',
                'Sep': '09', 'Oct': '10', 'Nov': '11', 'Dec': '12'
            };

            const isoDate = `${year}-${monthMap[month]}-${day.padStart(2, '0')}T${hour.padStart(2, '0')}:${minute}:${second}`;
            return isoDate;
        } catch (error) {
            console.warn('[KIDS Parser] Date parse error:', error);
            return null;
        }
    }

    /**
     * Validate KIDS file format
     * @param {string} content - File content
     * @returns {boolean} True if valid KIDS format
     */
    isValidKIDS(content) {
        if (!content) return false;
        return content.includes('KIDS Distribution') && content.includes('**KIDS**');
    }

    /**
     * Extract routine checksums from KIDS file
     * @param {string} content - KIDS file content
     * @returns {Object} Map of routine name to checksum
     */
    extractChecksums(content) {
        const checksums = {};
        const checksumRegex = /"BLD",\d+,"KRN",9\.8,"NM",\d+,0\)\n([A-Z0-9]+)\^\^0\^B(\d+)/g;

        let match;
        while ((match = checksumRegex.exec(content)) !== null) {
            const routineName = match[1];
            const checksum = match[2];
            checksums[routineName] = checksum;
        }

        return checksums;
    }
}

// Export singleton instance
module.exports = new KIDSParser();
