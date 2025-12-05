// MUMPS Language Parser and Analyzer
// Provides syntax analysis, token parsing, and linting capabilities

const MUMPS_COMMANDS = [
  'BREAK', 'CLOSE', 'DO', 'ELSE', 'FOR', 'GOTO', 'HALT', 'HANG', 'IF', 'JOB',
  'KILL', 'LOCK', 'MERGE', 'NEW', 'OPEN', 'QUIT', 'READ', 'SET', 'TCOMMIT',
  'TRESTART', 'TROLLBACK', 'TSTART', 'USE', 'VIEW', 'WRITE', 'XECUTE',
  'ZBREAK', 'ZGOTO', 'ZHALT', 'ZKILL', 'ZLINK', 'ZMESSAGE', 'ZNSPACE',
  'ZPRINT', 'ZQUIT', 'ZSHOW', 'ZSTEP', 'ZSYSTEM', 'ZTCOMMIT', 'ZTRAP',
  'ZTSTART', 'ZWITHDRAW', 'ZWRITE'
];

const COMMAND_ABBREV = {
  'B': 'BREAK', 'C': 'CLOSE', 'D': 'DO', 'E': 'ELSE', 'F': 'FOR', 'G': 'GOTO',
  'H': 'HALT', 'I': 'IF', 'J': 'JOB', 'K': 'KILL', 'L': 'LOCK', 'M': 'MERGE',
  'N': 'NEW', 'O': 'OPEN', 'Q': 'QUIT', 'R': 'READ', 'S': 'SET', 'TC': 'TCOMMIT',
  'TRE': 'TRESTART', 'TRO': 'TROLLBACK', 'TS': 'TSTART', 'U': 'USE', 'V': 'VIEW',
  'W': 'WRITE', 'X': 'XECUTE', 'ZB': 'ZBREAK', 'ZG': 'ZGOTO', 'ZH': 'ZHALT',
  'ZK': 'ZKILL', 'ZL': 'ZLINK', 'ZM': 'ZMESSAGE', 'ZN': 'ZNSPACE', 'ZP': 'ZPRINT',
  'ZQ': 'ZQUIT', 'ZS': 'ZSHOW', 'ZST': 'ZSTEP', 'ZSYS': 'ZSYSTEM',
  'ZTC': 'ZTCOMMIT', 'ZTS': 'ZTSTART', 'ZWI': 'ZWITHDRAW', 'ZW': 'ZWRITE'
};

const INTRINSIC_FUNCTIONS = [
  '$ASCII', '$CHAR', '$DATA', '$EXTRACT', '$FIND', '$FNUMBER', '$GET',
  '$JUSTIFY', '$LENGTH', '$NAME', '$ORDER', '$PIECE', '$QLENGTH',
  '$QSUBSCRIPT', '$QUERY', '$RANDOM', '$REVERSE', '$SELECT', '$STACK',
  '$TEXT', '$TRANSLATE', '$VIEW', '$ZCONVERT', '$ZDATE', '$ZPARSE',
  '$ZPIECE', '$ZSEARCH', '$ZTRANSLATE', '$ZTRIGGER', '$ZWIDTH', '$ZWRITE'
];

class MumpsToken {
  constructor(type, value, line, column, length) {
    this.type = type;
    this.value = value;
    this.line = line;
    this.column = column;
    this.length = length || (value ? value.length : 0);
  }
}

class MumpsParser {
  constructor(code) {
    this.code = code || '';
    this.lines = this.code.split('\n');
    this.tokens = [];
    this.diagnostics = [];
    this.labels = new Map();  // label -> line number
    this.tags = new Map();     // tag -> {line, params}
  }

  parse() {
    this.tokens = [];
    this.diagnostics = [];
    this.labels.clear();
    this.tags.clear();

    for (let lineNum = 0; lineNum < this.lines.length; lineNum++) {
      const line = this.lines[lineNum];
      this.parseLine(line, lineNum + 1);
    }

    return {
      tokens: this.tokens,
      diagnostics: this.diagnostics,
      labels: this.labels,
      tags: this.tags
    };
  }

  parseLine(line, lineNum) {
    if (!line || !line.trim()) return;

    const trimmed = line.trim();

    // Skip comments
    if (trimmed.startsWith(';')) {
      this.tokens.push(new MumpsToken('comment', trimmed, lineNum, 0, line.length));
      return;
    }

    let column = 0;
    const leadingSpaces = line.length - line.trimStart().length;

    // Check for label/tag (starts at column 0, no leading space)
    if (leadingSpaces === 0 && /^[A-Za-z%][A-Za-z0-9]*/.test(line)) {
      const match = line.match(/^([A-Za-z%][A-Za-z0-9]*)(\([^)]*\))?/);
      if (match) {
        const labelName = match[1];
        const params = match[2] || '';

        this.labels.set(labelName, lineNum);

        if (params) {
          // This is a tag with parameters
          this.tags.set(labelName, {
            line: lineNum,
            params: params.slice(1, -1).split(',').map(p => p.trim()).filter(Boolean)
          });
        }

        this.tokens.push(new MumpsToken('label', labelName, lineNum, 0, labelName.length));
        column = match[0].length;

        // Parse the rest of the line after the label
        const rest = line.slice(column).trim();
        if (rest && !rest.startsWith(';')) {
          this.parseCommands(rest, lineNum, column + (line.slice(column).length - rest.length));
        }
        return;
      }
    }

    // Parse commands (indented lines or after label)
    const commandText = line.slice(leadingSpaces);
    if (commandText && !commandText.startsWith(';')) {
      this.parseCommands(commandText, lineNum, leadingSpaces);
    }
  }

  parseCommands(text, lineNum, startCol) {
    let pos = 0;

    while (pos < text.length) {
      // Skip whitespace
      while (pos < text.length && /\s/.test(text[pos])) pos++;
      if (pos >= text.length) break;

      // Check for comment
      if (text[pos] === ';') break;

      // Try to parse a command
      const cmdMatch = text.slice(pos).match(/^([A-Z]+)/i);
      if (cmdMatch) {
        const cmdText = cmdMatch[1].toUpperCase();
        const fullCmd = COMMAND_ABBREV[cmdText] || (MUMPS_COMMANDS.includes(cmdText) ? cmdText : null);

        if (fullCmd) {
          this.tokens.push(new MumpsToken(
            'command',
            fullCmd,
            lineNum,
            startCol + pos,
            cmdText.length
          ));
          pos += cmdText.length;

          // Parse command arguments
          pos = this.parseArguments(text, pos, lineNum, startCol, fullCmd);
          continue;
        }
      }

      // Skip unrecognized content
      pos++;
    }
  }

  parseArguments(text, pos, lineNum, startCol, command) {
    // Skip spaces after command
    while (pos < text.length && text[pos] === ' ') pos++;

    // Parse arguments until next command or end of line
    while (pos < text.length) {
      const char = text[pos];

      // Command separator or comment
      if (char === ';') break;
      if (/[A-Z]/i.test(char) && (pos === 0 || text[pos - 1] === ' ')) {
        // Might be next command
        const nextCmd = text.slice(pos).match(/^([A-Z]+)/i);
        if (nextCmd && (COMMAND_ABBREV[nextCmd[1].toUpperCase()] || MUMPS_COMMANDS.includes(nextCmd[1].toUpperCase()))) {
          break;
        }
      }

      // Parse string literal
      if (char === '"') {
        pos++;
        while (pos < text.length) {
          if (text[pos] === '"') {
            if (text[pos + 1] === '"') {
              pos += 2; // Escaped quote
            } else {
              pos++;
              break;
            }
          } else {
            pos++;
          }
        }
        continue;
      }

      // Parse function call
      if (char === '$') {
        const funcMatch = text.slice(pos).match(/^\$([A-Z]+)/i);
        if (funcMatch) {
          this.tokens.push(new MumpsToken(
            'function',
            funcMatch[0],
            lineNum,
            startCol + pos,
            funcMatch[0].length
          ));
          pos += funcMatch[0].length;
          continue;
        }
      }

      // Parse variable reference
      if (/[A-Z%]/i.test(char)) {
        const varMatch = text.slice(pos).match(/^([A-Z%][A-Za-z0-9]*)/i);
        if (varMatch) {
          this.tokens.push(new MumpsToken(
            'variable',
            varMatch[1],
            lineNum,
            startCol + pos,
            varMatch[1].length
          ));
          pos += varMatch[1].length;
          continue;
        }
      }

      pos++;
    }

    return pos;
  }

  addDiagnostic(severity, message, line, column, length) {
    this.diagnostics.push({
      severity,  // 'error', 'warning', 'info'
      message,
      line,
      column,
      length
    });
  }
}

class MumpsLinter {
  constructor(code) {
    this.code = code;
    this.parser = new MumpsParser(code);
    this.diagnostics = [];
  }

  lint() {
    const parsed = this.parser.parse();
    this.diagnostics = [];

    this.checkNewVariables(parsed);
    this.checkUnreachableCode();
    this.checkLabels(parsed);

    return {
      diagnostics: [...this.diagnostics, ...parsed.diagnostics],
      labels: parsed.labels,
      tags: parsed.tags
    };
  }

  checkNewVariables(parsed) {
    const lines = this.code.split('\n');
    const newedVars = new Set();
    const usedVars = new Set();
    const ignorePatterns = ['%', 'ZTRAP', 'ETRAP', 'ZSTEP'];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();
      if (!line || line.startsWith(';')) continue;

      // Find NEW commands
      const newMatch = line.match(/\b(?:N(?:EW)?)\s+([A-Z%][A-Za-z0-9,\s]*)/i);
      if (newMatch) {
        const vars = newMatch[1].split(',').map(v => v.trim());
        vars.forEach(v => {
          if (v && !v.includes('(')) {  // Exclude NEWed expressions like NEW (X,Y)
            newedVars.add(v);
          }
        });
      }

      // Find variable usage (SET, WRITE, IF, etc.)
      const tokens = parsed.tokens.filter(t => t.line === i + 1 && t.type === 'variable');
      tokens.forEach(token => {
        const varName = token.value;
        if (!ignorePatterns.some(p => varName.startsWith(p))) {
          usedVars.add(varName);
        }
      });
    }

    // Check for used but not NEWed variables
    usedVars.forEach(varName => {
      if (!newedVars.has(varName) && !varName.startsWith('%')) {
        // Find first usage for error position
        const token = parsed.tokens.find(t => t.value === varName && t.type === 'variable');
        if (token) {
          this.addDiagnostic(
            'warning',
            `Variable '${varName}' used without NEW`,
            token.line,
            token.column,
            token.length
          );
        }
      }
    });

    // Check for NEWed but never used variables
    newedVars.forEach(varName => {
      if (!usedVars.has(varName)) {
        // Find NEW statement
        for (let i = 0; i < lines.length; i++) {
          if (lines[i].includes(varName)) {
            this.addDiagnostic(
              'info',
              `Variable '${varName}' NEWed but never used`,
              i + 1,
              lines[i].indexOf(varName),
              varName.length
            );
            break;
          }
        }
      }
    });
  }

  checkUnreachableCode() {
    const lines = this.code.split('\n');

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();
      if (!line || line.startsWith(';')) continue;

      // Check if line has QUIT/HALT/GOTO
      if (/\b(?:Q(?:UIT)?|H(?:ALT)?|G(?:OTO)?)\b/i.test(line)) {
        // Check if there's unreachable code after this line
        for (let j = i + 1; j < lines.length; j++) {
          const nextLine = lines[j];
          const trimmed = nextLine.trim();

          // Skip empty lines and comments
          if (!trimmed || trimmed.startsWith(';')) continue;

          // If next line is a label, it's reachable
          if (/^[A-Za-z%][A-Za-z0-9]*/.test(nextLine)) break;

          // If next line is indented code, it's unreachable
          if (nextLine.startsWith(' ') || nextLine.startsWith('\t')) {
            this.addDiagnostic(
              'warning',
              'Unreachable code after QUIT/HALT/GOTO',
              j + 1,
              0,
              nextLine.length
            );
            break;
          }

          break;
        }
      }
    }
  }

  checkLabels(parsed) {
    const lines = this.code.split('\n');

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();
      if (!line || line.startsWith(';')) continue;

      // Find DO/GOTO calls to labels
      const labelCalls = line.match(/\b(?:D(?:O)?|G(?:OTO)?)\s+([A-Z%][A-Za-z0-9]*)/gi);
      if (labelCalls) {
        labelCalls.forEach(call => {
          const labelMatch = call.match(/\b(?:D(?:O)?|G(?:OTO)?)\s+([A-Z%][A-Za-z0-9]*)/i);
          if (labelMatch) {
            const labelName = labelMatch[1];

            // Skip if it's an external routine call (has ^)
            if (line.includes('^')) return;

            // Check if label exists
            if (!parsed.labels.has(labelName)) {
              const col = lines[i].indexOf(labelName);
              this.addDiagnostic(
                'error',
                `Label '${labelName}' not found`,
                i + 1,
                col,
                labelName.length
              );
            }
          }
        });
      }
    }
  }

  addDiagnostic(severity, message, line, column, length) {
    this.diagnostics.push({
      severity,
      message,
      line,
      column,
      length
    });
  }
}

module.exports = {
  MumpsParser,
  MumpsLinter,
  MumpsToken,
  MUMPS_COMMANDS,
  INTRINSIC_FUNCTIONS
};
