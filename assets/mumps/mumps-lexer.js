/**
 * MUMPS Lexer - Token Stream Generator
 * Converts MUMPS source code into a stream of tokens
 */

// Token Types
const TokenType = {
    // Literals
    STRING: 'STRING',
    NUMBER: 'NUMBER',

    // Identifiers
    LABEL: 'LABEL',
    COMMAND: 'COMMAND',
    VARIABLE: 'VARIABLE',
    GLOBAL: 'GLOBAL',
    INTRINSIC: 'INTRINSIC',
    INTRINSIC_VAR: 'INTRINSIC_VAR',

    // Operators
    PLUS: 'PLUS',                    // +
    MINUS: 'MINUS',                  // -
    MULTIPLY: 'MULTIPLY',            // *
    DIVIDE: 'DIVIDE',                // /
    INT_DIVIDE: 'INT_DIVIDE',        // \
    MODULO: 'MODULO',                // #
    POWER: 'POWER',                  // **
    CONCAT: 'CONCAT',                // _
    AND: 'AND',                      // &
    OR: 'OR',                        // !
    NOT: 'NOT',                      // '
    EQUALS: 'EQUALS',                // =
    NOT_EQUALS: 'NOT_EQUALS',        // '=
    LESS_THAN: 'LESS_THAN',          // <
    GREATER_THAN: 'GREATER_THAN',    // >
    LESS_EQUAL: 'LESS_EQUAL',        // <=
    GREATER_EQUAL: 'GREATER_EQUAL',  // >=
    CONTAINS: 'CONTAINS',            // [
    FOLLOWS: 'FOLLOWS',              // ]
    SORTS_AFTER: 'SORTS_AFTER',      // ]]
    PATTERN_MATCH: 'PATTERN_MATCH',  // ?

    // Delimiters
    LPAREN: 'LPAREN',                // (
    RPAREN: 'RPAREN',                // )
    COMMA: 'COMMA',                  // ,
    COLON: 'COLON',                  // :
    SEMICOLON: 'SEMICOLON',          // ;
    DOT: 'DOT',                      // .
    AT: 'AT',                        // @ (indirection)
    CARET: 'CARET',                  // ^ (global or routine reference)

    // Special
    COMMENT: 'COMMENT',
    NEWLINE: 'NEWLINE',
    SPACE: 'SPACE',
    EOF: 'EOF',
    ERROR: 'ERROR'
};

class Token {
    constructor(type, value, line, column, length = null) {
        this.type = type;
        this.value = value;
        this.line = line;
        this.column = column;
        this.length = length || value.length;
    }

    toString() {
        return `Token(${this.type}, "${this.value}", ${this.line}:${this.column})`;
    }
}

class MUMPSLexer {
    constructor(source = '') {
        this.source = source;
        this.position = 0;
        this.line = 1;
        this.column = 1;
        this.tokens = [];
        this.errors = [];

        // MUMPS command keywords (full and abbreviated forms)
        this.commands = new Set([
            'BREAK', 'B',
            'CLOSE', 'C',
            'DO', 'D',
            'ELSE', 'E',
            'FOR', 'F',
            'GOTO', 'G',
            'HALT', 'H',
            'HANG',
            'IF', 'I',
            'JOB', 'J',
            'KILL', 'K',
            'LOCK', 'L',
            'MERGE', 'M',
            'NEW', 'N',
            'OPEN', 'O',
            'QUIT', 'Q',
            'READ', 'R',
            'SET', 'S',
            'TCOMMIT', 'TC',
            'TRESTART', 'TRE',
            'TROLLBACK', 'TRO',
            'TSTART', 'TS',
            'USE', 'U',
            'VIEW', 'V',
            'WRITE', 'W',
            'XECUTE', 'X',
            'ZBREAK', 'ZB',
            'ZDEALLOCATE', 'ZD',
            'ZGOTO', 'ZG',
            'ZKILL', 'ZK',
            'ZLINK', 'ZL',
            'ZPRINT', 'ZP',
            'ZSHOW', 'ZSH',
            'ZSYSTEM', 'ZSY',
            'ZWITHDRAW', 'ZWI',
            'ZWRITE', 'ZW'
        ]);

        // Intrinsic functions
        this.intrinsicFunctions = new Set([
            '$ASCII', '$A',
            '$CHAR', '$C',
            '$DATA', '$D',
            '$EXTRACT', '$E',
            '$FIND', '$F',
            '$FNUMBER', '$FN',
            '$GET', '$G',
            '$JUSTIFY', '$J',
            '$LENGTH', '$L',
            '$NAME', '$NA',
            '$ORDER', '$O',
            '$PIECE', '$P',
            '$QLENGTH', '$QL',
            '$QSUBSCRIPT', '$QS',
            '$QUERY', '$Q',
            '$RANDOM', '$R',
            '$REVERSE',
            '$SELECT', '$S',
            '$TEXT', '$T',
            '$TRANSLATE', '$TR',
            '$VIEW', '$V',
            '$ZDATE', '$ZD',
            '$ZSEARCH', '$ZSE'
        ]);

        // Intrinsic special variables
        this.intrinsicVars = new Set([
            '$DEVICE', '$D',
            '$ECODE', '$EC',
            '$ESTACK', '$ES',
            '$ETRAP', '$ET',
            '$HOROLOG', '$H',
            '$IO', '$I',
            '$JOB', '$J',
            '$KEY', '$K',
            '$PRINCIPAL', '$P',
            '$QUIT', '$Q',
            '$STACK', '$ST',
            '$STORAGE', '$S',
            '$SYSTEM', '$SY',
            '$TEST', '$T',
            '$X',
            '$Y',
            '$ZEOF', '$ZE',
            '$ZJOB', '$ZJ',
            '$ZPOS', '$ZP',
            '$ZSTATUS', '$ZS'
        ]);
    }

    tokenize(source = null) {
        if (source !== null) {
            this.source = source;
        }

        this.position = 0;
        this.line = 1;
        this.column = 1;
        this.tokens = [];
        this.errors = [];

        while (!this.isAtEnd()) {
            this.scanToken();
        }

        this.addToken(TokenType.EOF, '');
        return this.tokens;
    }

    scanToken() {
        const start = this.position;
        const startColumn = this.column;
        const char = this.advance();

        // Newline
        if (char === '\n') {
            this.addToken(TokenType.NEWLINE, char, this.line - 1, startColumn);
            return;
        }

        // Comment (semicolon)
        if (char === ';') {
            this.scanComment(startColumn);
            return;
        }

        // Whitespace
        if (char === ' ' || char === '\t') {
            let spaces = char;
            while (this.peek() === ' ' || this.peek() === '\t') {
                spaces += this.advance();
            }
            this.addToken(TokenType.SPACE, spaces, this.line, startColumn);
            return;
        }

        // String literal
        if (char === '"') {
            this.scanString(startColumn);
            return;
        }

        // Number
        if (this.isDigit(char) || (char === '.' && this.isDigit(this.peek()))) {
            this.scanNumber(char, startColumn);
            return;
        }

        // Intrinsic function or variable
        if (char === '$') {
            this.scanIntrinsic(startColumn);
            return;
        }

        // Global variable
        if (char === '^') {
            if (this.isAlpha(this.peek())) {
                this.scanGlobal(startColumn);
            } else {
                this.addToken(TokenType.CARET, char, this.line, startColumn);
            }
            return;
        }

        // Indirection
        if (char === '@') {
            this.addToken(TokenType.AT, char, this.line, startColumn);
            return;
        }

        // Operators and delimiters
        switch (char) {
            case '+':
                this.addToken(TokenType.PLUS, char, this.line, startColumn);
                break;
            case '-':
                this.addToken(TokenType.MINUS, char, this.line, startColumn);
                break;
            case '*':
                if (this.peek() === '*') {
                    this.advance();
                    this.addToken(TokenType.POWER, '**', this.line, startColumn);
                } else {
                    this.addToken(TokenType.MULTIPLY, char, this.line, startColumn);
                }
                break;
            case '/':
                this.addToken(TokenType.DIVIDE, char, this.line, startColumn);
                break;
            case '\\':
                this.addToken(TokenType.INT_DIVIDE, char, this.line, startColumn);
                break;
            case '#':
                this.addToken(TokenType.MODULO, char, this.line, startColumn);
                break;
            case '_':
                this.addToken(TokenType.CONCAT, char, this.line, startColumn);
                break;
            case '&':
                this.addToken(TokenType.AND, char, this.line, startColumn);
                break;
            case '!':
                this.addToken(TokenType.OR, char, this.line, startColumn);
                break;
            case '\'':
                if (this.peek() === '=') {
                    this.advance();
                    this.addToken(TokenType.NOT_EQUALS, '\'=', this.line, startColumn);
                } else {
                    this.addToken(TokenType.NOT, char, this.line, startColumn);
                }
                break;
            case '=':
                this.addToken(TokenType.EQUALS, char, this.line, startColumn);
                break;
            case '<':
                if (this.peek() === '=') {
                    this.advance();
                    this.addToken(TokenType.LESS_EQUAL, '<=', this.line, startColumn);
                } else {
                    this.addToken(TokenType.LESS_THAN, char, this.line, startColumn);
                }
                break;
            case '>':
                if (this.peek() === '=') {
                    this.advance();
                    this.addToken(TokenType.GREATER_EQUAL, '>=', this.line, startColumn);
                } else {
                    this.addToken(TokenType.GREATER_THAN, char, this.line, startColumn);
                }
                break;
            case '[':
                this.addToken(TokenType.CONTAINS, char, this.line, startColumn);
                break;
            case ']':
                if (this.peek() === ']') {
                    this.advance();
                    this.addToken(TokenType.SORTS_AFTER, ']]', this.line, startColumn);
                } else {
                    this.addToken(TokenType.FOLLOWS, char, this.line, startColumn);
                }
                break;
            case '?':
                this.addToken(TokenType.PATTERN_MATCH, char, this.line, startColumn);
                break;
            case '(':
                this.addToken(TokenType.LPAREN, char, this.line, startColumn);
                break;
            case ')':
                this.addToken(TokenType.RPAREN, char, this.line, startColumn);
                break;
            case ',':
                this.addToken(TokenType.COMMA, char, this.line, startColumn);
                break;
            case ':':
                this.addToken(TokenType.COLON, char, this.line, startColumn);
                break;
            case '.':
                this.addToken(TokenType.DOT, char, this.line, startColumn);
                break;
            default:
                // Identifier (variable, label, or command)
                if (this.isAlpha(char) || char === '%') {
                    this.scanIdentifier(char, startColumn);
                } else {
                    this.addError(`Unexpected character: '${char}'`, this.line, startColumn);
                    this.addToken(TokenType.ERROR, char, this.line, startColumn);
                }
        }
    }

    scanComment(startColumn) {
        const start = this.position - 1;
        while (!this.isAtEnd() && this.peek() !== '\n') {
            this.advance();
        }
        const text = this.source.substring(start, this.position);
        this.addToken(TokenType.COMMENT, text, this.line, startColumn);
    }

    scanString(startColumn) {
        const start = this.position - 1;
        let value = '';

        while (!this.isAtEnd() && this.peek() !== '"') {
            if (this.peek() === '\n') {
                this.addError('Unclosed string literal', this.line, startColumn);
                break;
            }
            value += this.advance();
        }

        if (this.isAtEnd()) {
            this.addError('Unclosed string literal', this.line, startColumn);
        } else {
            this.advance(); // Closing "
        }

        const fullText = this.source.substring(start, this.position);
        this.addToken(TokenType.STRING, value, this.line, startColumn, fullText.length);
    }

    scanNumber(firstChar, startColumn) {
        let value = firstChar;

        while (this.isDigit(this.peek())) {
            value += this.advance();
        }

        // Decimal part
        if (this.peek() === '.' && this.isDigit(this.peekNext())) {
            value += this.advance(); // .
            while (this.isDigit(this.peek())) {
                value += this.advance();
            }
        }

        // Scientific notation
        if (this.peek() === 'E' || this.peek() === 'e') {
            value += this.advance();
            if (this.peek() === '+' || this.peek() === '-') {
                value += this.advance();
            }
            while (this.isDigit(this.peek())) {
                value += this.advance();
            }
        }

        this.addToken(TokenType.NUMBER, value, this.line, startColumn);
    }

    scanIntrinsic(startColumn) {
        let name = '$';

        while (this.isAlphaNumeric(this.peek())) {
            name += this.advance();
        }

        const upperName = name.toUpperCase();

        // Check if it's followed by parenthesis (function) or not (variable)
        if (this.peek() === '(') {
            this.addToken(TokenType.INTRINSIC, upperName, this.line, startColumn);
        } else {
            this.addToken(TokenType.INTRINSIC_VAR, upperName, this.line, startColumn);
        }
    }

    scanGlobal(startColumn) {
        let name = '^';

        while (this.isAlphaNumeric(this.peek())) {
            name += this.advance();
        }

        this.addToken(TokenType.GLOBAL, name, this.line, startColumn);
    }

    scanIdentifier(firstChar, startColumn) {
        let text = firstChar;

        while (this.isAlphaNumeric(this.peek())) {
            text += this.advance();
        }

        const upperText = text.toUpperCase();

        // Check if it's a command
        if (this.commands.has(upperText)) {
            this.addToken(TokenType.COMMAND, upperText, this.line, startColumn);
        } else {
            // It's a variable or label
            // Labels are detected contextually by the parser
            this.addToken(TokenType.VARIABLE, text, this.line, startColumn);
        }
    }

    // Helper methods
    advance() {
        const char = this.source.charAt(this.position);
        this.position++;

        if (char === '\n') {
            this.line++;
            this.column = 1;
        } else {
            this.column++;
        }

        return char;
    }

    peek() {
        if (this.isAtEnd()) return '\0';
        return this.source.charAt(this.position);
    }

    peekNext() {
        if (this.position + 1 >= this.source.length) return '\0';
        return this.source.charAt(this.position + 1);
    }

    isAtEnd() {
        return this.position >= this.source.length;
    }

    isDigit(char) {
        return char >= '0' && char <= '9';
    }

    isAlpha(char) {
        return (char >= 'a' && char <= 'z') ||
               (char >= 'A' && char <= 'Z') ||
               char === '%';
    }

    isAlphaNumeric(char) {
        return this.isAlpha(char) || this.isDigit(char);
    }

    addToken(type, value, line = null, column = null, length = null) {
        const token = new Token(
            type,
            value,
            line !== null ? line : this.line,
            column !== null ? column : this.column,
            length
        );
        this.tokens.push(token);
    }

    addError(message, line, column) {
        this.errors.push({
            message,
            line,
            column,
            severity: 'error'
        });
    }

    // Get tokens without whitespace and comments (for parser)
    getSignificantTokens() {
        return this.tokens.filter(t =>
            t.type !== TokenType.SPACE &&
            t.type !== TokenType.COMMENT
        );
    }

    // Get all tokens including whitespace (for syntax highlighting)
    getAllTokens() {
        return this.tokens;
    }
}

// Export
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { MUMPSLexer, Token, TokenType };
}

if (typeof window !== 'undefined') {
    window.MUMPSLexer = MUMPSLexer;
    window.MUMPSToken = Token;
    window.MUMPSTokenType = TokenType;
}
