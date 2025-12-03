/*
 * MUMPS Parser - Converts Token Stream to Abstract Syntax Tree
 * Implements a recursive descent parser for MUMPS syntax
 */

// Import dependencies (will be loaded from separate files in browser)
// Use existing global variables (already loaded from mumps-lexer.js, etc.)
if (typeof require !== 'undefined') {
    const lexer = require('./mumps-lexer.js');
    MUMPSLexer = lexer.MUMPSLexer;
    TokenType = lexer.TokenType;
    MUMPSAST = require('./mumps-ast.js');
}

class MUMPSParser {
    constructor() {
        this.tokens = [];
        this.current = 0;
        this.errors = [];
        // Use global MUMPSLexer from window if in browser
        const LexerClass = typeof MUMPSLexer !== 'undefined' ? MUMPSLexer : (typeof window !== 'undefined' ? window.MUMPSLexer : null);
        if (!LexerClass) {
            throw new Error('MUMPSLexer not available');
        }
        this.lexer = new LexerClass();
        // Store references to global classes for use in methods
        this.TokenType = typeof TokenType !== 'undefined' ? TokenType : (typeof window !== 'undefined' ? window.MUMPSTokenType : null);
        this.MUMPSAST = typeof MUMPSAST !== 'undefined' ? MUMPSAST : (typeof window !== 'undefined' ? window.MUMPSAST : null);
    }

    /**
     * Parse MUMPS source code into an AST
     * @param {string} source - MUMPS source code
     * @returns {RoutineNode} - Root AST node
     */
    parse(source) {
        // Tokenize
        this.lexer.tokenize(source);
        this.tokens = this.lexer.getSignificantTokens();
        this.current = 0;
        this.errors = [...this.lexer.errors];

        // Parse routine
        return this.parseRoutine();
    }

    /**
     * Parse a complete routine
     */
    parseRoutine() {
        const labels = [];
        let routineName = null;

        while (!this.isAtEnd()) {
            // Skip newlines
            while (this.match(TokenType.NEWLINE)) {}

            if (this.isAtEnd()) break;

            // Check for label at start of line
            const label = this.parseLabel();
            if (label) {
                labels.push(label);
                // First label is the routine name
                if (!routineName) {
                    routineName = label.name;
                }
            } else if (!this.isAtEnd()) {
                // Not a label, might be an error or end of file
                this.error(`Unexpected token: ${this.peek().value}`);
                this.advance(); // Skip to try to continue
            }
        }

        return new MUMPSAST.RoutineNode(
            routineName || 'UNNAMED',
            labels,
            new MUMPSAST.Location(1, 1)
        );
    }

    /**
     * Parse a label and its commands
     */
    parseLabel() {
        const startToken = this.peek();

        // Label must be at start or after newline (tracked externally)
        // For now, we look for VARIABLE token that could be a label
        if (!this.check(TokenType.VARIABLE) && !this.check(TokenType.COMMAND)) {
            return null;
        }

        let name = null;
        let parameters = [];
        let comment = null;
        const commands = [];

        // Check if this is a label (VARIABLE followed by optional params or comment/newline)
        if (this.check(TokenType.VARIABLE)) {
            const nameToken = this.advance();
            name = nameToken.value;

            // Check for parameters
            if (this.match(TokenType.LPAREN)) {
                parameters = this.parseParameterList();
                this.consume(TokenType.RPAREN, "Expected ')' after parameters");
            }

            // Check for comment after label
            if (this.match(TokenType.SEMICOLON)) {
                // Actually, semicolon is tokenized as COMMENT
                // Check next token
                const next = this.peek();
                if (next.type === TokenType.NEWLINE) {
                    comment = '';
                }
            }

            // Must be followed by newline or comment
            this.match(TokenType.NEWLINE);
        }

        // Parse commands for this label
        while (!this.isAtEnd() && !this.isNextLabel()) {
            // Skip empty lines
            if (this.match(TokenType.NEWLINE)) {
                continue;
            }

            // Skip lines that start with space (command lines)
            const cmd = this.parseCommandLine();
            if (cmd) {
                commands.push(...cmd);
            }

            // Consume newline after command
            this.match(TokenType.NEWLINE);
        }

        if (name) {
            return new MUMPSAST.LabelNode(
                name,
                parameters,
                commands,
                comment,
                new MUMPSAST.Location(startToken.line, startToken.column)
            );
        }

        return null;
    }

    /**
     * Check if next non-newline token is a label
     */
    isNextLabel() {
        let offset = 0;
        while (this.peek(offset).type === TokenType.NEWLINE) {
            offset++;
        }
        const next = this.peek(offset);
        return next.type === TokenType.VARIABLE &&
               !this.check(TokenType.COMMAND, offset);
    }

    /**
     * Parse a command line (may have multiple commands separated by spaces)
     */
    parseCommandLine() {
        const commands = [];

        while (!this.isAtEnd() && !this.check(TokenType.NEWLINE)) {
            const cmd = this.parseCommand();
            if (cmd) {
                commands.push(cmd);
            } else {
                break;
            }
        }

        return commands;
    }

    /**
     * Parse a single command
     */
    parseCommand() {
        const startToken = this.peek();

        if (!this.check(TokenType.COMMAND)) {
            return null;
        }

        const commandToken = this.advance();
        const commandType = commandToken.value;

        // Handle post-conditional (command:condition)
        let postConditional = null;
        if (this.match(TokenType.COLON)) {
            postConditional = this.parseExpression();
        }

        let command = null;

        switch (commandType) {
            case 'SET':
            case 'S':
                command = this.parseSetCommand(startToken);
                break;
            case 'WRITE':
            case 'W':
                command = this.parseWriteCommand(startToken);
                break;
            case 'DO':
            case 'D':
                command = this.parseDoCommand(startToken);
                break;
            case 'QUIT':
            case 'Q':
                command = this.parseQuitCommand(startToken);
                break;
            case 'IF':
            case 'I':
                command = this.parseIfCommand(startToken);
                break;
            case 'FOR':
            case 'F':
                command = this.parseForCommand(startToken);
                break;
            case 'NEW':
            case 'N':
                command = this.parseNewCommand(startToken);
                break;
            case 'KILL':
            case 'K':
                command = this.parseKillCommand(startToken);
                break;
            case 'READ':
            case 'R':
                command = this.parseReadCommand(startToken);
                break;
            case 'GOTO':
            case 'G':
                command = this.parseGotoCommand(startToken);
                break;
            case 'HALT':
            case 'H':
                command = new MUMPSAST.HaltCommandNode(
                    new MUMPSAST.Location(startToken.line, startToken.column)
                );
                break;
            case 'HANG':
                command = this.parseHangCommand(startToken);
                break;
            case 'MERGE':
            case 'M':
                command = this.parseMergeCommand(startToken);
                break;
            case 'LOCK':
            case 'L':
                command = this.parseLockCommand(startToken);
                break;
            case 'OPEN':
            case 'O':
                command = this.parseOpenCommand(startToken);
                break;
            case 'USE':
            case 'U':
                command = this.parseUseCommand(startToken);
                break;
            case 'CLOSE':
            case 'C':
                command = this.parseCloseCommand(startToken);
                break;
            default:
                this.error(`Unknown command: ${commandType}`);
                command = new MUMPSAST.ErrorNode(
                    `Unknown command: ${commandType}`,
                    'UNKNOWN_COMMAND',
                    new MUMPSAST.Location(startToken.line, startToken.column)
                );
        }

        return command;
    }

    // ========================================================================
    // COMMAND PARSERS
    // ========================================================================

    parseSetCommand(startToken) {
        const assignments = [];

        do {
            const target = this.parseVariable();
            this.consume(TokenType.EQUALS, "Expected '=' in SET command");
            const value = this.parseExpression();
            assignments.push({ target, value });
        } while (this.match(TokenType.COMMA));

        return new MUMPSAST.SetCommandNode(
            assignments,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    parseWriteCommand(startToken) {
        const expressions = [];

        do {
            // Handle special format controls (#, !, ?)
            if (this.check(TokenType.NOT)) {
                this.advance();
                expressions.push(new MUMPSAST.LiteralNode(
                    '!',
                    'string',
                    new MUMPSAST.Location(this.previous().line, this.previous().column)
                ));
            } else if (this.check(TokenType.MODULO)) {
                this.advance();
                expressions.push(new MUMPSAST.LiteralNode(
                    '#',
                    'string',
                    new MUMPSAST.Location(this.previous().line, this.previous().column)
                ));
            } else {
                expressions.push(this.parseExpression());
            }
        } while (this.match(TokenType.COMMA));

        return new MUMPSAST.WriteCommandNode(
            expressions,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    parseDoCommand(startToken) {
        // DO can be: DO, DO label, DO label^routine, DO ^routine
        if (this.check(TokenType.NEWLINE) || this.check(TokenType.COMMAND)) {
            // Argumentless DO
            return new MUMPSAST.DoCommandNode(
                null,
                [],
                new MUMPSAST.Location(startToken.line, startToken.column)
            );
        }

        const target = this.parseRoutineReference();
        const parameters = [];

        if (this.match(TokenType.LPAREN)) {
            parameters.push(...this.parseExpressionList());
            this.consume(TokenType.RPAREN, "Expected ')' after DO parameters");
        }

        return new MUMPSAST.DoCommandNode(
            target,
            parameters,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    parseQuitCommand(startToken) {
        let expression = null;

        if (!this.check(TokenType.NEWLINE) && !this.check(TokenType.COMMAND)) {
            expression = this.parseExpression();
        }

        return new MUMPSAST.QuitCommandNode(
            expression,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    parseIfCommand(startToken) {
        const condition = this.parseExpression();
        const thenCommands = [];
        const elseCommands = [];

        // Parse consequent commands
        while (!this.isAtEnd() && !this.check(TokenType.NEWLINE) && !this.checkCommand('ELSE')) {
            const cmd = this.parseCommand();
            if (cmd) thenCommands.push(cmd);
            else break;
        }

        // Parse ELSE if present
        if (this.checkCommand('ELSE') || this.checkCommand('E')) {
            this.advance();
            while (!this.isAtEnd() && !this.check(TokenType.NEWLINE)) {
                const cmd = this.parseCommand();
                if (cmd) elseCommands.push(cmd);
                else break;
            }
        }

        return new MUMPSAST.IfCommandNode(
            condition,
            thenCommands,
            elseCommands,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    parseForCommand(startToken) {
        const variable = this.consume(TokenType.VARIABLE, "Expected variable in FOR command").value;
        this.consume(TokenType.EQUALS, "Expected '=' in FOR command");

        const initializer = this.parseExpression();
        let increment = null;
        let condition = null;

        if (this.match(TokenType.COLON)) {
            increment = this.parseExpression();
            if (this.match(TokenType.COLON)) {
                condition = this.parseExpression();
            }
        }

        const commands = [];
        while (!this.isAtEnd() && !this.check(TokenType.NEWLINE)) {
            const cmd = this.parseCommand();
            if (cmd) commands.push(cmd);
            else break;
        }

        return new MUMPSAST.ForCommandNode(
            variable,
            initializer,
            increment,
            condition,
            commands,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    parseNewCommand(startToken) {
        const variables = [];

        do {
            variables.push(this.consume(TokenType.VARIABLE, "Expected variable").value);
        } while (this.match(TokenType.COMMA));

        return new MUMPSAST.NewCommandNode(
            variables,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    parseKillCommand(startToken) {
        const variables = [];

        do {
            variables.push(this.parseVariable());
        } while (this.match(TokenType.COMMA));

        return new MUMPSAST.KillCommandNode(
            variables,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    parseReadCommand(startToken) {
        let prompt = null;
        let timeout = null;

        // Check for prompt string
        if (this.check(TokenType.STRING)) {
            prompt = this.advance().value;
        }

        const target = this.parseVariable();

        // Check for timeout
        if (this.match(TokenType.COLON)) {
            timeout = this.parseExpression();
        }

        return new MUMPSAST.ReadCommandNode(
            target,
            prompt,
            timeout,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    parseGotoCommand(startToken) {
        const target = this.parseRoutineReference();

        return new MUMPSAST.GotoCommandNode(
            target,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    parseHangCommand(startToken) {
        const duration = this.parseExpression();

        return new MUMPSAST.HangCommandNode(
            duration,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    parseMergeCommand(startToken) {
        const target = this.parseVariable();
        this.consume(TokenType.EQUALS, "Expected '=' in MERGE command");
        const source = this.parseVariable();

        return new MUMPSAST.MergeCommandNode(
            target,
            source,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    parseLockCommand(startToken) {
        const variables = [];
        let timeout = null;
        let increment = true;

        // Check for + or - prefix
        if (this.match(TokenType.PLUS)) {
            increment = true;
        } else if (this.match(TokenType.MINUS)) {
            increment = false;
        }

        do {
            variables.push(this.parseVariable());
        } while (this.match(TokenType.COMMA));

        // Check for timeout
        if (this.match(TokenType.COLON)) {
            timeout = this.parseExpression();
        }

        return new MUMPSAST.LockCommandNode(
            variables,
            timeout,
            increment,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    parseOpenCommand(startToken) {
        const device = this.parseExpression();
        const parameters = [];

        if (this.match(TokenType.LPAREN)) {
            parameters.push(...this.parseExpressionList());
            this.consume(TokenType.RPAREN, "Expected ')' after OPEN parameters");
        }

        return new MUMPSAST.OpenCommandNode(
            device,
            parameters,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    parseUseCommand(startToken) {
        const device = this.parseExpression();
        const parameters = [];

        if (this.match(TokenType.LPAREN)) {
            parameters.push(...this.parseExpressionList());
            this.consume(TokenType.RPAREN, "Expected ')' after USE parameters");
        }

        return new MUMPSAST.UseCommandNode(
            device,
            parameters,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    parseCloseCommand(startToken) {
        const device = this.parseExpression();

        return new MUMPSAST.CloseCommandNode(
            device,
            new MUMPSAST.Location(startToken.line, startToken.column)
        );
    }

    // ========================================================================
    // EXPRESSION PARSERS
    // ========================================================================

    parseExpression() {
        return this.parseLogicalOr();
    }

    parseLogicalOr() {
        let expr = this.parseLogicalAnd();

        while (this.match(TokenType.OR)) {
            const operator = '!';
            const right = this.parseLogicalAnd();
            expr = new MUMPSAST.BinaryOperationNode(operator, expr, right);
        }

        return expr;
    }

    parseLogicalAnd() {
        let expr = this.parseRelational();

        while (this.match(TokenType.AND)) {
            const operator = '&';
            const right = this.parseRelational();
            expr = new MUMPSAST.BinaryOperationNode(operator, expr, right);
        }

        return expr;
    }

    parseRelational() {
        let expr = this.parseAdditive();

        while (this.match(TokenType.EQUALS, TokenType.NOT_EQUALS, TokenType.LESS_THAN,
                          TokenType.GREATER_THAN, TokenType.LESS_EQUAL, TokenType.GREATER_EQUAL,
                          TokenType.CONTAINS, TokenType.FOLLOWS, TokenType.PATTERN_MATCH)) {
            const operator = this.previous().value;
            const right = this.parseAdditive();
            expr = new MUMPSAST.BinaryOperationNode(operator, expr, right);
        }

        return expr;
    }

    parseAdditive() {
        let expr = this.parseMultiplicative();

        while (this.match(TokenType.PLUS, TokenType.MINUS, TokenType.CONCAT)) {
            const operator = this.previous().value;
            const right = this.parseMultiplicative();
            expr = new MUMPSAST.BinaryOperationNode(operator, expr, right);
        }

        return expr;
    }

    parseMultiplicative() {
        let expr = this.parseUnary();

        while (this.match(TokenType.MULTIPLY, TokenType.DIVIDE, TokenType.INT_DIVIDE,
                          TokenType.MODULO, TokenType.POWER)) {
            const operator = this.previous().value;
            const right = this.parseUnary();
            expr = new MUMPSAST.BinaryOperationNode(operator, expr, right);
        }

        return expr;
    }

    parseUnary() {
        if (this.match(TokenType.PLUS, TokenType.MINUS, TokenType.NOT)) {
            const operator = this.previous().value;
            const operand = this.parseUnary();
            return new MUMPSAST.UnaryOperationNode(operator, operand);
        }

        return this.parsePrimary();
    }

    parsePrimary() {
        const token = this.peek();

        // String literal
        if (this.match(TokenType.STRING)) {
            return new MUMPSAST.LiteralNode(
                this.previous().value,
                'string',
                new MUMPSAST.Location(token.line, token.column)
            );
        }

        // Number literal
        if (this.match(TokenType.NUMBER)) {
            return new MUMPSAST.LiteralNode(
                this.previous().value,
                'number',
                new MUMPSAST.Location(token.line, token.column)
            );
        }

        // Intrinsic function
        if (this.match(TokenType.INTRINSIC)) {
            return this.parseIntrinsicFunction(this.previous());
        }

        // Intrinsic variable
        if (this.match(TokenType.INTRINSIC_VAR)) {
            return new MUMPSAST.VariableNode(
                this.previous().value,
                [],
                'intrinsic',
                new MUMPSAST.Location(token.line, token.column)
            );
        }

        // Variable (local or global)
        if (this.check(TokenType.VARIABLE) || this.check(TokenType.GLOBAL)) {
            return this.parseVariable();
        }

        // Indirection
        if (this.match(TokenType.AT)) {
            const expression = this.parseExpression();
            return new MUMPSAST.IndirectionNode(
                expression,
                new MUMPSAST.Location(token.line, token.column)
            );
        }

        // Parenthesized expression
        if (this.match(TokenType.LPAREN)) {
            const expr = this.parseExpression();
            this.consume(TokenType.RPAREN, "Expected ')' after expression");
            return expr;
        }

        this.error(`Unexpected token: ${token.value}`);
        return new MUMPSAST.ErrorNode(
            `Unexpected token: ${token.value}`,
            'UNEXPECTED_TOKEN',
            new MUMPSAST.Location(token.line, token.column)
        );
    }

    parseVariable() {
        const token = this.peek();
        let scope = 'local';
        let name;

        if (this.match(TokenType.GLOBAL)) {
            scope = 'global';
            name = this.previous().value;
        } else if (this.match(TokenType.VARIABLE)) {
            name = this.previous().value;
        } else {
            this.error("Expected variable");
            return null;
        }

        const subscripts = [];

        // Parse subscripts
        if (this.match(TokenType.LPAREN)) {
            subscripts.push(...this.parseExpressionList());
            this.consume(TokenType.RPAREN, "Expected ')' after subscripts");
        }

        return new MUMPSAST.VariableNode(
            name,
            subscripts,
            scope,
            new MUMPSAST.Location(token.line, token.column)
        );
    }

    parseIntrinsicFunction(nameToken) {
        const name = nameToken.value;
        const args = [];

        if (this.match(TokenType.LPAREN)) {
            args.push(...this.parseExpressionList());
            this.consume(TokenType.RPAREN, `Expected ')' after ${name} arguments`);
        }

        return new MUMPSAST.IntrinsicFunctionNode(
            name,
            args,
            new MUMPSAST.Location(nameToken.line, nameToken.column)
        );
    }

    parseRoutineReference() {
        // Format: label^routine, ^routine, or label
        let label = null;
        let routine = null;

        if (this.match(TokenType.CARET)) {
            // ^ROUTINE
            if (this.check(TokenType.VARIABLE)) {
                routine = this.advance().value;
            }
        } else if (this.check(TokenType.VARIABLE)) {
            label = this.advance().value;
            if (this.match(TokenType.CARET)) {
                // label^routine
                if (this.check(TokenType.VARIABLE)) {
                    routine = this.advance().value;
                }
            }
        }

        return { label, routine };
    }

    parseExpressionList() {
        const expressions = [];

        if (!this.check(TokenType.RPAREN) && !this.isAtEnd()) {
            do {
                expressions.push(this.parseExpression());
            } while (this.match(TokenType.COMMA));
        }

        return expressions;
    }

    parseParameterList() {
        const parameters = [];

        if (!this.check(TokenType.RPAREN) && !this.isAtEnd()) {
            do {
                parameters.push(this.consume(TokenType.VARIABLE, "Expected parameter name").value);
            } while (this.match(TokenType.COMMA));
        }

        return parameters;
    }

    // ========================================================================
    // HELPER METHODS
    // ========================================================================

    match(...types) {
        for (const type of types) {
            if (this.check(type)) {
                this.advance();
                return true;
            }
        }
        return false;
    }

    check(type, offset = 0) {
        if (this.isAtEnd()) return false;
        return this.peek(offset).type === type;
    }

    checkCommand(commandName) {
        if (!this.check(TokenType.COMMAND)) return false;
        return this.peek().value === commandName;
    }

    advance() {
        if (!this.isAtEnd()) this.current++;
        return this.previous();
    }

    isAtEnd() {
        return this.current >= this.tokens.length ||
               this.peek().type === TokenType.EOF;
    }

    peek(offset = 0) {
        const index = this.current + offset;
        if (index >= this.tokens.length) {
            return this.tokens[this.tokens.length - 1] ||
                   { type: TokenType.EOF, value: '', line: 0, column: 0 };
        }
        return this.tokens[index];
    }

    previous() {
        return this.tokens[this.current - 1];
    }

    consume(type, message) {
        if (this.check(type)) return this.advance();

        this.error(message);
        return { type: TokenType.ERROR, value: '', line: 0, column: 0 };
    }

    error(message) {
        const token = this.peek();
        this.errors.push({
            message,
            line: token.line,
            column: token.column,
            severity: 'error',
            code: 'PARSE_ERROR'
        });
    }

    getErrors() {
        return [...this.errors, ...this.lexer.errors];
    }
}

// Export
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { MUMPSParser };
}

if (typeof window !== 'undefined') {
    window.MUMPSParser = MUMPSParser;
}
