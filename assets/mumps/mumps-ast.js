/**
 * MUMPS Abstract Syntax Tree (AST) Node Definitions
 * Defines the structure of parsed MUMPS code
 */

class ASTNode {
    constructor(type, location = null) {
        this.type = type;
        this.location = location; // {line, column, length}
    }
}

// ============================================================================
// TOP-LEVEL NODES
// ============================================================================

class RoutineNode extends ASTNode {
    constructor(name, labels = [], location = null) {
        super('Routine', location);
        this.name = name;
        this.labels = labels; // Array of LabelNode
    }
}

class LabelNode extends ASTNode {
    constructor(name, parameters = [], commands = [], comment = null, location = null) {
        super('Label', location);
        this.name = name;
        this.parameters = parameters; // Array of strings
        this.commands = commands; // Array of CommandNode
        this.comment = comment; // String or null
    }
}

// ============================================================================
// COMMAND NODES
// ============================================================================

class CommandNode extends ASTNode {
    constructor(commandType, location = null) {
        super('Command', location);
        this.commandType = commandType;
    }
}

class SetCommandNode extends CommandNode {
    constructor(assignments, location = null) {
        super('SET', location);
        this.assignments = assignments; // Array of {target, value}
    }
}

class WriteCommandNode extends CommandNode {
    constructor(expressions, location = null) {
        super('WRITE', location);
        this.expressions = expressions; // Array of ExpressionNode
    }
}

class DoCommandNode extends CommandNode {
    constructor(target, parameters = [], location = null) {
        super('DO', location);
        this.target = target; // {label, routine} or null for argumentless DO
        this.parameters = parameters; // Array of ExpressionNode
    }
}

class QuitCommandNode extends CommandNode {
    constructor(expression = null, location = null) {
        super('QUIT', location);
        this.expression = expression; // ExpressionNode or null
    }
}

class IfCommandNode extends CommandNode {
    constructor(condition, thenCommands = [], elseCommands = [], location = null) {
        super('IF', location);
        this.condition = condition; // ExpressionNode
        this.thenCommands = thenCommands; // Array of CommandNode
        this.elseCommands = elseCommands; // Array of CommandNode
    }
}

class ForCommandNode extends CommandNode {
    constructor(variable, initializer = null, increment = null, condition = null, commands = [], location = null) {
        super('FOR', location);
        this.variable = variable; // String
        this.initializer = initializer; // ExpressionNode or null
        this.increment = increment; // ExpressionNode or null
        this.condition = condition; // ExpressionNode or null
        this.commands = commands; // Array of CommandNode
    }
}

class NewCommandNode extends CommandNode {
    constructor(variables, location = null) {
        super('NEW', location);
        this.variables = variables; // Array of strings
    }
}

class KillCommandNode extends CommandNode {
    constructor(variables, location = null) {
        super('KILL', location);
        this.variables = variables; // Array of VariableNode
    }
}

class HaltCommandNode extends CommandNode {
    constructor(location = null) {
        super('HALT', location);
    }
}

class HangCommandNode extends CommandNode {
    constructor(duration, location = null) {
        super('HANG', location);
        this.duration = duration; // ExpressionNode
    }
}

class ReadCommandNode extends CommandNode {
    constructor(target, prompt = null, timeout = null, location = null) {
        super('READ', location);
        this.target = target; // VariableNode
        this.prompt = prompt; // String or null
        this.timeout = timeout; // ExpressionNode or null
    }
}

class GotoCommandNode extends CommandNode {
    constructor(target, location = null) {
        super('GOTO', location);
        this.target = target; // {label, routine, offset}
    }
}

class MergeCommandNode extends CommandNode {
    constructor(target, source, location = null) {
        super('MERGE', location);
        this.target = target; // VariableNode
        this.source = source; // VariableNode
    }
}

class LockCommandNode extends CommandNode {
    constructor(variables, timeout = null, increment = true, location = null) {
        super('LOCK', location);
        this.variables = variables; // Array of VariableNode
        this.timeout = timeout; // ExpressionNode or null
        this.increment = increment; // Boolean (true for +, false for -)
    }
}

class OpenCommandNode extends CommandNode {
    constructor(device, parameters = [], location = null) {
        super('OPEN', location);
        this.device = device; // ExpressionNode
        this.parameters = parameters; // Array of ExpressionNode
    }
}

class UseCommandNode extends CommandNode {
    constructor(device, parameters = [], location = null) {
        super('USE', location);
        this.device = device; // ExpressionNode
        this.parameters = parameters; // Array of ExpressionNode
    }
}

class CloseCommandNode extends CommandNode {
    constructor(device, location = null) {
        super('CLOSE', location);
        this.device = device; // ExpressionNode
    }
}

// ============================================================================
// EXPRESSION NODES
// ============================================================================

class ExpressionNode extends ASTNode {
    constructor(expressionType, location = null) {
        super('Expression', location);
        this.expressionType = expressionType;
    }
}

class LiteralNode extends ExpressionNode {
    constructor(value, dataType, location = null) {
        super('Literal', location);
        this.value = value;
        this.dataType = dataType; // 'string' | 'number'
    }
}

class VariableNode extends ExpressionNode {
    constructor(name, subscripts = [], scope = 'local', location = null) {
        super('Variable', location);
        this.name = name;
        this.subscripts = subscripts; // Array of ExpressionNode
        this.scope = scope; // 'local' | 'global' | 'naked'
    }
}

class BinaryOperationNode extends ExpressionNode {
    constructor(operator, left, right, location = null) {
        super('BinaryOperation', location);
        this.operator = operator; // +, -, *, /, \, #, _, &, !, etc.
        this.left = left; // ExpressionNode
        this.right = right; // ExpressionNode
    }
}

class UnaryOperationNode extends ExpressionNode {
    constructor(operator, operand, location = null) {
        super('UnaryOperation', location);
        this.operator = operator; // +, -, '
        this.operand = operand; // ExpressionNode
    }
}

class IntrinsicFunctionNode extends ExpressionNode {
    constructor(name, arguments_ = [], location = null) {
        super('IntrinsicFunction', location);
        this.name = name; // $DATA, $GET, $ORDER, etc.
        this.arguments = arguments_; // Array of ExpressionNode
    }
}

class ExtrinsicFunctionNode extends ExpressionNode {
    constructor(label, routine = null, arguments_ = [], location = null) {
        super('ExtrinsicFunction', location);
        this.label = label;
        this.routine = routine;
        this.arguments = arguments_; // Array of ExpressionNode
    }
}

class PatternMatchNode extends ExpressionNode {
    constructor(expression, pattern, location = null) {
        super('PatternMatch', location);
        this.expression = expression; // ExpressionNode
        this.pattern = pattern; // String or PatternNode
    }
}

class IndirectionNode extends ExpressionNode {
    constructor(expression, location = null) {
        super('Indirection', location);
        this.expression = expression; // ExpressionNode
    }
}

// ============================================================================
// SPECIAL NODES
// ============================================================================

class CommentNode extends ASTNode {
    constructor(text, location = null) {
        super('Comment', location);
        this.text = text;
    }
}

class PostConditionalNode extends ASTNode {
    constructor(condition, location = null) {
        super('PostConditional', location);
        this.condition = condition; // ExpressionNode
    }
}

class ErrorNode extends ASTNode {
    constructor(message, code = 'PARSE_ERROR', location = null) {
        super('Error', location);
        this.message = message;
        this.code = code;
    }
}

// ============================================================================
// HELPER CLASSES
// ============================================================================

class Location {
    constructor(line, column, length = 1) {
        this.line = line;
        this.column = column;
        this.length = length;
    }

    toString() {
        return `Line ${this.line}, Column ${this.column}`;
    }
}

class ParseError {
    constructor(message, location, code = 'PARSE_ERROR', severity = 'error') {
        this.message = message;
        this.location = location;
        this.code = code;
        this.severity = severity; // 'error' | 'warning' | 'info'
    }

    toString() {
        return `[${this.code}] ${this.location}: ${this.message}`;
    }
}

// ============================================================================
// VISITOR PATTERN FOR AST TRAVERSAL
// ============================================================================

class ASTVisitor {
    visit(node) {
        if (!node) return null;

        const methodName = `visit${node.type}`;
        if (this[methodName]) {
            return this[methodName](node);
        }
        return this.visitDefault(node);
    }

    visitDefault(node) {
        // Default behavior: visit all child nodes
        for (const key in node) {
            if (node.hasOwnProperty(key) && key !== 'type' && key !== 'location') {
                const value = node[key];
                if (Array.isArray(value)) {
                    value.forEach(child => this.visit(child));
                } else if (value && typeof value === 'object' && value.type) {
                    this.visit(value);
                }
            }
        }
    }

    visitRoutine(node) { this.visitDefault(node); }
    visitLabel(node) { this.visitDefault(node); }
    visitCommand(node) { this.visitDefault(node); }
    visitExpression(node) { this.visitDefault(node); }
    visitComment(node) { this.visitDefault(node); }
    visitError(node) { this.visitDefault(node); }
}

// ============================================================================
// EXPORTS
// ============================================================================

if (typeof module !== 'undefined' && module.exports) {
    // Node.js export
    module.exports = {
        ASTNode,
        RoutineNode,
        LabelNode,
        CommandNode,
        SetCommandNode,
        WriteCommandNode,
        DoCommandNode,
        QuitCommandNode,
        IfCommandNode,
        ForCommandNode,
        NewCommandNode,
        KillCommandNode,
        HaltCommandNode,
        HangCommandNode,
        ReadCommandNode,
        GotoCommandNode,
        MergeCommandNode,
        LockCommandNode,
        OpenCommandNode,
        UseCommandNode,
        CloseCommandNode,
        ExpressionNode,
        LiteralNode,
        VariableNode,
        BinaryOperationNode,
        UnaryOperationNode,
        IntrinsicFunctionNode,
        ExtrinsicFunctionNode,
        PatternMatchNode,
        IndirectionNode,
        CommentNode,
        PostConditionalNode,
        ErrorNode,
        Location,
        ParseError,
        ASTVisitor
    };
}

// Browser export
if (typeof window !== 'undefined') {
    window.MUMPSAST = {
        ASTNode,
        RoutineNode,
        LabelNode,
        CommandNode,
        SetCommandNode,
        WriteCommandNode,
        DoCommandNode,
        QuitCommandNode,
        IfCommandNode,
        ForCommandNode,
        NewCommandNode,
        KillCommandNode,
        HaltCommandNode,
        HangCommandNode,
        ReadCommandNode,
        GotoCommandNode,
        MergeCommandNode,
        LockCommandNode,
        OpenCommandNode,
        UseCommandNode,
        CloseCommandNode,
        ExpressionNode,
        LiteralNode,
        VariableNode,
        BinaryOperationNode,
        UnaryOperationNode,
        IntrinsicFunctionNode,
        ExtrinsicFunctionNode,
        PatternMatchNode,
        IndirectionNode,
        CommentNode,
        PostConditionalNode,
        ErrorNode,
        Location,
        ParseError,
        ASTVisitor
    };
}
