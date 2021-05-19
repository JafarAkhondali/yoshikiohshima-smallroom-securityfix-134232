/* globals ohm */

/*
  Ohm grammar for SOM (som-st.github.io), a minimal Smalltalk for teaching and research.

  Based on https://github.com/SOM-st/SOM/blob/190fd72d5509bbfd5c190d3ed091920565cf79ae/specification/SOM.g4
  with some inspiration from https://github.com/moosetechnology/PetitParser/blob/development/src/PetitSmalltalk/PPSmalltalkGrammar.class.st
*/

let SOMGrammar = String.raw`
SOM {
    ExpanderDef =
      identifier "instVarNames:" "'" identifier* "'"

    Method = Pattern newBlock MethodBlock endBlock

    MethodBlock = BlockContents

    Pattern = UnaryPattern | BinaryPattern | KeywordPattern

    UnaryPattern = unarySelector

    BinaryPattern = binarySelector identifier

    KeywordPattern = (keyword identifier)+

    BlockContents = (or LocalDefs or)? BlockBody

    LocalDefs = identifier*

    BlockBody =
      | exit Result  -- return
      | Expression (period BlockBody?)? -- rec

    Result = Expression period?

    Expression =
      | variable assign Expression  -- assignment
      | KeywordExpression

    KeywordExpression =
      | KeywordExpression KeywordMessage  -- rec
      | BinaryExpression

    KeywordMessage = (keyword BinaryExpression)+

    BinaryExpression =
      | BinaryExpression BinaryMessage  -- rec
      | UnaryExpression

    BinaryMessage = binarySelector UnaryExpression

    UnaryExpression =
      | UnaryExpression UnaryMessage  -- rec
      | Primary

    UnaryMessage = unarySelector

    Primary = variable | NestedTerm | NestedBlock | Literal

    NestedTerm = newTerm Expression endTerm

    Literal = LiteralArray | LiteralSymbol | LiteralString | LiteralNumber

    LiteralArray = pound newTerm Literal* endTerm

    LiteralNumber =
      | minus? double  -- double
      | minus? integer  -- int

    LiteralSymbol = pound (string | selector)

    LiteralString = string

    NestedBlock = newBlock BlockPattern? BlockContents? endBlock

    BlockPattern = BlockArguments or

    BlockArguments = (colon identifier)+

    // Lexical rules

    selector = unarySelector | binarySelector | keywordSelector

    unarySelector = (primitive | identifier) ~colon
    binarySelector = operatorSequence | operator
    keywordSelector = keyword+
    keyword = identifier colon

    comment = quote (~quote any)* quote
    quote = "\""
    space += comment

    variable = pseudoVariable | identifier

    identifier (an identifier) = letter idRest*
    idRest = letter | digit | "_"

    pseudoVariable = nil | true | false | self | super

    primitive = "primitive" ~idRest
    nil = "nil" ~idRest
    true = "true" ~idRest
    false = "false" ~idRest
    self = "self" ~idRest
    super = "super" ~idRest

    equal = "="

    separator = "----" "-"*

    newTerm = "("
    endTerm = ")"
    or = "|"

    comma = ","
    minus = "-"
    not = "~"
    and = "&"
    star = "*"
    div = "/"
    mod = "\\"
    plus = "+"
    more = ">"
    less = "<"
    at = "@"
    per = "%"

    operator =
      not | and | or | star | div | mod | plus | equal | more | less | comma | at | per | minus
    operatorSequence = ~separator operator+

    newBlock = "["
    endBlock = "]"

    colon = ":"
    pound = "#"
    exit = "^"
    period = "."
    assign = ":="

    integer = digit+
    double = digit+ "." digit+

    string = "'" (escapeChar | ~("'" | "\\") any)* "'"

    escapeChar (an escape sequence) =
      | "\\t"  -- tab
      | "\\b"  -- backspace
      | "\\n"  -- lineFeed
      | "\\r"  -- carriageReturn
      | "\\f"  -- formFeed
      | "\\0"  -- null
      | "\\\'"  -- singleQuote
      | "\\\\"  -- backslash
}
`;

let grammar = ohm.grammar(SOMGrammar);
let semantics = grammar.createSemantics();

function getMessageArgs(message) {
    const { ctorName } = message._node;
    switch (ctorName) {
    case 'KeywordMessage':
    case 'BinaryMessage':
        return message.child(1).toJS();
    case 'UnaryMessage':
        return [];
    default:
        console.log(`unexpected node type: '${ctorName}'`);
    }
    return null;
}

semantics.addOperation("toJS", {
    ExpanderDef(ident, _key, _q1, vars, _q2) {
        const name = ident.toJS();
        let varNames = vars.toJS();
        return `class ${name} {
                    static instVars() {return ${varNames};}
                }`;

    },

    Method(pattern, _o, body, _c) {
        // Calculate the `lexicalVars` attribute on all nodes.
        this.lexicalVars; // eslint-disable-line no-unused-expressions

        const selector = pattern.selector();
        const paramList = pattern.params().join(", ");
        return `'${selector}'(${paramList}){${body.toJS()}}`;
    },

    MethodBlock(blockContentsOpt) {
        const bodyA = blockContentsOpt.toJS();
        const body = bodyA;
        return `const _rv={};try{${body}}catch(e){if(e===_rv)return e.v;throw e}return this`;
    },

    BlockContents(_or, localDefsOpt, _, blockBody) {
        const defs = localDefsOpt.toJS();
        const body = blockBody.toJS();
        return defs.join('') + body;
    },

    LocalDefs(identifiers) {
        return `let ${identifiers.toJS().join(',')};`;
    },

    BlockBody_return(_, result) {
        return `_rv.v=${result.toJS()};throw _rv`;
    },

    BlockBody_rec(exp, _, blockBodyOptOpt) {
        const head = exp.toJS();
        const tail = blockBodyOptOpt.toJS()[0];
        if (tail === undefined) {
            return `return ${head}`;
        }
        return `${head};${tail}`;
    },

    Expression_assignment(ident, _, exp) {
        return `${ident.toJS()}=${exp.toJS()}`;
    },

    KeywordExpression_rec(exp, message) {
        const selector = message.selector();
        const args = getMessageArgs(message);
        return `${exp.toJS()}['${selector}'](${args})`;
    },

    BinaryExpression_rec(exp, message) {
        const selector = message.selector();
        const args = getMessageArgs(message);
        return `${exp.toJS()}['${selector}'](${args})`;
    },
    UnaryExpression_rec(exp, message) {
        const selector = message.selector();
        const args = getMessageArgs(message);
        return `${exp.toJS()}.${selector}(${args})`;
    },

    Result(exp, _) {
        return exp.toJS();
    },

    NestedTerm(_open, exp, _close) {
        return exp.toJS();
    },

    NestedBlock(_open, blockPatternOpt, blockContentsOpt, _close) {
        const arity = this.blockArity() + 1;
        // Block1 takes 0 args, Block2 takes 1, etc.
        return `this._block${arity}((${blockPatternOpt.toJS()})=>{${blockContentsOpt.toJS()}})`;
    },
    BlockPattern(blockArguments, _) {
        return blockArguments.toJS();
    },
    BlockArguments(_, identIter) {
        return identIter.toJS().join(',');
    },

    LiteralArray(_, _open, literalIter, _close) {
        return `[${literalIter.toJS().join(',')}]`;
    },

    LiteralNumber_double(_, _double) {
        return `${this.sourceString}`;
    },

    LiteralNumber_int(_, _integer) {
        return `this._int(${this.sourceString})`;
    },
    LiteralSymbol(_, stringOrSelector) {
        return `this.$Symbol._new(${stringOrSelector.asString()})`;
    },
    LiteralString(str) {
        return `this.$String._new(${str.asString()})`;
    },

    variable(pseudoVarOrIdent) {
        if (pseudoVarOrIdent._node.ctorName === 'identifier') {
            const id = pseudoVarOrIdent.toJS();
            return id in this.lexicalVars ? id : `this.$${id}`;
        }
        return pseudoVarOrIdent.toJS();
    },

    self(_) {
        return 'this';
    },

    super(_) {
        return 'this._super(this)';
    },
    nil(_) {
        return 'this.$nil';
    },
    true(_) {
        return 'this.$true';
    },
    false(_) {
        return 'this.$false';
    },

    identifier(_first, _rest) {
        const id = this.sourceString;
        return jsReservedWords.includes(id) ? `_${id}` : id;
    },
});

semantics.addAttribute(
    'lexicalVars',
    (() => {
        const envStack = [Object.create(null)];

        const withEnv = (ids, fn) => {
            const env = Object.create(envStack[envStack.length - 1]);
            ids.forEach(id => {
                env[id] = id;
            });
            envStack.push(env);
            fn();
            return envStack.pop();
        };

        return {
            Method(pattern, _o, body, _c) {
                return withEnv(pattern.identifiers(), () => {
                    body.lexicalVars; // eslint-disable-line no-unused-expressions
                });
            },

            BlockContents(_, localDefsOpt, _1, blockBody) {
                const localDefs = localDefsOpt.child(0);
                const ids = localDefs ? localDefs.identifiers() : [];
                return withEnv(ids, () => {
                    blockBody.lexicalVars; // eslint-disable-line no-unused-expressions
                });
            },
            NestedBlock(_, blockPatternOpt, blockContentsOpt, _1) {
                const blockPattern = blockPatternOpt.child(0);
                const ids = blockPattern ? blockPattern.identifiers() : [];
                return withEnv(ids, () => {
                    blockContentsOpt.lexicalVars; // eslint-disable-line no-unused-expressions
                });
            },
            _nonterminal(children) {
                children.forEach(c => c.lexicalVars);
                return envStack[envStack.length - 1];
            },
            _terminal() {
                return envStack[envStack.length - 1];
            }
        };
    })()
);

semantics.addOperation('identifiers()', {
    UnaryPattern(_selector) {
        return [];
    },

    BinaryPattern(selector, ident) {
        return ident.identifiers();
    },
    KeywordPattern(keywordIter, identIter) {
        return identIter.identifiers();
    },
    BlockPattern(blockArguments, _) {
        return blockArguments.identifiers();
    },
    BlockArguments(_, identIter) {
        return identIter.identifiers();
    },
    _nonterminal(children) {
        return children.flatMap(c => c.identifiers());
    },
    _iter(children) {
        return children.flatMap(c => c.identifiers());
    },
    identifier(_first, _rest) {
        return [this.sourceString];
    }
});

// From https://262.ecma-international.org/11.0/#sec-keywords-and-reserved-words
// prettier-ignore
const jsReservedWords = [
  // Reserved words:
  'await', 'break', 'case', 'catch', 'class', 'const', 'continue', 'debugger',
  'default', 'delete', 'do', 'else', 'enum', 'export', 'extends', 'false',
  'finally', 'for', 'function', 'if', 'import', 'in', 'instance', 'of', 'new',
  'null', 'return', 'super', 'switch', 'this', 'throw', 'true', 'try',
  'typeof', 'var', 'void', 'while', 'with', 'yield',

  // Contextually disallowed as identifiers, in strict mode code:
  'let', 'static', 'implements', 'interface', 'package', 'private', 'protected', 'public',

  // Not keywords, but subject to some restrictions in strict mode code:
  'arguments', 'eval'
];

semantics.addOperation('blockArity', {
    NestedBlock(_open, blockPatternOpt, _blockContentsOpt, _close) {
        const blockPattern = blockPatternOpt.child(0);
        return blockPattern ? blockPattern.blockArity() : 0;
    },
    BlockPattern(blockArguments, _) {
        return blockArguments.blockArity();
    },
    BlockArguments(_, identIter) {
        return identIter._node.numChildren();
    }
});

semantics.addOperation('selector', {
    Method(pattern, _o, _, _c) {
        return pattern.selector();
    },
    UnaryPattern(selector) {
        return selector.sourceString;
    },
    UnaryMessage(selector) {
        return selector.sourceString;
    },
    BinaryPattern(selector, _) {
        return selector.sourceString;
    },
    BinaryMessage(selector, _) {
        return selector.sourceString;
    },
    KeywordPattern(keywordIter, _) {
        return keywordIter.children.map(c => c.sourceString).join('');
    },
    KeywordMessage(keywordIter, _) {
        return keywordIter.children.map(c => c.sourceString).join('');
    }
});

semantics.addOperation('asString', {
    keyword(_ident, _) {
        return `'${this.sourceString}'`;
    },
    unarySelector(_) {
        return `'${this.sourceString}'`;
    },
    binarySelector(_) {
        return `'${this.sourceString}'`;
    },
    keywordSelector(_keywordIter) {
        return `'${this.sourceString}'`;
    },
    string(_open, charIter, _close) {
        return '`' + charIter.sourceString + '`';
    }
});

semantics.addOperation('params', {
    UnaryPattern(_) {
        return [];
    },
    BinaryPattern(_, param) {
        return [param.toJS()];
    },
    KeywordPattern(_, params) {
        return params.toJS();
    }
});

semantics.addOperation('hasPrimitiveMethods()', {
    _nonterminal(children) {
        return children.some(c => c.hasPrimitiveMethods());
    },
    _iter(children) {
        return children.some(c => c.hasPrimitiveMethods());
    },
    Method(pattern, _o, primitiveOrMethodBlock, _c) {
        return primitiveOrMethodBlock._node.ctorName === 'primitive';
    },
    _terminal() {
        return false;
    }
});

export class Translator {
    parse(str, optRule) {
        return grammar.match(str, optRule || "ExpanderDef");
    }

    translate(str, optRule) {
        let match = this.parse(str, optRule);
        if (match.succeeded()) {
            let s = semantics(match);
            return s.toJS();
        }
        return null;
    }
}
