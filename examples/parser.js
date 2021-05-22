/* globals ohm */

/*
  Ohm grammar for SOM (som-st.github.io), a minimal Smalltalk for teaching and research.

  Based on https://github.com/SOM-st/SOM/blob/190fd72d5509bbfd5c190d3ed091920565cf79ae/specification/SOM.g4
  with some inspiration from https://github.com/moosetechnology/PetitParser/blob/development/src/PetitSmalltalk/PPSmalltalkGrammar.class.st
*/

let SmallroomGrammar = String.raw`
Smallroom {
    Top = ListOf<(ExpanderDef | Method), "."> spaces

    ExpanderDef =
      identifier "instVarNames:" "'" identifier* "'"

    Method = ClassSpec Pattern newBlock MethodBlock endBlock

    MethodBlock = BlockContents

    ClassSpec =
      | identifier
      | LiteralArray

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

    TopExpression = Expression

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

    pseudoVariable = nil | true | false | self | super | undefined

    primitive = "primitive" ~idRest
    nil = "nil" ~idRest
    undefined = "undefined" ~idRest
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

let grammar = ohm.grammar(SmallroomGrammar);
let semantics = grammar.createSemantics();

function getMessageArgs(message, system, expander) {
    const { ctorName } = message._node;
    switch (ctorName) {
    case 'KeywordMessage':
    case 'BinaryMessage':
        return message.child(1).toJS(system, expander);
    case 'UnaryMessage':
        return [];
    default:
        console.log(`unexpected node type: '${ctorName}'`);
    }
    return null;
}

semantics.addOperation("toJS(system, expander)", {
    Top(list, _s) {
        const {system, expander} = this.args;
        return list.asIteration().children.map((n) => n.toJS(system, expander));
    },

    ExpanderDef(ident, _key, _q1, vars, _q2) {
        const {system, expander} = this.args;
        const name = ident.toJS(system, expander);
        const varNames = vars.toJS(system, expander);
        system[name] = {_instVarNames: varNames};
        return `{type: "expander", name: '${name}', instVars: '${varNames}'}`;
    },

    Method(spec, pattern, _o, body, _c) {
        // Calculate the `lexicalVars` attribute on all nodes.
        this.lexicalVars; // eslint-disable-line no-unused-expressions
        const {system} = this.args;
        let expander = null;

        const selector = pattern.selector();
        const paramList = pattern.params(system, expander).join(", ");
        let ids;
        if (spec._node.childAt(0).ctorName === "LiteralArray") {
            ids = spec.toJS(system, expander);
        } else {
            let id = spec.toJS(system, expander);
            ids = `['${id}']`;
            expander = id;
        }

        const fn = body.toJS(system, expander);

        return `{type: "method", name: '${selector}', classes: ${ids}, fn: (self, ${paramList}) => {${fn}}}`;
    },

    MethodBlock(blockContentsOpt) {
        const bodyA = blockContentsOpt.toJS(this.args.system, this.args.expander);
        const body = bodyA;
        return `const _rv={};try{${body}}catch(e){if(e===_rv)return e.v;throw e}return this`;
    },

    BlockContents(_or, localDefsOpt, _, blockBody) {
        const {system, expander} = this.args;
        const defs = localDefsOpt.toJS(system, expander);
        const body = blockBody.toJS(system, expander);
        return defs.join('') + body;
    },

    LocalDefs(identifiers) {
        const {system, expander} = this.args;
        return `let ${identifiers.toJS(system, expander).join(',')};`;
    },

    BlockBody_return(_, result) {
        const {system, expander} = this.args;
        return `_rv.v=${result.toJS(system, expander)};throw _rv`;
    },

    BlockBody_rec(exp, _, blockBodyOptOpt) {
        const {system, expander} = this.args;
        const head = exp.toJS(system, expander);
        const tail = blockBodyOptOpt.toJS(system, expander)[0];
        if (tail === undefined) {
            return `return ${head}`;
        }
        return `${head};${tail}`;
    },

    TopExpression(exp) {
        this.lexicalVars; // eslint-disable-line no-unused-expressions

        const {system, expander} = this.args;
        return exp.toJS(system, expander);
    },

    Expression_assignment(ident, _, exp) {
        const {system, expander} = this.args;

        const expanderVars = system[expander] ? system[expander]._instVarNames : [];
        let id = ident.sourceString;
        let jsId = ident.toJS(system, expander);
        let expJS = exp.toJS(system, expander);

        if (expanderVars.indexOf(id) >= 0) {
            return `this._set('${id}', ${expJS})`;
        }

        if (this.lexicalVars[id]) {
            return `${jsId}=${expJS}`;
        }

        throw new Error(`${id} not found in context`);
    },

    KeywordExpression_rec(exp, message) {
        const {system, expander} = this.args;
        const selector = message.selector();
        const args = getMessageArgs(message, system, expander);
        const self = exp.toJS(system, expander);
        return `system.stCall('${selector}', ${self}, ${args})`;
    },

    BinaryExpression_rec(exp, message) {
        const {system, expander} = this.args;
        const selector = message.selector();
        const args = getMessageArgs(message, system, expander);
        const self = exp.toJS(system, expander);
        return `system.stCall('${selector}', ${self}, ${args})`;
    },

    UnaryExpression_rec(exp, message) {
        const {system, expander} = this.args;
        const selector = message.selector();
        const self = exp.toJS(system, expander);
        if (selector === "halt") {
            return '((() => {debugger})())';
        }
        return `system.stCall('${selector}', ${self})`;
    },

    Result(exp, _) {
        return exp.toJS(this.args.system, this.args.expander);
    },

    NestedTerm(_open, exp, _close) {
        return exp.toJS(this.args.system, this.args.expander);
    },

    NestedBlock(_open, blockPatternOpt, blockContentsOpt, _close) {
        const {system, expander} = this.args;
        return `{stClass: "Block", self, fn: (${blockPatternOpt.toJS(system, expander)})=>{${blockContentsOpt.toJS(system, expander)}}}`;
    },
    BlockPattern(blockArguments, _) {
        return blockArguments.toJS(this.args.system, this.args.expander);
    },
    BlockArguments(_, identIter) {
        return identIter.toJS(this.args.system, this.args.expander).join(',');
    },

    LiteralArray(_, _open, literalIter, _close) {
        return `[${literalIter.toJS(this.args.system, this.args.expander).join(',')}]`;
    },

    LiteralNumber_double(_, _double) {
        return this.sourceString;
    },

    LiteralNumber_int(_, _integer) {
        return this.sourceString;
    },
    LiteralSymbol(_, stringOrSelector) {
        return `${stringOrSelector.sourceString}`;
    },
    LiteralString(str) {
        return `${str.sourceString}`;
    },

    variable(pseudoVarOrIdent) {
        const {system, expander} = this.args;
        const expanderArgs = system[expander] ? system[expander]._instVarNames : [];
        if (pseudoVarOrIdent._node.ctorName === 'identifier') {
            if (expanderArgs.includes(pseudoVarOrIdent.sourceString)) {
                return `this._get('${pseudoVarOrIdent.sourceString}')`;
            }
            const id = pseudoVarOrIdent.toJS(system, expander);

            if (system[id]) {
                return `{stClass: "${id} class"}`;
            }

            if (this.lexicalVars[id]) {
                return id;
            }

            throw new Error(`${id} not found in the context`);
            //return id; // in this.lexicalVars ? id : `this.$${id}`;
        }
        return pseudoVarOrIdent.toJS(system, expander);
    },

    self(_) {
        return 'self';
    },

    undefined(_) {
        return 'undefined';
    },
    nil(_) {
        return 'null';
    },
    true(_) {
        return 'true';
    },
    false(_) {
        return 'false';
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
            Method(id, pattern, _o, body, _c) {
                return withEnv(pattern.identifiers(), () => {
                    body.lexicalVars; // eslint-disable-line no-unused-expressions
                });
            },

            TopExpression(exp) {
                return withEnv([], () => {
                    exp.lexicalVars; // eslint-disable-line no-unused-expressions
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

semantics.addOperation('selector', {
    Method(id, pattern, _o, _, _c) {
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

semantics.addOperation('params(system, expander)', {
    UnaryPattern(_) {
        return [];
    },
    BinaryPattern(_, param) {
        return [param.toJS(this.args.system, this.args.expander)];
    },
    KeywordPattern(_, params) {
        return params.toJS(this.args.system, this.args.expander);
    }
});

semantics.addOperation('hasPrimitiveMethods()', {
    _nonterminal(children) {
        return children.some(c => c.hasPrimitiveMethods());
    },
    _iter(children) {
        return children.some(c => c.hasPrimitiveMethods());
    },
    Method(id, pattern, _o, primitiveOrMethodBlock, _c) {
        return primitiveOrMethodBlock._node.ctorName === 'primitive';
    },
    _terminal() {
        return false;
    }
});

export class Translator {
    parse(str, optRule) {
        return grammar.match(str, optRule || "Top");
    }

    translate(system, str, optRule) {
        let match = this.parse(str, optRule);
        if (match.succeeded()) {
            let s = semantics(match);
            return s.toJS(system, null);
        }

        let error = {};
        error.reason = "parse error";
        error.expected = "Expected: " + match.getExpectedText();
        error.pos = match.getRightmostFailurePosition();
        error.src = str;
        error.around = str.slice(error.pos - 10, error.pos + 10);
        console.log(error);
        return null;
    }
}
