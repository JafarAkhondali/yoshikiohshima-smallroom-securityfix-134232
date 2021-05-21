import {Translator} from "./parser.js";

export class System {
    constructor() {
        this['Array class'] = {
            'new:'(self, length) {return new Array(length);},
            'newFrom:'(self, ary) {
                if (self.constructor === Array) {return ary.slice();}
                if (self.constructor === Map) {return Array.newFrom(self);}
                if (self.stClass === "Interval") {
                    let result = [];
                    let by = self.by || 1;
                    let v = self.start;
                    while (v <= self.end) {
                        result.push(v);
                        v += by;
                    }
                    return result;
                }
                return null;
            }
        };

        this['Block'] = {
            'value'(self) {return self.fn.call(self.self);},
            'value:'(self, v1) {return self.fn.call(self.self, v1);},
            'value:value:'(self, v1, v2) {return self.fn.call(self.self, v1, v2);},
        };

        this['Number'] = {
            '+'(self, other) {return self + other;},
            '-'(self, other) {return self - other;},
            '*'(self, other) {return self * other;},
            '/'(self, other) {return self / other;},
            '//'(self, other) {Math.floor(self / other);},
            '\\'(self, other) {const q = Math.floor(self / other); return self - q * other;},
            'sqrt'(self) {return Math.sqrt(self);},
            'atRandom'(self) {
                if (Number.isInteger) {return Math.floor(Math.random() * (self + 1));}
                return Math.random() * self;
            },
            '='(self, other) {return self === other;},
            '<'(self, other) {return self < other;},
            '>'(self, other) {return self > other;},
            'asString'(self) {return self.toString();},
            'negated'(self) {return -self;},
            'to:'(self, to, by) {return {stClass: "Interval", start: self, end: to, by};}
        };

        this['Number class'] = {
            'fromString:'(self, aString) {return parseFloat(aString);}
        };

        this['Boolean'] = {
            'ifTrue:'(self, block) {
                if (self === true) {block.fn.call(block.self);}
                return self;
            },
            'ifFalse:'(self, block) {
                if (self === false) {block.fn.call(block.self);}
                return self;
            },
            'ifTrue:ifFalse:'(self, trueBlock, falseBlock) {
                if (self === true) {trueBlock.fn.call(trueBlock.self);}
                if (self === false) {false.fn.call(falseBlock.self);}
                return self;
            },
        };

        this['String'] = {
            ','(self, other) {return self + other;},
            'size'(self) {return self.length;},
            'asUppercase'(self) {return self.toUpperCase();},
            '='(self, other) {return self === other;}
        };

        this['Dictionary'] = {
            'at:'(self, key) {return self.get(key);},
            'at:put:'(self, key, value) {return self.set(key, value);},
            'includesKey:'(self, key) {return self.has(key);},
            'do:'(self, block) {
                for (let value of self.values()) {block.fn.call(block.self, value);}
                return self;
            },
        };

        this['Interval'] = {
            'at:'(self, index) {
                const by = self.by || 1;
                return (index - 1) * by + self.start;
            },
            'size'(self) {
                const by = self.by || 1;
                return (self.end - self.start) / by  + 1;
            },

            'do:'(self, block) {
                const by = self.by || 1;
                for (let i = self.start; i <= self.end; i += by) {
                    block.fn.call(block.self, i);
                }
                return self;
            }
        };

        this['Array'] = {
            'at:'(self, index) {return self[index - 1];},
            'at:put:'(self, index, value) {return self[index - 1] = value;},
            'do:'(self, block) {
                for (let i = 1; i <= self.length; i++) {
                    block.fn.call(block.self, self[i]);
                }
                return self;
            },
            'size'(self) {return self.length;}
        };
    }

    stCall(sel, rec, ...args) {
        let cls = this.stClassOf(rec);
        if (!cls) {throw new Error("receiver is not a Smalltalk object");}
        let mth = cls[sel];
        if (!mth) {throw new Error(`message ${sel} not understood`);}
        return mth(rec, ...args);
    }

    stClassOf(a) {
        if (a === undefined) {
            return this.UndefinedObject;
        }

        if (a === null) {
            return this.NullObject;
        }

        let type = typeof a;

        if (type === "number") {
            return this.Number;
        }

        if (type === "boolean") {
            return this.Boolean;
        }

        if (type === "string") {
            return this.String;
        }

        if (type === "object") {
            if (a.constructor === Array) {
                return this.Array;
            }

            if (a.constructor === Map) {
                return this.Dictionary;
            }

            if (a.stClass) {
                return this[a.stClass];
            }
        }

        return null;
    }

    defineMethod(obj) {
        const {classes, name, fn} = obj;
        classes.forEach((systemKey) => this[systemKey][name] = fn);
    }

    defineExpander(obj) {
        //const {name, instVars} = obj;
        //this[name] = {_instVars: instVarNamess.split(',')};
    }
}

export class Evaluator {
    evaluate(system, str, rcvr) {
        let translator = new Translator();
        let st = translator.translate(str, "Expression");
        st = 'return (' + st + ')';

        return (new Function("system", "self", st))(system, rcvr || null);
    }

    compile(system, str) {
        let translator = new Translator();
        let sts = translator.translate(system, str, "Top");
        return sts.map((st) => {
            st = 'return (' + st + ')';
            let obj = (new Function(st))();
            return obj;
        });
    }
}
