import {Evaluator, System} from "./evaluator.js";

const library = `
Morph instVarNames: 'a b c'.

Morph onClick: evt [
   | d |
   a := 10.
   d := 10.
]

`;

export function test() {
    let ev = new Evaluator();
    let system = new System();

    let defs = ev.compile(system, library);

    defs.forEach((def) => {
        if (def.type === "method") {
            system.defineMethod(def);
        }
        if (def.type === "expander") {
            system.defineExpander(def);
        }
    });
    return system;
}

/*

export function test() {
    let ev = new Evaluator();

    let tests = [
        ["3 + 4", "Expression"],
        ["'a', 'b'", "Expression"],
        ["3 negated", "Expression"],
        ["((2 - 1) to: 3)", "Expression"],
        ["((2 - 1) to: 3) collect: [:each | each + 3]", "Expression"],
        ["#(1 4 9) detect: [:each | each = 4]", "Expression"],
        ["((2 - 1) to: 3) detect: [:each | each = 4]", "Expression"],
        ["(1 to: 3) withIndexDo: [:each :index | each * index]", "Expression"],

    ];

    return tests.map(test => ev.evaluate(test[0]));
}

*/
