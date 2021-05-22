import {Evaluator, System, addDOM} from "./evaluator.js";

const library = `
Morph instVarNames: 'angle'.

Morph initialize [
   self addEventListener: #onClick for: 'click'.
   self style width: 50.
   self style height: 40.
   self style backgroundColor: #blue.
   angle := 0.
].

Morph onClick: evt [
   | a |
   a := self style width.
   self style width: a + 10.
   angle := angle + 1.

   self rotateTo: angle.
].

Morph openIn: parent [
   | elem |
   elem := Element new.
   elem addExpander: Morph.

   parent appendChild: elem.
]

`;

const action = `Morph openIn: (document querySelector: #playground)`;

export function test() {
    let ev = new Evaluator();
    let system = new System();
    addDOM(system);

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
