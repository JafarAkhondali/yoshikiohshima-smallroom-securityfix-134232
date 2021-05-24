import {Evaluator, System} from "./evaluator.js";

/*
const library = `
Morph instVars: 'angle'.

Morph initialize [
   self addEventListenerFor: 'click' with: #onClick:.
   self style width: 50.
   self style height: 40.
   self style backgroundColor: 'blue'.
   angle := 0.
].

Morph onClick: evt [
   | a |
   a := self style width.
   self style width: a + 10.
   angle := angle + 1.

   self rotateTo: angle.
].

Morph class openIn: parent [
   | elem |
   elem := Element new.
   elem addExpander: Morph.

   parent appendChild: elem.
]

`;

const action = `Morph openIn: (document querySelector: #room)`;

*/

const library = `
#('Interval' 'Dictionary' 'Array') collect: aBlock [
    | result i |
    result := Array new: self size.
    i := 1.
    self do: [:each | result at: i put: (aBlock value: each). i := i + 1].
    ^ result
].

#('Interval' 'Dictionary' 'Array') detect: aBlock [
    self do: [:each | (aBlock value: each) ifTrue: [^ each]].
    ^ nil
].

#('Interval' 'Dictionary' 'Array') withIndexDo: aBlock [
    | i |
    i := 1.
    self do: [:each | aBlock value: each value: i. i := i + 1].
    ^ self.
].

Array class spiecies [^'foo']
`;

/*

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

*/

function addLibrary(system) {
    let ev = new Evaluator();
    ev.compile(system, library);
    return system;
}

export function test() {
    let ev = new Evaluator();
    let system = new System();

    addLibrary(system);
    let tests = [
        ["3 + 4"],
        ["#click"],
        ["#click:"],
        ["'a', 'b'"],
        ["3 negated"],
        ["((2 - 1) to: 3)"],
        ["((2 - 1) to: 3) collect: [:each | each + 3]"],
        ["#(1 4 9) detect: [:each | each = 4]"],
        ["((2 - 1) to: 3) detect: [:each | each = 4]"],
        ["(1 to: 3) withIndexDo: [:each :index | each * index]"],
    ];
    return tests.map(t => ev.evaluate(system, t));
}
