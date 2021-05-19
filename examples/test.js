import {Translator} from "./parser.js";

export function test() {
    let t = new Translator();

    let tests = [
        ["3 + 4", "Expression"],
        ["3 negated", "Expression"],
        ["(1 to: 3)", "Expression"],
        ["foo: n [^ (1 to: n) detect: [:each | each even]]", "Method"]
    ];

    return tests.map(test => t.translate(...test));
}
