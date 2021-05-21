/* 

Model intVarNames: ‘a b c’.

initialize [
    a := 100.
    b := 200.
    self foo: a + b.
]

foo: x [
]

foo: x with: [
]

pointerDown: event [
]


Object expanderNamed: #Button


on: #initialize [
]

on: #pointerdown with: event [
].

fire [
   self publishInScope: scope event: event data: data
].

*/

