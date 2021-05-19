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

use TriangleShapeView


/* 
  3 + 4
          => 3 + 4;

  self a: 3 b: 4
          => this.a_b(3, 4);

  3 negated
          => getClass(3).negated(3);

  #(1 2 3)
          => [1, 2, 3]

  "a" => "a"

  [:a :b | a + b]
      => (a, b) => {return a + b;}

  [:a :b | ^ a + b]
      => (a, b) => {throw new ReturnValue(a + b);}

  foo [
     ^ (1 to: 3) detect: [:i | i even]
  ]

  foo() {
     return 1.to(3).detect((i) => return classOf(i).even(i));
  }

  detect: aBlock [
     self do: [:each | (aBlock value: each) ifTrue: [^ each]].
     ^ null
  ]

  detect() {
     try {
       this._do((each) => if (aBlock(each)) {throw new ReturnValue(each);});
     } catch (e) {
       if (e instanceof ReturnValue) {
         return e.value;
       }
       throw e;
     }
     return null;
  }


Array new: 10.

this._array.['new:'](10) 

 => Array(10)

a at: 3 put: 5 

 ==> stClassOf(a)['at:put:'](3, 5)



stClassOf(a) {
   if (a === undefined) {
      return system.UndefinedObject;
   }

   if (a === null) {
      return system.NullObject;
   }

   let type = typeof a;

   if (type === "number") {
      return system.Number;
   }

   if (type === "object") {
      if (a.constructor === Array) {
        return system.Array;
      }

      if (a.constructor === Map) {
        return system.Dictionary;
      }
   }



   
*/
