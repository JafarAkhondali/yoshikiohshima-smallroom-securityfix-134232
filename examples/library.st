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
    ^ nil
]
