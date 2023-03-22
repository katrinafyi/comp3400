# comp3400
Exercises and assignments for [COMP3400](https://my.uq.edu.au/programs-courses/course.html?course_code=COMP3400) (Functional Programming). Done in Semester 1, 2021 with lectures presented by Paul Vrbik.

## assignments

Assignments were relatively straightforward, with few questions. 
Where able, they were solved with great overcomplication and functional programming arcana ^_^

- **a1**: easy implementation of 
[elementary](https://github.com/katrinafyi/comp3400/blob/master/a1/src/ApplyAll.hs) 
[higher-order](https://github.com/katrinafyi/comp3400/blob/master/a1/src/CartProd.hs) 
[functions](https://github.com/katrinafyi/comp3400/blob/master/a1/src/Magic.hs), 
and a simple digit manipulation [exercise](https://github.com/katrinafyi/comp3400/blob/master/a1/src/Checksum.hs).
- **a2**: [arrangement](https://github.com/katrinafyi/comp3400/blob/master/a2/Blockus.hs) 
of tiles in a 2d grid with translation/rotation, 
brute-force propositional logic [tautology checker](https://github.com/katrinafyi/comp3400/blob/master/a2/Prop.hs) (using Free monad), and
structural [pattern matching](https://github.com/katrinafyi/comp3400/blob/master/a2/WordPattern.hs) for lists (?).
- **a3**: recursive [tree traversal](https://github.com/katrinafyi/comp3400/blob/master/a3/src/MaxPath.hs) for maximum path within binary tree, and [reimplementing](https://github.com/katrinafyi/comp3400/blob/master/a3/src/Stack.hs) a list.
- **a4**: [poker hand](https://github.com/katrinafyi/comp3400/blob/master/a4/src/Poker.hs) determination, [Potato game](https://github.com/katrinafyi/comp3400/blob/master/a4/src/Potato.hs) solving.

## exercism

The course used Exercism for weekly exercises, some were given each week as homework. 
All the exercises completed are in the [exercism](https://github.com/katrinafyi/comp3400/tree/master/exercism/haskell) folders.

These were, at times, quite interesting. We had motivation and freedom to explore the abstractions of Haskell.

A few highlights are:
- [DNA](https://github.com/katrinafyi/comp3400/blob/master/exercism/haskell/rna-transcription/src/DNA.hs): most simple use of traverse in parsing, fascinating in your first weeks of Haskell. 
- [Brackets](https://github.com/katrinafyi/comp3400/blob/master/exercism/haskell/matching-brackets/src/Brackets.hs): matching paired brackets in a string, a recursive algorithm with an implementation using foldr. 
- [Yacht](https://github.com/katrinafyi/comp3400/blob/master/exercism/haskell/yacht/src/Yacht.hs): scoring a mildly interesting dice game. 
- [Dominoes](https://github.com/katrinafyi/comp3400/blob/master/exercism/haskell/dominoes/src/Dominoes.hs): arranging dominoes such that they form a cycle.
- [Deque](https://github.com/katrinafyi/comp3400/blob/master/exercism/haskell/linked-list/src/Deque.hs): doubly-linked list using mutable state and IO.
- [Garden](https://github.com/katrinafyi/comp3400/blob/master/exercism/haskell/kindergarten-garden/src/Garden.hs): more complex parsing and logic to process input with transposition (also has a playground with Foldable and Applicative!).

