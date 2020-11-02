# Write up

## The original keynote we watched 

[William E Byrd - Relational Interpreters, Program Synthesis, Barliman](https://www.youtube.com/watch?v=RVDCRlW1f1Y)

## Takeaways

- Declarative programming

  - Talk about the properties of our programs

  - We get the exact properties we ask for

- Program synthesis

  - Generating functions by just talking about inputs and outputs
  
  - Functionality is implicitly produced from specifications

## Benefits of miniKanren family of languages

1. Easy to extend and modify.

2. Make writing declarative programs easy, imperative programs hard.

## Using microKanren

Demonstrate solving some basic equality / disjunction & conjunction relations.

Demonstrate a BST solver with miniKanren.

## Differences with Prolog

- Interleaving complete search VS DFS

- Cut and other effectful operators

## Implementation

- Usage of delay for disjunction
- Unify
- ===
- walk
- conj
- disj
- stream

## Extensions

[MiniKanren](http://minikanren.org/)

- extending to support miniKanren

- Program synthesis

- Quines

- Functions


**TODO** provide examples for the following:

- Disequality operator

- Temporal logic programming
- Constraint logic programming
- Probabilistic logic programming
- Nominal logic programming
