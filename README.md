# SC-Lang

## A simple monadic compiler for a simple imperative language

## What is a monadic compiler?

First we must know what parser combinators are...

Parser Combinators:
Parser combinators are a technique used in programming languages to build parsers. A parser is a program that takes input (usually text) and analyzes its structure according to a defined grammar. Parser combinators allow us to construct complex parsers by combining simpler parsers together.
Imagine you have a set of basic parsers, each capable of recognizing a specific language construct. With parser combinators, you can combine these basic parsers using operators and functions to create more complex parsers. For example, you can combine parsers for numbers, strings, and operators to create a parser for arithmetic expressions.

By combining these basic parsers in various ways, you can define the grammar of a language and parse input according to that grammar. This approach provides flexibility and expressiveness in building parsers, making it easier to handle complex languages and grammars.

Applicative Parsing:
Applicative parsing is a parsing technique that uses applicative functors to combine parsers. In applicative parsing, parsers are treated as values that can be applied to each other to produce more complex parsers.
Applicative parsers are composed using applicative combinators, such as sequencing, mapping, and alternative operators. These combinators allow you to combine parsers and specify how the results should be combined.

With applicative parsing, you can define the order in which parsers are applied and how their results are combined. It provides a way to express the structure and dependencies between different parts of the input.

Monadic Parsing:
Monadic parsing is another parsing technique that uses monads to combine parsers. Monads provide a more powerful and flexible way to handle parsing than applicative parsing.
In monadic parsing, parsers are combined using monadic operators, such as bind and sequencing operators. Monads allow for more dynamic and context-sensitive parsing, where the behavior of a parser can depend on the results of previous parsers.

Monadic parsing is considered more powerful than applicative parsing because monads provide more flexibility and control over the parsing process. Monads allow for dynamic decision-making, backtracking, and custom behavior during parsing, which makes it easier to handle complex grammars and language features.

With monadic parsing, you can express more advanced parsing strategies and handle cases where the behavior of a parser depends on the results of previous parsers. This flexibility comes at the cost of increased complexity and potential performance overhead compared to applicative parsing.

This monadic compiler written in Haskell is a program that takes source code written in a basic imperative language and translates it into a form that can be executed by a virtual machine. The compiler performs several steps to achieve this.

First, it generates an Abstract Syntax Tree (AST) from the source code. The AST represents the structure of the code in a more abstract and organized way.

Then, static analysis is performed on the AST. Static analysis involves examining the code without executing it to identify potential issues or optimizations. In this case, the compiler performs static analysis to identify and remove dead code, which is code that will never be executed.

Finally, the compiler produces bytecode, which is a low-level representation of the source code that can be executed by a virtual machine. The bytecode contains instructions that the virtual machine can interpret and execute.

The generated bytecode can then be run by a virtual machine


