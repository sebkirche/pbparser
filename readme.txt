PbParser started as a little implementation in Powerbuilder of 
a Shunting-Yard expression rewriter for infixed mathematical expressions 
to postfixed representations, followed by a simple stack machine evaluator.

It is evolving to something more powerful towards a modular evaluator for a corporate expression language.

Supported types :
- integer
- decimal
- boolean
- string 

The evaluator can resolve Excel-like function calls and supports the usage of variables.

Strings are also supported (you can use single, double or back quotes) that can be concatenated
or used for functions like len().

The addition of new operators or functions needs currently to modify the code, but an attempt
to define seperate objects that allow clean declaration for new items is in progress in a dedicated branch.
