PbParser started as a little implementation in Powerbuilder of 
a Shunting-Yard expression rewriter for infixed mathematical expressions 
to postfixed representations, followed by a simple stack machine evaluator.

It is evolving to something more powerful towards a modular evaluator for a corporate expression language:
- the evaluator can resolve Excel-like function calls and supports the usage of variables (resolved at execution time).
- strings are also supported that can be concatenated or used for functions like len().
- there is some coercion between types whene mixing string and numerical values for '+' operator: if the first operand is string, the second is 'stringified', and when the first operand is numeric, the string is converted to numeric when possible.

Supported types:
---------------
- integer
- decimal
- boolean
- string (single, double and back quoted)

Functions:
---------
- sum : summarize a list of operands
- mul : multiply the list of operands
- abs : absolute value
- min : select the minimal value from list
- max : select the maximum value from list
- len : returns the length from a string
- msgbox : displays a message

Examples of expressions :
-----------------------
- 33+42*2.5/(0.1-5)^2^3  -> 33,00031595167263415
- len(`string`) -> 6
- 2 - 1 <= 1 + 1 -> True
- +1 + --+1 -> 2
- min(len('a string')+7;-len("another");4.2)+42 -> 35
- 'aa' + 42 -> 'aa42'
- 40 + '2' -> 42
- so due to coercion len('aa'+42) -> 4 but len(40+'2') -> "len() can only take string argument at 1"
- 'aa'='bb' = false -> True
- 'aa'='bb' = vrai (vrai is a variable set to true) -> False
- msgbox('length = ' + len("message")) -. displays a message and return 1

The addition of new operators or functions needs currently to modify the code, but an attempt
to define seperate objects that allow clean declaration for new items is in progress in a dedicated branch.
