To make your current grammar.ebnf accept optimizations.rs, you must:

Extend keywords
Add "const" and "loop" to the Keyword nonterminal.

Add crate-level const items

Extend Item to include ConstItem.

Define ConstItem = "const" , Ident , ":" , Type , "=" , Expr , ";" ;.

Add the loop { ... } construct
Add a LoopExpr of the form "loop" , Block and include it as a case of Expr (or of your statement nonterminal, depending on your design).

Support % in multiplicative expressions
Add "%" to the multiplicative operator group, e.g. MulExpr = UnaryExpr , { ( "*" | "/" | "%" ) , UnaryExpr } ;.

Support macro calls (println!) if not already
Add Path , "!" , "(" , [ ArgList ] , ")" as a primary expression form, and define ArgList if necessary.

Ensure primitive types cover bool and i64
If missing, add "bool" and "i64" to your primitive type nonterminal.
