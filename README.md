# nixbot


# Ideas

## ~,~ should translate commands to nix expressions and evaluate them

ben: ,foo -> pretty (eval foo { nick = "ben"; } [])
ben: ,foo nick -> pretty (eval foo { nick = "ben"; } ["nick"])
ben: ,foo nick paul -> pretty (eval foo { nick = "ben"; } ["nick" "paul"])

But if invoked with ,foo and the result is a string, remove the quotes around it and do something about the other escapes (\n and co.). If the result isn't a string, try to pretty print the value

eval f def args with default args def should do the following:
- If f is a value:
  - If f is an attrset, args isn't empty and the first argument is an attribute of f, do return eval f.${head args} (tail args)
  - If f is an attrset, args isn't empty and the first argument isn't an attribute of f, but there are "similar" attributes, do a suggestion "Did you mean <>?"
  - If f is a list, args isn't empty and the first argument can be converted to an int which is an index of the list, do return eval (elemAt (head args) f) (tail args)
  - otherwise return f
- If f is a function and takes an attrset:
  - return eval (f <intersection of default args and used ones for f>) args
- If f is a function but doesn't take an attrset
  - Evaluate the function applied to all the arguments intercalated with a space
  - If this result is still a function, return eval (f (head args)) (tail args)
  - Else, return the result 



,foo = bar -> (define) foo = "bar"
,foo = x: bar -> (define) foo = "x: bar"
> foo = x: "${x}" -> (define) foo = x: "${x}"
