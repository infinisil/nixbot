let

a = builtins.addErrorContext "referenced by a" 10;

b = throw "b was undefined";

y = builtins.addErrorContext "referenced by y" (a + b);

z = builtins.addErrorContext "referenced by z" "Result is ${toString y}";

lib = import <nixpkgs/lib>;

tell = { nick }: target: "${target}: Hello there from ${nick}!";

eval = f: def: args: if ! builtins.isFunction f then f else let
  # Argument if we were to pass an attrset
  attrArg = builtins.intersectAttrs (builtins.functionArgs f) def;
  # Argument if we were to pass arguments from the argument list
  strArg = lib.concatStringsSep " " args;
  # We use the attrset one if we can pass at least one argument of our def set
  isAttrs = def != {} && attrArg != {};

  # Out determined argument applied to the function. If it's a str argument and we don't have any more arguments, throw an error
  applied = f (if isAttrs then attrArg else (if args == [] then throw "not enough arguments" else strArg));

  # If the arguments applied are still a function, recurse, otherwise use it as a result
  result = if builtins.isFunction applied then recurse else applied;

  # To be used if !isAttrs and we have another function as a result
  head = lib.head args;
  tail = lib.tail args;

  # If we applied our attributes, recurse with the applied value
  # Otherwise use the first element of the list as our new first argument for the function and pass the rest along
  recurse = if isAttrs then eval applied def args else eval (f head) def tail;
  
in result;

in
  # ,tell infinisil
  eval tell { nick = "paul"; } ["hi" "there"]
