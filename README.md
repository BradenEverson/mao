# `MaoLang`
## A language with rules you won't know, until it's too late

Inspired by Mao, a card game where the rules are not known, added randomly other players, and learnt by trial and error, `MaoLang` strives to be the most confusing language, with a dynamic syntax that changes upon each compilation

A valid hello world in Mao could be:

```
auto message is "Hello, World";
println(message);
```

Or maybe it could be 

```
$val = "Hello!".

cond (👍 alonside not 👎):
    puts(val).
end
```

Or maybe even 

```
new msg equals "Hey :D";
Console.WriteLine(msg);
```

The interpretter chooses at random each time by generating a seed and creating lexer, parser and evaluation rules each time you attempt to run your program.
