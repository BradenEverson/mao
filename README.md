# `MaoLang`
## A language with rules you won't know, until it's too late

Inspired by Mao, a card game where the rules are not known, added randomly other players, and learnt by trial and error, `MaoLang` strives to be the most confusing language, with a dynamic syntax that changes upon each different compilation

A valid hello world in Mao could be:

```
auto message is "Hello, World";
println(message);
```

Or maybe it could be 

```
$val = "Hello!".

cond (:) alonside not :( ):
    puts(val).
end
```

Or maybe even 

```
new msg equals "Hey :D";
Console.WriteLine(msg);
```

The interpretter chooses at random each time by generating a seed and creating lexer, parser and evaluation rules each time you attempt to run your program.

These rules are chaotic, but consistent. Within a single attempt to run a program, variable assignment will always be `var` and printing will always be `print`, and as long as that file remains the exact same (Sha256 that ignores whitespace) those rules will remain. This means a valid `mao` program will always be valid, it's just excruciatingly difficult without brute force to find a valid program to begin.

## Installation

You can install the `mao` interpretter by running

```bash
cargo install maolang
```

In your terminal, it can then be run on a file using the 

```bash
mao {file}.mao
```

Command, good luck.
