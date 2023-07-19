# Topiary

Topiary is a narrative scripting tool which uses the in the Topi language to write interactive stories.
It is designed to be integrated into video games which require a large amount of state and control flow in their narrative.

It is written in [Zig](https://ziglang.org).

## Usage

Currently Topiary can only be run as a CLI tool.
Simply download the appropiary binary then pass in a `[PATH]` argument.

You can then press `enter` for the next line, and select a choice with `0-9` then `enter`

Example:
```
zig build
./zig-out/bin/topi-cli ./examples/hello.topi
```

## Writing in Topi

You can view a full breakdown in the [syntax](https://github.com/peartreegames/topiary/blob/main/docs/syntax.md) file.

And examples in the [example folder](https://github.com/peartreegames/topiary/tree/main/examples)

## Future

Topiary is in early development and still has a long way to go.
Please check the Issues tab on the repo to see features I'd like to implement.

## Credits

First off this project never would have been attempted without first being inspired by [Ink](https://github.com/inkle/ink) and [Yarn Spinner](https://yarnspinner.dev). 

Secondly writing my own language would have never been attempted without the wonderfully written books on [Crafting Interpreters](https://craftinginterpreters.com) by Robert Nystrom and [Writing an Interpreter/Complier in Go](https://interpreterbook.com)
by Thorsten Ball. 

Lastly when I started on this project I was fairly new to Zig, so I have to thank two other projects [Luf](https://github.com/Luukdegram/luf/tree/master), 
and [LoLa](https://github.com/MasterQ32/LoLa/tree/master) both of which are credited in the Licence since their parsers were used as inspiration when starting out.

## Contributing

Download and install [Zig](https://ziglang.org). Currently Topiary uses version `0.11.0-dev.3777+64f0059cd`

```
git clone https://github.com/peartreegames/topiary
cd topiary
zig build
```

I'm confident there are many areas of improvement so please feel free to submit pull requests.
This is my first time writing a compiler and I'm still not sure I fully understand everything.
