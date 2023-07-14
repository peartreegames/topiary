# Topiary

Topiary is a narrative scripting tool which uses the in the Topi language to write interactive stories.
It is designed to be integrated into video games which require a large amount of state and control flow in their narrative.

It is written in [Zig](https://ziglang.org) and includes a C binary.

## Future

Topiary is in early development and still has a long way to go. 
Please check the Project tab on the repo to see the plans.

## Credits

First off this project never would have been attempted without first being inspired by [Ink](https://github.com/inkle/ink) and [Yarn Spinner](https://yarnspinner.dev). 

Secondly writing my own language would have never been attempted without the wonderfully written books on [Crafting Interpreters](https://craftinginterpreters.com) by Robert Nystrom and [Writing an Interpreter/Complier in Go](https://interpreterbook.com)
by Thorsten Ball. 

Lastly when I started on this project I was fairly new to Zig, so I have to thank two other projects [Luf](https://github.com/Luukdegram/luf/tree/master), 
and [LoLa](https://github.com/MasterQ32/LoLa/tree/master) both of which are credited in the Licence since their parsers were used as inspiration when starting out.

## Contributing

Download and install [Zig](https://ziglang.org). Currently Topiary uses 0.11.0-dev.3777+64f0059cd

```
git clone https://github.com/peartreegames/topiary
cd topiary
zig build
```

I'm confident there are many areas of improvement so please feel free to submit pull requests.
This is my first time writing a compiler and I'm still not sure I fully understand everything.