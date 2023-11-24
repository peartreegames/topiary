# Topiary

Topiary is a narrative scripting tool which uses the Topi language to write interactive stories.
It is designed to be integrated into video games which require a large amount of state and control flow in their narrative.

Think of Topiary as programming a screenplay for your video game.

It is written in [Zig](https://ziglang.org).

## Usage

The CLI tool is a very simple runner that can be used as a starting point. 
It will compile a file, and run your story in the terminal.
It is recommended to start there to see how you can use topiary in your projects.

To run the CLI:

```
zig build
./zig-out/bin/topi-cli ./examples/hello.topi
```

You can then press `enter` for the next line,
and select a choice with `0-9` then `enter`

Docs on full capabilities and integration are still coming.

## Writing in Topi

You can view a full breakdown in the [syntax](https://github.com/peartreegames/topiary/blob/main/docs/syntax.md) file.
And examples in the [example folder](https://github.com/peartreegames/topiary/tree/main/examples)

Here you can see what writing in topi looks like

```
=== START {
    :User: "How does this work?"
    :Author: "You write your dialogue like this, then you can use the compiler and virtual matchine to run it in game."
    fork TOPIC {
        ~ "What does the compiler do?" {
            :Author: "The compiler will take your source code and convert it into bytecode."
            :Author: "With that you can either pass it directly to the virtual machine, or serialize it to save to a file."
            => TOPIC
        }
        ~ "What does the virtual machine do?" {
            :Author: "The virtual machine will take the bytecode and a runner and process each line of code, dialogue, and choices."
            :Author: "The runner has two functions, onDialogue, and onChoice. Which will be called and used to call your game code."
            => TOPIC
        }
        ~ "Okay, thanks." :Author: "Let me know in the issues if you have any questions!"
    }
}
=> START
```

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

Download and install [Zig](https://ziglang.org). Currently Topiary uses version `0.12.0-dev.1710+2bffd8101`

```
git clone https://github.com/peartreegames/topiary
cd topiary
zig build
```

I'm confident there are many areas of improvement so please feel free to submit pull requests.
This is my first time writing a compiler and I'm still not sure I fully understand everything.
