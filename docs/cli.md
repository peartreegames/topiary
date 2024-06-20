# CLI

The CLI tool is a very simple runner that can be used as a starting point. 
It will compile a file, and run your story in the terminal. 
It is recommended to start here to see how you can use topiary in your projects.

To run the CLI:

```shell
topi - command line topiary processor
Usage:
        topi <command> <file> [flags]

Commands:
        topi version
        topi run <file> [start_bough] [--auto|-a] [--lang language_key] [--verbose]
        topi test <file> <count> [--quiet] [--verbose]
        topi compile <file> <output_file|--dry|-d> [--loc] [--verbose] 
        topi loc validate <file> [--verbose]
        topi loc export <file> <output_file|--dry|-d> [--verbose]

Flags:
        --verbose: Output debug logs
        --auto, -a: Automatically continue to next line
        --lang: Localization language key
        --loc: Include localization in compiled bytecode
        --dry, -d: Compile only without output file
        --quiet: Do not output results
```

While running a topi file you can press `enter` for the next line, and select a choice with `0-9` then `enter`
