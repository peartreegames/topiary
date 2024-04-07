# CLI

The CLI tool is a very simple runner that can be used as a starting point. 
It will compile a file, and run your story in the terminal. 
It is recommended to start here to see how you can use topiary in your projects.

To run the CLI:

```shell
topi - command line topiary processor
Usage:
        topi [-v | --version] [-h | --help] <command> <file> [flags]

Commands:
        topi run <file> [start_bough] [--verbose]
        topi auto <file> <count> [--verbose]
        topi compile <file> <output_file|--dry|-d> [--verbose] 

Flags:
        --version, -v: Output current version
        --verbose: Output debug logs
        --dry, -d: Compile only
```

While running a topi file you can press `enter` for the next line, and select a choice with `0-9` then `enter`
